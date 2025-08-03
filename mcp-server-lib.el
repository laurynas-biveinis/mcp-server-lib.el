;;; mcp-server-lib.el --- Model Context Protocol server library -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Keywords: comm, tools
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/laurynas-biveinis/mcp-server-lib.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library enables Emacs packages to expose their functionality to AI
;; applications via the Model Context Protocol (MCP).
;;
;; For users of MCP-enabled Emacs packages:
;; 1. Run M-x mcp-server-lib-install to install the stdio transport script
;; 2. Run M-x mcp-server-lib-start to start the MCP server
;; 3. Register your MCP server with an AI client using the installed script
;;    (see your specific MCP server's documentation for details)
;;
;; Additional commands:
;; - M-x mcp-server-lib-stop: Stop the MCP server
;; - M-x mcp-server-lib-show-metrics: View usage statistics
;; - M-x mcp-server-lib-uninstall: Remove the stdio transport script
;;
;; The library handles JSON-RPC 2.0 communication, manages tool and resource
;; registration, and provides error handling suitable for LLM interactions.
;;
;; See https://modelcontextprotocol.io/ for the protocol specification.

;;; Code:

(require 'json)
(require 'mcp-server-lib-metrics)

;;; Customization variables

(defgroup mcp-server-lib nil
  "Model Context Protocol for Emacs."
  :group 'comm
  :prefix "mcp-server-lib-")

(defcustom mcp-server-lib-log-io nil
  "If non-nil, log all JSON-RPC messages to the *mcp-server-lib-log* buffer."
  :group 'mcp-server-lib
  :type 'boolean)

(defcustom mcp-server-lib-install-directory user-emacs-directory
  "Directory where emacs-mcp-stdio.sh will be installed.
Defaults to `user-emacs-directory' but can be customized."
  :type 'directory
  :group 'mcp-server-lib)

;;; Public Constants

(defconst mcp-server-lib-name "emacs-mcp-server-lib"
  "Name of the MCP server.")

(defconst mcp-server-lib-protocol-version "2025-03-26"
  "Current MCP protocol version supported by this server.")

;;; Public API - JSON-RPC 2.0 Error Codes

(defconst mcp-server-lib-jsonrpc-error-parse -32700
  "JSON-RPC 2.0 Parse Error code.")

(defconst mcp-server-lib-jsonrpc-error-invalid-request -32600
  "JSON-RPC 2.0 Invalid Request error code.")

(defconst mcp-server-lib-jsonrpc-error-method-not-found -32601
  "JSON-RPC 2.0 Method Not Found error code.")

(defconst mcp-server-lib-jsonrpc-error-invalid-params -32602
  "JSON-RPC 2.0 Invalid Params error code.")

(defconst mcp-server-lib-jsonrpc-error-internal -32603
  "JSON-RPC 2.0 Internal Error code.")

;;; Internal Constants

(defconst mcp-server-lib--uri-scheme-regex
  "[a-zA-Z][a-zA-Z0-9+.-]*://"
  "Regex pattern matching URI scheme according to RFC 3986.
Matches scheme names that start with a letter followed by any combination
of letters, digits, plus, period, or hyphen, ending with ://")

(defconst mcp-server-lib--uri-scheme-start-regex
  (concat "^" mcp-server-lib--uri-scheme-regex)
  "Regex pattern matching URI scheme at start of string.")

(defconst mcp-server-lib--uri-with-scheme-regex
  (concat "\\`" mcp-server-lib--uri-scheme-regex ".+")
  "Regex pattern matching complete URI starting with scheme.")

;;; Internal global state variables

(defvar mcp-server-lib--running nil
  "Whether the MCP server is currently running.")

(defvar mcp-server-lib--tools (make-hash-table :test 'equal)
  "Hash table of registered MCP tools.")

(defvar mcp-server-lib--resources (make-hash-table :test 'equal)
  "Hash table of registered MCP resources.
Keys are URIs, values are plists with resource metadata and handlers.")

(defvar mcp-server-lib--resource-templates
  (make-hash-table :test 'equal)
  "Hash table of registered MCP resource templates.
Keys are URI templates, values are plists with template metadata and handlers.")

;;; Core helpers

(defun mcp-server-lib--jsonrpc-response (id result)
  "Create a JSON-RPC response with ID and RESULT."
  (json-encode `((jsonrpc . "2.0") (id . ,id) (result . ,result))))

(defun mcp-server-lib--extract-param-descriptions (docstring arglist)
  "Extract parameter descriptions from DOCSTRING based on ARGLIST.
The docstring should contain an \"MCP Parameters:\" section at the end,
with each parameter described as \"parameter-name - description\".
ARGLIST should be the function's argument list.
Returns an alist mapping parameter names to their descriptions.
Signals an error if a parameter is described multiple times,
doesn't match function arguments, or if any parameter is not documented."
  (let ((descriptions nil))
    (when docstring
      (when
          (string-match
           "MCP Parameters:[\n\r]+\\(\\(?:[ \t]+[^ \t\n\r].*[\n\r]*\\)*\\)"
           docstring)
        (let ((params-text (match-string 1 docstring))
              (param-regex
               "[ \t]+\\([^ \t\n\r]+\\)[ \t]*-[ \t]*\\(.*\\)[\n\r]*"))
          (with-temp-buffer
            (insert params-text)
            (goto-char (point-min))
            (while (re-search-forward param-regex nil t)
              (let ((param-name (match-string 1))
                    (param-desc (match-string 2)))
                ;; Check for duplicate parameter names
                (when (assoc param-name descriptions)
                  (error
                   "Duplicate parameter '%s' in MCP Parameters"
                   param-name))
                ;; Check parameter name matches function arguments
                (unless (and (= 1 (length arglist))
                             (symbolp (car arglist))
                             (string=
                              param-name (symbol-name (car arglist))))
                  (error
                   "Parameter '%s' in MCP Parameters not in function args %S"
                   param-name
                   arglist))
                ;; Add to descriptions
                (push (cons param-name (string-trim param-desc))
                      descriptions))))))
      ;; Check that all function parameters have descriptions
      (when (and (= 1 (length arglist))
                 (symbolp (car arglist))
                 (not (memq (car arglist) '(&optional &rest))))
        (let ((arg-name (symbol-name (car arglist))))
          (unless (assoc arg-name descriptions)
            (error
             "Function parameter '%s' missing from MCP Parameters section"
             arg-name)))))
    descriptions))

(defun mcp-server-lib--generate-schema-from-function (func)
  "Generate JSON schema by analyzing FUNC's signature.
Returns a schema object suitable for tool registration.
Supports functions with zero or one argument only.
Extracts parameter descriptions from the docstring if available."
  (let* ((arglist (help-function-arglist func t))
         (docstring (documentation func))
         (param-descriptions
          (mcp-server-lib--extract-param-descriptions
           docstring arglist)))
    (cond
     ;; No arguments case
     ((null arglist)
      '((type . "object")))

     ;; One argument case
     ((and (= 1 (length arglist))
           (symbolp (car arglist))
           (not (memq (car arglist) '(&optional &rest))))
      (let* ((arg-name (symbol-name (car arglist)))
             (description (cdr (assoc arg-name param-descriptions)))
             ;; Build property schema with type
             (property-schema `((type . "string")))
             ;; Add description if provided
             (property-schema
              (if description
                  (cons `(description . ,description) property-schema)
                property-schema)))
        `((type . "object")
          (properties . ((,arg-name . ,property-schema)))
          (required . [,arg-name]))))

     ;; Everything else is unsupported
     (t
      (error
       "Only functions with zero or one argument are supported")))))

(defun mcp-server-lib--ref-counted-register (key item table)
  "Register ITEM with KEY in TABLE with reference counting.
If KEY already exists, increment its reference count.
Otherwise, add ITEM to TABLE with :ref-count 1."
  (if-let* ((existing (gethash key table)))
    ;; Item already exists - increment ref count
    (let ((ref-count (or (plist-get existing :ref-count) 1)))
      (plist-put existing :ref-count (1+ ref-count)))
    ;; New item - ensure it has ref-count = 1
    (plist-put item :ref-count 1)
    (puthash key item table)))

(defun mcp-server-lib--ref-counted-unregister (key table)
  "Unregister item with KEY from TABLE using reference counting.
If reference count > 1, decrement it.
Otherwise, remove the item from TABLE.
Returns t if item was found, nil otherwise."
  (if-let* ((item (gethash key table))
            (ref-count (or (plist-get item :ref-count) 1)))
    (if (> ref-count 1)
        ;; Decrement ref count
        (progn
          (plist-put item :ref-count (1- ref-count))
          t)
      ;; Last reference - remove the item
      (remhash key table)
      t)))

(defun mcp-server-lib--jsonrpc-error (id code message)
  "Create a JSON-RPC error response with ID, error CODE and MESSAGE."
  (json-encode
   `((jsonrpc . "2.0")
     (id . ,id)
     (error . ((code . ,code) (message . ,message))))))

(defun mcp-server-lib--append-optional-fields (alist &rest pairs)
  "Append optional field PAIRS to ALIST.

PAIRS should be alternating keys and values.
Only adds a key-value pair if the value is non-nil.

Example:
  (mcp-server-lib--append-optional-fields
   \\='((uri . \"test://resource\") (name . \"Test\"))
   \\='description description-var
   \\='mimeType mime-type-var)

This adds description only if description-var is non-nil,
and mimeType only if mime-type-var is non-nil."
  (let ((additions nil))
    (while pairs
      (let ((key (pop pairs))
            (value (pop pairs)))
        (when value
          (push (cons key value) additions))))
    (append alist additions)))

(defun mcp-server-lib--respond-with-result
    (request-context result-data)
  "Send RESULT-DATA as response to the client through REQUEST-CONTEXT.

Arguments:
  REQUEST-CONTEXT  The MCP request context from the handler
  RESULT-DATA      The data to return to the client (any Elisp value)

The RESULT-DATA will be automatically converted to JSON-compatible format:
  - Strings, numbers, booleans are sent as-is
  - Symbols are converted to strings
  - Lists are converted to JSON arrays
  - Alists with string keys are converted to JSON objects
  - Other Elisp types are stringified appropriately

Example:
  ;; In a tool handler:
  (mcp-server-lib--respond-with-result
   context
   \\='((status . \"success\")
     (files . [\"file1.txt\" \"file2.txt\"])
     (count . 2)))"
  (let ((id (plist-get request-context :id)))
    (mcp-server-lib--jsonrpc-response id result-data)))

(defun mcp-server-lib--log-json-rpc (direction json-message)
  "Log JSON-RPC message in DIRECTION with JSON-MESSAGE.
DIRECTION should be \"in\" for incoming, \"out\" for outgoing."
  (when mcp-server-lib-log-io
    (let ((buffer (get-buffer-create "*mcp-server-lib-log*"))
          (direction-prefix
           (if (string= direction "in")
               "->"
             "<-"))
          (direction-name
           (if (string= direction "in")
               "(request)"
             "(response)")))
      (with-current-buffer buffer
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (view-mode 1)
          (insert
           (format "%s %s [%s]\n"
                   direction-prefix
                   direction-name
                   json-message)))))))

(defun mcp-server-lib--handle-error (err)
  "Handle error ERR in MCP process by logging and creating an error response.
Returns a JSON-RPC error response string for internal errors."
  (mcp-server-lib--jsonrpc-error
   nil
   mcp-server-lib-jsonrpc-error-internal
   (format "Internal error: %s" (error-message-string err))))

(defun mcp-server-lib--validate-and-dispatch-request (request)
  "Process a JSON-RPC REQUEST object and validate JSON-RPC 2.0 compliance.

REQUEST is a parsed JSON object (alist) containing the JSON-RPC request fields.

The function performs JSON-RPC 2.0 validation, checking:
- Protocol version (must be \"2.0\")
- ID field presence (required for regular requests, forbidden for notifications)
- Method field presence (always required)

If validation succeeds, dispatches the request to the appropriate handler.
Returns a JSON-RPC formatted response string, or nil for notifications."
  (let* ((jsonrpc (alist-get 'jsonrpc request))
         (id (alist-get 'id request))
         (method (alist-get 'method request))
         (params (alist-get 'params request))
         (is-notification
          (and method (string-prefix-p "notifications/" method))))
    ;; Check for JSON-RPC 2.0 compliance first
    (cond
     ;; Return error for non-2.0 requests
     ((not (equal jsonrpc "2.0"))
      (mcp-server-lib--jsonrpc-error
       id
       mcp-server-lib-jsonrpc-error-invalid-request
       "Invalid Request: Not JSON-RPC 2.0"))

     ;; Check if id is present for notifications/* methods
     ((and id is-notification)
      (mcp-server-lib--jsonrpc-error
       nil
       mcp-server-lib-jsonrpc-error-invalid-request
       "Invalid Request: Notifications must not include 'id' field"))
     ;; Check if id is missing
     ((and (not id) (not is-notification))
      (mcp-server-lib--jsonrpc-error
       nil
       mcp-server-lib-jsonrpc-error-invalid-request
       "Invalid Request: Missing required 'id' field"))
     ;; Check if method is missing
     ((not method)
      (mcp-server-lib--jsonrpc-error
       id
       mcp-server-lib-jsonrpc-error-invalid-request
       "Invalid Request: Missing required 'method' field"))

     ;; Process valid request
     (t
      (mcp-server-lib--dispatch-jsonrpc-method id method params)))))

;;; Resource Template Support

(defun mcp-server-lib--match-uri-template (uri parsed-template)
  "Match URI against PARSED-TEMPLATE.
Returns:
- alist of captured parameters if URI matches and has variables
- \\='match-no-params if URI matches but template has no variables
- nil if URI doesn't match template"
  (let ((segments (plist-get parsed-template :segments))
        (pos 0)
        (params '())
        (uri-len (length uri)))
    (catch 'no-match
      ;; Handle first segment with scheme - it's always literal
      (let* ((first-segment (car segments))
             (value (plist-get first-segment :value))
             (val-len (length value)))
        ;; First literal segment always contains scheme - handle
        ;; case-insensitive matching
        (unless (<= val-len uri-len)
          (throw 'no-match nil))
        (let* ((uri-part (substring uri 0 val-len))
               (scheme-end (string-match "://" value))
               (scheme-len (+ scheme-end 3)))
          (unless (and (>= (length uri-part) val-len)
                       (>= val-len scheme-len)
                       (let ((val-scheme
                              (substring value 0 scheme-len))
                             (uri-scheme
                              (substring uri-part 0 scheme-len)))
                         (string=
                          (downcase val-scheme)
                          (downcase uri-scheme)))
                       (string=
                        (substring value scheme-len)
                        (substring uri-part scheme-len)))
            (throw 'no-match nil))
          (setq pos val-len)
          (setq segments (cdr segments))))

      ;; Process remaining segments
      (dolist (segment segments)
        (let ((seg-type (plist-get segment :type)))
          (cond
           ;; Literal segment - must match exactly (case-sensitive)
           ((eq seg-type 'literal)
            (let* ((value (plist-get segment :value))
                   (val-len (length value)))
              (unless (and (<= (+ pos val-len) uri-len)
                           (string=
                            value
                            (substring uri pos (+ pos val-len))))
                (throw 'no-match nil))
              (setq pos (+ pos val-len))))
           ;; Variable segment - capture value
           ((eq seg-type 'variable)
            (let* ((var-name (plist-get segment :name))
                   ;; Find next segment to determine delimiter
                   (segment-index (seq-position segments segment))
                   (next-segment
                    (when (< (1+ segment-index) (length segments))
                      (nth (1+ segment-index) segments)))
                   (delimiter
                    (when (and next-segment
                               (eq
                                'literal
                                (plist-get next-segment :type)))
                      (plist-get next-segment :value))))
              (if delimiter
                  ;; Variable followed by literal - match until delimiter
                  (let ((end-pos
                         (string-match (regexp-quote delimiter) uri
                                       pos)))
                    (unless end-pos
                      (throw 'no-match nil))
                    (push (cons var-name (substring uri pos end-pos))
                          params)
                    (setq pos end-pos))
                ;; Variable at end - consume rest
                (push (cons var-name (substring uri pos)) params)
                (setq pos uri-len)))))))
      ;; Check we consumed entire URI
      (when (= pos uri-len)
        ;; Return params or 'match-no-params to distinguish from no match
        (if params
            (nreverse params)
          'match-no-params)))))

(defun mcp-server-lib--find-matching-template (uri)
  "Find a resource template that matches URI.
Returns cons of (template . params) or nil if no match found."
  (catch 'found
    (maphash
     (lambda (_template-pattern template-data)
       (let* ((parsed (plist-get template-data :parsed))
              (match-result
               (mcp-server-lib--match-uri-template uri parsed)))
         ;; match-result is nil for no match, 'match-no-params or alist
         (when match-result
           (throw 'found
                  (cons
                   template-data
                   (if (eq match-result 'match-no-params)
                       nil
                     match-result))))))
     mcp-server-lib--resource-templates)
    nil))

(defun mcp-server-lib--execute-resource-handler
    (resource-data uri handler-params id method-metrics)
  "Execute a resource handler and return the response.
RESOURCE-DATA is the plist containing handler and metadata.
URI is the resource URI.
HANDLER-PARAMS are parameters to pass to the handler (nil for direct resources).
ID is the request ID.
METHOD-METRICS is used to track errors."
  (condition-case err
      (let* ((handler (plist-get resource-data :handler))
             (mime-type (plist-get resource-data :mime-type))
             (content
              (if handler-params
                  (funcall handler handler-params)
                (funcall handler)))
             (content-entry
              (mcp-server-lib--append-optional-fields
               `((uri . ,uri) (text . ,content))
               'mimeType mime-type)))
        (mcp-server-lib--jsonrpc-response
         id `((contents . ,(vector content-entry)))))
    ;; Handle any error from the handler
    (error
     (cl-incf (mcp-server-lib-metrics-errors method-metrics))
     (mcp-server-lib--jsonrpc-error
      id mcp-server-lib-jsonrpc-error-internal
      (format "Error reading resource %s: %s"
              uri (error-message-string err))))))

(defun mcp-server-lib--handle-resources-read
    (id params method-metrics)
  "Handle resources/read request with ID and PARAMS.
METHOD-METRICS is used to track errors for this method."
  (let*
      ((uri (alist-get 'uri params))
       (resource (gethash uri mcp-server-lib--resources))
       ;; Try to find matching resource template if no direct resource
       (template-match
        (unless resource
          (mcp-server-lib--find-matching-template uri))))
    (cond
     ;; Direct resource found
     (resource
      (mcp-server-lib--execute-resource-handler
       resource uri nil id method-metrics))
     ;; Resource template match found
     (template-match
      (let ((template-data (car template-match))
            (params (cdr template-match)))
        (mcp-server-lib--execute-resource-handler
         template-data uri params id method-metrics)))
     ;; No resource or resource template found
     (t
      (mcp-server-lib--jsonrpc-error
       id
       mcp-server-lib-jsonrpc-error-invalid-params
       (format "Resource not found: %s" uri))))))

(defun mcp-server-lib--dispatch-jsonrpc-method (id method params)
  "Dispatch a JSON-RPC request to the appropriate handler.
ID is the JSON-RPC request ID to use in response.
METHOD is the JSON-RPC method name to dispatch.
PARAMS is the JSON-RPC params object from the request.
Returns a JSON-RPC response string for the request."
  (let ((method-metrics (mcp-server-lib-metrics--get method)))
    (cl-incf (mcp-server-lib-metrics-calls method-metrics))

    (cond
     ((equal method "initialize")
      (mcp-server-lib--handle-initialize id))
     ((equal method "notifications/initialized")
      (mcp-server-lib--handle-initialized)
      nil)
     ((equal method "notifications/cancelled")
      nil)
     ((equal method "tools/list")
      (mcp-server-lib--handle-tools-list id))
     ((equal method "resources/list")
      (mcp-server-lib--handle-resources-list id))
     ((equal method "resources/templates/list")
      (mcp-server-lib--handle-resources-templates-list id))
     ((equal method "resources/read")
      (mcp-server-lib--handle-resources-read
       id params method-metrics))
     ((equal method "tools/call")
      (mcp-server-lib--handle-tools-call id params method-metrics))
     (t
      (mcp-server-lib--jsonrpc-error
       id
       mcp-server-lib-jsonrpc-error-method-not-found
       (format "Method not found: %s" method))))))

;;; Notification handlers

(defun mcp-server-lib--handle-initialize (id)
  "Handle initialize request with ID.

This implements the MCP initialize handshake, which negotiates protocol
version and capabilities between the client and server."
  (let ((capabilities '()))
    ;; Only include tools capability if tools are registered
    (when (and mcp-server-lib--tools
               (> (hash-table-count mcp-server-lib--tools) 0))
      (push `(tools . ,(make-hash-table)) capabilities))
    ;; Include resources capability if resources are registered
    (when (or (and mcp-server-lib--resources
                   (> (hash-table-count mcp-server-lib--resources) 0))
              (and mcp-server-lib--resource-templates
                   (> (hash-table-count
                       mcp-server-lib--resource-templates)
                      0)))
      (push `(resources . ,(make-hash-table)) capabilities))
    (mcp-server-lib--jsonrpc-response
     id
     `((protocolVersion . ,mcp-server-lib-protocol-version)
       (serverInfo
        .
        ((name . ,mcp-server-lib-name)
         (version . ,mcp-server-lib-protocol-version)))
       (capabilities . ,capabilities)))))

(defun mcp-server-lib--handle-initialized ()
  "Handle initialized notification from client.

This is called after successful initialization to complete the handshake.
The client sends this notification to acknowledge the server's response
to the initialize request.")

(defun mcp-server-lib--handle-tools-list (id)
  "Handle tools/list request with ID.

Returns a list of all registered tools with their metadata."
  (let ((tool-list (vector)))
    (maphash
     (lambda (tool-id tool)
       (let* ((tool-description (plist-get tool :description))
              (tool-title (plist-get tool :title))
              (tool-read-only (plist-get tool :read-only))
              (tool-schema
               (or (plist-get tool :schema) '((type . "object"))))
              (tool-entry
               `((name . ,tool-id)
                 (description . ,tool-description)
                 (inputSchema . ,tool-schema)))
              (annotations nil))
         ;; Collect annotations if present
         (when tool-title
           (push (cons 'title tool-title) annotations))
         ;; Add readOnlyHint when :read-only is explicitly provided (both t
         ;; and nil)
         (when (plist-member tool :read-only)
           (let ((annot-value
                  (if tool-read-only
                      t
                    :json-false)))
             (push (cons 'readOnlyHint annot-value) annotations)))
         ;; Add annotations to tool entry if any exist
         (when annotations
           (setq tool-entry
                 (append tool-entry `((annotations . ,annotations)))))
         (setq tool-list (vconcat tool-list (vector tool-entry)))))
     mcp-server-lib--tools)
    (mcp-server-lib--jsonrpc-response id `((tools . ,tool-list)))))

(defun mcp-server-lib--build-resource-entry
    (uri-or-template resource-data is-template)
  "Build a resource entry for resources/list response.
URI-OR-TEMPLATE is the URI (for direct resources) or URI template.
RESOURCE-DATA is the plist containing resource metadata.
IS-TEMPLATE indicates whether this is a template resource."
  (let* ((name (plist-get resource-data :name))
         (description (plist-get resource-data :description))
         (mime-type (plist-get resource-data :mime-type))
         (uri-field
          (if is-template
              'uriTemplate
            'uri))
         (base-entry
          `((,uri-field . ,uri-or-template) (name . ,name))))
    (mcp-server-lib--append-optional-fields base-entry
                                            'description
                                            description
                                            'mimeType
                                            mime-type)))

(defun mcp-server-lib--collect-resources-from-hash
    (hash-table is-template)
  "Collect resource entries from HASH-TABLE.
IS-TEMPLATE indicates whether these are template resources.
Returns a vector of resource entries."
  (let ((entries (vector)))
    (maphash
     (lambda (uri-or-template resource-data)
       (setq entries
             (vconcat
              entries
              (vector
               (mcp-server-lib--build-resource-entry
                uri-or-template resource-data is-template)))))
     hash-table)
    entries))

(defun mcp-server-lib--handle-resources-list--COMBINED (id)
  "Handle resources/list request with ID.

Returns a list of all registered resources with their metadata."
  (let ((resource-list
         (vconcat
          ;; Direct resources
          (mcp-server-lib--collect-resources-from-hash
           mcp-server-lib--resources nil)
          ;; Resource templates
          (mcp-server-lib--collect-resources-from-hash
           mcp-server-lib--resource-templates t))))
    (mcp-server-lib--jsonrpc-response
     id `((resources . ,resource-list)))))

;; We need to handle the resources and resource templates separately:
(defun mcp-server-lib--handle-resources-list (id)
  "Handle resources/list request with ID - concrete resources only."
  (let ((resource-list
         (mcp-server-lib--collect-resources-from-hash
          mcp-server-lib--resources nil)))  ; Only concrete resources
    (mcp-server-lib--jsonrpc-response
     id `((resources . ,resource-list)))))

(defun mcp-server-lib--handle-resources-templates-list (id)
  "Handle resources/templates/list request with ID."
  (let ((templates
         (mcp-server-lib--collect-resources-from-hash
          mcp-server-lib--resource-templates t)))
    (mcp-server-lib--jsonrpc-response
     id `((resourceTemplates . ,templates)))))

(defun mcp-server-lib--handle-tools-call (id params method-metrics)
  "Handle tools/call request with ID and PARAMS.
METHOD-METRICS is used to track errors for this method."
  (let* ((tool-name (alist-get 'name params))
         (tool (gethash tool-name mcp-server-lib--tools))
         (tool-args (alist-get 'arguments params)))
    (if tool
        (let ((handler (plist-get tool :handler))
              (context (list :id id)))
          (condition-case err
              (let*
                  ((result
                    ;; Pass first arg value for single-string-arg tools
                    ;; when arguments are present
                    (if (and tool-args (not (equal tool-args '())))
                        (let ((first-arg-value (cdr (car tool-args))))
                          (funcall handler first-arg-value))
                      (funcall handler)))
                   ;; Ensure result is a string, convert nil to empty string
                   (result-text (or result ""))
                   ;; Wrap the handler result in the MCP format
                   (formatted-result
                    `((content
                       .
                       ,(vector
                         `((type . "text") (text . ,result-text))))
                      (isError . :json-false))))
                (mcp-server-lib-metrics--track-tool-call tool-name)
                (mcp-server-lib--respond-with-result
                 context formatted-result))
            ;; Handle tool-specific errors thrown with
            ;; mcp-server-lib-tool-throw
            (mcp-server-lib-tool-error
             (mcp-server-lib-metrics--track-tool-call tool-name t)
             (cl-incf (mcp-server-lib-metrics-errors method-metrics))
             (let ((formatted-error
                    `((content
                       .
                       ,(vector
                         `((type . "text") (text . ,(cadr err)))))
                      (isError . t))))
               (mcp-server-lib--respond-with-result
                context formatted-error)))
            ;; Keep existing handling for all other errors
            (error
             (mcp-server-lib-metrics--track-tool-call tool-name t)
             (cl-incf (mcp-server-lib-metrics-errors method-metrics))
             (mcp-server-lib--jsonrpc-error
              id mcp-server-lib-jsonrpc-error-internal
              (format "Internal error executing tool: %s"
                      (error-message-string err))))))
      (mcp-server-lib-metrics--track-tool-call tool-name t)
      (cl-incf (mcp-server-lib-metrics-errors method-metrics))
      (mcp-server-lib--jsonrpc-error
       id
       mcp-server-lib-jsonrpc-error-invalid-request
       (format "Tool not found: %s" tool-name)))))

;;; Error handling helpers

(defmacro mcp-server-lib-with-error-handling (&rest body)
  "Execute BODY with automatic error handling for MCP tools.

Any error that occurs during BODY execution is caught and converted to
an MCP tool error using `mcp-server-lib-tool-throw'.  This ensures
consistent error reporting to LLM clients.

Arguments:
  BODY  Forms to execute with error handling

Returns the result of BODY execution if successful.

Example:
  (defun my-tool-handler (path)
    \"Read and process a file at PATH.\"
    (mcp-server-lib-with-error-handling
      ;; Any errors here will be caught and reported properly
      (with-temp-buffer
        (insert-file-contents path)
        (process-buffer-contents))))

See also: `mcp-server-lib-tool-throw'"
  `(condition-case err
       (progn
         ,@body)
     (error
      (mcp-server-lib-tool-throw (format "Error: %S" err)))))

;;; Tool helpers

;;; API - Server


;;; API - Transport

(defun mcp-server-lib-process-jsonrpc (json-string)
  "Process a JSON-RPC message JSON-STRING and return the response.
This is the main entry point for stdio transport in MCP.

The function accepts a JSON-RPC 2.0 message string and returns
a JSON-RPC response string suitable for returning to clients via stdout.

When using the MCP server with emacsclient, invoke this function like:
emacsclient -e \\='(mcp-server-lib-process-jsonrpc \"[JSON-RPC message]\")\\='

Example:
  (mcp-server-lib-process-jsonrpc
   \"{\\\"jsonrpc\\\":\\\"2.0\\\",
     \\\"method\\\":\\\"mcp.server.describe\\\",\\\"id\\\":1}\")

See also: `mcp-server-lib-process-jsonrpc-parsed'"
  (unless mcp-server-lib--running
    (error
     "No active MCP server, start server with `mcp-server-lib-start' first"))

  (mcp-server-lib--log-json-rpc "in" json-string)

  ;; Step 1: Try to parse the JSON, handle parsing errors
  (let ((json-object nil)
        (response nil))
    ;; Attempt to parse the JSON
    (condition-case json-err
        (setq json-object (json-read-from-string json-string))
      (json-error
       ;; If JSON parsing fails, create a parse error response
       (setq response
             (mcp-server-lib--jsonrpc-error
              nil mcp-server-lib-jsonrpc-error-parse
              (format "Parse error: %s"
                      (error-message-string json-err))))))
    ;; Step 2: Process the request if JSON parsing succeeded
    (unless response
      (condition-case err
          (setq response
                (mcp-server-lib--validate-and-dispatch-request
                 json-object))
        (error
         (setq response (mcp-server-lib--handle-error err)))))

    ;; Only log and return responses when they exist (not for notifications)
    (when response
      (mcp-server-lib--log-json-rpc "out" response))
    response))

(defun mcp-server-lib-process-jsonrpc-parsed (request)
  "Send REQUEST to the MCP server and return parsed response.
REQUEST should be a JSON string containing a valid JSON-RPC 2.0 request.

Call `mcp-server-lib-process-jsonrpc' and return its result as a parsed alist.

Example:
  (let* ((request (mcp-server-lib-create-tools-list-request))
         (response (mcp-server-lib-process-jsonrpc-parsed request))
         (tools (alist-get \\='tools (alist-get \\='result response))))
    ;; tools is now an array of tool definitions
    (seq-doseq (tool tools)
      (message \"Tool: %s\" (alist-get \\='name tool))))

See also: `mcp-server-lib-process-jsonrpc'"
  (json-read-from-string (mcp-server-lib-process-jsonrpc request)))

;;; API - Utilities

(defun mcp-server-lib-create-tools-list-request (&optional id)
  "Create a tools/list JSON-RPC request with optional ID.
If ID is not provided, it defaults to 1."
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "tools/list")
     ("id" . ,(or id 1)))))

(defun mcp-server-lib-create-tools-call-request
    (tool-name &optional id args)
  "Create a tools/call JSON-RPC request for TOOL-NAME.
Optional ID and ARGS are also supported.
TOOL-NAME is the registered identifier of the tool to call.
ID is the JSON-RPC request ID, defaults to 1 if not provided.
ARGS is an association list of arguments to pass to the tool.

Example:
  (mcp-server-lib-create-tools-call-request
   \"list-files\" 42 \\='((\"path\" . \"/tmp\")))"
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "tools/call") ("id" . ,(or id 1))
     ("params" .
      (("name" . ,tool-name) ("arguments" . ,(or args '())))))))

(defun mcp-server-lib-create-resources-list-request (&optional id)
  "Create a resources/list JSON-RPC request with optional ID.
If ID is not provided, it defaults to 1."
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "resources/list")
     ("id" . ,(or id 1)))))

(defun mcp-server-lib-create-resources-read-request (uri &optional id)
  "Create a resources/read JSON-RPC request for URI with optional ID.
If ID is not provided, it defaults to 1.

Arguments:
  URI    Resource URI to read
  ID     Optional request ID (defaults to 1)

Example:
  (mcp-server-lib-create-resources-read-request \"test://resource\" 42)"
  (json-encode
   `(("jsonrpc" . "2.0")
     ("method" . "resources/read")
     ("id" . ,(or id 1))
     ("params" . (("uri" . ,uri))))))

;;; API - Tools

(defun mcp-server-lib-register-tool (handler &rest properties)
  "Register a tool with the MCP server.

Arguments:
  HANDLER          Function to handle tool invocations
  PROPERTIES       Property list with tool attributes

Required properties:
  :id              String identifier for the tool (e.g., \"list-files\")
  :description     String describing what the tool does

Optional properties:
  :title           User-friendly display name for the tool
  :read-only       If true, indicates tool doesn't modify its environment

The HANDLER function's signature determines its input schema.
Currently only no-argument and single-argument handlers are supported.

Example:
  ;; Simple tool with no arguments
  (mcp-server-lib-register-tool #\\='my-org-files-handler
    :id \"org-list-files\"
    :description \"Lists all available Org mode files for task management\")

  ;; With optional properties
  (mcp-server-lib-register-tool #\\='my-org-files-handler
    :id \"org-list-files\"
    :description \"Lists all available Org mode files for task management\"
    :title \"List Org Files\"
    :read-only t)

  ;; Tool with one argument - parameter description in docstring
  (defun my-file-reader (path)
    \"Read file at PATH.

MCP Parameters:
  path - absolute path to the file to read\"
    (mcp-server-lib-with-error-handling
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))))
  (mcp-server-lib-register-tool #\\='my-file-reader
    :id \"read-file\"
    :description \"Read contents of a file\")

See also:
  `mcp-server-lib-unregister-tool' - Remove a registered tool
  `mcp-server-lib-with-error-handling' - Error handling for tool handlers
  `mcp-server-lib-tool-throw' - Signal errors from tool handlers"
  (let* ((id (plist-get properties :id))
         (description (plist-get properties :description))
         (title (plist-get properties :title))
         (read-only (plist-get properties :read-only)))
    ;; Error checking for required properties
    (unless (functionp handler)
      (error "Tool registration requires handler function"))
    (unless id
      (error "Tool registration requires :id property"))
    (unless description
      (error "Tool registration requires :description property"))
    ;; Check for existing registration
    (if-let* ((existing (gethash id mcp-server-lib--tools)))
      (mcp-server-lib--ref-counted-register
       id existing mcp-server-lib--tools)
      (let* ((schema
              (mcp-server-lib--generate-schema-from-function handler))
             (tool
              (list
               :id id
               :description description
               :handler handler
               :schema schema)))
        ;; Add optional properties if provided
        (when title
          (setq tool (plist-put tool :title title)))
        ;; Always include :read-only if it was specified, even if nil
        (when (plist-member properties :read-only)
          (setq tool (plist-put tool :read-only read-only)))
        ;; Register the tool
        (mcp-server-lib--ref-counted-register
         id tool mcp-server-lib--tools)))))

(defun mcp-server-lib-unregister-tool (tool-id)
  "Unregister a tool with ID TOOL-ID from the MCP server.

Arguments:
  TOOL-ID  String identifier for the tool to unregister

Returns t if the tool was found and removed, nil otherwise.

Example:
  (mcp-server-lib-unregister-tool \"org-list-files\")

See also: `mcp-server-lib-register-tool'"
  (mcp-server-lib--ref-counted-unregister
   tool-id mcp-server-lib--tools))

;; Custom error type for tool errors
(define-error 'mcp-server-lib-tool-error "MCP tool error" 'user-error)

(defun mcp-server-lib-tool-throw (error-message)
  "Signal a tool error with ERROR-MESSAGE.
The error will be properly formatted and sent to the client.
This should be used within tool handlers to indicate failures.

Arguments:
  ERROR-MESSAGE  String describing the error

Example:
  (defun my-tool-handler (path)
    \"List files in PATH.

MCP Parameters:
  path - directory path to list\"
    (unless (file-directory-p path)
      (mcp-server-lib-tool-throw
       (format \"Not a directory: %s\" path)))
    ;; ... rest of implementation ...)

See also: `mcp-server-lib-with-error-handling'"
  (signal 'mcp-server-lib-tool-error (list error-message)))

(defun mcp-server-lib--parse-uri-template (template)
  "Parse URI TEMPLATE into segments.
Returns plist with :segments, :variables, and :pattern.
Supports RFC 6570 simple variables {var} and reserved expansion {+var}."
  ;; First check for balanced braces
  (let ((open-count 0)
        (close-count 0)
        (i 0))
    (while (< i (length template))
      (cond
       ((eq (aref template i) ?{)
        (setq open-count (1+ open-count)))
       ((eq (aref template i) ?})
        (setq close-count (1+ close-count))))
      (setq i (1+ i)))
    ;; Check for balanced braces after processing entire string
    (unless (= open-count close-count)
      (error "Unbalanced braces in resource template: %s" template)))

  (let ((segments '())
        (variables '())
        (pos 0)
        (len (length template)))
    ;; Process template character by character
    (while (< pos len)
      (if-let ((var-start (string-match "{" template pos)))
        ;; Found variable start
        (progn
          ;; Add literal segment before variable if any
          (when (> var-start pos)
            (push (list
                   :type 'literal
                   :value (substring template pos var-start))
                  segments))
          ;; Find variable end (guaranteed to exist due to balance check)
          (let* ((var-end (string-match "}" template var-start))
                 ;; Extract variable content
                 (var-content
                  (substring template (1+ var-start) var-end))
                 (reserved
                  (and (> (length var-content) 0)
                       (eq (aref var-content 0) ?+)))
                 (var-name
                  (if reserved
                      (substring var-content 1)
                    var-content)))
            ;; Validate variable name
            ;; RFC 6570: Variable names must start with ALPHA / "_"
            ;; and contain only ALPHA / DIGIT / "_" / pct-encoded
            (unless (string-match-p
                     "\\`[A-Za-z_][A-Za-z0-9_]*\\'" var-name)
              (error
               "Invalid variable name '%s' in resource template: %s"
               var-name
               template))
            ;; Add variable segment
            (push (list
                   :type 'variable
                   :name var-name
                   :reserved reserved)
                  segments)
            (push var-name variables)
            (setq pos (1+ var-end))))
        ;; No more variables, add remaining literal
        (when (< pos len)
          (push (list :type 'literal :value (substring template pos))
                segments))
        (setq pos len)))
    ;; Return parsed structure
    (list
     :segments (nreverse segments)
     :variables (nreverse variables)
     :pattern template)))

(defun mcp-server-lib--register-resource-internal
    (uri handler properties hash-table extra-props)
  "Internal helper for resource registration.
URI is the resource URI.
HANDLER is the handler function.
PROPERTIES is the plist of properties.
HASH-TABLE is either `mcp-server-lib--resources' or
`mcp-server-lib--resource-templates'.
EXTRA-PROPS is a plist of additional properties to include (e.g., :parsed)."
  (let ((name (plist-get properties :name))
        (description (plist-get properties :description))
        (mime-type (plist-get properties :mime-type)))
    ;; Error checking for required properties
    (unless (functionp handler)
      (error "Resource registration requires handler function"))
    (unless name
      (error "Resource registration requires :name property"))

    (if-let* ((existing (gethash uri hash-table)))
      (mcp-server-lib--ref-counted-register uri existing hash-table)
      (let ((entry
             (append (list :handler handler :name name) extra-props)))
        (when description
          (setq entry (plist-put entry :description description)))
        (when mime-type
          (setq entry (plist-put entry :mime-type mime-type)))
        (mcp-server-lib--ref-counted-register
         uri entry hash-table)))))

;;; API - Resources

(defun mcp-server-lib-register-resource (uri handler &rest properties)
  "Register a resource with the MCP server.

Automatically detects whether URI is a template based on presence of {}.

Arguments:
  URI              Resource URI or URI template (e.g., \"org://projects.org\"
                   or \"org://{filename}/outline\")
  HANDLER          Function that returns the content
                   - For static resources: takes no arguments
                   - For templates: takes params alist argument
  PROPERTIES       Property list with resource attributes

Required properties:
  :name            Human-readable name for the resource

Optional properties:
  :description     Description of the resource
  :mime-type       MIME type (default: \"text/plain\")

Examples:
  ;; Static resource
  (mcp-server-lib-register-resource
   \"org://projects.org\"
   (lambda ()
     (with-temp-buffer
       (insert-file-contents \"~/org/projects.org\")
       (buffer-string)))
   :name \"Projects\"
   :description \"Current project list\"
   :mime-type \"text/plain\")

  ;; Template resource
  (mcp-server-lib-register-resource
   \"org://{filename}/outline\"
   (lambda (params)
     (generate-outline
       (alist-get \"filename\" params nil nil #\\='string=)))
   :name \"Org file outline\"
   :description \"Hierarchical outline of an Org file\")

See also: `mcp-server-lib-unregister-resource'"
  ;; Check for proper URI structure: scheme://path
  (unless (string-match-p mcp-server-lib--uri-with-scheme-regex uri)
    (error "Resource URI must have format 'scheme://path': '%s'" uri))
  ;; Check for unmatched }
  (when (and (string-match-p "}" uri) (not (string-match-p "{" uri)))
    (error "Unmatched '}' in resource URI: %s" uri))
  ;; Check if this is a template by looking for unescaped {
  (if (string-match-p "{" uri)
      ;; It's a template - delegate to template logic
      (let ((parsed (mcp-server-lib--parse-uri-template uri)))
        (mcp-server-lib--register-resource-internal
         uri
         handler
         properties
         mcp-server-lib--resource-templates
         (list :parsed parsed)))
    ;; It's a static resource
    (mcp-server-lib--register-resource-internal
     uri handler properties mcp-server-lib--resources nil)))

(defun mcp-server-lib-unregister-resource (uri)
  "Unregister a resource by its URI.

Automatically detects whether URI is a template based on presence of {}.

Arguments:
    URI  The URI or URI template to unregister

Returns t if the resource was found and removed, nil otherwise.

Examples:
  (mcp-server-lib-unregister-resource \"org://projects.org\")
  (mcp-server-lib-unregister-resource \"org://{filename}/outline\")

See also: `mcp-server-lib-register-resource'"
  ;; Check if this is a template by looking for unescaped {
  (if (string-match-p "{" uri)
      ;; It's a template
      (mcp-server-lib--ref-counted-unregister
       uri mcp-server-lib--resource-templates)
    ;; It's a static resource
    (mcp-server-lib--ref-counted-unregister
     uri mcp-server-lib--resources)))

(provide 'mcp-server-lib)
;;; mcp-server-lib.el ends here
