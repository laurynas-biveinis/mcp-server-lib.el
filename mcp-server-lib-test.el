;;; mcp-server-lib-test.el --- Tests for mcp-server-lib.el -*- lexical-binding: t; -*-
;; jscpd:ignore-start

;; Copyright (C) 2025-2026 Laurynas Biveinis

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

;; ERT tests for mcp-server-lib.el.

;; jscpd:ignore-end

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mcp-server-lib)
(require 'mcp-server-lib-commands)
(require 'mcp-server-lib-metrics)
(require 'mcp-server-lib-ert)
(require 'json)

;;; Test helpers

(defun mcp-server-lib-test--register-server (&rest properties)
  "Call `mcp-server-lib-register-server' with :version pinned.
Most tests care only that registration succeeds, not about the server
:version.  When PROPERTIES omits :version, supply
`mcp-server-lib-ert-default-version' -- a fixed value the `initialize'
assertion helper also expects by default -- so test servers report a
deterministic version decoupled from the library default
`mcp-server-lib-default-server-version'.  PROPERTIES is otherwise
passed through unchanged, so validation and ref-count behavior are
exercised exactly as for a direct call."
  (unless (plist-member properties :version)
    (setq properties
          (append
           properties
           (list :version mcp-server-lib-ert-default-version))))
  (apply #'mcp-server-lib-register-server properties))

(defconst mcp-server-lib-test--repo-root
  (locate-dominating-file
   (or load-file-name buffer-file-name default-directory) "Eask")
  "Repository root directory, located by searching upward for `Eask'.
Used by meta tests that read project files (`Eask', the package
`.el' files, `NEWS') by name.")

(cl-assert
 (stringp mcp-server-lib-test--repo-root)
 t
 "Could not locate repo root (Eask not found)")

(defun mcp-server-lib-test--read-repo-file (relative-name)
  "Return the contents of RELATIVE-NAME under the repository root."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name relative-name mcp-server-lib-test--repo-root))
    (buffer-string)))

;;; Test data

(defconst mcp-server-lib-test--string-list-result "item1 item2 item3"
  "Test data for string list tool.")

(defconst mcp-server-lib-test--nonexistent-tool-id "non-existent-tool"
  "Tool ID for a non-existent tool used in tests.")

(defconst mcp-server-lib-test--unregister-tool-id "test-unregister"
  "Tool ID used for testing tool unregistration.")

(defconst mcp-server-lib-test--describe-setup-stopped-regexp
  (concat
   "\\`" ; Start of buffer
   "MCP Server Setup\n\n" "Status: Stopped\n\n" "\\'")
  "Regexp to match describe-setup output when server is stopped.")

(defconst mcp-server-lib-test--describe-setup-comprehensive-regexp
  (concat
   ;; Start of buffer, header and status
   "\\`MCP Server Setup\n\n"
   "Status: Running\n\n"
   ;; Servers section, alphabetical order: alpha, beta
   "Servers:\n"
   ;; Server alpha: instructions, three tools, three resources
   "\\s-+alpha\n"
   "\\s-+Instructions: Use the apple tool first.\n"
   "\\s-+Refcount: 1\n"
   "\\s-+Tools:\n"
   "\\s-+apple-tool\n"
   "\\s-+Description: Apple test tool\n"
   "\\s-+Title: Apple Tool Title\n"
   "\\s-+Handler: mcp-server-lib-test--tool-handler-empty-string\n"
   "\\s-+Usage: [0-9]+ calls\\(?:, [0-9]+ errors\\)?\n"
   "\\s-+mouse-tool\n"
   "\\s-+Description: Mouse test tool with lambda handler\n"
   "\\s-+Handler: closure\n"
   "\\s-+Usage: [0-9]+ calls\\(?:, [0-9]+ errors\\)?\n"
   "\\s-+zebra-tool\n"
   "\\s-+Description: Zebra test tool\n"
   "\\s-+Title: Zebra Tool Title\n"
   "\\s-+Read-only: t\n"
   "\\s-+Handler: mcp-server-lib-test--return-string\n"
   "\\s-+Usage: [0-9]+ calls\\(?:, [0-9]+ errors\\)?\n"
   "\\s-+Resources:\n"
   "\\s-+apple://resource\n"
   "\\s-+Name: Apple Resource\n"
   "\\s-+Description: Apple resource description with lambda handler\n"
   "\\s-+Mime-Type: application/json\n"
   "\\s-+Handler: closure\n"
   "\\s-+Usage: [0-9]+ calls\\(?:, [0-9]+ errors\\)?\n"
   "\\s-+mouse://resource\n"
   "\\s-+Name: Mouse Resource\n"
   "\\s-+Mime-Type: text/plain\n"
   "\\s-+Handler: mcp-server-lib-test--return-string\n"
   "\\s-+Usage: [0-9]+ calls\\(?:, [0-9]+ errors\\)?\n"
   "\\s-+zebra://resource\n"
   "\\s-+Name: Zebra Resource\n"
   "\\s-+Description: Zebra resource description\n"
   "\\s-+Mime-Type: text/plain\n"
   "\\s-+Handler: mcp-server-lib-test--return-string\n"
   "\\s-+Usage: [0-9]+ calls\\(?:, [0-9]+ errors\\)?\n"
   ;; Server beta: no instructions, one tool, one resource
   "\\s-+beta\n"
   "\\s-+Refcount: 1\n"
   "\\s-+Tools:\n"
   "\\s-+gamma-tool\n"
   "\\s-+Description: Gamma test tool\n"
   "\\s-+Handler: mcp-server-lib-test--return-string\n"
   "\\s-+Usage: [0-9]+ calls\\(?:, [0-9]+ errors\\)?\n"
   "\\s-+Resources:\n"
   "\\s-+gamma://resource\n"
   "\\s-+Name: Gamma Resource\n"
   "\\s-+Mime-Type: text/plain\n"
   "\\s-+Handler: mcp-server-lib-test--return-string\n"
   "\\s-+Usage: [0-9]+ calls\\(?:, [0-9]+ errors\\)?\n"
   "\\'")
  "Regexp matching describe-setup output with tools and resources.")

(defconst mcp-server-lib-test--describe-setup-empty-regexp
  (concat
   "\\`" ; Start of buffer
   "MCP Server Setup\n\n" "Status: Running\n\n" "\\'")
  "Regexp to match describe-setup output for empty state (no tools/resources).")

(defconst mcp-server-lib-test--describe-setup-nil-metrics-regexp
  (concat
   "\\`" ; Start of buffer
   "MCP Server Setup\n\n"
   "Status: Running\n\n"
   "Servers:\n"
   "  default\n"
   "    Refcount: 1\n"
   "    Tools:\n"
   "      test-tool\n"
   "        Description: Test tool\n"
   "        Handler: mcp-server-lib-test--return-string\n"
   "        Usage: 0 calls\n"
   "\\'")
  "Regexp to match describe-setup output with nil metrics (0 calls).")

;;; Generic test handlers

(defun mcp-server-lib-test--return-string ()
  "Generic handler to return a test string."
  "test result")

(defun mcp-server-lib-test--return-alternate-string ()
  "Generic handler returning a distinct string for discrimination."
  "alternate result")

(defun mcp-server-lib-test--generic-error-handler ()
  "Generic handler that throws an error for testing error handling."
  (error "Generic error occurred"))

(defun mcp-server-lib-test--handler-to-be-undefined ()
  "Generic handler that will be undefined after registration.
Used for testing behavior when handlers no longer exist."
  "Handler was defined when called")

(defun mcp-server-lib-test--return-nil ()
  "Generic handler to return nil."
  nil)

;;; Test tool handlers

(defun mcp-server-lib-test--tool-handler-mcp-server-lib-tool-throw ()
  "Test tool handler that always fails with `mcp-server-lib-tool-throw'."
  (mcp-server-lib-tool-throw "This tool intentionally fails"))

(defun mcp-server-lib-test--tool-handler-string-list ()
  "Test tool handler function to return a string with items."
  mcp-server-lib-test--string-list-result)

(defun mcp-server-lib-test--tool-handler-empty-string ()
  "Test tool handler function to return an empty string."
  "")

(defun mcp-server-lib-test--tool-handler-string-arg (input-string)
  "Test tool handler that accepts a string argument.
INPUT-STRING is the string argument passed to the tool.

MCP Parameters:
  input-string - test parameter for string input"
  (concat "Echo: " input-string))

(defun mcp-server-lib-test--tool-handler-duplicate-param
    (input-string)
  "Test handler with duplicate parameter.
INPUT-STRING is the string argument.

MCP Parameters:
  input-string - first description
  input-string - second description"
  (concat "Test: " input-string))

(defun mcp-server-lib-test--tool-handler-mismatched-param
    (input-string)
  "Test handler with mismatched parameter name.
INPUT-STRING is the string argument.

MCP Parameters:
  wrong-param-name - description for non-existent parameter"
  (concat "Test: " input-string))

(defun mcp-server-lib-test--tool-handler-missing-param (input-string)
  "Test handler with missing parameter documentation.
INPUT-STRING is the string argument.

MCP Parameters:"
  (concat "Test: " input-string))

(defun mcp-server-lib-test--tool-handler-two-params
    (first-name last-name)
  "Test handler with two parameters.
FIRST-NAME and LAST-NAME are the person's names.

MCP Parameters:
  first-name - Person's first name
  last-name - Person's last name"
  (format "Hello, %s %s!" first-name last-name))

(defun mcp-server-lib-test--tool-handler-three-params
    (title first-name last-name)
  "Test handler with three parameters.
TITLE, FIRST-NAME and LAST-NAME are the person's title and names.

MCP Parameters:
  title - Person's title (e.g. Mr, Ms, Dr)
  first-name - Person's first name
  last-name - Person's last name"
  (format "Hello, %s %s %s!" title first-name last-name))

(defun mcp-server-lib-test--tool-handler-one-optional-param
    (required-param &optional optional-param)
  "Test handler with one required and one optional parameter.
REQUIRED-PARAM is mandatory, OPTIONAL-PARAM is optional.

MCP Parameters:
  required-param - A required parameter
  optional-param - An optional parameter"
  (if optional-param
      (format "Required: %s, Optional: %s"
              required-param
              optional-param)
    (format "Required: %s" required-param)))

(defun mcp-server-lib-test--tool-handler-all-optional
    (&optional param-a param-b)
  "Test handler with all optional parameters.
PARAM-A and PARAM-B are both optional.

MCP Parameters:
  param-a - First optional parameter
  param-b - Second optional parameter"
  (cond
   ((and param-a param-b)
    (format "Both: %s, %s" param-a param-b))
   (param-a
    (format "Only A: %s" param-a))
   (param-b
    (format "Only B: %s" param-b))
   (t
    "None provided")))

(defun mcp-server-lib-test--tool-handler-some-optional
    (required-param &optional optional-a optional-b)
  "Test handler with one required and multiple optional parameters.
REQUIRED-PARAM is mandatory, OPTIONAL-A and OPTIONAL-B are optional.

MCP Parameters:
  required-param - A required parameter
  optional-a - First optional parameter
  optional-b - Second optional parameter"
  (format "Required: %s%s%s"
          required-param
          (if optional-a
              (format ", A: %s" optional-a)
            "")
          (if optional-b
              (format ", B: %s" optional-b)
            "")))

(defun mcp-server-lib-test--tool-handler-multiline-param (uri)
  "Test handler with multi-line parameter description containing hyphens.
URI is the resource identifier.

MCP Parameters:
  uri - URI of the headline (org-headline://{absolute-path}#{headline-path}
        or org-id://{id})"
  (format "Processing URI: %s" uri))

(defun mcp-server-lib-test--tool-handler-tab-indented-param
    (input-string)
  "Test handler with tab-indented parameter documentation.
INPUT-STRING is the test parameter.

MCP Parameters:
	input-string - parameter indented with tab character"
  (concat "Test: " input-string))

(defun mcp-server-lib-test--tool-handler-orphaned-continuation
    (param1)
  "Test handler with orphaned continuation line.
PARAM1 is a parameter.

MCP Parameters:
      This is an orphaned continuation line
  param1 - The actual parameter"
  (format "Result: %s" param1))

(defun mcp-server-lib-test--tool-handler-whitespace-around-hyphen
    (param1 param2)
  "Test handler with various whitespace around hyphens.
PARAM1 and PARAM2 are test parameters.

MCP Parameters:
  param1   -   multiple spaces around hyphen
  param2	-	tab characters around hyphen"
  (format "Results: %s, %s" param1 param2))

(defun mcp-server-lib-test--tool-handler-empty-continuation (param1)
  "Test handler with empty continuation line.
PARAM1 is a parameter.

MCP Parameters:
  param1 - first line

      second line after empty"
  (format "Result: %s" param1))

(defun mcp-server-lib-test--tool-handler-multiple-continuations
    (param1)
  "Test handler with multiple continuation lines.
PARAM1 is a parameter.

MCP Parameters:
  param1 - line one
      line two
      line three
      line four"
  (format "Result: %s" param1))

(defun mcp-server-lib-test--tool-handler-five-space-indent (param1)
  "Test handler with 5-space indentation line.
PARAM1 is a parameter.

MCP Parameters:
  param1 - description
     this line has 5 spaces and should be ignored
      but this continuation should work"
  (format "Result: %s" param1))

(defun mcp-server-lib-test--tool-handler-special-param-chars
    (param-name param_name param.name)
  "Test handler with special characters in parameter names.
PARAM-NAME, PARAM_NAME, and PARAM.NAME are test parameters.

MCP Parameters:
  param-name - parameter with hyphen
  param_name - parameter with underscore
  param.name - parameter with dot"
  (format "Results: %s, %s, %s" param-name param_name param.name))

(defun mcp-server-lib-test--tool-handler-returns-list ()
  "Test tool handler returning a list."
  '("item1" "item2" "item3"))

(defun mcp-server-lib-test--tool-handler-returns-vector ()
  "Test tool handler returning a vector."
  ["item1" "item2" "item3"])

(defun mcp-server-lib-test--tool-handler-returns-number ()
  "Test tool handler returning a number."
  42)

(defun mcp-server-lib-test--tool-handler-returns-symbol ()
  "Test tool handler that returning."
  'some-symbol)

(defun mcp-server-lib-test--tool-handler-with-rest (base &rest items)
  "Test handler with &rest parameter.
BASE is the base parameter.
ITEMS are additional items."
  (format "Base: %s, Items: %S" base items))

;; Bytecode handler function that will be loaded during tests
(declare-function mcp-server-lib-test-bytecode-handler--handler
                  "mcp-server-lib-bytecode-handler-test")

;;; Test resource template handlers

(defun mcp-server-lib-test--template-handler-error (_params)
  "Template handler to ignore PARAMS and throw an error."
  (error "Generic error occurred"))

(defun mcp-server-lib-test--resource-template-handler-dump-params
    (params)
  "Generic template handler that dumps the PARAMS alist."
  (format "params: %S" params))

(defun mcp-server-lib-test--resource-template-handler-dump-params-2
    (params)
  "Alternative template handler that dumps PARAMS."
  (format "Handler-2: params: %S" params))

(defun mcp-server-lib-test--resource-template-handler-nil (_params)
  "Test template handler that yields nil."
  nil)

(defun mcp-server-lib-test--resource-signal-error-invalid-params ()
  "Test handler that signals invalid params error."
  (mcp-server-lib-resource-signal-error
   mcp-server-lib-jsonrpc-error-invalid-params
   "Custom invalid params message"))

(defun mcp-server-lib-test--resource-signal-error-internal ()
  "Test handler that signals internal error."
  (mcp-server-lib-resource-signal-error
   mcp-server-lib-jsonrpc-error-internal
   "Database connection failed"))

;;; Test helpers

(defmacro mcp-server-lib-test--with-servers (server-specs &rest body)
  "Initialize multiple MCP servers and run BODY.
SERVER-SPECS is a list of (SERVER-ID :tools BOOL :resources BOOL
[:instructions STR-OR-NIL]) specs.

This macro:
1. Starts the MCP server with `mcp-server-lib-start'
2. For each SERVER-SPEC in order:
   a. Sends an `initialize' request under the spec's SERVER-ID
   b. Asserts the response shape against the spec's :tools,
      :resources, and :instructions values
   c. Sends the `initialized' notification
3. Executes BODY
4. Stops the server with `mcp-server-lib-stop'"
  (declare (indent 1) (debug t))
  `(unwind-protect
       (progn
         (mcp-server-lib-start)
         (dolist (spec ,server-specs)
           (cl-destructuring-bind
            (server-id &key tools resources instructions) spec
            (let ((mcp-server-lib-ert-server-id server-id))
              (mcp-server-lib-ert-assert-initialize-result
               (mcp-server-lib-ert--get-initialize-result)
               tools
               resources
               :instructions instructions))
            (should-not
             (mcp-server-lib-process-jsonrpc
              (json-encode
               '(("jsonrpc" . "2.0")
                 ("method" . "notifications/initialized")))
              server-id))))
         ,@body)
     (mcp-server-lib-stop)))

(defmacro mcp-server-lib-test--with-undefined-function
    (function-symbol &rest body)
  "Execute BODY with FUNCTION-SYMBOL undefined, then restore it.
FUNCTION-SYMBOL should be a quoted symbol.
The original function definition is saved and restored after BODY executes."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((original-def (symbol-function ,function-symbol)))
     (unwind-protect
         (progn
           (fmakunbound ,function-symbol)
           ,@body)
       (fset ,function-symbol original-def))))

(defmacro mcp-server-lib-test--with-request (method &rest body)
  "Execute BODY with MCP server active and verify METHOD metrics.
This macro:
1. Starts the MCP server
2. Captures metrics before BODY execution
3. Executes BODY
4. Verifies the method was called exactly once with no errors
5. Stops the server

IMPORTANT: This macro or `mcp-server-lib-ert-verify-req-success' MUST be used
for any successful request testing to ensure proper metric tracking."
  (declare (indent defun) (debug t))
  `(mcp-server-lib-ert-with-server
    :tools nil
    :resources
    nil
    (mcp-server-lib-ert-verify-req-success ,method ,@body)))

(defmacro mcp-server-lib-test--with-server (&rest plist-and-body)
  "Register a server, run BODY, then unregister.

PLIST-AND-BODY is the keyword arguments of
`mcp-server-lib-register-server' (\\='(:id :instructions :tools
:resources)\\=') followed by body forms.  The macro:

1. Calls `mcp-server-lib-register-server' with the keyword args.
2. Runs the body forms with `mcp-server-lib-ert-server-id' bound
   to the resolved :id.
3. In `unwind-protect' cleanup, calls
   `mcp-server-lib-unregister-server' on the registered :id (or
   \"default\" if :id was omitted)."
  (declare (indent defun) (debug t))
  (let ((plist '())
        (id-form nil)
        (body plist-and-body))
    (while (and body (keywordp (car body)))
      (let ((key (pop body))
            (value (pop body)))
        (if (eq key :id)
            (setq id-form value)
          (push key plist)
          (push value plist))))
    (setq plist (nreverse plist))
    (unless id-form
      (setq id-form "default"))
    (let ((id-var (gensym "id")))
      `(let ((,id-var ,id-form))
         (unwind-protect
             (progn
               (mcp-server-lib-test--register-server
                :id ,id-var ,@plist)
               (let ((mcp-server-lib-ert-server-id ,id-var))
                 ,@body))
           (mcp-server-lib-unregister-server ,id-var))))))

(defmacro mcp-server-lib-test--with-tools (tools &rest body)
  "Run BODY with MCP server active and TOOLS registered.
TOOLS is a list of tool specs `(HANDLER &rest PROPERTIES)' in the
shape accepted by `mcp-server-lib-register-server' under `:tools'.
All tools are registered in a single bundled call under server-id
\"default\" and automatically unregistered after BODY runs."
  (declare (indent 1) (debug t))
  `(mcp-server-lib-test--with-server
     :tools
     (list ,@ (mapcar (lambda (spec) `(list ,@spec)) tools))
     (mcp-server-lib-ert-with-server :tools t :resources nil ,@body)))

(defun mcp-server-lib-test--find-resource-by-uri (uri resources)
  "Find a resource in RESOURCES array by its URI field."
  (seq-find (lambda (r) (equal (alist-get 'uri r) uri)) resources))

(defun mcp-server-lib-test--find-resource-by-uri-template
    (uri-template resources)
  "Find a resource in RESOURCES array with URI-TEMPLATE field.
The URI-TEMPLATE is searched as uriTemplate in JSON."
  (seq-find
   (lambda (r)
     (equal (alist-get 'uriTemplate r) uri-template))
   resources))

;; These helpers run at macro-expansion time inside
;; `mcp-server-lib-test--with-resources', so they must be defined when the
;; file is byte-compiled standalone.
(eval-and-compile
  (defun mcp-server-lib-test--is-template-resource (resource-spec)
    "Return non-nil if RESOURCE-SPEC represents a template resource.
RESOURCE-SPEC is a list where the first element is the URI or template."
    (string-match-p "{" (car resource-spec)))

  (defun mcp-server-lib-test--build-resource-verification
      (resource-spec)
    "Build verification code for a single RESOURCE-SPEC.
RESOURCE-SPEC is a list of (URI HANDLER &rest PROPERTIES).
Returns a form that verifies the resource appears in --resource-list
with expected properties."
    (let* ((uri (car resource-spec))
           (props (cddr resource-spec))
           (name (plist-get props :name))
           (description (plist-get props :description))
           (mime-type (plist-get props :mime-type))
           (is-template (string-match-p "{" uri)))
      `(let
           ((--resource
             ,(if is-template
                  `(mcp-server-lib-test--find-resource-by-uri-template
                    ,uri --resource-list)
                `(mcp-server-lib-test--find-resource-by-uri
                  ,uri --resource-list))))
         (should --resource)
         (should
          (equal
           (alist-get
            ',(if is-template
                  'uriTemplate
                'uri)
            --resource)
           ,uri))
         (should (equal (alist-get 'name --resource) ,name))
         ,@
         (when description
           `((should
              (equal
               (alist-get 'description --resource) ,description))))
         ,@
         (when mime-type
           `((should
              (equal
               (alist-get 'mimeType --resource) ,mime-type))))))))

(defmacro mcp-server-lib-test--with-resources (resources &rest body)
  "Run BODY with MCP server active and RESOURCES registered.
RESOURCES is a list of resource specs `(URI HANDLER &rest PROPERTIES)'
in the shape accepted by `mcp-server-lib-register-server' under
`:resources'.  All resources are registered in a single bundled call
under server-id \"default\" and automatically unregistered after BODY
runs.

After registering all resources, automatically verifies that the
resource list contains exactly the registered resources with their
expected properties."
  (declare (indent 1) (debug t))
  ;; Build the verification code
  ;; Separate direct resources and templates
  (let*
      ((direct-resources
        (cl-remove-if
         #'mcp-server-lib-test--is-template-resource resources))
       (template-resources
        (cl-set-difference resources direct-resources :test #'equal))
       (verification-code
        `(progn
           ;; Verify direct resources in resources/list
           (let ((--resource-list
                  (mcp-server-lib-ert-get-resource-list)))
             ;; Check we have the expected number of DIRECT resources only
             (should
              (= ,(length direct-resources) (length --resource-list)))
             ;; Verify only direct resources appear in the list
             ,@
             (mapcar
              #'mcp-server-lib-test--build-resource-verification
              direct-resources))
           ;; Verify templates in resources/templates/list
           ,@
           (when template-resources
             `((let
                   ((--resource-list
                     (mcp-server-lib-ert-get-resource-templates-list)))
                 ;; Check we have the expected number of templates
                 (should
                  (= ,(length template-resources)
                     (length --resource-list)))
                 ;; Verify templates appear in the template list
                 ,@
                 (mapcar
                  #'mcp-server-lib-test--build-resource-verification
                  template-resources)))))))
    `(mcp-server-lib-test--with-server
       :resources
       (list ,@ (mapcar (lambda (spec) `(list ,@spec)) resources))
       (mcp-server-lib-ert-with-server
        :tools nil
        :resources
        t
        ,verification-code
        ,@body))))

(defun mcp-server-lib-test--emacs-error-message
    (error-symbol &rest data)
  "Return what `error-message-string' produces for (ERROR-SYMBOL DATA...).
Use instead of hardcoding messages that depend on the variable
`text-quoting-style'.
Do not use for `wrong-number-of-arguments': Emacs 29 and earlier strip
the `closure' head from the signaled function value, so reconstructing
the message from `symbol-function' does not match the runtime error.
Use `mcp-server-lib-test--wrong-args-message' instead."
  (error-message-string (cons error-symbol data)))

(defun mcp-server-lib-test--wrong-args-message (handler nargs)
  "Return `error-message-string' for calling HANDLER with NARGS arguments.
HANDLER must take an arity different from NARGS so the call signals
`wrong-number-of-arguments'.  Capturing the actual signal yields the
exact printed function representation the running Emacs produces; on
Emacs 29 and earlier, `funcall_lambda' strips the `closure' head from
the signaled function value, so reconstructing the message from
`symbol-function' would not match."
  (condition-case err
      (apply handler (make-list nargs nil))
    (wrong-number-of-arguments
     (error-message-string err))))

(defun mcp-server-lib-test--check-jsonrpc-error
    (request expected-code expected-message)
  "Test that REQUEST is rejected with EXPECTED-CODE and EXPECTED-MESSAGE."
  (let ((resp-obj
         (mcp-server-lib-process-jsonrpc-parsed
          request mcp-server-lib-ert-server-id)))
    (mcp-server-lib-ert-check-error-object
     resp-obj expected-code expected-message)))

(defun mcp-server-lib-test--check-invalid-jsonrpc-version (version)
  "Test that JSON-RPC request with VERSION is rejected properly."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--check-jsonrpc-error
    (json-encode
     `(("jsonrpc" . ,version) ("method" . "tools/list") ("id" . 42)))
    mcp-server-lib-jsonrpc-error-invalid-request
    "Invalid Request: Not JSON-RPC 2.0")))

(defun mcp-server-lib-test--call-tool (tool-id &optional id args)
  "Call a tool with TOOL-ID and return its successful result.
Optional ID is the JSON-RPC request ID (defaults to 1).
Optional ARGS is the association list of arguments to pass to the tool."
  (let* ((tool-metrics-key (format "tools/call:%s" tool-id))
         (tool-metrics (mcp-server-lib-metrics-get tool-metrics-key))
         (tool-calls-before
          (mcp-server-lib-metrics-calls tool-metrics))
         (tool-errors-before
          (mcp-server-lib-metrics-errors tool-metrics))
         (result
          (mcp-server-lib-ert-get-success-result
           "tools/call"
           (mcp-server-lib-create-tools-call-request
            tool-id id args))))
    (let ((tool-metrics-after
           (mcp-server-lib-metrics-get tool-metrics-key)))
      (should
       (= (1+ tool-calls-before)
          (mcp-server-lib-metrics-calls tool-metrics-after)))
      (should
       (= tool-errors-before
          (mcp-server-lib-metrics-errors tool-metrics-after))))
    result))

(defun mcp-server-lib-test--verify-tool-not-found (tool-id)
  "Verify a call to non-existent tool with TOOL-ID returning an error."
  (mcp-server-lib-test--check-jsonrpc-error
   (mcp-server-lib-create-tools-call-request tool-id 999)
   mcp-server-lib-jsonrpc-error-invalid-request
   (format "Tool not found: %s" tool-id)))

(defmacro mcp-server-lib-test--check-tool-call-error
    (tool-id &rest body)
  "Execute BODY and verify both call and error counts increased for TOOL-ID.
Creates a tools/call request and binds it to `request' for use in BODY.
Captures method and tool metrics before execution, executes BODY,
then verifies that both calls and errors increased by 1 at both levels."
  (declare (indent 1) (debug t))
  `(mcp-server-lib-ert-with-metrics-tracking
    (("tools/call" 1 1) ((format "tools/call:%s" ,tool-id) 1 1))
    (let ((request
           (mcp-server-lib-create-tools-call-request ,tool-id 999)))
      ,@body)))

(defun mcp-server-lib-test--get-tool-list ()
  "Get the successful response to a standard `tools/list` request."
  (let ((result
         (alist-get
          'tools
          (mcp-server-lib-ert-get-success-result
           "tools/list" (mcp-server-lib-create-tools-list-request)))))
    (should (arrayp result))
    result))

(defun mcp-server-lib-test--tool-name (tool)
  "Return the value of the `name' field in a TOOL alist."
  (alist-get 'name tool))

(defun mcp-server-lib-test--verify-list-counts
    (server-id tools resources templates)
  "Verify SERVER-ID's list endpoints return TOOLS, RESOURCES, TEMPLATES entries.
Sends tools/list, resources/list, and resources/templates/list requests
against SERVER-ID and asserts each response length matches the
corresponding expected count."
  (let ((mcp-server-lib-ert-server-id server-id))
    (should (= tools (length (mcp-server-lib-test--get-tool-list))))
    (should
     (= resources (length (mcp-server-lib-ert-get-resource-list))))
    (should
     (= templates
        (length (mcp-server-lib-ert-get-resource-templates-list))))))

(defun mcp-server-lib-test--verify-single-tool-param-desc
    (param-name expected-type expected-desc-regex)
  "Verify parameter type and description for a single tool.
Asserts that exactly one tool exists, parameter has correct type,
and description matches expected pattern.

PARAM-NAME: parameter name (symbol).
EXPECTED-TYPE: expected type string (e.g., \"string\").
EXPECTED-DESC-REGEX: regex that must match the description."
  (let* ((tools (mcp-server-lib-test--get-tool-list))
         (tool
          (progn
            (should (= 1 (length tools)))
            (aref tools 0)))
         (schema (alist-get 'inputSchema tool))
         (properties (alist-get 'properties schema))
         (param-prop (alist-get param-name properties)))
    (should (equal "object" (alist-get 'type schema)))
    (should param-prop)
    (should (equal expected-type (alist-get 'type param-prop)))
    (let ((desc (alist-get 'description param-prop)))
      (should (stringp desc))
      (should (string-match-p expected-desc-regex desc)))))

(defun mcp-server-lib-test--check-no-resources ()
  "Check that the resource list is empty."
  (let ((resources (mcp-server-lib-ert-get-resource-list)))
    (should (= 0 (length resources)))))

(defun mcp-server-lib-test--check-single-resource (expected-fields)
  "Check the resource list to contain exactly one resource with EXPECTED-FIELDS.
EXPECTED-FIELDS is an alist of (field . value) pairs to verify."
  (let ((resources (mcp-server-lib-ert-get-resource-list)))
    (should (= 1 (length resources)))
    (let ((resource (aref resources 0)))
      (should (= (length expected-fields) (length resource)))
      (dolist (field expected-fields)
        (should
         (equal (alist-get (car field) resource) (cdr field)))))))

(defun mcp-server-lib-test--check-templates (expected-templates)
  "Verify the template list against the EXPECTED-TEMPLATES set.
EXPECTED-TEMPLATES is a list of alists, where each alist contains
\(field . value) pairs to verify for one template.  The verification is
order-independent, matching templates by their \\='uriTemplate field."
  (let ((templates (mcp-server-lib-ert-get-resource-templates-list)))
    (should (= (length expected-templates) (length templates)))
    (dolist (expected-fields expected-templates)
      (let* ((uri-template (alist-get 'uriTemplate expected-fields))
             (template
              (mcp-server-lib-test--find-resource-by-uri-template
               uri-template templates)))
        (unless template
          (ert-fail
           (format
            "Template with uriTemplate '%s' not found in templates list"
            uri-template)))
        (should (= (length expected-fields) (length template)))
        (dolist (field expected-fields)
          (should
           (equal (alist-get (car field) template) (cdr field))))))))

(defmacro mcp-server-lib-test--check-resource-read-error
    (uri expected-code expected-message)
  "Read resource at URI and verify error response with metrics tracking.
Verifies that resources/read is called once with one error, and checks
that the error response has EXPECTED-CODE and EXPECTED-MESSAGE."
  (declare (indent 0) (debug t))
  `(mcp-server-lib-ert-with-metrics-tracking
    (("resources/read" 1 1))
    (mcp-server-lib-test--read-resource-error
     ,uri ,expected-code ,expected-message)))

(defun mcp-server-lib-test--check-resource-read-request-error
    (params expected-code expected-message)
  "Test that a resources/read request with PARAMS yields the expected error.
PARAMS is the params value to send in the JSON-RPC request.
EXPECTED-CODE is the expected error code.
EXPECTED-MESSAGE is the expected error message."
  (mcp-server-lib-test--check-jsonrpc-error
   (json-encode
    `((jsonrpc . "2.0")
      (id . 1)
      (method . "resources/read")
      (params . ,params)))
   expected-code expected-message))

(defun mcp-server-lib-test--read-resource-error
    (uri expected-code expected-message)
  "Read resource at URI expecting an EXPECTED-CODE with EXPECTED-MESSAGE.
EXPECTED-MESSAGE should be the exact error message string."
  (let ((response (mcp-server-lib-ert--read-resource uri)))
    ;; Check specific request ID for this resource read
    (should
     (equal
      mcp-server-lib-ert--resource-read-request-id
      (alist-get 'id response)))
    (mcp-server-lib-ert-check-error-object
     response expected-code expected-message)))

(defun mcp-server-lib-test--verify-tool-list-request (expected-tools)
  "Verify a `tools/list` response against EXPECTED-TOOLS.
EXPECTED-TOOLS should be an alist of (tool-name . tool-properties)."
  (let ((tools (mcp-server-lib-test--get-tool-list)))
    (should (= (length expected-tools) (length tools)))
    ;; Check each expected tool
    (dolist (expected expected-tools)
      (let* ((expected-name (car expected))
             (expected-props (cdr expected))
             (found-tool
              (seq-find
               (lambda (tool)
                 (string= expected-name (alist-get 'name tool)))
               tools)))
        (should found-tool)
        ;; Check expected properties
        (dolist (prop expected-props)
          (let ((prop-name (car prop))
                (prop-value (cdr prop)))
            (pcase prop-name
              ;; Special handling for nested annotations
              ('annotations
               (let ((annotations
                      (alist-get 'annotations found-tool)))
                 (should annotations)
                 (dolist (annot prop-value)
                   (should
                    (equal
                     (cdr annot)
                     (alist-get (car annot) annotations))))))
              ;; Regular property check
              (_
               (should
                (equal
                 prop-value (alist-get prop-name found-tool)))))))))))

(defun mcp-server-lib-test--verify-tool-schema-in-single-tool-list
    (param-specs &optional optional-param-names)
  "Verify schema of the only tool in the tool list.
PARAM-SPECS is a list of (NAME TYPE DESCRIPTION) for each parameter.
Empty list verifies a zero-parameter tool.

OPTIONAL-PARAM-NAMES is a list of parameter names that are optional.
If omitted, all parameters are treated as required."
  (let* ((tools (mcp-server-lib-test--get-tool-list))
         (tool (aref tools 0))
         (schema (alist-get 'inputSchema tool)))
    (should (equal "object" (alist-get 'type schema)))

    (if param-specs
        ;; One or more parameters
        (let*
            ((properties (alist-get 'properties schema))
             (required (alist-get 'required schema))
             (param-names (mapcar #'car param-specs))
             ;; Compute required params: all params minus optional ones
             (required-names
              (cl-remove-if
               (lambda (name)
                 (member name optional-param-names))
               param-names)))
          ;; Verify each parameter
          (dolist (spec param-specs)
            (let* ((name (nth 0 spec))
                   (type (nth 1 spec))
                   (desc (nth 2 spec))
                   (prop (alist-get (intern name) properties)))
              (should prop)
              (should (equal type (alist-get 'type prop)))
              (should (equal desc (alist-get 'description prop)))))

          ;; Verify required array contains expected required params
          (should (equal (vconcat required-names) required)))

      ;; Zero parameters
      (should-not (alist-get 'required schema))
      (should-not (alist-get 'properties schema)))))

(defun mcp-server-lib-test--check-mcp-server-lib-content-format
    (result expected-text)
  "Check that RESULT follows the MCP content format with EXPECTED-TEXT."
  (let* ((response `((result . ,result)))
         (text (mcp-server-lib-ert-check-text-response response)))
    (should (string= expected-text text))))

(defun mcp-server-lib-test--check-non-string-return-error
    (handler-func tool-id request-id expected-type)
  "Test that a non-string handler return throws a type-validation error.
HANDLER-FUNC is the test handler function that returns a non-string value.
TOOL-ID is the tool identifier string.
REQUEST-ID is the JSON-RPC request ID.
EXPECTED-TYPE is the expected type name in the error message."
  (mcp-server-lib-test--with-tools
      ((handler-func
        :id tool-id
        :description
        (format "A tool that returns %s (violates protocol)"
                expected-type)))
    (mcp-server-lib-test--check-jsonrpc-error
     (mcp-server-lib-create-tools-call-request tool-id request-id)
     mcp-server-lib-jsonrpc-error-invalid-params
     (format "Tool handler must return string or nil, got: %s"
             expected-type))))

(defmacro mcp-server-lib-test--do-describe-setup-test
    (pattern &rest body)
  "Execute BODY with MCP Server Setup buffer and assert it matches PATTERN.
PATTERN should be a regexp string to match against the buffer contents.
BODY forms are executed with the *MCP Server Setup* buffer as current buffer."
  (declare (indent 1) (debug t))
  `(unwind-protect
       (progn
         (mcp-server-lib-describe-setup)
         (with-current-buffer "*MCP Server Setup*"
           ,@body
           (let ((content (buffer-string)))
             (should (string-match-p ,pattern content)))))
     (when (get-buffer "*MCP Server Setup*")
       (kill-buffer "*MCP Server Setup*"))))

;;; Shared helpers for obsolete-API and server-lifecycle test boilerplate

(defconst mcp-server-lib-test--prior-bundle
  '(:id
    "default"
    :instructions "Prior instructions."
    :tools
    ((mcp-server-lib-test--return-string
      :id "prior-tool"
      :description "Prior tool"))
    :resources
    (("test://prior"
      mcp-server-lib-test--return-string
      :name "Prior resource")))
  "A `register-server' bundle for the atomicity preserve-prior-state tests.")

(defconst mcp-server-lib-test--invalid-resources-spec
  '(("test://valid" mcp-server-lib-test--return-string :name "Valid")
    ("test://invalid" "not-a-function" :name "Invalid handler"))
  "A :resources list whose second entry has a non-function handler.")

(defmacro mcp-server-lib-test--should-error-register (fn &rest args)
  "Assert obsolete registration FN called with ARGS errors in an empty server.
FN is `mcp-server-lib-register-tool' or `mcp-server-lib-register-resource';
its obsolescence warning is suppressed."
  (declare (debug t))
  `(with-suppressed-warnings ((obsolete ,fn))
     (mcp-server-lib-ert-with-server
      :tools nil
      :resources
      nil
      (should-error (,fn ,@args) :type 'error))))

(defmacro mcp-server-lib-test--with-obsolete-tool-api (&rest body)
  "Run BODY in an empty server with obsolete tool register/unregister suppressed."
  (declare (indent 0) (debug t))
  `(with-suppressed-warnings ((obsolete mcp-server-lib-register-tool)
                              (obsolete
                               mcp-server-lib-unregister-tool))
     (mcp-server-lib-ert-with-server
      :tools nil
      :resources
      nil
      ,@body)))

(defmacro mcp-server-lib-test--with-obsolete-resource-api (&rest body)
  "Run BODY in an empty server with obsolete resource calls suppressed."
  (declare (indent 0) (debug t))
  `(with-suppressed-warnings ((obsolete
                               mcp-server-lib-register-resource)
                              (obsolete
                               mcp-server-lib-unregister-resource))
     (mcp-server-lib-ert-with-server
      :tools nil
      :resources
      nil
      ,@body)))

(defmacro mcp-server-lib-test--with-temp-install-dir (&rest body)
  "Run BODY with a temporary install directory, binding `target'.
Binds `mcp-server-lib-install-directory' to a fresh temp directory and
`target' to `mcp-server-lib-installed-script-path', removing the directory
afterward."
  (declare (indent 0) (debug t))
  `(let* ((temp-dir (make-temp-file "mcp-test-" t))
          (mcp-server-lib-install-directory temp-dir)
          (target (mcp-server-lib-installed-script-path)))
     (unwind-protect
         (progn
           ,@body)
       (delete-directory temp-dir t))))

(defmacro mcp-server-lib-test--with-template-resources
    (resources &rest body)
  "Run BODY with RESOURCES registered via `--with-server' in an empty server."
  (declare (indent 1) (debug t))
  `(mcp-server-lib-ert-with-server
    :tools nil
    :resources nil
    (mcp-server-lib-test--with-server
      :resources
      ,resources
      ,@body)))

(defun mcp-server-lib-test--verify-default-server-empty ()
  "Assert the \"default\" server lists no tools, resources, or templates."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources
   nil
   (mcp-server-lib-test--verify-list-counts "default" 0 0 0)))

(defun mcp-server-lib-test--verify-counts-then-unregister
    (server-id tools resources templates)
  "Assert SERVER-ID lists TOOLS/RESOURCES/TEMPLATES counts, then unregister it once."
  (mcp-server-lib-test--verify-list-counts
   server-id tools resources templates)
  (mcp-server-lib-unregister-server server-id))

(defun mcp-server-lib-test--reregister-and-tear-down (bundle)
  "Re-register BUNDLE, unregister \"default\" twice, then assert it is empty."
  (apply #'mcp-server-lib-test--register-server bundle)
  (mcp-server-lib-unregister-server "default")
  (mcp-server-lib-unregister-server "default")
  (mcp-server-lib-test--verify-default-server-empty))

(defun mcp-server-lib-test--unregister-default-assert-no-instructions
    ()
  "Unregister \"default\" once, then assert initialize omits `instructions'.
`mcp-server-lib-ert-with-server' defaults to asserting `instructions' absent."
  (mcp-server-lib-unregister-server "default")
  (mcp-server-lib-ert-with-server :tools nil :resources nil))

(defun mcp-server-lib-test--call-tool-expect-invalid-params
    (tool-id args message)
  "Call TOOL-ID with ARGS; assert an invalid-params error carrying MESSAGE."
  (let ((response
         (mcp-server-lib-process-jsonrpc-parsed
          (mcp-server-lib-create-tools-call-request tool-id 42 args)
          mcp-server-lib-ert-server-id)))
    (mcp-server-lib-ert-check-error-object
     response mcp-server-lib-jsonrpc-error-invalid-params message)))

(defun mcp-server-lib-test--tools-list-parsed-response ()
  "Send tools/list in an empty server and return the parsed response."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-process-jsonrpc-parsed
    (mcp-server-lib-create-tools-list-request)
    mcp-server-lib-ert-server-id)))

(defun mcp-server-lib-test--assert-initialize-success (init-request)
  "Send INIT-REQUEST and assert a default-shaped successful initialize result."
  (mcp-server-lib-ert-assert-initialize-result
   (mcp-server-lib-ert-get-success-result "initialize" init-request)
   nil
   nil))

;;; Initialization and server capabilities tests

(ert-deftest mcp-server-lib-test-initialize-no-tools-no-resources ()
  "Test initialize when no tools or resources are registered.
When no tools or resources are registered, the capabilities object
should not include tools or resources fields at all."
  (mcp-server-lib-ert-with-server :tools nil :resources nil))

(ert-deftest
    mcp-server-lib-test-initialize-empty-capabilities-emits-object
    ()
  "Empty `capabilities' field serializes as JSON object, not null.
MCP 2025-03-26 schema requires `capabilities' to be a JSON object;
without coercion, `json-encode' serializes the empty Elisp list as
JSON null, which violates the spec."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 30)
               ("params" .
                (("protocolVersion"
                  .
                  ,mcp-server-lib-protocol-version)
                 ("capabilities" . ,(make-hash-table)))))))
           (raw
            (mcp-server-lib-process-jsonrpc
             init-request mcp-server-lib-ert-server-id)))
      (should (string-match-p "\"capabilities\":{}" raw))
      (should-not (string-match-p "\"capabilities\":null" raw)))))

(ert-deftest mcp-server-lib-test-initialize-with-tools-and-resources
    ()
  "Test initialize when both tools and resources are registered.
When both are registered, capabilities should include both fields."
  (mcp-server-lib-test--with-server
    :tools
    '((mcp-server-lib-test--return-string
       :id "test-tool"
       :description "Test tool"))
    :resources
    '(("test://resource"
       mcp-server-lib-test--return-string
       :name "Test Resource"))
    (mcp-server-lib-ert-with-server :tools t :resources t)))


(ert-deftest mcp-server-lib-test-initialize-old-protocol-version ()
  "Test server responds with its version for older client version."
  (mcp-server-lib-test--with-request "initialize"
    (mcp-server-lib-test--assert-initialize-success
     (json-encode
      `(("jsonrpc" . "2.0")
        ("method" . "initialize") ("id" . 16)
        ("params" .
         (("protocolVersion" . "2024-11-05")
          ("capabilities" . ,(make-hash-table)))))))))

(ert-deftest mcp-server-lib-test-initialize-missing-protocol-version
    ()
  "Test initialize request without protocolVersion field."
  (mcp-server-lib-test--with-request "initialize"
    (mcp-server-lib-test--assert-initialize-success
     (json-encode
      `(("jsonrpc" . "2.0")
        ("method" . "initialize")
        ("id" . 17)
        ("params" . (("capabilities" . ,(make-hash-table)))))))))

(ert-deftest
    mcp-server-lib-test-initialize-non-string-protocol-version
    ()
  "Test initialize request with non-string protocolVersion."
  (mcp-server-lib-test--with-request "initialize"
    (mcp-server-lib-test--assert-initialize-success
     (json-encode
      `(("jsonrpc" . "2.0")
        ("method" . "initialize") ("id" . 18)
        ("params" .
         (("protocolVersion" . 123) ; Number instead of string
          ("capabilities" . ,(make-hash-table)))))))))

(ert-deftest mcp-server-lib-test-initialize-malformed-params ()
  "Test initialize request with completely malformed params."
  ;; Test with params as a string instead of object
  (mcp-server-lib-test--with-request "initialize"
    (mcp-server-lib-test--assert-initialize-success
     (json-encode
      `(("jsonrpc" . "2.0")
        ("method" . "initialize")
        ("id" . 19)
        ("params" . "malformed"))))))

(ert-deftest mcp-server-lib-test-initialize-missing-params ()
  "Test initialize request without params field."
  (mcp-server-lib-test--with-request "initialize"
    (mcp-server-lib-test--assert-initialize-success
     (json-encode
      `(("jsonrpc" . "2.0") ("method" . "initialize") ("id" . 20))))))

(ert-deftest mcp-server-lib-test-initialize-null-protocol-version ()
  "Test initialize request with null protocolVersion."
  (mcp-server-lib-test--with-request "initialize"
    (mcp-server-lib-test--assert-initialize-success
     (json-encode
      `(("jsonrpc" . "2.0")
        ("method" . "initialize") ("id" . 21)
        ("params" .
         (("protocolVersion" . :json-null)
          ("capabilities" . ,(make-hash-table)))))))))

(ert-deftest mcp-server-lib-test-initialize-empty-protocol-version ()
  "Test initialize request with empty string protocolVersion."
  (mcp-server-lib-test--with-request "initialize"
    (mcp-server-lib-test--assert-initialize-success
     (json-encode
      `(("jsonrpc" . "2.0")
        ("method" . "initialize") ("id" . 22)
        ("params" .
         (("protocolVersion" . "")
          ("capabilities" . ,(make-hash-table)))))))))

(ert-deftest
    mcp-server-lib-test-initialize-with-valid-client-capabilities
    ()
  "Test initialize request with valid client capabilities."
  (mcp-server-lib-test--with-request "initialize"
    (mcp-server-lib-test--assert-initialize-success
     (json-encode
      `(("jsonrpc" . "2.0")
        ("method" . "initialize") ("id" . 23)
        ("params" .
         (("protocolVersion" . ,mcp-server-lib-protocol-version)
          ("capabilities" .
           (("roots" . ,(make-hash-table))
            ("sampling" . ,(make-hash-table))
            ("experimental" . ,(make-hash-table)))))))))))

;;; `mcp-server-lib-register-tool' tests

(ert-deftest mcp-server-lib-test-register-tool-error-missing-id ()
  "Test that tool registration with missing :id produces an error."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--return-string
      :description "Test tool without ID")
     :type 'error)))

(ert-deftest
    mcp-server-lib-test-register-tool-error-missing-description
    ()
  "Test that tool registration with missing :description produces an error."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--return-string
      :id "test-tool-no-desc")
     :type 'error)))

(ert-deftest mcp-server-lib-test-register-tool-error-missing-handler
    ()
  "Test that tool registration with non-function handler produces an error."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      "not-a-function"
      :id "test-tool-bad-handler"
      :description "Test tool with invalid handler")
     :type 'error)))

(ert-deftest
    mcp-server-lib-test-register-tool-error-duplicate-param-description
    ()
  "Test that duplicate parameter descriptions cause an error."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--tool-handler-duplicate-param
      :id "duplicate-param-tool"
      :description "Tool with duplicate parameter")
     :type 'error)))

(ert-deftest mcp-server-lib-test-register-tool-error-mismatched-param
    ()
  "Test that parameter names must match function arguments."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--tool-handler-mismatched-param
      :id "mismatched-param-tool"
      :description "Tool with mismatched parameter")
     :type 'error)))

(ert-deftest mcp-server-lib-test-register-tool-error-missing-param ()
  "Test that all function parameters must be documented."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--tool-handler-missing-param
      :id "missing-param-tool"
      :description "Tool with missing parameter docs")
     :type 'error)))

(ert-deftest mcp-server-lib-test-register-tool-error-rest-params ()
  "Test that tools with &rest parameters are rejected."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (let ((err
           (should-error
            (mcp-server-lib-register-tool
             #'mcp-server-lib-test--tool-handler-with-rest
             :id "rest-param-tool"
             :description "Tool with rest parameters")
            :type 'error)))
      (should
       (string-match-p
        "MCP tool handlers do not support &rest parameters"
        (error-message-string err))))))

(ert-deftest mcp-server-lib-test-register-tool-error-non-string-id ()
  "Test that non-string :id is rejected."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--return-string
      :id 42
      :description "Tool with non-string id")
     :type 'error)))

(ert-deftest
    mcp-server-lib-test-register-tool-error-non-string-description
    ()
  "Test that non-string :description is rejected."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--return-string
      :id "non-string-desc"
      :description 42)
     :type 'error)))

(ert-deftest mcp-server-lib-test-register-tool-error-non-string-title
    ()
  "Test that non-string :title is rejected."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--return-string
      :id "non-string-title"
      :description "Tool with non-string title"
      :title 42)
     :type 'error)))

(ert-deftest mcp-server-lib-test-register-tool-multiline-param ()
  "Test that multi-line parameter descriptions with hyphens parse correctly.
Regression test for bug where continuation lines with hyphenated words
like 'org-id' were incorrectly parsed as new parameter definitions."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-multiline-param
        :id "multiline-param-tool"
        :description "Tool with multi-line parameter description"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-single-tool-param-desc
      'uri "string" ".*org-headline://.*org-id://.*"))))

(ert-deftest
    mcp-server-lib-test-register-tool-error-tab-indented-param
    ()
  "Test that tab-indented parameters are rejected.
Regression test to ensure spaces-only indentation requirement is enforced."
  (mcp-server-lib-test--should-error-register
   mcp-server-lib-register-tool
   #'mcp-server-lib-test--tool-handler-tab-indented-param
   :id "tab-test"
   :description "Tool with tab-indented parameter"))

(ert-deftest
    mcp-server-lib-test-register-tool-error-orphaned-continuation
    ()
  "Test that orphaned continuation lines cause an error.
Regression test to ensure continuation lines before any parameter
definition are detected and reported with a clear error message."
  (mcp-server-lib-test--should-error-register
   mcp-server-lib-register-tool
   #'mcp-server-lib-test--tool-handler-orphaned-continuation
   :id "orphaned-test"
   :description "Tool with orphaned continuation"))

(ert-deftest
    mcp-server-lib-test-register-tool-whitespace-around-hyphen
    ()
  "Test that various whitespace around hyphens is handled correctly.
Regression test to ensure the parser accepts spaces and tabs around
the hyphen separator in parameter definitions."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-whitespace-around-hyphen
        :id "whitespace-hyphen-tool"
        :description "Tool with whitespace around hyphens"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-tool-schema-in-single-tool-list
      '(("param1" "string" "multiple spaces around hyphen")
        ("param2" "string" "tab characters around hyphen"))))))

(ert-deftest mcp-server-lib-test-register-tool-empty-continuation ()
  "Test that empty continuation lines are handled correctly.
Regression test to ensure lines with only continuation indentation
are processed without errors."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-empty-continuation
        :id "empty-continuation-tool"
        :description "Tool with empty continuation line"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-single-tool-param-desc
      'param1 "string" "^first line second line after empty$"))))

(ert-deftest mcp-server-lib-test-register-tool-multiple-continuations
    ()
  "Test that multiple continuation lines are concatenated correctly.
Regression test to ensure parser handles 3+ continuation lines
and concatenates them with spaces."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-multiple-continuations
        :id "multiple-continuations-tool"
        :description "Tool with multiple continuation lines"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-single-tool-param-desc
      'param1 "string" "^line one line two line three line four$"))))

(ert-deftest mcp-server-lib-test-register-tool-five-space-indent ()
  "Test that 5-space indentation lines are silently ignored.
Regression test to ensure lines with 5 spaces (between parameter 2-4
and continuation 6+) are skipped but parsing continues."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-five-space-indent
        :id "five-space-indent-tool"
        :description "Tool with 5-space indentation"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-single-tool-param-desc
      'param1
      "string"
      "^description but this continuation should work$"))))

(ert-deftest mcp-server-lib-test-register-tool-special-param-chars ()
  "Test that special characters in parameter names are handled correctly.
Regression test to ensure parser accepts valid Elisp identifier characters
like hyphens, underscores, and dots in parameter names."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-special-param-chars
        :id "special-param-chars-tool"
        :description "Tool with special chars in parameter names"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-tool-schema-in-single-tool-list
      '(("param-name" "string" "parameter with hyphen")
        ("param_name" "string" "parameter with underscore")
        ("param.name" "string" "parameter with dot"))))))

(ert-deftest mcp-server-lib-test-register-tool-error-duplicate-id ()
  "Test reference counting behavior when registering a tool with duplicate ID.
With reference counting, duplicate registrations should succeed and increment
the reference count, returning the original tool definition."
  (mcp-server-lib-test--with-obsolete-tool-api
    (unwind-protect
        (progn
          (mcp-server-lib-register-tool
           #'mcp-server-lib-test--return-string
           :id "duplicate-test"
           :description "First registration")
          (unwind-protect
              (progn
                (mcp-server-lib-register-tool
                 #'mcp-server-lib-test--return-string
                 :id "duplicate-test"
                 :description "Second registration - should be ignored")
                ;; Tool should be callable after registrations (ref count = 2).
                (let ((result
                       (mcp-server-lib-test--call-tool
                        "duplicate-test"
                        1)))
                  (mcp-server-lib-test--check-mcp-server-lib-content-format
                   result "test result")))
            (mcp-server-lib-unregister-tool "duplicate-test"))
          ;; After inner unregister (ref count 2 -> 1); tool should still
          ;; be callable because outer registration is still active.
          (let ((result
                 (mcp-server-lib-test--call-tool "duplicate-test" 2)))
            (mcp-server-lib-test--check-mcp-server-lib-content-format
             result "test result")))
      (mcp-server-lib-unregister-tool "duplicate-test"))
    ;; After outer unregister (ref count 1 -> 0); tool should no longer
    ;; be callable.
    (mcp-server-lib-test--verify-tool-not-found "duplicate-test")))

(ert-deftest mcp-server-lib-test-register-tool-explicit-server-id ()
  "Round-trip the obsolete `register-tool' shims with explicit :server-id.
Exercises the legacy shim's `:server-id' extraction (which strips
`:server-id' from the property list before forwarding to the validator)
and `unregister-tool''s optional SERVER-ID argument."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool)
                             (obsolete
                              mcp-server-lib-unregister-tool))
    (let ((mcp-server-lib-ert-server-id "shim-server"))
      (mcp-server-lib-ert-with-server
       :tools nil
       :resources nil
       (unwind-protect
           (progn
             (mcp-server-lib-register-tool
              #'mcp-server-lib-test--return-string
              :id "shim-tool"
              :description "Tool via obsolete shim with :server-id"
              :server-id "shim-server")
             (let ((result
                    (mcp-server-lib-test--call-tool "shim-tool" 1)))
               (mcp-server-lib-test--check-mcp-server-lib-content-format
                result "test result"))
             (should
              (mcp-server-lib-unregister-tool
               "shim-tool" "shim-server"))
             (mcp-server-lib-test--verify-tool-not-found "shim-tool"))
         ;; Defensive cleanup if a `should' failed before unregister.
         (mcp-server-lib-unregister-tool
          "shim-tool" "shim-server"))))))

(ert-deftest
    mcp-server-lib-test-register-tool-duplicate-server-id-rejected
    ()
  "Duplicate `:server-id' in obsolete `register-tool' shim is rejected.
The previous implementation silently used the first `:server-id' and
discarded subsequent occurrences via `plist-remove'."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool)
                             (obsolete
                              mcp-server-lib-unregister-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--return-string
      :server-id "dup-sid-first"
      :id "dup-sid-tool"
      :description "d"
      :server-id "dup-sid-second")
     :type 'error)
    ;; Rejection must leave no orphan tool under either server-id.
    (should-not
     (mcp-server-lib-unregister-tool "dup-sid-tool" "dup-sid-first"))
    (should-not
     (mcp-server-lib-unregister-tool
      "dup-sid-tool" "dup-sid-second"))))

(ert-deftest
    mcp-server-lib-test-register-tool-trailing-server-id-rejected
    ()
  "Trailing `:server-id' with no value in obsolete `register-tool' is rejected.
The previous implementation silently defaulted the server-id to
\"default\" because `plist-get' returned nil for the dangling key."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool)
                             (obsolete
                              mcp-server-lib-unregister-tool))
    (unwind-protect
        (should-error
         (mcp-server-lib-register-tool
          #'mcp-server-lib-test--return-string
          :id "trail-sid-tool"
          :description "d"
          :server-id)
         :type 'error)
      (mcp-server-lib-unregister-tool "trail-sid-tool"))))

(ert-deftest
    mcp-server-lib-test-register-tool-non-string-server-id-rejected
    ()
  "Non-string `:server-id' in obsolete `register-tool' shim is rejected.
The previous implementation silently used the non-string value as a
hash key, creating an orphan registration unreachable via the stdio
transport (which passes string server-ids) and capable of crashing
`mcp-server-lib-describe-setup' (whose `string<' sort errors on
numeric keys)."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--return-string
      :id "non-str-sid-tool"
      :description "d"
      :server-id 42)
     :type 'error)))

(ert-deftest
    mcp-server-lib-test-register-tool-duplicate-property-rejected
    ()
  "Duplicate property key in obsolete `register-tool' shim is rejected.
Mirrors the bundled `register-server' contract: a re-added
`:description' would otherwise silently keep the first value via
`plist-get'."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--return-string
      :id "dup-desc-tool"
      :description "first"
      :description "second")
     :type 'error)))

(ert-deftest
    mcp-server-lib-test-register-tool-unknown-property-rejected
    ()
  "Unknown property key in obsolete `register-tool' shim is rejected.
Mirrors the bundled `register-server' contract: a typo'd key would
otherwise be silently ignored."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (should-error
     (mcp-server-lib-register-tool
      #'mcp-server-lib-test--return-string
      :id "unknown-prop-tool"
      :description "d"
      :typo "x")
     :type 'error)))

(ert-deftest mcp-server-lib-test-register-tool-bytecode ()
  "Test schema generation for a handler loaded as bytecode.
This test verifies that MCP can correctly extract parameter information
from a function loaded from bytecode rather than interpreted elisp."
  (require 'bytecomp)
  (let* ((source-file
          (expand-file-name
           "mcp-server-lib-bytecode-handler-test.el"))
         (bytecode-file (byte-compile-dest-file source-file)))
    (should (byte-compile-file source-file))

    (should (load bytecode-file nil t t))

    (mcp-server-lib-test--with-tools
        ((#'mcp-server-lib-test-bytecode-handler--handler
          :id "bytecode-handler"
          :description "A tool with a handler loaded from bytecode"))
      (mcp-server-lib-test--verify-tool-schema-in-single-tool-list
       '(("input-string"
          "string"
          "Input string parameter for bytecode testing"))))

    (when (file-exists-p bytecode-file)
      (delete-file bytecode-file))))

;;; `mcp-server-lib-unregister-tool' tests

(ert-deftest mcp-server-lib-test-unregister-tool ()
  "Test that `mcp-server-lib-unregister-tool' removes a tool correctly."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--with-server
     :tools
     `((mcp-server-lib-test--return-string
        :id ,mcp-server-lib-test--unregister-tool-id
        :description "Tool for unregister test"))
     ;; Verify tool is registered and callable.
     (mcp-server-lib-test--verify-tool-list-request
      `((,mcp-server-lib-test--unregister-tool-id
         .
         ((description . "Tool for unregister test")
          (inputSchema . ((type . "object")))))))
     (let ((result
            (mcp-server-lib-test--call-tool
             mcp-server-lib-test--unregister-tool-id
             44)))
       (mcp-server-lib-test--check-mcp-server-lib-content-format
        result "test result"))
     ;; Unregister via the narrow (obsolete) API; verify return value
     ;; and that the tool is gone.
     (with-suppressed-warnings ((obsolete
                                 mcp-server-lib-unregister-tool))
       (should
        (mcp-server-lib-unregister-tool
         mcp-server-lib-test--unregister-tool-id)))
     (mcp-server-lib-test--verify-tool-list-request '())
     (mcp-server-lib-test--verify-tool-not-found
      mcp-server-lib-test--unregister-tool-id))))

(ert-deftest mcp-server-lib-test-unregister-tool-nonexistent ()
  "Test that `mcp-server-lib-unregister-tool' returns nil for missing tools."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-unregister-tool))
    (mcp-server-lib-test--with-server
      :tools
      '((mcp-server-lib-test--return-string
         :id "test-other"
         :description "Other test tool"))
      (should-not
       (mcp-server-lib-unregister-tool "nonexistent-tool")))))

(ert-deftest mcp-server-lib-test-unregister-tool-when-no-tools ()
  "Test `mcp-server-lib-unregister-tool' when no tools are registered."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-unregister-tool))
    (should-not (mcp-server-lib-unregister-tool "any-tool"))))

(ert-deftest mcp-server-lib-test-unregister-tool-nonexistent-server ()
  "Test that unregistering from non-existent server-id returns nil."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-unregister-tool))
    (mcp-server-lib-ert-with-server
     :tools nil
     :resources nil
     (mcp-server-lib-test--with-server
       :id "server-a"
       :tools
       '((mcp-server-lib-test--return-string
          :id "test-tool"
          :description "Test tool"))
       ;; Try to unregister from a different server - should return nil
       (should-not
        (mcp-server-lib-unregister-tool "test-tool" "server-b"))
       ;; Verify tool is still registered in server-a
       (should
        (= 1 (length (mcp-server-lib-test--get-tool-list))))))))

(ert-deftest
    mcp-server-lib-test-unregister-tool-returns-t-on-decrement
    ()
  "`mcp-server-lib-unregister-tool' returns t when ref-count is decremented.
A non-removing call (entry still registered because its reference count
was greater than one) must return t, matching the docstring contract
that t means the tool was found."
  (mcp-server-lib-test--with-obsolete-tool-api
    (mcp-server-lib-register-tool
     #'mcp-server-lib-test--return-string
     :id "rc-tool"
     :description "d")
    (mcp-server-lib-register-tool
     #'mcp-server-lib-test--return-string
     :id "rc-tool"
     :description "d")
    ;; ref-count 2 -> 1: returns t even though entry remains.
    (should (mcp-server-lib-unregister-tool "rc-tool"))
    (mcp-server-lib-test--check-mcp-server-lib-content-format
     (mcp-server-lib-test--call-tool "rc-tool" 1) "test result")
    ;; ref-count 1 -> 0: returns t, entry removed.
    (should (mcp-server-lib-unregister-tool "rc-tool"))
    ;; Entry gone: returns nil.
    (should-not (mcp-server-lib-unregister-tool "rc-tool"))))

;;; `mcp-server-lib-register-server' tests

(ert-deftest
    mcp-server-lib-test-register-server-serverinfo-name-and-version
    ()
  "Initialize reports the registered :name and :version in serverInfo.
The :version is the server's own version, distinct from the MCP
`protocolVersion' (which the assertion checks separately)."
  (mcp-server-lib-test--with-server
    :id "srv"
    :name "My Server"
    :version
    "2.5.0"
    (mcp-server-lib-ert-with-server
     :tools nil
     :resources nil
     :name "My Server"
     :version "2.5.0")))

(ert-deftest
    mcp-server-lib-test-register-server-serverinfo-name-defaults-to-id
    ()
  "Without :name, serverInfo.name defaults to the server :id."
  (mcp-server-lib-test--with-server
    :id "srv-id-as-name"
    :version
    "1.2.3"
    (mcp-server-lib-ert-with-server
     :tools nil
     :resources nil
     :name "srv-id-as-name"
     :version "1.2.3")))

(ert-deftest mcp-server-lib-test-register-server-version-defaults ()
  "Without :version, serverInfo.version defaults to the library default.
Calls `mcp-server-lib-register-server' directly (not the version-
defaulting test wrapper) so the library default is exercised."
  (unwind-protect
      (progn
        (mcp-server-lib-register-server :id "default")
        (mcp-server-lib-ert-with-server
         :tools nil
         :resources nil
         :version mcp-server-lib-default-server-version))
    (mcp-server-lib-unregister-server "default")))

(ert-deftest
    mcp-server-lib-test-initialize-legacy-only-serverinfo-defaults
    ()
  "Legacy-only registration still yields non-null serverInfo name/version.
A server configured purely through the obsolete
`mcp-server-lib-register-tool' has no bundled metadata record, yet
`initialize' must report the server-id as the name and the default
version -- never JSON null.  Also pins the documented
`mcp-server-lib-server-registered-p' contract that a legacy-only server
has no record and so reports as not registered."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (let ((mcp-server-lib-ert-server-id "legacy-only-srv"))
      (unwind-protect
          (progn
            (mcp-server-lib-register-tool
             #'mcp-server-lib-test--return-string
             :id "legacy-tool"
             :description "Tool via obsolete shim"
             :server-id "legacy-only-srv")
            (should-not
             (mcp-server-lib-server-registered-p "legacy-only-srv"))
            (unwind-protect
                (progn
                  (mcp-server-lib-start)
                  (mcp-server-lib-ert-assert-initialize-result
                   (mcp-server-lib-ert--get-initialize-result)
                   t
                   nil
                   :name "legacy-only-srv"
                   :version mcp-server-lib-default-server-version))
              (mcp-server-lib-stop)))
        (mcp-server-lib-unregister-server "legacy-only-srv")))))

(ert-deftest mcp-server-lib-test-server-registered-p ()
  "`mcp-server-lib-server-registered-p' reflects the server record lifecycle.
Returns nil before registration and after teardown, non-nil while a
record registered via `mcp-server-lib-register-server' is live."
  (unwind-protect
      (progn
        (should-not (mcp-server-lib-server-registered-p "reg-p-srv"))
        (mcp-server-lib-register-server :id "reg-p-srv")
        (should (mcp-server-lib-server-registered-p "reg-p-srv"))
        (mcp-server-lib-unregister-server "reg-p-srv")
        (should-not (mcp-server-lib-server-registered-p "reg-p-srv")))
    (mcp-server-lib-unregister-server "reg-p-srv")))

(ert-deftest
    mcp-server-lib-test-register-server-non-string-version-rejected
    ()
  "Non-string :version signals an error."
  (should-error
   (mcp-server-lib-register-server :id "bad-version" :version 42)
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-non-string-name-rejected
    ()
  "Non-string :name signals an error."
  (should-error
   (mcp-server-lib-register-server
    :id "bad-name"
    :name 42
    :version "1.0.0")
   :type 'error))

(ert-deftest mcp-server-lib-test-register-server-emits-instructions ()
  "With :instructions, register-server makes initialize include the field."
  (mcp-server-lib-test--with-server
    :id "default"
    :instructions
    "Test instructions string."
    (mcp-server-lib-ert-with-server
     :tools nil
     :resources nil
     :instructions "Test instructions string.")))

(ert-deftest mcp-server-lib-test-register-server-default-id ()
  "Without :id, register-server registers under \"default\"."
  (mcp-server-lib-test--with-server
    :instructions
    "From default."
    (mcp-server-lib-ert-with-server
     :tools nil
     :resources nil
     :instructions "From default.")))

(ert-deftest
    mcp-server-lib-test-register-server-no-instructions-omits-field
    ()
  "Without :instructions, register-server leaves initialize without the field."
  (mcp-server-lib-test--with-server
    :id
    "default"
    (mcp-server-lib-ert-with-server :tools nil :resources nil)))

(ert-deftest
    mcp-server-lib-test-ert-with-server-auto-register-forwards-instructions
    ()
  "`mcp-server-lib-ert-with-server' auto-registration forwards :instructions.
A caller that supplies :instructions without pre-registering a server still
sees the field in the initialize result."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   :instructions "Auto-registered instructions."))

(ert-deftest
    mcp-server-lib-test-register-server-empty-call-requires-paired-unregister
    ()
  "Empty `(register-server)' creates a per-server metadata record.
The record is keyed under \"default\" and must be paired with
`unregister-server' for clean teardown.  Verified observably via
ref-count: a subsequent call that
supplies `:instructions' bumps the same record, so one unregister leaves
the instructions visible (record at ref-count 1) and a second unregister
removes them (record gone)."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server)
        (mcp-server-lib-test--register-server
         :instructions "Persisted.")
        ;; Ref-count 2 -> 1: record still alive, instructions visible.
        (mcp-server-lib-unregister-server "default")
        (mcp-server-lib-ert-with-server
         :tools nil
         :resources nil
         :instructions "Persisted.")
        ;; Ref-count 1 -> 0: record gone.
        (mcp-server-lib-test--unregister-default-assert-no-instructions))
    (mcp-server-lib-unregister-server "default")
    (mcp-server-lib-unregister-server "default")))

(ert-deftest
    mcp-server-lib-test-register-server-nil-instructions-treated-as-missing
    ()
  "Explicit nil :instructions behaves like missing (no field emitted)."
  (mcp-server-lib-test--with-server
    :id "default"
    :instructions
    nil
    (mcp-server-lib-ert-with-server :tools nil :resources nil)))

(ert-deftest
    mcp-server-lib-test-register-server-empty-string-instructions
    ()
  "Empty-string :instructions is accepted and emitted as-is."
  (mcp-server-lib-test--with-server
    :id "default"
    :instructions
    ""
    (mcp-server-lib-ert-with-server
     :tools nil
     :resources nil
     :instructions "")))

(ert-deftest mcp-server-lib-test-register-server-last-writer-wins ()
  "Calling register-server twice replaces the prior :instructions."
  (mcp-server-lib-test--with-server
    :id "default"
    :instructions
    "First."
    (mcp-server-lib-test--with-server
      :id "default"
      :instructions
      "Second."
      (mcp-server-lib-ert-with-server
       :tools nil
       :resources nil
       :instructions "Second."))))

(ert-deftest mcp-server-lib-test-register-server-per-server-isolation
    ()
  "Each server-id gets its own :instructions in initialize."
  (mcp-server-lib-test--with-server
    :id "server-a"
    :instructions
    "Instructions for A."
    (mcp-server-lib-test--with-server
      :id "server-b"
      :instructions
      "Instructions for B."
      (mcp-server-lib-test--with-servers
          '(("server-a"
             :tools nil
             :resources nil
             :instructions "Instructions for A.")
            ("server-b"
             :tools nil
             :resources nil
             :instructions "Instructions for B."))))))

(defun mcp-server-lib-test--assert-invalid-instructions (value)
  "Assert `mcp-server-lib-register-server' rejects VALUE for :instructions.
Then run `with-server' to verify no record leaked into the per-server
table on the failed registration; the macro asserts `instructions'
absent in the `initialize' response by default."
  (should-error
   (mcp-server-lib-test--register-server :instructions value)
   :type 'error)
  (mcp-server-lib-ert-with-server :tools nil :resources nil))

(ert-deftest
    mcp-server-lib-test-register-server-non-string-instructions-integer
    ()
  "Integer :instructions signals an error."
  (mcp-server-lib-test--assert-invalid-instructions 42))

(ert-deftest
    mcp-server-lib-test-register-server-non-string-instructions-symbol
    ()
  "Symbol :instructions signals an error."
  (mcp-server-lib-test--assert-invalid-instructions 'foo))

(ert-deftest
    mcp-server-lib-test-register-server-non-string-instructions-list
    ()
  "List :instructions signals an error."
  (mcp-server-lib-test--assert-invalid-instructions
   '("not a string")))

(ert-deftest mcp-server-lib-test-register-server-empty-bundle ()
  "Bundle with empty :tools and :resources registers nothing."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "default"
         :tools '()
         :resources '())
        (mcp-server-lib-ert-with-server
         :tools nil
         :resources
         nil
         (mcp-server-lib-test--verify-list-counts "default" 0 0 0)))
    (mcp-server-lib-unregister-server "default")))

(ert-deftest mcp-server-lib-test-register-server-tools-only ()
  "Bundled :tools registers tools that appear in tools/list."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "default"
         :tools
         '((mcp-server-lib-test--return-string
            :id "tool-a"
            :description "Tool A")
           (mcp-server-lib-test--return-string
            :id "tool-b"
            :description "Tool B")))
        (mcp-server-lib-ert-with-server
         :tools t
         :resources nil
         (should
          (= 2 (length (mcp-server-lib-test--get-tool-list))))))
    (mcp-server-lib-unregister-server "default")))

(ert-deftest mcp-server-lib-test-register-server-resources-only-direct
    ()
  "Bundled :resources registers direct (non-template) resources."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "default"
         :resources
         '(("test://r1"
            mcp-server-lib-test--return-string
            :name "R1")))
        (mcp-server-lib-ert-with-server
         :tools nil
         :resources
         t
         (mcp-server-lib-test--verify-list-counts "default" 0 1 0)))
    (mcp-server-lib-unregister-server "default")))

(ert-deftest
    mcp-server-lib-test-register-server-resources-only-template
    ()
  "Bundled :resources auto-detects template URIs."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "default"
         :resources
         '(("test://{var}"
            mcp-server-lib-test--return-string
            :name "Template")))
        (mcp-server-lib-ert-with-server
         :tools nil
         :resources
         t
         (mcp-server-lib-test--verify-list-counts "default" 0 0 1)))
    (mcp-server-lib-unregister-server "default")))

(ert-deftest
    mcp-server-lib-test-register-server-cross-server-id-isolation
    ()
  "Two bundled `register-server' calls under different ids isolate.
Each server-id sees only the tools registered under it."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "server-a"
         :tools
         '((mcp-server-lib-test--return-string
            :id "tool-a"
            :description "Tool A")))
        (mcp-server-lib-test--register-server
         :id "server-b"
         :tools
         '((mcp-server-lib-test--return-string
            :id "tool-b"
            :description "Tool B")))
        (mcp-server-lib-test--with-servers '(("server-a"
                                              :tools t
                                              :resources nil)
                                             ("server-b"
                                              :tools t
                                              :resources nil))
          (let* ((mcp-server-lib-ert-server-id "server-a")
                 (tools (mcp-server-lib-test--get-tool-list)))
            (should (= 1 (length tools)))
            (should
             (string=
              "tool-a"
              (mcp-server-lib-test--tool-name (aref tools 0)))))
          (let* ((mcp-server-lib-ert-server-id "server-b")
                 (tools (mcp-server-lib-test--get-tool-list)))
            (should (= 1 (length tools)))
            (should
             (string=
              "tool-b"
              (mcp-server-lib-test--tool-name (aref tools 0)))))))
    (mcp-server-lib-unregister-server "server-a")
    (mcp-server-lib-unregister-server "server-b")))

(ert-deftest mcp-server-lib-test-register-server-mixed-bundle ()
  "Bundled :tools and :resources both register together."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "default"
         :tools
         '((mcp-server-lib-test--return-string
            :id "tool-a"
            :description "Tool A"))
         :resources
         '(("test://r1" mcp-server-lib-test--return-string :name "R1")
           ("test://{var}"
            mcp-server-lib-test--return-string
            :name "Template")))
        (mcp-server-lib-ert-with-server
         :tools t
         :resources
         t
         (mcp-server-lib-test--verify-list-counts "default" 1 1 1)))
    (mcp-server-lib-unregister-server "default")))

(ert-deftest mcp-server-lib-test-register-server-atomicity ()
  "Invalid spec in :tools aborts the call; no state is mutated."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :tools
    '((mcp-server-lib-test--return-string
       :id "valid"
       :description "Valid")
      ("not-a-function" :id "invalid" :description "Invalid")))
   :type 'error)
  ;; Nothing should be registered; metadata also untouched.
  (mcp-server-lib-test--verify-default-server-empty))

(ert-deftest mcp-server-lib-test-register-server-atomicity-resources
    ()
  "Invalid spec in :resources aborts the call; no state is mutated."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :resources mcp-server-lib-test--invalid-resources-spec)
   :type 'error)
  (mcp-server-lib-test--verify-default-server-empty))

(ert-deftest mcp-server-lib-test-register-server-atomicity-mixed ()
  "Invalid :resources entry after valid :tools blocks all apply.
Verifies the all-validation-before-any-apply ordering across both lists:
neither tools nor resources are partially registered."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :tools
    '((mcp-server-lib-test--return-string
       :id "valid-tool"
       :description "Valid tool"))
    :resources mcp-server-lib-test--invalid-resources-spec)
   :type 'error)
  (mcp-server-lib-test--verify-default-server-empty))

(ert-deftest
    mcp-server-lib-test-register-server-atomicity-schema-error
    ()
  "Tool error inside `--generate-schema-from-function' aborts the bundle.
The error is raised by a function called from
`mcp-server-lib--build-tool-entry', which runs before any state
mutation in `mcp-server-lib-register-server'.  This regression guard
ensures the earlier valid tool is NOT registered, so any refactor
that allows schema-error to leak past the entry-building phase (e.g.
for performance) will fail this test instead of silently breaking
atomicity for the schema-error branch."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :tools
    '((mcp-server-lib-test--return-string
       :id "valid"
       :description "Valid")
      (mcp-server-lib-test--tool-handler-with-rest
       :id
       "bad-schema"
       :description "Has &rest parameters")))
   :type 'error)
  (mcp-server-lib-test--verify-default-server-empty))

(ert-deftest
    mcp-server-lib-test-register-server-atomicity-template-error
    ()
  "Resource whose error fires inside `--parse-uri-template' aborts the bundle.
Symmetric to the schema-error guard above, but for the resource path:
the invalid template (variable name starting with a digit) is rejected
only inside `mcp-server-lib--parse-uri-template', which is called from
`mcp-server-lib--build-resource-entry' before any state mutation.  The
earlier valid resource must NOT be registered."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :resources
    '(("test://valid"
       mcp-server-lib-test--return-string
       :name "Valid")
      ("test://{1bad}"
       mcp-server-lib-test--return-string
       :name "Bad template")))
   :type 'error)
  (mcp-server-lib-test--verify-default-server-empty))

(ert-deftest
    mcp-server-lib-test-register-server-atomicity-preserves-prior-state
    ()
  "Failed `register-server' call leaves previously-registered state intact.
Verifies the strongest atomicity property: :instructions, tools, and
resources registered by a prior successful call survive a subsequent
failing call unchanged.  After the failing call, re-registers the prior
bundle (ref-counts -> 2) and tears down with two `unregister-server'
calls; under any ref-count bump regression an entry would survive at
ref-count 1 and stay listed."
  (let ((prior-bundle mcp-server-lib-test--prior-bundle))
    (apply #'mcp-server-lib-test--register-server prior-bundle)
    ;; Failing call that tries to update everything.
    (should-error
     (mcp-server-lib-test--register-server
      :id "default"
      :instructions "New instructions that should not stick."
      :tools
      '(("not-a-function" :id "would-be-new" :description "Invalid")))
     :type 'error)
    ;; Prior state must be intact: instructions value, one tool,
    ;; one resource.
    (mcp-server-lib-ert-with-server
     :tools t
     :resources t
     :instructions
     "Prior instructions."
     (mcp-server-lib-test--verify-list-counts "default" 1 1 0))
    ;; Re-register the prior bundle: ref-counts go to 2.  Two
    ;; unregister-server calls then bring them to 0; any bump
    ;; introduced by the failing call would leave the corresponding
    ;; entry at ref-count 1 and still listed.
    (mcp-server-lib-test--reregister-and-tear-down prior-bundle)))

(ert-deftest
    mcp-server-lib-test-register-server-atomicity-preserves-prior-state-resource-error
    ()
  "Symmetric to `atomicity-preserves-prior-state'; bad spec is in :resources.
The failing :resources includes a valid
entry that overlaps the prior URI before the invalid entry: under an
\"apply-resource-as-built\" regression that re-bumps an existing entry
before failing, the prior resource would survive the two unregisters
below at ref-count 1 and stay listed."
  (let ((prior-bundle mcp-server-lib-test--prior-bundle))
    (apply #'mcp-server-lib-test--register-server prior-bundle)
    (should-error
     (mcp-server-lib-test--register-server
      :id "default"
      :instructions "New instructions that should not stick."
      :resources
      '(("test://prior"
         mcp-server-lib-test--return-string
         :name "Same URI as prior")
        ("test://invalid" "not-a-function" :name "Invalid handler")))
     :type 'error)
    (mcp-server-lib-ert-with-server
     :tools t
     :resources t
     :instructions
     "Prior instructions."
     (mcp-server-lib-test--verify-list-counts "default" 1 1 0))
    (mcp-server-lib-test--reregister-and-tear-down prior-bundle)))

(ert-deftest
    mcp-server-lib-test-register-server-atomicity-server-record-not-bumped
    ()
  "Failed `register-server' must not bump the metadata record's ref-count.
Verified observably via `:instructions': after a
register + failing call + one `unregister-server', the record should be
torn down (initialize must omit `instructions').  A regression that
bumps the record's ref-count during the failing call would leave the
record at ref-count 1, and initialize would still emit the prior
`:instructions' value."
  (mcp-server-lib-test--register-server
   :id "default"
   :instructions "Sticky.")
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :tools
    '(("not-a-function" :id "would-be-new" :description "Invalid")))
   :type 'error)
  ;; One unregister: under no bug, ref 1 -> 0, record removed.
  ;; Under bug bumping the record during the failing call, ref
  ;; 2 -> 1, record persists with :instructions intact.
  (mcp-server-lib-test--unregister-default-assert-no-instructions))

(ert-deftest
    mcp-server-lib-test-register-server-tool-inner-server-id-rejected
    ()
  "Tool spec containing :server-id is rejected.
The bundled API has no per-entry server-id; the outer :id of
register-server applies to all entries.  :server-id is rejected via
the same generic unknown-property path as any other unaccepted key."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :tools
    '((mcp-server-lib-test--return-string
       :id "tool-a"
       :description "Tool A"
       :server-id "other")))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-tool-unknown-property-rejected
    ()
  "Tool spec containing an unknown property key is rejected."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :tools
    '((mcp-server-lib-test--return-string
       :id "tool-a"
       :description "Tool A"
       :read-onyl t)))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-tool-non-boolean-read-only-rejected
    ()
  "Tool spec with non-boolean :read-only value is rejected."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :tools
    '((mcp-server-lib-test--return-string
       :id "tool-a"
       :description "Tool A"
       :read-only "yes")))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-resource-inner-server-id-rejected
    ()
  "Resource spec containing :server-id is rejected.
Like the tool variant: a bundle has no per-entry server-id, so
`:server-id' is refused as an unknown property."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :resources
    '(("test://r"
       mcp-server-lib-test--return-string
       :name "R"
       :server-id "other")))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-resource-unknown-property-rejected
    ()
  "Resource spec containing an unknown property key is rejected."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :resources
    '(("test://r"
       mcp-server-lib-test--return-string
       :name "R"
       :mime_type "text/plain")))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-unknown-top-level-property-rejected
    ()
  "Unknown top-level property in `mcp-server-lib-register-server' is rejected."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :isntructions "typo")
   :type 'error))

(ert-deftest
    mcp-server-lib-test-ert-with-server-unknown-keyword-rejected
    ()
  "Unknown keyword in `mcp-server-lib-ert-with-server' is rejected.
Rejection happens at macro-expansion time."
  (should-error
   (macroexpand '(mcp-server-lib-ert-with-server :tols t (should t)))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-ert-with-server-trailing-keyword-rejected
    ()
  "Trailing keyword to `mcp-server-lib-ert-with-server' is rejected.
Rejection happens at macro-expansion time."
  (should-error
   (macroexpand '(mcp-server-lib-ert-with-server :tools))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-ert-with-server-keyword-as-value-rejected
    ()
  "Keyword in value position to `mcp-server-lib-ert-with-server' is rejected.
Rejection happens at macro-expansion time."
  (should-error
   (macroexpand
    '(mcp-server-lib-ert-with-server :tools :resources t (should t)))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-top-level-server-id-rejected
    ()
  "Top-level `:server-id' in `mcp-server-lib-register-server' is rejected.
The bundled API uses `:id'; `:server-id' is rejected via the same
generic unknown-property path as any other unaccepted key."
  (should-error
   (mcp-server-lib-test--register-server :server-id "x")
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-tool-odd-length-plist-rejected
    ()
  "Tool spec with odd-length property list is rejected.
A trailing key with no value (e.g. `:read-only' missing the value)
would otherwise silently register `:read-only nil', the opposite of
the user's likely intent."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :tools
    '((mcp-server-lib-test--return-string
       :id "t"
       :description "d"
       :read-only)))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-tool-duplicate-property-rejected
    ()
  "Tool spec with duplicate property key is rejected."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :tools
    '((mcp-server-lib-test--return-string
       :id "t"
       :description "first"
       :description "second")))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-tool-spec-dotted-properties-rejected
    ()
  "Tool spec with improper (dotted) property list is rejected.
A plain `length' call would error with `wrong-type-argument'; the
validator must surface a contextual `proper list' error message
instead."
  (let ((err
         (should-error
          (mcp-server-lib-test--register-server
           :id "default"
           :tools
           '((mcp-server-lib-test--return-string
              :id "t"
              :description
              "d"
              .
              junk)))
          :type 'error)))
    (should
     (string-match-p "proper list" (error-message-string err)))))

(ert-deftest
    mcp-server-lib-test-register-server-resource-odd-length-plist-rejected
    ()
  "Resource spec with odd-length property list is rejected."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :resources
    '(("test://r"
       mcp-server-lib-test--return-string
       :name "r"
       :description)))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-resource-duplicate-property-rejected
    ()
  "Resource spec with duplicate property key is rejected."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :resources
    '(("test://r"
       mcp-server-lib-test--return-string
       :name "first"
       :name "second")))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-resource-spec-dotted-properties-rejected
    ()
  "Resource spec with improper (dotted) property list is rejected."
  (let ((err
         (should-error
          (mcp-server-lib-test--register-server
           :id "default"
           :resources
           '(("test://r"
              mcp-server-lib-test--return-string
              :name
              "r"
              .
              junk)))
          :type 'error)))
    (should
     (string-match-p "proper list" (error-message-string err)))))

(ert-deftest
    mcp-server-lib-test-register-server-top-level-odd-length-plist-rejected
    ()
  "Top-level register-server odd-length property list is rejected.
For example `(:id \"x\" :instructions)' would otherwise silently clear
existing `:instructions' (treated as explicit nil)."
  (should-error
   (mcp-server-lib-register-server :id "x" :instructions)
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-top-level-duplicate-property-rejected
    ()
  "Top-level register-server duplicate property key is rejected.
For example `(:id \"a\" :id \"b\")' would otherwise silently drop the
second value (`plist-get' returns the first match)."
  (should-error
   (mcp-server-lib-register-server :id "a" :id "b")
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-duplicate-tool-id-rejected
    ()
  "Two tool specs with the same :id are rejected in validation."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :tools
    '((mcp-server-lib-test--return-string :id "same" :description "A")
      (mcp-server-lib-test--return-string
       :id "same"
       :description "B")))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-duplicate-resource-uri-rejected
    ()
  "Two resource specs with the same URI are rejected in validation."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :resources
    '(("test://r" mcp-server-lib-test--return-string :name "First")
      ("test://r" mcp-server-lib-test--return-string :name "Second")))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-tools-not-list-rejected
    ()
  ":tools that isn't a list is rejected."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :tools "not-a-list")
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-tools-dotted-list-rejected
    ()
  ":tools that is a dotted (improper) list is rejected with a contextual error."
  (let ((err
         (should-error
          (mcp-server-lib-test--register-server
           :id "default"
           :tools
           '((mcp-server-lib-test--return-string
              :id "a"
              :description "A")
             . junk))
          :type 'error)))
    (should
     (string-match-p "proper list" (error-message-string err)))))

(ert-deftest
    mcp-server-lib-test-register-server-resources-not-list-rejected
    ()
  ":resources that isn't a list is rejected."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :resources "not-a-list")
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-resources-dotted-list-rejected
    ()
  "Dotted (improper) :resources list is rejected with a contextual error."
  (let ((err
         (should-error
          (mcp-server-lib-test--register-server
           :id "default"
           :resources
           '(("test://r" mcp-server-lib-test--return-string :name "R")
             .
             junk))
          :type 'error)))
    (should
     (string-match-p "proper list" (error-message-string err)))))

(ert-deftest
    mcp-server-lib-test-register-server-non-string-id-rejected
    ()
  ":id that isn't a string is rejected."
  (should-error
   (mcp-server-lib-test--register-server :id 42)
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-tool-spec-not-cons-rejected
    ()
  "An entry in :tools that isn't a cons cell is rejected."
  (should-error
   (mcp-server-lib-test--register-server :id "default" :tools '(nil))
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-server-resource-spec-not-cons-rejected
    ()
  "A :resources entry that isn't a `(URI HANDLER . PROPS)' list is rejected."
  (should-error
   (mcp-server-lib-test--register-server
    :id "default"
    :resources '(nil))
   :type 'error))

(ert-deftest mcp-server-lib-test-register-server-tools-ref-count ()
  "Two bundled calls bump tool ref-count; two unregisters clean up."
  (let ((bundle
         '(:id
           "default"
           :tools
           ((mcp-server-lib-test--return-string
             :id "shared"
             :description "Shared tool")))))
    (unwind-protect
        (progn
          (apply #'mcp-server-lib-test--register-server bundle)
          (apply #'mcp-server-lib-test--register-server bundle)
          (mcp-server-lib-ert-with-server
           :tools t
           :resources nil
           (let ((mcp-server-lib-ert-server-id "default"))
             (should
              (= 1 (length (mcp-server-lib-test--get-tool-list))))
             (mcp-server-lib-unregister-server "default")
             ;; Ref-count 2 -> 1, tool still listed.
             (should
              (= 1 (length (mcp-server-lib-test--get-tool-list))))
             (mcp-server-lib-unregister-server "default")
             ;; Ref-count 1 -> 0, tool removed.
             (should
              (= 0 (length (mcp-server-lib-test--get-tool-list)))))))
      (mcp-server-lib-unregister-server "default")
      (mcp-server-lib-unregister-server "default"))))

(ert-deftest
    mcp-server-lib-test-register-server-tool-key-collision-keeps-original
    ()
  "Re-registering a tool keeps the FIRST call's handler and properties.
The second call's spec is discarded (only the ref-count is
bumped).  Pins the documented \"first wins\" semantic for
`mcp-server-lib-register-server'."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "default"
         :tools
         '((mcp-server-lib-test--return-string
            :id "shared-tool"
            :description "First description")))
        (mcp-server-lib-test--register-server
         :id "default"
         :tools
         '((mcp-server-lib-test--return-alternate-string
            :id "shared-tool"
            :description "Second description - should be discarded")))
        (mcp-server-lib-ert-with-server
         :tools t
         :resources nil
         ;; First call's :description survives in tools/list.
         (let ((tools (mcp-server-lib-test--get-tool-list)))
           (should (= 1 (length tools)))
           (should
            (string=
             "First description"
             (alist-get 'description (aref tools 0)))))
         ;; First call's handler is what runs.
         (let ((result
                (mcp-server-lib-test--call-tool "shared-tool" 1)))
           (mcp-server-lib-test--check-mcp-server-lib-content-format
            result "test result"))))
    (mcp-server-lib-unregister-server "default")
    (mcp-server-lib-unregister-server "default")))

(ert-deftest
    mcp-server-lib-test-register-server-resource-key-collision-keeps-original
    ()
  "Re-registering a resource keeps the FIRST call's handler and properties.
The second call's spec is discarded (only the ref-count
is bumped).  Pins the documented \"first wins\" semantic for
`mcp-server-lib-register-server'."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "default"
         :resources
         '(("shared://uri"
            mcp-server-lib-test--return-string
            :name "First Name")))
        (mcp-server-lib-test--register-server
         :id "default"
         :resources
         '(("shared://uri"
            mcp-server-lib-test--return-alternate-string
            :name "Second Name - should be discarded")))
        (mcp-server-lib-ert-with-server
         :tools nil
         :resources t
         ;; First call's :name survives in resources/list.
         (mcp-server-lib-test--check-single-resource
          '((uri . "shared://uri") (name . "First Name")))
         ;; First call's handler is what runs.
         (mcp-server-lib-ert-verify-resource-read
          "shared://uri"
          '((uri . "shared://uri") (text . "test result")))))
    (mcp-server-lib-unregister-server "default")
    (mcp-server-lib-unregister-server "default")))

(ert-deftest
    mcp-server-lib-test-register-server-instructions-preserved-when-absent
    ()
  "Second `register-server' call without :instructions preserves prior value."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "default"
         :instructions "First call's instructions.")
        ;; Second call only adds a tool, does not mention :instructions.
        (mcp-server-lib-test--register-server
         :id "default"
         :tools
         '((mcp-server-lib-test--return-string
            :id "later-tool"
            :description "Added by a later bundled call")))
        (mcp-server-lib-ert-with-server
         :tools t
         :resources nil
         :instructions "First call's instructions."))
    (mcp-server-lib-unregister-server "default")
    (mcp-server-lib-unregister-server "default")))

(ert-deftest
    mcp-server-lib-test-register-server-instructions-explicit-nil-clears
    ()
  "Explicit `:instructions nil' clears a previously-set value."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "default"
         :instructions "Will be cleared.")
        (mcp-server-lib-test--register-server
         :id "default"
         :instructions nil)
        (mcp-server-lib-ert-with-server :tools nil :resources nil))
    (mcp-server-lib-unregister-server "default")
    (mcp-server-lib-unregister-server "default")))

(ert-deftest
    mcp-server-lib-test-register-server-instructions-ref-count-survives-partial-teardown
    ()
  "Two register-server calls + one unregister-server keeps `instructions'.
The per-server metadata record is ref-counted in lockstep with
tools/resources, so a single `unregister-server' that leaves tools
listed (ref-count > 0) must also leave `instructions' in place.  A
second `unregister-server' fully tears down."
  (let ((bundle
         '(:id
           "default"
           :instructions "Persisted instructions."
           :tools
           ((mcp-server-lib-test--return-string
             :id "shared"
             :description "Shared tool")))))
    (apply #'mcp-server-lib-test--register-server bundle)
    (apply #'mcp-server-lib-test--register-server bundle)
    ;; After one unregister: tool still listed AND instructions
    ;; still emitted.
    (mcp-server-lib-unregister-server "default")
    (mcp-server-lib-ert-with-server
     :tools t
     :resources nil
     :instructions
     "Persisted instructions."
     (should (= 1 (length (mcp-server-lib-test--get-tool-list)))))
    ;; After the second unregister: record and tool both gone.
    (mcp-server-lib-unregister-server "default")
    (mcp-server-lib-ert-with-server
     :tools nil
     :resources
     nil
     (should (= 0 (length (mcp-server-lib-test--get-tool-list)))))))

(ert-deftest
    mcp-server-lib-test-register-server-instructions-last-writer-wins-survives-unregister
    ()
  "`unregister-server' does not revert the `instructions' value.
Two registers with different values leave the most-recent value in the
record; a single `unregister-server' decrements the count but does not
roll the value back to the prior writer's string."
  (mcp-server-lib-test--register-server
   :id "default"
   :instructions "First.")
  (mcp-server-lib-test--register-server
   :id "default"
   :instructions "Second.")
  (mcp-server-lib-unregister-server "default")
  ;; Value is still "Second.", not reverted to "First.".
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   :instructions "Second.")
  ;; Final unregister: ref 1 -> 0, record removed.
  (mcp-server-lib-test--unregister-default-assert-no-instructions))

(ert-deftest
    mcp-server-lib-test-register-server-instructions-preserved-across-tool-only-call
    ()
  "Instructions persist through a later register-server with no `:instructions'.
Registers `:instructions' alone, then a tool-only bundle, then one
`unregister-server'.  The tool was at ref-count 1 so it goes away; the
metadata record was at ref-count 2 so it stays with `:instructions'
intact."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "default"
         :instructions "Sticky.")
        (mcp-server-lib-test--register-server
         :id "default"
         :tools
         '((mcp-server-lib-test--return-string
            :id "later-tool"
            :description "Tool added by a later call")))
        ;; One unregister: tool gone, instructions still emitted.
        (mcp-server-lib-unregister-server "default")
        (mcp-server-lib-ert-with-server
         :tools nil
         :resources nil
         :instructions "Sticky."
         (should
          (= 0 (length (mcp-server-lib-test--get-tool-list))))))
    (mcp-server-lib-unregister-server "default")
    (mcp-server-lib-unregister-server "default")))

(ert-deftest
    mcp-server-lib-test-register-server-name-version-preserved-across-tool-only-call
    ()
  "Name/version persist through a later register-server with no :name/:version.
Calls `mcp-server-lib-register-server' directly (not the version-
defaulting test wrapper) so an omitted :version is genuinely absent.
Mirrors the `:instructions' preserve-on-omit contract: a tool-only
re-registration must not reset :name/:version to the id/default."
  (unwind-protect
      (progn
        (mcp-server-lib-register-server
         :id "default"
         :name "Custom Name"
         :version "2.5.0")
        (mcp-server-lib-register-server
         :id "default"
         :tools
         '((mcp-server-lib-test--return-string
            :id "later-tool"
            :description "Tool added by a later call")))
        (mcp-server-lib-ert-with-server
         :tools t
         :resources nil
         :name "Custom Name"
         :version "2.5.0"))
    (mcp-server-lib-unregister-server "default")
    (mcp-server-lib-unregister-server "default")))

(ert-deftest
    mcp-server-lib-test-register-server-instructions-only-call-does-not-bump-entries
    ()
  "An `:instructions'-only call bumps no tool or resource ref counts.
Tools and resources registered in call 1 remain
at ref-count 1; a single `unregister-server' fully tears them down
while leaving the metadata record alive at ref-count 1."
  (mcp-server-lib-test--register-server
   :id "default"
   :tools
   '((mcp-server-lib-test--return-string
      :id "tool-a"
      :description "Tool A"))
   :resources
   '(("test://r1"
      mcp-server-lib-test--return-string
      :name "Resource 1")))
  (mcp-server-lib-test--register-server
   :id "default"
   :instructions "Adds instructions only.")
  ;; After both calls: tool/resource at ref=1, record at ref=2.
  (mcp-server-lib-test--with-servers
      '(("default"
         :tools t
         :resources t
         :instructions "Adds instructions only."))
    (mcp-server-lib-test--verify-list-counts "default" 1 1 0))
  ;; One unregister: tool/resource at ref=1→0 (gone); record at
  ;; ref=2→1 (alive, still emits :instructions).
  (mcp-server-lib-unregister-server "default")
  (mcp-server-lib-test--with-servers
      '(("default"
         :tools nil
         :resources nil
         :instructions "Adds instructions only."))
    (mcp-server-lib-test--verify-list-counts "default" 0 0 0))
  ;; Final unregister: record at ref=1→0, fully torn down.
  (mcp-server-lib-test--unregister-default-assert-no-instructions))

;;; `mcp-server-lib-unregister-server' tests

(ert-deftest mcp-server-lib-test-unregister-server-basic ()
  "Test bulk unregister removes tool, resource, and template for a server-id."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "server-a"
         :tools
         '((mcp-server-lib-test--return-string
            :id "test-tool"
            :description "Test tool"))
         :resources
         '(("test://static"
            mcp-server-lib-test--return-string
            :name "Static")
           ("test://{var}"
            mcp-server-lib-test--return-string
            :name "Template")))
        (mcp-server-lib-test--with-servers '(("server-a"
                                              :tools t
                                              :resources t))
          (mcp-server-lib-test--verify-list-counts "server-a" 1 1 1)
          (mcp-server-lib-unregister-server "server-a")
          (mcp-server-lib-test--verify-list-counts "server-a" 0 0 0)))
    (mcp-server-lib-unregister-server "server-a")))

(ert-deftest mcp-server-lib-test-unregister-server-unknown-id-noop ()
  "Unregister on a wholly-unknown server-id returns nil without error.
Verifies the documented \"silent no-op\" contract and locks in current
laxness against future tightening refactors."
  (should-not (mcp-server-lib-unregister-server "never-registered")))

(ert-deftest mcp-server-lib-test-unregister-server-clears-instructions
    ()
  "After `mcp-server-lib-unregister-server', the record and field are gone.
Pins the resource-accounting contract -- the per-server record is fully
removed, not kept with `:instructions' nil-cleared -- via
`mcp-server-lib-server-registered-p', and the observable contract that a
freshly registered default server's `initialize' omits instructions."
  (mcp-server-lib-test--register-server
   :id "default"
   :instructions "Will be dropped.")
  (mcp-server-lib-unregister-server "default")
  (should-not (mcp-server-lib-server-registered-p "default"))
  (mcp-server-lib-ert-with-server :tools nil :resources nil))

(ert-deftest
    mcp-server-lib-test-unregister-server-cross-server-isolation
    ()
  "Test bulk unregister leaves other server-ids' registrations intact."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "server-a"
         :tools
         '((mcp-server-lib-test--return-string
            :id "tool-a"
            :description "Tool A"))
         :resources
         '(("test://a/static"
            mcp-server-lib-test--return-string
            :name "Resource A")
           ("test://a/{var}"
            mcp-server-lib-test--return-string
            :name "Template A")))
        (mcp-server-lib-test--register-server
         :id "server-b"
         :tools
         '((mcp-server-lib-test--return-string
            :id "tool-b"
            :description "Tool B"))
         :resources
         '(("test://b/static"
            mcp-server-lib-test--return-string
            :name "Resource B")
           ("test://b/{var}"
            mcp-server-lib-test--return-string
            :name "Template B")))
        (mcp-server-lib-test--with-servers '(("server-a"
                                              :tools t
                                              :resources t)
                                             ("server-b"
                                              :tools t
                                              :resources t))
          (mcp-server-lib-unregister-server "server-a")
          (mcp-server-lib-test--verify-list-counts "server-a" 0 0 0)
          (mcp-server-lib-test--verify-list-counts "server-b" 1 1 1)))
    (mcp-server-lib-unregister-server "server-a")
    (mcp-server-lib-unregister-server "server-b")))

(ert-deftest
    mcp-server-lib-test-unregister-server-cross-server-instructions-isolation
    ()
  "Unregistering one server-id leaves another's `:instructions' intact."
  (mcp-server-lib-test--register-server
   :id "server-a"
   :instructions "Instructions for A.")
  (mcp-server-lib-test--register-server
   :id "server-b"
   :instructions "Instructions for B.")
  (unwind-protect
      (progn
        ;; Tear down only server-a's record.
        (mcp-server-lib-unregister-server "server-a")
        ;; server-b's record (and its instructions) survives server-a's
        ;; teardown.
        (mcp-server-lib-test--with-servers
            '(("server-b"
               :tools nil
               :resources nil
               :instructions "Instructions for B."))))
    (mcp-server-lib-unregister-server "server-b")))

(ert-deftest mcp-server-lib-test-unregister-server-ref-count ()
  "Test bulk unregister decrements ref-count once per call.
Registering the same tool twice produces ref-count 2; one bulk-unregister
leaves ref-count 1 (tool still listed); a second call removes it."
  (let ((spec
         '(:id
           "ref-count-test"
           :tools
           ((mcp-server-lib-test--return-string
             :id "shared-tool"
             :description "Shared tool")))))
    (unwind-protect
        (progn
          (apply #'mcp-server-lib-test--register-server spec)
          (apply #'mcp-server-lib-test--register-server spec)
          (mcp-server-lib-test--with-servers '(("ref-count-test"
                                                :tools t
                                                :resources nil))
            (let ((mcp-server-lib-ert-server-id "ref-count-test"))
              (should
               (= 1 (length (mcp-server-lib-test--get-tool-list))))
              (mcp-server-lib-unregister-server "ref-count-test")
              (should
               (= 1 (length (mcp-server-lib-test--get-tool-list))))
              (mcp-server-lib-unregister-server "ref-count-test")
              (should
               (= 0 (length (mcp-server-lib-test--get-tool-list)))))))
      (mcp-server-lib-unregister-server "ref-count-test")
      (mcp-server-lib-unregister-server "ref-count-test"))))

(ert-deftest mcp-server-lib-test-unregister-server-ref-count-per-key
    ()
  "Test bulk unregister decrements each key independently.
Tool A with ref-count 1 and tool B with ref-count 2 (registered twice)
under the same server-id: one bulk-unregister removes A (was 1) and
leaves B with ref-count 1; a second call removes B."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "mixed-ref"
         :tools
         '((mcp-server-lib-test--return-string
            :id "tool-a"
            :description "Tool A")
           (mcp-server-lib-test--return-string
            :id "tool-b"
            :description "Tool B")))
        ;; Second call registers tool-b again to bump its ref-count to 2.
        (mcp-server-lib-test--register-server
         :id "mixed-ref"
         :tools
         '((mcp-server-lib-test--return-string
            :id "tool-b"
            :description "Tool B")))
        (mcp-server-lib-test--with-servers '(("mixed-ref"
                                              :tools t
                                              :resources nil))
          (let ((mcp-server-lib-ert-server-id "mixed-ref"))
            (should
             (= 2 (length (mcp-server-lib-test--get-tool-list))))
            (mcp-server-lib-unregister-server "mixed-ref")
            (let ((tool-names
                   (mapcar
                    #'mcp-server-lib-test--tool-name
                    (mcp-server-lib-test--get-tool-list))))
              (should (equal '("tool-b") tool-names)))
            (mcp-server-lib-unregister-server "mixed-ref")
            (should
             (= 0 (length (mcp-server-lib-test--get-tool-list)))))))
    (mcp-server-lib-unregister-server "mixed-ref")
    (mcp-server-lib-unregister-server "mixed-ref")))

(ert-deftest mcp-server-lib-test-unregister-server-ref-count-resource
    ()
  "Test bulk unregister decrements ref-count for resources and templates.
Registering the same resource URI twice and the same template URI twice
produces ref-count 2 each; one bulk-unregister leaves both with
ref-count 1 (still listed); a second call removes them."
  (let ((spec
         '(:id
           "ref-count-resources"
           :resources
           (("test://resource-shared"
             mcp-server-lib-test--return-string
             :name "Shared Resource")
            ("test://template-shared/{var}"
             mcp-server-lib-test--return-string
             :name "Shared Template")))))
    (unwind-protect
        (progn
          (apply #'mcp-server-lib-test--register-server spec)
          (apply #'mcp-server-lib-test--register-server spec)
          (mcp-server-lib-test--with-servers '(("ref-count-resources"
                                                :tools nil
                                                :resources t))
            (mcp-server-lib-test--verify-counts-then-unregister
             "ref-count-resources" 0 1 1)
            (mcp-server-lib-test--verify-counts-then-unregister
             "ref-count-resources" 0 1 1)
            (mcp-server-lib-test--verify-list-counts
             "ref-count-resources" 0 0 0)))
      (mcp-server-lib-unregister-server "ref-count-resources")
      (mcp-server-lib-unregister-server "ref-count-resources"))))

(ert-deftest mcp-server-lib-test-unregister-server-mixed ()
  "Test bulk unregister handles multiple entries across all three types."
  (unwind-protect
      (progn
        (mcp-server-lib-test--register-server
         :id "mixed-server"
         :tools
         '((mcp-server-lib-test--return-string
            :id "mixed-tool-1"
            :description "Tool 1")
           (mcp-server-lib-test--return-string
            :id "mixed-tool-2"
            :description "Tool 2"))
         :resources
         '(("test://mixed/r1"
            mcp-server-lib-test--return-string
            :name "Resource 1")
           ("test://mixed/r2"
            mcp-server-lib-test--return-string
            :name "Resource 2")
           ("test://mixed/{v1}"
            mcp-server-lib-test--return-string
            :name "Template 1")
           ("test://mixed/other/{v2}"
            mcp-server-lib-test--return-string
            :name "Template 2")))
        (mcp-server-lib-test--with-servers '(("mixed-server"
                                              :tools t
                                              :resources t))
          (mcp-server-lib-test--verify-counts-then-unregister
           "mixed-server" 2 2 2)
          (mcp-server-lib-test--verify-list-counts
           "mixed-server" 0 0 0)))
    (mcp-server-lib-unregister-server "mixed-server")))

(ert-deftest mcp-server-lib-test-unregister-server-clears-legacy-tool
    ()
  "Bulk unregister clears tools registered via the obsolete shim.
Pins the README \"two API styles can be mixed\" contract: legacy
`mcp-server-lib-register-tool' state is torn down by bundled
`mcp-server-lib-unregister-server' under the same server-id."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool))
    (unwind-protect
        (progn
          (mcp-server-lib-register-tool
           #'mcp-server-lib-test--return-string
           :id "legacy-tool"
           :description "Tool via obsolete shim"
           :server-id "legacy-srv")
          ;; A bundled record gives the server its initialize identity;
          ;; the legacy tool lives alongside it (the "mixed" contract).
          (mcp-server-lib-test--register-server :id "legacy-srv")
          (mcp-server-lib-test--with-servers '(("legacy-srv"
                                                :tools t
                                                :resources nil))
            (mcp-server-lib-test--verify-counts-then-unregister
             "legacy-srv" 1 0 0)
            (mcp-server-lib-test--verify-list-counts
             "legacy-srv" 0 0 0)))
      (mcp-server-lib-unregister-server "legacy-srv"))))

(ert-deftest
    mcp-server-lib-test-unregister-server-clears-legacy-resource
    ()
  "Bulk unregister clears resources registered via the obsolete shim.
Pins the README \"two API styles can be mixed\" contract: legacy
`mcp-server-lib-register-resource' state is torn down by bundled
`mcp-server-lib-unregister-server' under the same server-id."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-register-resource))
    (unwind-protect
        (progn
          (mcp-server-lib-register-resource
           "test://legacy-static"
           #'mcp-server-lib-test--return-string
           :name "Legacy resource"
           :server-id "legacy-srv")
          ;; A bundled record gives the server its initialize identity;
          ;; the legacy resource lives alongside it (the "mixed" contract).
          (mcp-server-lib-test--register-server :id "legacy-srv")
          (mcp-server-lib-test--with-servers '(("legacy-srv"
                                                :tools nil
                                                :resources t))
            (mcp-server-lib-test--verify-counts-then-unregister
             "legacy-srv" 0 1 0)
            (mcp-server-lib-test--verify-list-counts
             "legacy-srv" 0 0 0)))
      (mcp-server-lib-unregister-server "legacy-srv"))))

;;; Notification tests

(ert-deftest mcp-server-lib-test-notifications-cancelled ()
  "Test the MCP `notifications/cancelled` request handling."
  (mcp-server-lib-test--with-request "notifications/cancelled"
    (let* ((notifications-cancelled
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "notifications/cancelled"))))
           (response
            (mcp-server-lib-process-jsonrpc
             notifications-cancelled mcp-server-lib-ert-server-id)))
      ;; Notifications are one-way, should return nil
      (should-not response))))

;;; `mcp-server-lib-create-tools-list-request' tests

(ert-deftest mcp-server-lib-test-create-tools-list-request-with-id ()
  "Test `mcp-server-lib-create-tools-list-request' with a specified ID."
  (let* ((id 42)
         (request (mcp-server-lib-create-tools-list-request id))
         (parsed (json-read-from-string request)))
    ;; Verify basic JSON-RPC structure
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/list" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))))

(ert-deftest mcp-server-lib-test-create-tools-list-request-default-id
    ()
  "Test `mcp-server-lib-create-tools-list-request' with default ID."
  (let* ((request (mcp-server-lib-create-tools-list-request))
         (parsed (json-read-from-string request)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/list" (alist-get 'method parsed)))
    (should (equal 1 (alist-get 'id parsed)))))

;;; `mcp-server-lib-create-resources-list-request' tests

(ert-deftest mcp-server-lib-test-create-resources-list-request-with-id
    ()
  "Test `mcp-server-lib-create-resources-list-request' with a specified ID."
  (let* ((id 42)
         (request (mcp-server-lib-create-resources-list-request id))
         (parsed (json-read-from-string request)))
    ;; Verify basic JSON-RPC structure
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "resources/list" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))))

(ert-deftest
    mcp-server-lib-test-create-resources-list-request-default-id
    ()
  "Test `mcp-server-lib-create-resources-list-request' with default ID."
  (let* ((request (mcp-server-lib-create-resources-list-request))
         (parsed (json-read-from-string request)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "resources/list" (alist-get 'method parsed)))
    (should (equal 1 (alist-get 'id parsed)))))

;;; `mcp-server-lib-create-resources-read-request' tests

(ert-deftest mcp-server-lib-test-create-resources-read-request-with-id
    ()
  "Test `mcp-server-lib-create-resources-read-request' with a specified ID."
  (let* ((id 42)
         (uri "test://resource")
         (request
          (mcp-server-lib-create-resources-read-request uri id))
         (parsed (json-read-from-string request)))
    ;; Verify basic JSON-RPC structure
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "resources/read" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))
    ;; Verify params
    (let ((params (alist-get 'params parsed)))
      (should (equal uri (alist-get 'uri params))))))

(ert-deftest
    mcp-server-lib-test-create-resources-read-request-default-id
    ()
  "Test `mcp-server-lib-create-resources-read-request' with default ID."
  (let* ((uri "test://resource")
         (request (mcp-server-lib-create-resources-read-request uri))
         (parsed (json-read-from-string request)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "resources/read" (alist-get 'method parsed)))
    (should (equal 1 (alist-get 'id parsed)))
    ;; Verify params
    (let ((params (alist-get 'params parsed)))
      (should (equal uri (alist-get 'uri params))))))

;;; tools/list tests

(ert-deftest mcp-server-lib-test-tools-list-one ()
  "Test `tools/list` returning one tool with correct fields and schema."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "test-tool"
        :description "A tool for testing"))
    (mcp-server-lib-test--verify-tool-list-request
     '(("test-tool" .
        ((description . "A tool for testing")
         (inputSchema . ((type . "object")))))))))

(ert-deftest mcp-server-lib-test-tools-list-with-title ()
  "Test that `tools/list` includes title in response."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "tool-with-title"
        :description "A tool for testing titles"
        :title "Friendly Tool Name"))
    (mcp-server-lib-test--verify-tool-list-request
     '(("tool-with-title" .
        ((description . "A tool for testing titles")
         (annotations . ((title . "Friendly Tool Name")))
         (inputSchema . ((type . "object")))))))))

(ert-deftest mcp-server-lib-test-tools-list-two ()
  "Test the `tools/list` method returning multiple tools with correct fields."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "test-tool-1"
        :description "First tool for testing")
       (#'mcp-server-lib-test--return-string
        :id "test-tool-2"
        :description "Second tool for testing"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-tool-list-request
      '(("test-tool-1" .
         ((description . "First tool for testing")
          (inputSchema . ((type . "object")))))
        ("test-tool-2" .
         ((description . "Second tool for testing")
          (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-server-lib-test-tools-list-zero ()
  "Test the `tools/list` method returning empty array with no tools."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources
   nil
   (mcp-server-lib-test--verify-tool-list-request '())))

(ert-deftest mcp-server-lib-test-tools-list-schema-one-arg-handler ()
  "Test that `tools/list` schema includes parameter descriptions."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-string-arg
        :id "requires-arg"
        :description "A tool that requires an argument"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-tool-schema-in-single-tool-list
      '(("input-string"
         "string"
         "test parameter for string input"))))))

(ert-deftest mcp-server-lib-test-tools-list-schema-two-param-handler
    ()
  "Test that `tools/list` schema includes multiple parameter descriptions."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-two-params
        :id "two-params"
        :description "A tool that requires two arguments"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-tool-schema-in-single-tool-list
      '(("first-name" "string" "Person's first name")
        ("last-name" "string" "Person's last name"))))))

(ert-deftest mcp-server-lib-test-tools-list-schema-optional-param ()
  "Test that `tools/list` schema correctly marks optional parameters.
Parameters after &optional should not be in the required array."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-one-optional-param
        :id "optional-param"
        :description "A tool with one optional parameter"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-tool-schema-in-single-tool-list
      '(("required-param" "string" "A required parameter")
        ("optional-param" "string" "An optional parameter"))
      '("optional-param")))))

(ert-deftest mcp-server-lib-test-tools-list-schema-all-optional ()
  "Test that `tools/list` schema correctly handles all optional parameters.
When all parameters are optional, required array should be empty."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-all-optional
        :id "all-optional"
        :description "A tool with all optional parameters"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-tool-schema-in-single-tool-list
      '(("param-a" "string" "First optional parameter")
        ("param-b" "string" "Second optional parameter"))
      '("param-a" "param-b")))))

(ert-deftest mcp-server-lib-test-tools-list-schema-multiple-optional
    ()
  "Test schema with one required and multiple optional parameters.
Only the required parameter should be in the required array."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-some-optional
        :id "multiple-optional"
        :description "A tool with multiple optional parameters"))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-tool-schema-in-single-tool-list
      '(("required-param" "string" "A required parameter")
        ("optional-a" "string" "First optional parameter")
        ("optional-b" "string" "Second optional parameter"))
      '("optional-a" "optional-b")))))

(ert-deftest mcp-server-lib-test-tools-call-two-param-handler ()
  "Test invoking a tool with two parameters."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-two-params
        :id "two-params"
        :description "A tool that requires two arguments"))
    (let* ((args '((first-name . "John") (last-name . "Doe")))
           (result (mcp-server-lib-ert-call-tool "two-params" args)))
      (should (string= "Hello, John Doe!" result)))))

(ert-deftest mcp-server-lib-test-tools-call-three-param-handler ()
  "Test invoking a tool with three parameters."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-three-params
        :id "three-params"
        :description "A tool that requires three arguments"))
    (let* ((args
            '((title . "Dr")
              (first-name . "Jane")
              (last-name . "Smith")))
           (result
            (mcp-server-lib-ert-call-tool "three-params" args)))
      (should (string= "Hello, Dr Jane Smith!" result)))))

(ert-deftest
    mcp-server-lib-test-tools-call-optional-param-with-all-params
    ()
  "Test invoking a tool with optional parameter, providing all parameters."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-one-optional-param
        :id "optional-tool"
        :description "Tool with optional parameter"))
    (let* ((args '((required-param . "req") (optional-param . "opt")))
           (result
            (mcp-server-lib-ert-call-tool "optional-tool" args)))
      (should (string= "Required: req, Optional: opt" result)))))

(ert-deftest
    mcp-server-lib-test-tools-call-optional-param-without-optional
    ()
  "Test invoking a tool with optional parameter, omitting the optional one."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-one-optional-param
        :id "optional-tool"
        :description "Tool with optional parameter"))
    (let* ((args '((required-param . "req")))
           (result
            (mcp-server-lib-ert-call-tool "optional-tool" args)))
      (should (string= "Required: req" result)))))

(ert-deftest mcp-server-lib-test-tools-call-all-optional-none-provided
    ()
  "Test invoking a tool where all parameters are optional, providing none."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-all-optional
        :id "all-optional-tool"
        :description "Tool with all optional parameters"))
    (let* ((args '())
           (result
            (mcp-server-lib-ert-call-tool "all-optional-tool" args)))
      (should (string= "None provided" result)))))

(ert-deftest mcp-server-lib-test-tools-call-all-optional-some-provided
    ()
  "Test invoking a tool where all parameters are optional, providing some."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-all-optional
        :id "all-optional-tool"
        :description "Tool with all optional parameters"))
    (let* ((args '((param-a . "A")))
           (result
            (mcp-server-lib-ert-call-tool "all-optional-tool" args)))
      (should (string= "Only A: A" result)))))

(ert-deftest
    mcp-server-lib-test-tools-call-multiple-optional-first-only
    ()
  "Test tool with multiple optional params, providing first optional only."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-some-optional
        :id "multi-opt"
        :description "Tool with multiple optional parameters"))
    (let* ((args '((required-param . "req") (optional-a . "A")))
           (result (mcp-server-lib-ert-call-tool "multi-opt" args)))
      (should (string= "Required: req, A: A" result)))))

(ert-deftest mcp-server-lib-test-tools-call-multiple-optional-sparse
    ()
  "Test tool with multiple optional params, providing second optional only.
This tests sparse optional parameter provision where optional-a is skipped
but optional-b is provided."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-some-optional
        :id "multi-opt"
        :description "Tool with multiple optional parameters"))
    (let* ((args '((required-param . "req") (optional-b . "B")))
           (result (mcp-server-lib-ert-call-tool "multi-opt" args)))
      (should (string= "Required: req, B: B" result)))))

(ert-deftest mcp-server-lib-test-tools-call-optional-missing-required
    ()
  "Test error when optional param provided but required param missing.
Verifies that required parameter validation works correctly even when
optional parameters are provided."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-one-optional-param
        :id "optional-tool"
        :description "Tool with optional parameter"))
    (mcp-server-lib-test--call-tool-expect-invalid-params
     "optional-tool"
     '((optional-param . "opt"))
     "Missing required parameter: required-param")))

(ert-deftest mcp-server-lib-test-tools-call-missing-required-param ()
  "Test that a multi-param tool with missing parameters errors."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-two-params
        :id "two-params"
        :description "A tool that requires two arguments"))
    ;; Call with only one parameter when two are required
    (mcp-server-lib-test--call-tool-expect-invalid-params
     "two-params"
     '((first-name . "John"))
     "Missing required parameter: last-name")))

(ert-deftest mcp-server-lib-test-tools-call-too-many-params ()
  "Test that calling a tool with extra parameters returns an error."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-two-params
        :id "two-params"
        :description "A tool that requires two arguments"))
    ;; Call with three parameters when only two are expected
    (let* ((args
            '((first-name . "John")
              (last-name . "Doe")
              (middle-name . "Extra"))) ; Extra parameter
           (request
            (mcp-server-lib-create-tools-call-request
             "two-params" 42 args))
           (response
            (mcp-server-lib-process-jsonrpc-parsed
             request mcp-server-lib-ert-server-id))
           (error-obj (alist-get 'error response)))
      (should error-obj)
      (should
       (equal
        mcp-server-lib-jsonrpc-error-invalid-params
        (alist-get 'code error-obj)))
      (should
       (string-match-p
        "Unexpected parameter" (alist-get 'message error-obj))))))


(ert-deftest mcp-server-lib-test-tools-list-read-only-hint ()
  "Test that `tools/list` response includes readOnlyHint=true."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "read-only-tool"
        :description "A tool that doesn't modify its environment"
        :read-only t))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-tool-list-request
      '(("read-only-tool" .
         ((description . "A tool that doesn't modify its environment")
          (annotations . ((readOnlyHint . t)))
          (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-server-lib-test-tools-list-read-only-hint-false ()
  "Test that `tools/list` response includes readOnlyHint=false."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "non-read-only-tool"
        :description "Tool that modifies its environment"
        :read-only nil))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-tool-list-request
      '(("non-read-only-tool" .
         ((description . "Tool that modifies its environment")
          (annotations . ((readOnlyHint . :json-false)))
          (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-server-lib-test-tools-list-multiple-annotations ()
  "Test `tools/list` response including multiple annotations."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "multi-annotated-tool"
        :description "A tool with multiple annotations"
        :title "Friendly Multi-Tool"
        :read-only t))
    (mcp-server-lib-ert-verify-req-success
     "tools/list"
     (mcp-server-lib-test--verify-tool-list-request
      '(("multi-annotated-tool" .
         ((description . "A tool with multiple annotations")
          (annotations
           . ((title . "Friendly Multi-Tool") (readOnlyHint . t)))
          (inputSchema . ((type . "object"))))))))))

;;; `mcp-server-lib-create-tools-call-request' tests

(ert-deftest mcp-server-lib-test-create-tools-call-request-id-and-args
    ()
  "Test `mcp-server-lib-create-tools-call-request' with ID and arguments."
  (let* ((tool-name "test-tool")
         (id 42)
         (args '(("arg1" . "value1") ("arg2" . "value2")))
         (request
          (mcp-server-lib-create-tools-call-request
           tool-name id args))
         (parsed (json-read-from-string request))
         (params (alist-get 'params parsed)))
    ;; Verify basic JSON-RPC structure
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/call" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))
    ;; Verify params structure
    (should params)
    (should (equal tool-name (alist-get 'name params)))
    (should (alist-get 'arguments params))
    (should
     (equal "value1" (alist-get 'arg1 (alist-get 'arguments params))))
    (should
     (equal
      "value2" (alist-get 'arg2 (alist-get 'arguments params))))))

(ert-deftest mcp-server-lib-test-create-tools-call-request-default-id
    ()
  "Test `mcp-server-lib-create-tools-call-request' with default ID."
  (let* ((tool-name "test-tool")
         (request
          (mcp-server-lib-create-tools-call-request tool-name))
         (parsed (json-read-from-string request))
         (params (alist-get 'params parsed)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/call" (alist-get 'method parsed)))
    (should (equal 1 (alist-get 'id parsed)))
    (should params)
    (should (equal tool-name (alist-get 'name params)))
    (should (equal '() (alist-get 'arguments params)))))

(ert-deftest mcp-server-lib-test-create-tools-call-request-empty-args
    ()
  "Test `mcp-server-lib-create-tools-call-request' with empty arguments list."
  (let* ((tool-name "test-tool")
         (id 43)
         (request
          (mcp-server-lib-create-tools-call-request tool-name id '()))
         (parsed (json-read-from-string request))
         (params (alist-get 'params parsed)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "tools/call" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))
    (should params)
    (should (equal tool-name (alist-get 'name params)))
    (should (equal '() (alist-get 'arguments params)))))

;;; tools/call tests

(ert-deftest mcp-server-lib-test-tools-call-mcp-server-lib-tool-throw
    ()
  "Test tool handler calling `mcp-server-lib-tool-throw'."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-mcp-server-lib-tool-throw
        :id "failing-tool"
        :description "A tool that always fails"))
    (mcp-server-lib-test--check-tool-call-error "failing-tool"
      (let* ((resp-obj
              (mcp-server-lib-process-jsonrpc-parsed
               request mcp-server-lib-ert-server-id))
             (text
              (mcp-server-lib-ert-check-text-response resp-obj t)))
        (should (string= "This tool intentionally fails" text))))))

(ert-deftest mcp-server-lib-test-tools-call-generic-error ()
  "Test that generic errors use standard JSON-RPC error format."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--generic-error-handler
        :id "generic-error-tool"
        :description "A tool that throws a generic error"))
    (mcp-server-lib-test--check-tool-call-error "generic-error-tool"
      (mcp-server-lib-test--check-jsonrpc-error
       request
       mcp-server-lib-jsonrpc-error-internal
       "Internal error executing tool: Generic error occurred"))))

(ert-deftest mcp-server-lib-test-tools-call-no-args ()
  "Test the `tools/call` request with a tool that takes no arguments."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-string-list
        :id "string-list-tool"
        :description "A tool that returns a string with items"))
    (let ((result
           (mcp-server-lib-ert-call-tool "string-list-tool" nil)))
      (should
       (string= mcp-server-lib-test--string-list-result result)))))

(ert-deftest mcp-server-lib-test-tools-call-empty-string ()
  "Test the `tools/call` request with a tool that returns an empty string."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-empty-string
        :id "empty-string-tool"
        :description "A tool that returns an empty string"))
    (mcp-server-lib-test--verify-tool-schema-in-single-tool-list '())

    (let ((result
           (mcp-server-lib-ert-call-tool "empty-string-tool" nil)))
      (should (string= "" result)))))

(ert-deftest mcp-server-lib-test-tools-call-with-string-arg ()
  "Test the `tools/call` request with a tool that takes a string argument."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-string-arg
        :id "string-arg-tool"
        :description "A tool that echoes a string argument"))
    (let* ((test-input "Hello, world!")
           (args `(("input-string" . ,test-input))))

      (let ((result
             (mcp-server-lib-ert-call-tool "string-arg-tool" args)))
        (should (string= (concat "Echo: " test-input) result))))))

(ert-deftest mcp-server-lib-test-tools-call-unregistered-tool ()
  "Test the `tools/call` request with a tool that was never registered."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--verify-tool-not-found
    mcp-server-lib-test--nonexistent-tool-id)))

(ert-deftest mcp-server-lib-test-tools-call-handler-returns-nil ()
  "Test tool handler that returns nil value."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-nil
        :id "nil-returning-tool"
        :description "A tool that returns nil"))
    (let* ((result
            (mcp-server-lib-test--call-tool "nil-returning-tool" 14))
           (response `((result . ,result)))
           (text (mcp-server-lib-ert-check-text-response response)))
      (should (string= "" text)))))

(ert-deftest mcp-server-lib-test-tools-call-handler-returns-non-string
    ()
  "Test tool handler that returns non-string value throws error."
  (mcp-server-lib-test--check-non-string-return-error
   #'mcp-server-lib-test--tool-handler-returns-list
   "list-returning-tool"
   15
   "cons"))

(ert-deftest mcp-server-lib-test-tools-call-handler-returns-vector ()
  "Test tool handler that returns vector throws type validation error."
  (mcp-server-lib-test--check-non-string-return-error
   #'mcp-server-lib-test--tool-handler-returns-vector
   "vector-returning-tool"
   16
   "vector"))

(ert-deftest mcp-server-lib-test-tools-call-handler-returns-number ()
  "Test tool handler that returns number throws type validation error."
  (mcp-server-lib-test--check-non-string-return-error
   #'mcp-server-lib-test--tool-handler-returns-number
   "number-returning-tool"
   17
   "integer"))

(ert-deftest mcp-server-lib-test-tools-call-handler-returns-symbol ()
  "Test tool handler that returns symbol throws type validation error."
  (mcp-server-lib-test--check-non-string-return-error
   #'mcp-server-lib-test--tool-handler-returns-symbol
   "symbol-returning-tool"
   18
   "symbol"))

(ert-deftest mcp-server-lib-test-tools-call-handler-undefined ()
  "Test calling a tool whose handler function no longer exists."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--handler-to-be-undefined
        :id "undefined-handler-tool"
        :description "A tool whose handler will be undefined"))
    (mcp-server-lib-test--with-undefined-function
        'mcp-server-lib-test--handler-to-be-undefined
      (mcp-server-lib-test--check-tool-call-error
          "undefined-handler-tool"
        ;; Try to call the tool - should return an error
        (mcp-server-lib-test--check-jsonrpc-error
         request mcp-server-lib-jsonrpc-error-internal
         (concat
          "Internal error executing tool: "
          (mcp-server-lib-test--emacs-error-message
           'void-function
           'mcp-server-lib-test--handler-to-be-undefined)))))))


;;; `mcp-server-lib-ert-process-tool-response' tests

(ert-deftest mcp-server-lib-test-ert-process-tool-response-success ()
  "Test `mcp-server-lib-ert-process-tool-response' on a success response."
  ;; Create a mock successful tool response with JSON content
  (let* ((json-data
          '((status . "ok") (count . 42) (items . ["a" "b" "c"])))
         (json-string (json-encode json-data))
         (response
          `((jsonrpc . "2.0")
            (id . 123)
            (result
             .
             ((content . [((type . "text") (text . ,json-string))])
              (isError . :json-false)))))
         (parsed-result
          (mcp-server-lib-ert-process-tool-response response)))
    ;; Verify the parsed JSON matches expected structure
    (should (equal "ok" (alist-get 'status parsed-result)))
    (should (equal 42 (alist-get 'count parsed-result)))
    (should (equal ["a" "b" "c"] (alist-get 'items parsed-result)))))

(ert-deftest mcp-server-lib-test-ert-process-tool-response-error ()
  "Test `mcp-server-lib-ert-process-tool-response' on an isError response."
  ;; Create a mock error response from tool
  (let* ((error-message "Tool execution failed: File not found")
         (response
          `((jsonrpc . "2.0")
            (id . 456)
            (result
             .
             ((content . [((type . "text") (text . ,error-message))])
              (isError . t))))))
    ;; Verify that it signals the expected error with correct message
    (should-error
     (mcp-server-lib-ert-process-tool-response response)
     :type 'mcp-server-lib-tool-error)
    ;; Check the error message is preserved
    (condition-case err
        (mcp-server-lib-ert-process-tool-response response)
      (mcp-server-lib-tool-error
       (should (string= error-message (car (cdr err))))))))

(ert-deftest mcp-server-lib-test-ert-process-tool-response-invalid ()
  "Test `mcp-server-lib-ert-process-tool-response' on an invalid response."
  ;; Test with JSON-RPC error present
  (let ((response-with-error
         `((jsonrpc . "2.0")
           (id . 789)
           (error
            . ((code . -32600) (message . "Invalid Request"))))))
    (should-error
     (mcp-server-lib-ert-process-tool-response response-with-error)))

  ;; Test with missing result field
  (let ((response-no-result `((jsonrpc . "2.0") (id . 789))))
    (should-error
     (mcp-server-lib-ert-process-tool-response response-no-result))))

;;; `mcp-server-lib-process-jsonrpc' tests

(ert-deftest mcp-server-lib-test-parse-error ()
  "Test that invalid JSON input returns a parse error."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--check-jsonrpc-error
    "This is not valid JSON"
    mcp-server-lib-jsonrpc-error-parse
    "Parse error: JSON readtable error: 84")))

(ert-deftest mcp-server-lib-test-method-not-found ()
  "Test that unknown methods return method-not-found error."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--check-jsonrpc-error
    (json-encode
     '(("jsonrpc" . "2.0") ("method" . "unknown/method") ("id" . 99)))
    mcp-server-lib-jsonrpc-error-method-not-found
    "Method not found: unknown/method")))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc ()
  "Test that valid JSON that is not JSON-RPC returns an invalid request error."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--check-jsonrpc-error
    (json-encode '(("name" . "Test Object") ("value" . 42)))
    mcp-server-lib-jsonrpc-error-invalid-request
    "Invalid Request: Not JSON-RPC 2.0")))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc-older-version ()
  "Test that JSON-RPC with older version (1.1) is rejected properly."
  (mcp-server-lib-test--check-invalid-jsonrpc-version "1.1"))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc-non-standard-version
    ()
  "Test that JSON-RPC with non-standard version string is rejected properly."
  (mcp-server-lib-test--check-invalid-jsonrpc-version "non-standard"))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc-missing-id ()
  "Test that JSON-RPC request lacking the `id` key is rejected properly."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--check-jsonrpc-error
    (json-encode '(("jsonrpc" . "2.0") ("method" . "tools/list")))
    mcp-server-lib-jsonrpc-error-invalid-request
    "Invalid Request: Missing required 'id' field")))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc-missing-method ()
  "Test that JSON-RPC request lacking the `method` key is rejected properly."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--check-jsonrpc-error
    (json-encode '(("jsonrpc" . "2.0") ("id" . 42)))
    mcp-server-lib-jsonrpc-error-invalid-request
    "Invalid Request: Missing required 'method' field")))

;;; `mcp-server-lib-process-jsonrpc-parsed' tests

(ert-deftest mcp-server-lib-test-process-jsonrpc-parsed ()
  "Test that `mcp-server-lib-process-jsonrpc-parsed' returns parsed response."
  (let ((response (mcp-server-lib-test--tools-list-parsed-response)))
    ;; Response should be a parsed alist, not a string
    (should (listp response))
    (should (alist-get 'result response))
    (should
     (arrayp (alist-get 'tools (alist-get 'result response))))))

;;; Logging tests

(ert-deftest mcp-server-lib-test-log-io-t ()
  "Test that when `mcp-server-lib-log-io' is t, JSON-RPC messages are logged."
  (setq mcp-server-lib-log-io t)

  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (let* ((request (mcp-server-lib-create-tools-list-request))
          (response
           (mcp-server-lib-process-jsonrpc
            request mcp-server-lib-ert-server-id)))

     (let ((log-buffer (get-buffer "*mcp-server-lib-log*")))
       (should log-buffer)

       (with-current-buffer log-buffer
         (let ((content (buffer-string))
               (expected-suffix
                (concat
                 "-> (request) [server:"
                 mcp-server-lib-ert-server-id
                 "] ["
                 request
                 "]\n"
                 "<- (response) [server:"
                 mcp-server-lib-ert-server-id
                 "] ["
                 response
                 "]\n")))
           (should (string-suffix-p expected-suffix content)))))))

  (setq mcp-server-lib-log-io nil))

(ert-deftest mcp-server-lib-test-log-io-nil ()
  "Test that when `mcp-server-lib-log-io' is nil, messages are not logged."
  (setq mcp-server-lib-log-io nil)

  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (let ((request (mcp-server-lib-create-tools-list-request)))
     (mcp-server-lib-process-jsonrpc
      request mcp-server-lib-ert-server-id)
     (should-not (get-buffer "*mcp-server-lib-log*")))))

;;; Misc tests

(ert-deftest mcp-server-lib-test-server-restart-preserves-tools ()
  "Test that server restart preserves registered tools."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "persistent-tool"
        :description "Test persistence across restarts"))
    (mcp-server-lib-stop)
    (mcp-server-lib-start)

    (let ((tools (mcp-server-lib-test--get-tool-list)))
      (should (= 1 (length tools)))
      (should
       (string=
        "persistent-tool" (alist-get 'name (aref tools 0)))))))

(ert-deftest mcp-server-lib-test-interactive-commands ()
  "Verify that all package commands are interactive."
  (should (commandp #'mcp-server-lib-start))
  (should (commandp #'mcp-server-lib-stop))
  (should (commandp #'mcp-server-lib-install))
  (should (commandp #'mcp-server-lib-uninstall))
  (should (commandp #'mcp-server-lib-reset-metrics))
  (should (commandp #'mcp-server-lib-show-metrics)))

(ert-deftest mcp-server-lib-test-describe-setup-shows-stopped-status
    ()
  "Test that describe-setup shows stopped status when server is stopped."
  (mcp-server-lib-test--do-describe-setup-test
      mcp-server-lib-test--describe-setup-stopped-regexp))

(ert-deftest mcp-server-lib-test-describe-setup-comprehensive ()
  "Test describe-setup output for a comprehensive multi-server setup.
Covers running status, alphabetical server order, per-server
:instructions/Refcount, nested tools with full property set, nested
resources with optional fields, different handler types, and recorded
metrics."
  (mcp-server-lib-test--with-server
    :id "beta"
    :tools
    `((mcp-server-lib-test--return-string
       :id "gamma-tool"
       :description "Gamma test tool"))
    :resources
    `(("gamma://resource"
       mcp-server-lib-test--return-string
       :name "Gamma Resource"))
    (mcp-server-lib-test--with-server
      :id "alpha"
      :instructions "Use the apple tool first."
      :tools
      `((mcp-server-lib-test--return-string
         :id "zebra-tool"
         :description "Zebra test tool"
         :title "Zebra Tool Title"
         :read-only t)
        (mcp-server-lib-test--tool-handler-empty-string
         :id "apple-tool"
         :description "Apple test tool"
         :title "Apple Tool Title")
        (,(lambda () "Mouse tool result")
         :id "mouse-tool"
         :description "Mouse test tool with lambda handler"))
      :resources
      `(("zebra://resource"
         mcp-server-lib-test--return-string
         :name "Zebra Resource"
         :description "Zebra resource description")
        ("apple://resource"
         ,(lambda () "Apple resource content")
         :name "Apple Resource"
         :description "Apple resource description with lambda handler"
         :mime-type "application/json")
        ("mouse://resource"
         mcp-server-lib-test--return-string
         :name "Mouse Resource"
         :mime-type "text/plain"))
      (mcp-server-lib-ert-with-server
       :tools t
       :resources t
       :instructions "Use the apple tool first."
       ;; Call some tools on alpha to generate metrics
       (dotimes (_ 42)
         (mcp-server-lib-ert-call-tool "apple-tool" nil))
       (dotimes (_ 10)
         (mcp-server-lib-ert-call-tool "zebra-tool" nil))
       (mcp-server-lib-test--do-describe-setup-test
           mcp-server-lib-test--describe-setup-comprehensive-regexp)))))

(ert-deftest mcp-server-lib-test-describe-setup-empty-state ()
  "Test that describe-setup handles empty state correctly.
Uses `mcp-server-lib-start' directly rather than
`mcp-server-lib-ert-with-server', whose initialize handshake would
register a server and so make the state non-empty."
  (unwind-protect
      (progn
        (mcp-server-lib-start)
        (mcp-server-lib-test--do-describe-setup-test
            mcp-server-lib-test--describe-setup-empty-regexp))
    (mcp-server-lib-stop)))

(ert-deftest mcp-server-lib-test-describe-setup-nil-metrics ()
  "Test that describe-setup handles nil metrics correctly."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "test-tool"
        :description "Test tool"))
    (mcp-server-lib-test--do-describe-setup-test
        mcp-server-lib-test--describe-setup-nil-metrics-regexp)))

(defconst mcp-server-lib-test--describe-setup-legacy-regexp
  (concat
   "\\`MCP Server Setup\n\n"
   "Status: Stopped\n\n"
   "Servers:\n"
   "\\s-+default\n"
   "\\s-+Tools:\n"
   "\\s-+legacy-tool\n"
   "\\s-+Description: Legacy tool\n"
   "\\s-+Handler: mcp-server-lib-test--return-string\n"
   "\\s-+Usage: 0 calls\n"
   "\\'")
  "Regexp for describe-setup with a tool registered via the obsolete API.
The server block under `default' must appear with the Tools sub-section
but NO `Instructions:' line and NO `Refcount:' line, because the legacy
`mcp-server-lib-register-tool' shim does not populate
`mcp-server-lib--servers'.")

(ert-deftest mcp-server-lib-test-describe-setup-legacy-register-tool
    ()
  "Legacy `register-tool' produces a server block without metadata fields.
The server-id appears in the Servers section because tools are
registered under it, but Instructions/Refcount lines are absent — those
are sourced only from `mcp-server-lib--servers', which the legacy shim
does not touch."
  (with-suppressed-warnings ((obsolete mcp-server-lib-register-tool)
                             (obsolete
                              mcp-server-lib-unregister-tool))
    (unwind-protect
        (progn
          (mcp-server-lib-register-tool
           #'mcp-server-lib-test--return-string
           :id "legacy-tool"
           :description "Legacy tool")
          (mcp-server-lib-test--do-describe-setup-test
              mcp-server-lib-test--describe-setup-legacy-regexp))
      (mcp-server-lib-unregister-tool "legacy-tool"))))

;;; `mcp-server-lib-with-error-handling' tests

(ert-deftest mcp-server-lib-test-with-error-handling-success ()
  "Test that `mcp-server-lib-with-error-handling' executes BODY normally."
  (let ((result (mcp-server-lib-with-error-handling (+ 1 2))))
    (should (= 3 result))))

(ert-deftest mcp-server-lib-test-with-error-handling-catches-error ()
  "Test that `mcp-server-lib-with-error-handling' catches errors."
  (should-error
   (mcp-server-lib-with-error-handling (error "Test error"))
   :type 'mcp-server-lib-tool-error))

(ert-deftest mcp-server-lib-test-with-error-handling-error-message ()
  "Test that `mcp-server-lib-with-error-handling' formats errors correctly."
  (condition-case err
      (mcp-server-lib-with-error-handling
       (error "Original error message"))
    (mcp-server-lib-tool-error
     (should
      (string-match
       "Error: (error \"Original error message\")" (cadr err))))))

(ert-deftest
    mcp-server-lib-test-with-error-handling-multiple-expressions
    ()
  "Test that `mcp-server-lib-with-error-handling' handles multiple forms."
  (let ((result
         (mcp-server-lib-with-error-handling
          (let ((test-var 42))
            (+ test-var 8)))))
    (should (= 50 result))))

;;; Script installation tests

(ert-deftest mcp-server-lib-test-installed-script-path ()
  "Installed path is the stdio script under the configured directory."
  (let* ((mcp-server-lib-install-directory "/tmp/mcp-test-dir")
         (path (mcp-server-lib-installed-script-path)))
    (should
     (string= "emacs-mcp-stdio.sh" (file-name-nondirectory path)))
    (should
     (string=
      (file-name-as-directory
       mcp-server-lib-install-directory)
      (file-name-directory path)))))

(ert-deftest mcp-server-lib-test-install ()
  "Test script installation to temporary directory."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (mcp-server-lib-install))
          (should
           (file-exists-p (mcp-server-lib-installed-script-path)))
          (should
           (file-executable-p
            (mcp-server-lib-installed-script-path))))
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-install-overwrite ()
  "Test script installation when file already exists."
  (mcp-server-lib-test--with-temp-install-dir
    (write-region "existing content" nil target)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (mcp-server-lib-install))
    (should (file-exists-p target))
    (should (file-executable-p target))
    (should (> (file-attribute-size (file-attributes target)) 20))))

(ert-deftest mcp-server-lib-test-install-cancel ()
  "Test cancelling installation when file exists."
  (mcp-server-lib-test--with-temp-install-dir
    (write-region "existing content" nil target)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (should-error (mcp-server-lib-install) :type 'user-error))
    (should
     (string=
      "existing content"
      (with-temp-buffer
        (insert-file-contents target)
        (buffer-string))))))

(ert-deftest mcp-server-lib-test-uninstall ()
  "Test script removal from temporary directory."
  (mcp-server-lib-test--with-temp-install-dir
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (mcp-server-lib-install)
      (should (file-exists-p target))
      (mcp-server-lib-uninstall))
    (should-not (file-exists-p target))))

(ert-deftest mcp-server-lib-test-uninstall-missing ()
  "Test uninstalling when script doesn't exist."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir))
    (unwind-protect
        (should-error (mcp-server-lib-uninstall) :type 'user-error)
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-uninstall-cancel ()
  "Test cancelling uninstall."
  (mcp-server-lib-test--with-temp-install-dir
    (write-region "test content" nil target)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (mcp-server-lib-uninstall))
    (should (file-exists-p target))))

;;; Metrics tests

(ert-deftest mcp-server-lib-test-metrics ()
  "Test metrics collection and reset."
  (mcp-server-lib-test--with-tools
      ( ;; Register a test tool
       (#'mcp-server-lib-test--return-string
        :id "metrics-test-tool"
        :description "Tool for testing metrics"))
    ;; Make some operations to generate metrics
    (mcp-server-lib-process-jsonrpc
     (mcp-server-lib-create-tools-list-request 100)
     mcp-server-lib-ert-server-id)
    (mcp-server-lib-test--call-tool "metrics-test-tool" 101)
    (mcp-server-lib-test--call-tool "metrics-test-tool" 102)

    ;; Verify non-zero before reset
    (let ((summary-before (mcp-server-lib-metrics-summary)))
      (should (stringp summary-before))
      (should-not
       (string-match "^MCP metrics: 0 calls" summary-before)))

    ;; Reset
    (mcp-server-lib-reset-metrics)

    ;; Verify zero after reset
    (let ((summary-after (mcp-server-lib-metrics-summary)))
      (should (stringp summary-after))
      (should (string-match "^MCP metrics: 0 calls" summary-after)))))

(ert-deftest mcp-server-lib-test-show-metrics ()
  "Test metrics display command."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "display-test-tool"
        :description "Tool for testing display"))
    ;; Generate some metrics
    (mcp-server-lib-process-jsonrpc
     (mcp-server-lib-create-tools-list-request 200)
     mcp-server-lib-ert-server-id)
    (mcp-server-lib-test--call-tool "display-test-tool" 201)

    ;; Show metrics
    (mcp-server-lib-show-metrics)

    ;; Verify buffer exists and contains expected content
    (with-current-buffer "*MCP Metrics*"
      (let ((content (buffer-string)))
        (should (string-match "MCP Usage Metrics" content))
        (should
         (string-match
          "Method Calls:\nMethod +Calls +Errors +Error %\n-" content))
        ;; Should have at least 1 tools/list call from our test
        (should (string-match "tools/list\\s-+\\([0-9]+\\)" content))
        (let ((tools-list-count
               (string-to-number (match-string 1 content))))
          (should (>= tools-list-count 1)))

        (should
         (string-match
          "Notifications:\nNotification +Calls\n-" content))
        (should
         (string-match
          "Tool Usage:\nTool +Calls +Errors +Error %\n-" content))
        ;; Should have exactly 1 call to our test tool
        (should
         (string-match
          "display-test-tool\\s-+1\\s-+0\\s-+0\\.0%" content))
        (should (string-match "Summary:" content))
        (should (string-match "Methods: [0-9]+ calls" content))
        (should (string-match "Tools: [0-9]+ calls" content))))))

(ert-deftest mcp-server-lib-test-metrics-reset-on-start ()
  "Test that starting the server resets metrics."
  ;; First part: generate metrics and verify they exist
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-process-jsonrpc
    (mcp-server-lib-create-tools-list-request 100)
    mcp-server-lib-ert-server-id)
   ;; Verify metrics exist
   (let ((summary (mcp-server-lib-metrics-summary)))
     (should (stringp summary))
     ;; Should show at least 2 calls (initialize + tools/list)
     (should
      (string-match
       "[2-9][0-9]* calls\\|[0-9][0-9]+ calls" summary))))
  ;; Second part: start server again and verify metrics were reset
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   ;; After server restart, only the initialize call should be counted
   (let ((summary (mcp-server-lib-metrics-summary)))
     (should (string-match "^MCP metrics: [12] calls" summary)))))

(ert-deftest mcp-server-lib-test-metrics-on-stop ()
  "Test metrics display on server stop."
  ;; Capture messages throughout the entire test
  (cl-letf* ((messages nil)
             ((symbol-function 'message)
              (lambda (fmt &rest args)
                (push (apply #'format fmt args) messages))))
    (mcp-server-lib-test--with-tools
        ((#'mcp-server-lib-test--return-string
          :id "stop-test-tool"
          :description "Tool for testing stop"))
      ;; Generate some metrics
      (mcp-server-lib-test--call-tool "stop-test-tool" 300))

    ;; Check that metrics summary was displayed when server stopped
    (should
     (cl-some
      (lambda (msg)
        (string-match "MCP metrics:.*calls.*errors" msg))
      messages))))

;;; Resource tests

(ert-deftest mcp-server-lib-test-resources-list-empty ()
  "Test resources/list with no registered resources."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources
   nil
   (mcp-server-lib-test--check-no-resources)))

(ert-deftest mcp-server-lib-test-register-resource ()
  "Test registering a direct resource."
  (mcp-server-lib-test--with-resources
      (("test://resource1"
        #'mcp-server-lib-test--return-string
        :name "Test Resource"
        :description "A test resource"
        :mime-type "text/plain"))))

(ert-deftest mcp-server-lib-test-register-resource-minimal ()
  "Test registering a resource with only required fields."
  (mcp-server-lib-test--with-resources
      (("test://minimal"
        #'mcp-server-lib-test--return-string
        :name "Minimal Resource"))
    ;; Verify resource can be read without mime-type
    (mcp-server-lib-ert-verify-resource-read
     "test://minimal"
     '((uri . "test://minimal") (text . "test result")))))

(ert-deftest mcp-server-lib-test-resources-read ()
  "Test reading a resource."
  (mcp-server-lib-test--with-resources
      (("test://resource1"
        #'mcp-server-lib-test--return-string
        :name "Test Resource"
        :mime-type "text/plain"))
    ;; Read the resource
    (mcp-server-lib-ert-verify-resource-read
     "test://resource1"
     '((uri . "test://resource1")
       (mimeType . "text/plain")
       (text . "test result")))))

(ert-deftest mcp-server-lib-test-resources-read-handler-nil ()
  "Test that a nil-returning resource handler yields empty text."
  (mcp-server-lib-test--with-resources
      (("test://nil-resource"
        #'mcp-server-lib-test--return-nil
        :name "Nil Resource"))
    ;; Read the resource
    (mcp-server-lib-ert-verify-resource-read
     "test://nil-resource"
     '((uri . "test://nil-resource") (text . nil)))))

(ert-deftest mcp-server-lib-test-resources-read-not-found ()
  "Test reading a non-existent resource returns error."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--read-resource-error
    "test://nonexistent"
    mcp-server-lib-jsonrpc-error-invalid-params
    "Resource not found: test://nonexistent")))

(ert-deftest mcp-server-lib-test-register-resource-duplicate ()
  "Test registering the same resource twice increments ref count."
  (mcp-server-lib-test--with-obsolete-resource-api
    (unwind-protect
        (progn
          (mcp-server-lib-register-resource
           "test://resource1"
           #'mcp-server-lib-test--return-string
           :name "Test Resource")
          (unwind-protect
              (progn
                (mcp-server-lib-register-resource
                 "test://resource1"
                 #'mcp-server-lib-test--return-string
                 :name "Test Resource")
                ;; Two registrations under same URI: list still contains
                ;; one entry.
                (mcp-server-lib-test--check-single-resource
                 '((uri . "test://resource1")
                   (name . "Test Resource"))))
            (mcp-server-lib-unregister-resource "test://resource1"))
          ;; After inner unregister (ref count 2 -> 1); resource still
          ;; listed because outer registration is still active.
          (mcp-server-lib-test--check-single-resource
           '((uri . "test://resource1") (name . "Test Resource"))))
      (mcp-server-lib-unregister-resource "test://resource1"))
    ;; After outer unregister (ref count 1 -> 0); resource no longer
    ;; listed.
    (mcp-server-lib-test--check-no-resources)))

(ert-deftest mcp-server-lib-test-register-resource-explicit-server-id
    ()
  "Round-trip the obsolete `register-resource' shims with explicit :server-id.
Exercises the legacy shim's `:server-id' extraction (which strips
`:server-id' from the property list before forwarding to the validator)
and `unregister-resource''s optional SERVER-ID argument."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-register-resource)
                             (obsolete
                              mcp-server-lib-unregister-resource))
    (let ((mcp-server-lib-ert-server-id "shim-server"))
      (mcp-server-lib-ert-with-server
       :tools nil
       :resources nil
       (mcp-server-lib-register-resource
        "shim://resource"
        #'mcp-server-lib-test--return-string
        :name "Shim Resource"
        :server-id "shim-server")
       (mcp-server-lib-test--check-single-resource
        '((uri . "shim://resource") (name . "Shim Resource")))
       (should
        (mcp-server-lib-unregister-resource
         "shim://resource" "shim-server"))
       (mcp-server-lib-test--check-no-resources)))))

(ert-deftest
    mcp-server-lib-test-register-resource-duplicate-server-id-rejected
    ()
  "Duplicate `:server-id' in obsolete `register-resource' shim is rejected.
The previous implementation silently used the first `:server-id' and
discarded subsequent occurrences via `plist-remove'."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-register-resource)
                             (obsolete
                              mcp-server-lib-unregister-resource))
    (should-error
     (mcp-server-lib-register-resource
      "dup-sid://r"
      #'mcp-server-lib-test--return-string
      :server-id "dup-sid-first"
      :name "r"
      :server-id "dup-sid-second")
     :type 'error)
    ;; Rejection must leave no orphan resource under either server-id.
    (should-not
     (mcp-server-lib-unregister-resource
      "dup-sid://r" "dup-sid-first"))
    (should-not
     (mcp-server-lib-unregister-resource
      "dup-sid://r" "dup-sid-second"))))

(ert-deftest
    mcp-server-lib-test-register-resource-trailing-server-id-rejected
    ()
  "Trailing `:server-id' (no value) in obsolete `register-resource' rejected.
The previous implementation silently defaulted the server-id to
\"default\" because `plist-get' returned nil for the dangling key."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-register-resource)
                             (obsolete
                              mcp-server-lib-unregister-resource))
    (unwind-protect
        (should-error
         (mcp-server-lib-register-resource
          "trail-sid://r"
          #'mcp-server-lib-test--return-string
          :name "r"
          :server-id)
         :type 'error)
      (mcp-server-lib-unregister-resource "trail-sid://r"))))

(ert-deftest
    mcp-server-lib-test-register-resource-non-string-server-id-rejected
    ()
  "Non-string `:server-id' in obsolete `register-resource' shim is rejected.
Mirrors the obsolete `register-tool' contract: a non-string value
would otherwise be used as a hash key and create an orphan
registration."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-register-resource))
    (should-error
     (mcp-server-lib-register-resource
      "non-str-sid://r"
      #'mcp-server-lib-test--return-string
      :name "r"
      :server-id 42)
     :type 'error)))

(ert-deftest
    mcp-server-lib-test-register-resource-duplicate-property-rejected
    ()
  "Duplicate property key in obsolete `register-resource' shim is rejected.
Mirrors the bundled `register-server' contract: a re-added `:name'
would otherwise silently keep the first value via `plist-get'."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-register-resource))
    (should-error
     (mcp-server-lib-register-resource
      "dup-name://r"
      #'mcp-server-lib-test--return-string
      :name "first"
      :name "second")
     :type 'error)))

(ert-deftest
    mcp-server-lib-test-register-resource-unknown-property-rejected
    ()
  "Unknown property key in obsolete `register-resource' shim is rejected.
Mirrors the bundled `register-server' contract: a typo'd key would
otherwise be silently ignored."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-register-resource))
    (should-error
     (mcp-server-lib-register-resource
      "unknown-prop://r"
      #'mcp-server-lib-test--return-string
      :name "r"
      :typo "x")
     :type 'error)))

(ert-deftest mcp-server-lib-test-register-resource-error-missing-name
    ()
  "Test that resource registration with missing :name produces an error."
  (mcp-server-lib-test--should-error-register
   mcp-server-lib-register-resource
   "test://resource"
   #'mcp-server-lib-test--return-string
   :description "Resource without name"))

(ert-deftest
    mcp-server-lib-test-register-resource-error-missing-handler
    ()
  "Test that resource registration with non-function handler produces an error."
  (mcp-server-lib-test--should-error-register
   mcp-server-lib-register-resource
   "test://resource"
   "not-a-function"
   :name "Test Resource"))

(ert-deftest mcp-server-lib-test-register-resource-error-missing-uri
    ()
  "Test that resource registration with nil URI produces an error."
  (mcp-server-lib-test--should-error-register
   mcp-server-lib-register-resource
   nil
   #'mcp-server-lib-test--return-string
   :name "Test Resource"))

(ert-deftest
    mcp-server-lib-test-register-resource-error-non-string-uri
    ()
  "Test that non-string URI is rejected."
  (mcp-server-lib-test--should-error-register
   mcp-server-lib-register-resource
   42
   #'mcp-server-lib-test--return-string
   :name "Test Resource"))

(ert-deftest
    mcp-server-lib-test-register-resource-error-non-string-name
    ()
  "Test that non-string :name is rejected."
  (mcp-server-lib-test--should-error-register
   mcp-server-lib-register-resource
   "test://resource"
   #'mcp-server-lib-test--return-string
   :name 42))

(ert-deftest
    mcp-server-lib-test-register-resource-error-non-string-description
    ()
  "Test that non-string :description is rejected."
  (mcp-server-lib-test--should-error-register
   mcp-server-lib-register-resource
   "test://resource"
   #'mcp-server-lib-test--return-string
   :name "Test Resource"
   :description 42))

(ert-deftest
    mcp-server-lib-test-register-resource-error-non-string-mime-type
    ()
  "Test that non-string :mime-type is rejected."
  (mcp-server-lib-test--should-error-register
   mcp-server-lib-register-resource
   "test://resource"
   #'mcp-server-lib-test--return-string
   :name "Test Resource"
   :mime-type 42))

(ert-deftest mcp-server-lib-test-unregister-resource-nonexistent ()
  "Test `mcp-server-lib-unregister-resource' on a missing resource."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-unregister-resource))
    (mcp-server-lib-ert-with-server
     :tools nil
     :resources nil
     (should-not
      (mcp-server-lib-unregister-resource "test://nonexistent")))))

(ert-deftest
    mcp-server-lib-test-unregister-resource-returns-t-on-decrement
    ()
  "`mcp-server-lib-unregister-resource' returns t when ref-count is decremented.
A non-removing call (entry still registered because its reference count
was greater than one) must return t for a static URI, matching the
docstring contract that t means the resource was found."
  (mcp-server-lib-test--with-obsolete-resource-api
    (unwind-protect
        (progn
          (mcp-server-lib-register-resource
           "rc://r"
           #'mcp-server-lib-test--return-string
           :name "r")
          (mcp-server-lib-register-resource
           "rc://r"
           #'mcp-server-lib-test--return-string
           :name "r")
          ;; ref-count 2 -> 1: returns t even though entry remains.
          (should (mcp-server-lib-unregister-resource "rc://r"))
          (mcp-server-lib-test--check-single-resource
           '((uri . "rc://r") (name . "r"))))
      (mcp-server-lib-unregister-resource "rc://r"))))

(ert-deftest
    mcp-server-lib-test-unregister-resource-template-returns-t-on-decrement
    ()
  "`mcp-server-lib-unregister-resource' yields t on a template decrement.
Templates take a separate dispatch path from static URIs in
`mcp-server-lib-unregister-resource'; pin the same contract for the
template branch."
  (mcp-server-lib-test--with-obsolete-resource-api
    (unwind-protect
        (progn
          (mcp-server-lib-register-resource
           "rc://{id}"
           #'mcp-server-lib-test--resource-template-handler-dump-params
           :name "t")
          (mcp-server-lib-register-resource
           "rc://{id}"
           #'mcp-server-lib-test--resource-template-handler-dump-params
           :name "t")
          ;; ref-count 2 -> 1: returns t even though template remains.
          (should (mcp-server-lib-unregister-resource "rc://{id}"))
          (mcp-server-lib-test--check-templates
           '(((uriTemplate . "rc://{id}") (name . "t")))))
      (mcp-server-lib-unregister-resource "rc://{id}"))))

(ert-deftest mcp-server-lib-test-resources-list-multiple ()
  "Test listing multiple registered resources."
  (mcp-server-lib-test--with-resources
      (("test://resource1"
        #'mcp-server-lib-test--return-string
        :name "First Resource"
        :description "The first test resource")
       ("test://resource2"
        #'mcp-server-lib-test--return-string
        :name "Second Resource"
        :mime-type "text/markdown"))
    ;; Verify both resources are listed
    (let ((resources (mcp-server-lib-ert-get-resource-list)))
      (should (= 2 (length resources)))
      ;; Check each resource
      (let ((resource1
             (mcp-server-lib-test--find-resource-by-uri
              "test://resource1" resources))
            (resource2
             (mcp-server-lib-test--find-resource-by-uri
              "test://resource2" resources)))
        ;; Verify first resource
        (should resource1)
        (should (equal (alist-get 'name resource1) "First Resource"))
        (should
         (equal
          (alist-get 'description resource1)
          "The first test resource"))
        (should-not (alist-get 'mimeType resource1))
        ;; Verify second resource
        (should resource2)
        (should (equal (alist-get 'name resource2) "Second Resource"))
        (should-not (alist-get 'description resource2))
        (should
         (equal (alist-get 'mimeType resource2) "text/markdown"))))))

(ert-deftest mcp-server-lib-test-resources-read-handler-error ()
  "Test that resource handler errors yield a JSON-RPC error and bump metrics."
  (mcp-server-lib-test--with-resources
      (("test://error-resource"
        #'mcp-server-lib-test--generic-error-handler
        :name "Error Resource"))
    (mcp-server-lib-test--check-resource-read-error
      "test://error-resource"
      mcp-server-lib-jsonrpc-error-internal
      "Error reading resource test://error-resource: Generic error occurred")))

(ert-deftest mcp-server-lib-test-resource-signal-error-invalid-params
    ()
  "Test signaling invalid params error from resource handler."
  (mcp-server-lib-test--with-resources
      (("test://signal-error"
        #'mcp-server-lib-test--resource-signal-error-invalid-params
        :name "Signal Error Resource"))
    (mcp-server-lib-test--check-resource-read-error
      "test://signal-error"
      mcp-server-lib-jsonrpc-error-invalid-params
      "Custom invalid params message")))

(ert-deftest mcp-server-lib-test-resource-signal-error-internal ()
  "Test signaling internal error from resource handler."
  (mcp-server-lib-test--with-resources
      (("test://internal-error"
        #'mcp-server-lib-test--resource-signal-error-internal
        :name "Internal Error Resource"))
    (mcp-server-lib-test--check-resource-read-error
      "test://internal-error"
      mcp-server-lib-jsonrpc-error-internal
      "Database connection failed")))

(ert-deftest
    mcp-server-lib-test-resource-regular-error-backward-compat
    ()
  "Test that regular errors still work and return internal error code."
  (mcp-server-lib-test--with-resources
      (("test://regular-error"
        #'mcp-server-lib-test--generic-error-handler
        :name "Regular Error Resource"))
    (mcp-server-lib-test--check-resource-read-error
      "test://regular-error"
      mcp-server-lib-jsonrpc-error-internal
      "Error reading resource test://regular-error: Generic error occurred")))

(ert-deftest mcp-server-lib-test-resources-read-handler-undefined ()
  "Test reading a resource whose handler function no longer exists."
  (mcp-server-lib-test--with-resources
      (("test://undefined-handler"
        #'mcp-server-lib-test--handler-to-be-undefined
        :name "Undefined Handler Resource"))
    (mcp-server-lib-test--with-undefined-function
        'mcp-server-lib-test--handler-to-be-undefined
      (mcp-server-lib-test--check-resource-read-error
        "test://undefined-handler"
        mcp-server-lib-jsonrpc-error-internal
        (concat
         "Error reading resource test://undefined-handler: "
         (mcp-server-lib-test--emacs-error-message
          'void-function
          'mcp-server-lib-test--handler-to-be-undefined))))))

(ert-deftest mcp-server-lib-test-resources-list-mixed ()
  "Test listing both direct resources and templates."
  (mcp-server-lib-test--with-resources
      (("test://direct1"
        #'mcp-server-lib-test--return-string
        :name "Direct Resource 1")
       ("org://{filename}"
        #'mcp-server-lib-test--resource-template-handler-dump-params
        :name "Org Template")
       ("test://direct2"
        #'mcp-server-lib-test--return-string
        :name "Direct Resource 2"
        :mime-type "text/plain")
       ("doc://{docname}"
        #'mcp-server-lib-test--resource-template-handler-dump-params-2
        :name "Doc Template"))
    ;; Verify we can read both types
    (mcp-server-lib-ert-verify-resource-read
     "test://direct1"
     '((uri . "test://direct1") (text . "test result")))
    (mcp-server-lib-ert-verify-resource-read
     "org://example.org"
     '((uri . "org://example.org")
       (text . "params: ((\"filename\" . \"example.org\"))")))))

;;; Resource Template Invalid Syntax Tests

(defun mcp-server-lib-test--assert-invalid-template-registration (uri)
  "Assert that registering a resource template with URI fails.
Routes through `mcp-server-lib-register-server' to exercise the bundled
form's `--build-resource-entry' code path."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (should-error
    (mcp-server-lib-test--register-server
     :id "default"
     :resources
     `((,uri
        mcp-server-lib-test--resource-template-handler-dump-params
        :name "Test Template"))))))

(defun mcp-server-lib-test--assert-invalid-handler-registration
    (handler handler-desc)
  "Check that registering a resource with invalid HANDLER & HANDLER-DESC fails."
  (mcp-server-lib-test--should-error-register
   mcp-server-lib-register-resource
   "test://resource"
   handler
   :name
   (format "Resource with %s" handler-desc)))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-unclosed
    ()
  "Test resource template with unclosed variable syntax error."
  (mcp-server-lib-test--assert-invalid-template-registration
   "org://{filename"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-unmatched-close
    ()
  "Test resource template with unmatched closing brace syntax error."
  (mcp-server-lib-test--assert-invalid-template-registration
   "org://filename}"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-empty
    ()
  "Test resource template with empty variable name syntax error."
  (mcp-server-lib-test--assert-invalid-template-registration
   "org://{}"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-numeric-variable
    ()
  "Test resource template with numeric variable name is rejected."
  (mcp-server-lib-test--assert-invalid-template-registration
   "org://{123}/content"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-special-chars-variable
    ()
  "Test resource template with special characters in variable name is rejected."
  (mcp-server-lib-test--assert-invalid-template-registration
   "org://{var-name}/content"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-scheme-only
    ()
  "Test resource template with only scheme and no path."
  (mcp-server-lib-test--assert-invalid-template-registration
   "org://"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-no-scheme
    ()
  "Test resource template without a scheme."
  (mcp-server-lib-test--assert-invalid-template-registration "{foo}"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-single-colon
    ()
  "Test resource template with single colon instead of ://."
  (mcp-server-lib-test--assert-invalid-template-registration
   "foo:bar"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-single-slash
    ()
  "Test resource template with :/ instead of ://."
  (mcp-server-lib-test--assert-invalid-template-registration
   "foo:/bar"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-no-colon
    ()
  "Test resource template with path but no scheme separator."
  (mcp-server-lib-test--assert-invalid-template-registration
   "foo/bar"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-extra-colon
    ()
  "Test resource template with extra colon before ://."
  (mcp-server-lib-test--assert-invalid-template-registration
   "foo:://bar"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-no-scheme-prefix
    ()
  "Test resource template starting with ://."
  (mcp-server-lib-test--assert-invalid-template-registration
   "://bar"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-double-slash-only
    ()
  "Test resource template with // but no scheme."
  (mcp-server-lib-test--assert-invalid-template-registration
   "foo//bar"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-trailing-colon
    ()
  "Test resource template with trailing colon only."
  (mcp-server-lib-test--assert-invalid-template-registration "foo:"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-colon-slash
    ()
  "Test resource template with :/ at end."
  (mcp-server-lib-test--assert-invalid-template-registration "foo:/"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-leading-colon
    ()
  "Test resource template starting with colon."
  (mcp-server-lib-test--assert-invalid-template-registration
   ":foo//bar"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-space-in-scheme
    ()
  "Test resource template with space in scheme."
  (mcp-server-lib-test--assert-invalid-template-registration
   "foo bar://baz"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-numeric-scheme
    ()
  "Test resource template with numeric scheme."
  (mcp-server-lib-test--assert-invalid-template-registration
   "123://bar"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-underscore-in-scheme
    ()
  "Test resource template with underscore in scheme."
  (mcp-server-lib-test--assert-invalid-template-registration
   "foo_bar://baz"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-hyphen-first
    ()
  "Test resource template with hyphen as first character in scheme."
  (mcp-server-lib-test--assert-invalid-template-registration
   "-foo://bar"))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-empty-string
    ()
  "Test resource template with empty string."
  (mcp-server-lib-test--assert-invalid-template-registration ""))

(ert-deftest
    mcp-server-lib-test-resource-template-invalid-syntax-whitespace-only
    ()
  "Test resource template with whitespace only."
  (mcp-server-lib-test--assert-invalid-template-registration "   "))

(ert-deftest mcp-server-lib-test-register-resource-nil-handler ()
  "Test resource registration with nil handler fails."
  (mcp-server-lib-test--assert-invalid-handler-registration
   nil "nil handler"))

(ert-deftest mcp-server-lib-test-register-resource-string-handler ()
  "Test resource registration with string handler fails."
  (mcp-server-lib-test--assert-invalid-handler-registration
   "not a function" "string handler"))

(ert-deftest mcp-server-lib-test-register-resource-number-handler ()
  "Test resource registration with number handler fails."
  (mcp-server-lib-test--assert-invalid-handler-registration
   42 "number handler"))

(ert-deftest mcp-server-lib-test-resource-template-simple-variable ()
  "Test resource template with simple variable."
  (mcp-server-lib-test--with-resources
      (("org://{filename}"
        #'mcp-server-lib-test--resource-template-handler-dump-params
        :name "Org file content"
        :mime-type "text/plain"))
    ;; Test successful match
    (mcp-server-lib-ert-verify-resource-read
     "org://projects.org"
     '((uri . "org://projects.org")
       (mimeType . "text/plain")
       (text . "params: ((\"filename\" . \"projects.org\"))")))
    ;; Test non-matching prefix
    (mcp-server-lib-test--read-resource-error
     "file://projects.org"
     mcp-server-lib-jsonrpc-error-invalid-params
     "Resource not found: file://projects.org")))

(ert-deftest mcp-server-lib-test-resource-template-reserved-expansion
    ()
  "Test resource template with reserved expansion."
  (mcp-server-lib-test--with-resources
      (("org://{+path}"
        #'mcp-server-lib-test--resource-template-handler-dump-params
        :name "Org path"
        :description "Access org files by path with slashes"))
    ;; Test with slashes in variable
    (mcp-server-lib-ert-verify-resource-read
     "org://folder/subfolder/file.org"
     '((uri . "org://folder/subfolder/file.org")
       (text
        . "params: ((\"path\" . \"folder/subfolder/file.org\"))")))))

(ert-deftest mcp-server-lib-test-resource-template-multiple-variables
    ()
  "Test resource template with multiple variables."
  (mcp-server-lib-test--with-resources
      (("org://{filename}/headline/{+path}"
        #'mcp-server-lib-test--resource-template-handler-dump-params
        :name "Org headline"
        :mime-type "text/plain"))
    (mcp-server-lib-ert-verify-resource-read
     "org://todo.org/headline/Tasks/Urgent"
     '((uri . "org://todo.org/headline/Tasks/Urgent")
       (mimeType . "text/plain")
       (text
        .
        "params: ((\"filename\" . \"todo.org\") (\"path\" . \"Tasks/Urgent\"))")))))

(ert-deftest mcp-server-lib-test-register-resource-missing-name ()
  "Test error when registering template without name."
  (with-suppressed-warnings ((obsolete
                              mcp-server-lib-register-resource))
    (mcp-server-lib-ert-with-server
     :tools nil
     :resources nil
     (should-error
      (mcp-server-lib-register-resource
       "test://{id}"
       #'mcp-server-lib-test--resource-template-handler-dump-params)
      :type 'error))))

(ert-deftest mcp-server-lib-test-resources-read-direct-precedence ()
  "Test that direct resources take precedence over resource templates."
  (mcp-server-lib-test--with-template-resources
      '(("test://{id}"
         mcp-server-lib-test--resource-template-handler-dump-params
         :name "Template Resource")
        ("test://exact"
         mcp-server-lib-test--return-string
         :name "Direct Resource"))
    ;; Should get direct resource content
    (mcp-server-lib-ert-verify-resource-read
     "test://exact"
     '((uri . "test://exact") (text . "test result")))))

(ert-deftest
    mcp-server-lib-test-resources-read-multiple-template-schemes
    ()
  "Test that resource templates with different schemes route correctly."
  (mcp-server-lib-test--with-template-resources
      '(("org://{filename}"
         mcp-server-lib-test--resource-template-handler-dump-params
         :name "Org Files")
        ("doc://{docname}"
         mcp-server-lib-test--resource-template-handler-dump-params-2
         :name "Doc Files"))
    (mcp-server-lib-ert-verify-resource-read
     "org://projects.org"
     '((uri . "org://projects.org")
       (text . "params: ((\"filename\" . \"projects.org\"))")))
    (mcp-server-lib-ert-verify-resource-read
     "doc://manual.pdf"
     '((uri . "doc://manual.pdf")
       (text
        . "Handler-2: params: ((\"docname\" . \"manual.pdf\"))")))))

(ert-deftest mcp-server-lib-test-resources-read-no-template-match ()
  "Test error when no resource template matches the URI."
  (mcp-server-lib-test--with-template-resources
      '(("test://{id}"
         mcp-server-lib-test--resource-template-handler-dump-params
         :name "Test Template"))
    ;; Verify the template is registered
    (mcp-server-lib-test--check-templates
     '(((uriTemplate . "test://{id}") (name . "Test Template"))))
    ;; Try to read with non-matching URI
    (mcp-server-lib-test--read-resource-error
     "other://123"
     mcp-server-lib-jsonrpc-error-invalid-params
     "Resource not found: other://123")))

(ert-deftest
    mcp-server-lib-test-resource-template-empty-parameter-value
    ()
  "Test resource template matching with empty parameter value."
  (mcp-server-lib-test--with-resources
      (("org://{filename}"
        #'mcp-server-lib-test--resource-template-handler-dump-params
        :name "Org file template"))
    ;; Test URI with empty filename parameter
    (mcp-server-lib-ert-verify-resource-read
     "org://"
     '((uri . "org://") (text . "params: ((\"filename\" . \"\"))")))))

(ert-deftest mcp-server-lib-test-unregister-resource-multiple ()
  "Test unregistering one resource when multiple are registered."
  (mcp-server-lib-test--with-resources
      (("org://{filename}"
        #'mcp-server-lib-test--resource-template-handler-dump-params
        :name "Org Files")
       ("doc://{docname}"
        #'mcp-server-lib-test--resource-template-handler-dump-params-2
        :name "Doc Files")
       ("test://{id}"
        #'mcp-server-lib-test--resource-template-handler-dump-params
        :name "Test Files"))
    ;; Verify all three are listed
    (let ((resources
           (mcp-server-lib-ert-get-resource-templates-list)))
      (should (= 3 (length resources))))
    ;; Unregister the middle one
    (with-suppressed-warnings ((obsolete
                                mcp-server-lib-unregister-resource))
      (mcp-server-lib-unregister-resource "doc://{docname}"))
    ;; Verify only two remain
    (let ((resources
           (mcp-server-lib-ert-get-resource-templates-list)))
      (should (= 2 (length resources)))
      ;; Check the remaining ones
      (should
       (mcp-server-lib-test--find-resource-by-uri-template
        "org://{filename}" resources))
      (should
       (mcp-server-lib-test--find-resource-by-uri-template
        "test://{id}" resources))
      ;; Verify the unregistered one is gone
      (should-not
       (mcp-server-lib-test--find-resource-by-uri-template
        "doc://{docname}" resources)))
    ;; Verify the remaining templates still work
    (mcp-server-lib-ert-verify-resource-read
     "org://test.org"
     '((uri . "org://test.org")
       (text . "params: ((\"filename\" . \"test.org\"))")))
    (mcp-server-lib-ert-verify-resource-read
     "test://123"
     '((uri . "test://123") (text . "params: ((\"id\" . \"123\"))")))
    ;; Verify the unregistered template no longer matches
    (mcp-server-lib-test--read-resource-error
     "doc://manual.pdf"
     mcp-server-lib-jsonrpc-error-invalid-params
     "Resource not found: doc://manual.pdf")))

(ert-deftest mcp-server-lib-test-resources-read-template-handler-error
    ()
  "Test template handler errors bumping metrics and returning JSON-RPC errors."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--with-server
     :resources
     '(("error://{id}"
        mcp-server-lib-test--template-handler-error
        :name "Error Template"))
     ;; Verify the template is registered
     (mcp-server-lib-test--check-templates
      '(((uriTemplate . "error://{id}") (name . "Error Template"))))
     (mcp-server-lib-test--check-resource-read-error
       "error://test"
       mcp-server-lib-jsonrpc-error-internal
       "Error reading resource error://test: Generic error occurred"))))

(ert-deftest mcp-server-lib-test-resources-read-template-handler-nil
    ()
  "Test nil-returning template handler produces valid response with empty text."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--with-server
     :resources
     '(("nil://{id}"
        mcp-server-lib-test--resource-template-handler-nil
        :name "Nil Template"))
     ;; Verify the template is registered
     (mcp-server-lib-test--check-templates
      '(((uriTemplate . "nil://{id}") (name . "Nil Template"))))
     ;; Read the resource
     (mcp-server-lib-ert-verify-resource-read
      "nil://test" '((uri . "nil://test") (text . nil))))))

(ert-deftest
    mcp-server-lib-test-resources-read-template-handler-undefined
    ()
  "Test reading a resource template whose handler function no longer exists."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--with-server
     :resources
     '(("undefined://{id}"
        mcp-server-lib-test--handler-to-be-undefined
        :name "Undefined Handler Template"))
     ;; Verify the template is registered
     (mcp-server-lib-test--check-templates
      '(((uriTemplate . "undefined://{id}")
         (name . "Undefined Handler Template"))))
     (mcp-server-lib-test--with-undefined-function
         'mcp-server-lib-test--handler-to-be-undefined
       (mcp-server-lib-ert-with-metrics-tracking
        (("resources/read" 1 1))
        ;; Try to read the resource - should return an error
        (mcp-server-lib-test--read-resource-error
         "undefined://test-123" mcp-server-lib-jsonrpc-error-internal
         (concat
          "Error reading resource undefined://test-123: "
          (mcp-server-lib-test--emacs-error-message
           'void-function
           'mcp-server-lib-test--handler-to-be-undefined))))))))

(ert-deftest
    mcp-server-lib-test-resource-template-scheme-case-insensitive
    ()
  "Test that URI schemes should be case-insensitive per RFC 3986."
  (mcp-server-lib-test--with-resources
      (("test://{id}"
        #'mcp-server-lib-test--resource-template-handler-dump-params
        :name "Test Template"))
    ;; Test uppercase scheme should match
    (mcp-server-lib-ert-verify-resource-read
     "TEST://123"
     '((uri . "TEST://123") (text . "params: ((\"id\" . \"123\"))")))
    ;; Test mixed case scheme should match
    (mcp-server-lib-ert-verify-resource-read
     "Test://456"
     '((uri . "Test://456")
       (text . "params: ((\"id\" . \"456\"))")))))

(ert-deftest
    mcp-server-lib-test-resource-template-variable-names-case-sensitive
    ()
  "Test that variable names in templates are case-sensitive per RFC 6570."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--with-server
     :resources
     '(("test://{username}"
        mcp-server-lib-test--resource-template-handler-dump-params
        :name "Lowercase Template")
       ("test://{USERNAME}"
        mcp-server-lib-test--resource-template-handler-dump-params-2
        :name "Uppercase Template"))
     ;; Both templates should be registered
     (mcp-server-lib-test--check-templates
      '(((uriTemplate . "test://{username}")
         (name . "Lowercase Template"))
        ((uriTemplate . "test://{USERNAME}")
         (name . "Uppercase Template"))))
     ;; Test that they extract different variables
     (mcp-server-lib-ert-verify-resource-read
      "test://john"
      '((uri . "test://john")
        (text . "params: ((\"username\" . \"john\"))"))))))

(ert-deftest
    mcp-server-lib-test-resource-template-path-literals-case-sensitive
    ()
  "Test that literal path segments are case-sensitive."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--with-server
     :resources
     '(("test://path/{id}"
        mcp-server-lib-test--resource-template-handler-dump-params
        :name "Lowercase Path Template")
       ("test://PATH/{id}"
        mcp-server-lib-test--resource-template-handler-dump-params-2
        :name "Uppercase Path Template"))
     ;; Both templates should be registered
     (mcp-server-lib-test--check-templates
      '(((uriTemplate . "test://path/{id}")
         (name . "Lowercase Path Template"))
        ((uriTemplate . "test://PATH/{id}")
         (name . "Uppercase Path Template"))))
     ;; Test lowercase path matches only lowercase template
     (mcp-server-lib-ert-verify-resource-read
      "test://path/123"
      '((uri . "test://path/123")
        (text . "params: ((\"id\" . \"123\"))")))
     ;; Test uppercase path matches only uppercase template
     (mcp-server-lib-ert-verify-resource-read
      "test://PATH/456"
      '((uri . "test://PATH/456")
        (text . "Handler-2: params: ((\"id\" . \"456\"))")))
     ;; Test mixed case path doesn't match either
     (mcp-server-lib-test--read-resource-error
      "test://Path/789"
      mcp-server-lib-jsonrpc-error-invalid-params
      "Resource not found: test://Path/789"))))

(ert-deftest
    mcp-server-lib-test-resource-template-unicode-in-variables
    ()
  "Test Unicode characters in variable values with proper percent-encoding."
  (mcp-server-lib-test--with-resources
      (("org://{filename}"
        #'mcp-server-lib-test--resource-template-handler-dump-params
        :name "Org file template"))
    ;; Test with direct Unicode character in URI
    (mcp-server-lib-ert-verify-resource-read
     "org://café.org"
     '((uri . "org://café.org")
       (text . "params: ((\"filename\" . \"café.org\"))")))
    ;; Test with percent-encoded Unicode in URI
    (mcp-server-lib-ert-verify-resource-read
     "org://caf%C3%A9.org"
     '((uri . "org://caf%C3%A9.org")
       (text . "params: ((\"filename\" . \"caf%C3%A9.org\"))")))
    ;; Test with multiple Unicode characters
    (mcp-server-lib-ert-verify-resource-read
     "org://文档.org"
     '((uri . "org://文档.org")
       (text . "params: ((\"filename\" . \"文档.org\"))")))))

(ert-deftest
    mcp-server-lib-test-resource-template-percent-encoded-extraction
    ()
  "Test that extracted parameters remain percent-encoded."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--with-server
     :resources
     '(("file://{path}"
        mcp-server-lib-test--resource-template-handler-dump-params
        :name "File template"))
     ;; Test spaces remain encoded
     (mcp-server-lib-ert-verify-resource-read
      "file://my%20document.txt"
      '((uri . "file://my%20document.txt")
        (text . "params: ((\"path\" . \"my%20document.txt\"))")))
     ;; Test Unicode remains encoded
     (mcp-server-lib-ert-verify-resource-read
      "file://caf%C3%A9.txt"
      '((uri . "file://caf%C3%A9.txt")
        (text . "params: ((\"path\" . \"caf%C3%A9.txt\"))")))
     ;; Test special characters remain encoded
     (mcp-server-lib-ert-verify-resource-read
      "file://file%2Bwith%2Bplus.txt"
      '((uri . "file://file%2Bwith%2Bplus.txt")
        (text
         . "params: ((\"path\" . \"file%2Bwith%2Bplus.txt\"))"))))))

(ert-deftest
    mcp-server-lib-test-resource-template-reserved-expansion-passthrough
    ()
  "Test that {+var} allows reserved chars without encoding."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   (mcp-server-lib-test--with-server
     :resources
     '(("file:///{+path}"
        mcp-server-lib-test--resource-template-handler-dump-params
        :name "File path template"))
     ;; Test mixed reserved characters
     (mcp-server-lib-ert-verify-resource-read
      "file:///path/with?query=value#section"
      '((uri . "file:///path/with?query=value#section")
        (text
         .
         "params: ((\"path\" . \"path/with?query=value#section\"))"))))))

(ert-deftest mcp-server-lib-test-resources-read-malformed-params ()
  "Test resources/read with invalid params (a string, not an object)."
  (mcp-server-lib-test--with-resources
      (("test://resource"
        #'mcp-server-lib-test--return-string
        :name "Test Resource"))
    ;; Test with string params instead of object
    (mcp-server-lib-test--check-resource-read-request-error
     "invalid string params"
     mcp-server-lib-jsonrpc-error-internal
     "Internal error: Wrong type argument: listp, \"invalid string params\"")))

(ert-deftest mcp-server-lib-test-resources-read-missing-uri ()
  "Test resources/read without uri parameter."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   ;; Test missing uri parameter
   (mcp-server-lib-test--check-resource-read-request-error
    nil ; No uri
    mcp-server-lib-jsonrpc-error-invalid-params
    "Resource not found: nil")))

(ert-deftest mcp-server-lib-test-resources-read-numeric-uri ()
  "Test resources/read with numeric uri."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   ;; Test with number uri
   (mcp-server-lib-test--check-resource-read-request-error
    '((uri . 123))
    mcp-server-lib-jsonrpc-error-invalid-params
    "Resource not found: 123")))

(ert-deftest mcp-server-lib-test-resources-read-array-uri ()
  "Test resources/read with array uri."
  (mcp-server-lib-ert-with-server
   :tools nil
   :resources nil
   ;; Test with array uri
   (mcp-server-lib-test--check-resource-read-request-error
    '((uri . ["test" "array"]))
    mcp-server-lib-jsonrpc-error-invalid-params
    "Resource not found: [test array]")))

(ert-deftest
    mcp-server-lib-test-resource-template-handler-wrong-signature
    ()
  "Test template handler that doesn't accept params argument."
  (mcp-server-lib-test--with-resources
      (("test://{id}"
        #'mcp-server-lib-test--return-string
        :name "Wrong Signature Handler"))
    (mcp-server-lib-test--read-resource-error
     "test://123" mcp-server-lib-jsonrpc-error-internal
     (concat
      "Error reading resource test://123: "
      (mcp-server-lib-test--wrong-args-message
       #'mcp-server-lib-test--return-string 1)))))

;;; Multi-server isolation tests

(ert-deftest mcp-server-lib-test-multi-server-tool-dispatch ()
  "Test multi-server tool isolation.
Verifies that tools with the same ID registered to different servers
maintain separate namespaces."
  (mcp-server-lib-test--with-server
    :id "server1"
    :tools
    '((mcp-server-lib-test--return-string
       :id "test-tool"
       :description "Server 1 tool"))
    (mcp-server-lib-test--with-server
      :id "server2"
      :tools
      '((mcp-server-lib-test--tool-handler-empty-string
         :id "test-tool"
         :description "Server 2 tool"))
      (mcp-server-lib-test--with-servers '(("server1"
                                            :tools t
                                            :resources nil)
                                           ("server2"
                                            :tools t
                                            :resources nil))
        ;; Call tool on server1 - should get "test result"
        (let ((mcp-server-lib-ert-server-id "server1"))
          (should
           (string=
            "test result"
            (mcp-server-lib-ert-call-tool "test-tool" nil))))

        ;; Call tool on server2 - should get ""
        (let ((mcp-server-lib-ert-server-id "server2"))
          (should
           (string=
            "" (mcp-server-lib-ert-call-tool "test-tool" nil))))))))

(ert-deftest mcp-server-lib-test-multi-server-resource-dispatch ()
  "Test multi-server resource isolation.
Verifies that resources with the same URI registered to different servers
maintain separate namespaces."
  (mcp-server-lib-test--with-server
    :id "server1"
    :resources
    '(("test://resource"
       mcp-server-lib-test--return-string
       :name "Test Resource"
       :description "Server 1 resource"))
    (mcp-server-lib-test--with-server
      :id "server2"
      :resources
      '(("test://resource"
         mcp-server-lib-test--tool-handler-empty-string
         :name "Test Resource"
         :description "Server 2 resource"))
      (mcp-server-lib-test--with-servers '(("server1"
                                            :tools nil
                                            :resources t)
                                           ("server2"
                                            :tools nil
                                            :resources t))
        ;; Read from server1 - should get "test result"
        (let ((mcp-server-lib-ert-server-id "server1"))
          (mcp-server-lib-ert-verify-resource-read
           "test://resource"
           '((uri . "test://resource") (text . "test result"))))

        ;; Read from server2 - should get ""
        (let ((mcp-server-lib-ert-server-id "server2"))
          (mcp-server-lib-ert-verify-resource-read
           "test://resource"
           '((uri . "test://resource") (text . ""))))))))

(ert-deftest mcp-server-lib-test-multi-server-template-dispatch ()
  "Test multi-server resource template isolation.
Verifies that resource templates with the same pattern registered to different
servers maintain separate namespaces."
  (mcp-server-lib-test--with-server
    :id "server1"
    :resources
    '(("test://{id}"
       mcp-server-lib-test--resource-template-handler-dump-params
       :name "Test Template"))
    (mcp-server-lib-test--with-server
      :id "server2"
      :resources
      '(("test://{id}"
         mcp-server-lib-test--resource-template-handler-dump-params-2
         :name "Test Template"))
      (mcp-server-lib-test--with-servers '(("server1"
                                            :tools nil
                                            :resources t)
                                           ("server2"
                                            :tools nil
                                            :resources t))
        ;; Read from server1 - should get handler 1 output
        (let ((mcp-server-lib-ert-server-id "server1"))
          (mcp-server-lib-ert-verify-resource-read
           "test://123"
           '((uri . "test://123")
             (text . "params: ((\"id\" . \"123\"))"))))

        ;; Read from server2 - should get handler 2 output
        (let ((mcp-server-lib-ert-server-id "server2"))
          (mcp-server-lib-ert-verify-resource-read
           "test://123"
           '((uri . "test://123")
             (text . "Handler-2: params: ((\"id\" . \"123\"))"))))))))

;;; Meta tests

(ert-deftest mcp-server-lib-test-meta-version-strings-agree ()
  "Test that the package version is consistent across all sites.
The `Eask' file's `(package ...)' form, the `;; Version:' headers of
`mcp-server-lib.el' and `mcp-server-lib-ert.el', and the `NEWS'
top-section heading must all report the same version."
  (let* ((eask-content (mcp-server-lib-test--read-repo-file "Eask"))
         (el-content
          (mcp-server-lib-test--read-repo-file "mcp-server-lib.el"))
         (ert-content
          (mcp-server-lib-test--read-repo-file
           "mcp-server-lib-ert.el"))
         (news-content (mcp-server-lib-test--read-repo-file "NEWS"))
         (eask-version
          (and (string-match
                "(package[ \t\n]+\"[^\"]+\"[ \t\n]+\"\\([^\"]+\\)\""
                eask-content)
               (match-string 1 eask-content)))
         (el-version
          (and (string-match
                "^;;[ \t]+Version:[ \t]+\\(\\S-+\\)" el-content)
               (match-string 1 el-content)))
         (ert-version
          (and (string-match
                "^;;[ \t]+Version:[ \t]+\\(\\S-+\\)" ert-content)
               (match-string 1 ert-content)))
         (news-version
          (and
           (string-match
            "^\\* Changes in mcp-server-lib \\(\\S-+\\)" news-content)
           (match-string 1 news-content))))
    (unless (stringp eask-version)
      (ert-fail
       "version not found in Eask via `(package ... \"VERSION\")'"))
    (unless (stringp el-version)
      (ert-fail
       "version not found in mcp-server-lib.el via `;; Version:'"))
    (unless (stringp ert-version)
      (ert-fail
       "version not found in mcp-server-lib-ert.el via `;; Version:'"))
    (unless (stringp news-version)
      (ert-fail
       "version not found in NEWS via `* Changes in mcp-server-lib VERSION'"))
    (should (equal eask-version el-version))
    (should (equal eask-version ert-version))
    (should (equal eask-version news-version))))

(provide 'mcp-server-lib-test)

;; Local Variables:
;; package-lint-main-file: "mcp-server-lib.el"
;; End:

;;; mcp-server-lib-test.el ends here
