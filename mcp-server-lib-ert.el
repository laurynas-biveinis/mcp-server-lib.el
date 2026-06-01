;;; mcp-server-lib-ert.el --- ERT test utilities for MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Keywords: tools, testing
;; Version: 0.4.0
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

;; Test utilities for writing ERT tests for MCP servers.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mcp-server-lib)
(require 'mcp-server-lib-metrics)
(require 'mcp-server-lib-commands)

(defvar mcp-server-lib-ert-server-id "default"
  "Server ID used by ERT test helper functions.
Child packages testing a single server should set this once at the top of
their test file using setq.  Multi-server tests should use let-binding to
temporarily override this value.")

(defconst mcp-server-lib-ert-default-version "1.0.0-test"
  "Default server :version used by test helpers that register a server.
Test helpers that register a server without an explicit :version use
this value, and the `initialize' assertion defaults its expected
version to it, so registration and assertion agree by default.")

(defun mcp-server-lib-ert-check-text-response
    (response &optional expected-error)
  "Validate RESPONSE structure and extract text content.
If EXPECTED-ERROR is non-nil, expects isError to be true.
Returns the text content string on success.
Signals test failure if response structure is invalid."
  (let ((result (alist-get 'result response)))
    ;; Response must have a result field
    (should result)
    ;; Check result has exactly the expected fields
    (let ((result-keys (mapcar #'car result)))
      (should (= 2 (length result-keys)))
      (should (member 'content result-keys))
      (should (member 'isError result-keys)))
    ;; Check content field structure
    (let ((content (alist-get 'content result)))
      (should (arrayp content))
      (should (= 1 (length content)))
      ;; Check content item structure
      (let* ((text-item (aref content 0))
             (item-keys (mapcar #'car text-item)))
        (should (= 2 (length item-keys)))
        (should (member 'type item-keys))
        (should (member 'text item-keys))
        ;; Check content item values
        (should (string= "text" (alist-get 'type text-item)))
        (let ((text (alist-get 'text text-item)))
          (should (stringp text))
          ;; Check isError field
          (should
           (eq
            (alist-get 'isError result)
            (if expected-error
                t
              :json-false)))
          ;; Return the text content
          text)))))

(defmacro mcp-server-lib-ert-with-metrics-tracking
    (metrics-specs &rest body)
  "Execute BODY and verify metrics changed as expected.
METRICS-SPECS is a list of (METRICS-KEY EXPECTED-CALLS EXPECTED-ERRORS) lists.
Returns the result of the last form in BODY.

Arguments:
  METRICS-SPECS - List of metric specifications, each containing:
    - METRICS-KEY: String key for the metric to track
    - EXPECTED-CALLS: Number of expected call increments
    - EXPECTED-ERRORS: Number of expected error increments
  BODY - Forms to execute while tracking metrics

Example:
  (mcp-server-lib-ert-with-metrics-tracking
      ((\"initialize\" 1 0)
       (\"tools/list\" 2 0))
    ;; Code that should increment initialize calls by 1
    ;; and tools/list calls by 2 with no errors
    ...)"
  (declare (indent 1) (debug t))
  (let ((before-bindings '())
        (after-checks '())
        (result-var (gensym "result")))
    ;; Build bindings and checks for each metric
    (dolist (spec metrics-specs)
      (let* ((key (car spec))
             (expected-calls (cadr spec))
             (expected-errors (caddr spec))
             (metrics-var (gensym "metrics"))
             (calls-var (gensym "calls"))
             (errors-var (gensym "errors")))
        ;; Add before bindings
        (push `(,metrics-var (mcp-server-lib-metrics-get ,key))
              before-bindings)
        (push `(,calls-var
                (mcp-server-lib-metrics-calls ,metrics-var))
              before-bindings)
        (push `(,errors-var
                (mcp-server-lib-metrics-errors ,metrics-var))
              before-bindings)
        ;; Add after checks
        (push `(let ((metrics-after
                      (mcp-server-lib-metrics-get ,key)))
                 (should
                  (= (+ ,calls-var ,expected-calls)
                     (mcp-server-lib-metrics-calls metrics-after)))
                 (should
                  (= (+ ,errors-var ,expected-errors)
                     (mcp-server-lib-metrics-errors metrics-after))))
              after-checks)))
    `(let* (,@
            (nreverse before-bindings)
            (,result-var
             (progn
               ,@body)))
       ,@
       (nreverse after-checks)
       ,result-var)))

(defun mcp-server-lib-ert--validate-jsonrpc-response
    (response expected-payload-field &optional expected-id)
  "Validate JSON-RPC RESPONSE structure and return payload field.
EXPECTED-PAYLOAD-FIELD should be either \\='result or \\='error.
If EXPECTED-ID is provided, validates that the response ID matches it.
Returns the value of the expected payload field."
  (should (memq expected-payload-field '(result error)))
  (let ((response-keys (mapcar #'car response))
        (payload (alist-get expected-payload-field response)))
    (should (= 3 (length response-keys)))
    (should (member 'jsonrpc response-keys))
    (should (member 'id response-keys))
    (should payload)
    (should (string= "2.0" (alist-get 'jsonrpc response)))
    ;; Verify id field exists (value can be nil for error responses)
    (should (assq 'id response))
    (when expected-id
      (let ((id (alist-get 'id response)))
        (should (equal expected-id id))))
    payload))

(defmacro mcp-server-lib-ert-verify-req-success (method &rest body)
  "Execute BODY and verify METHOD metrics show success (+1 call, +0 errors).
Captures metrics before BODY execution and asserts after that:
- calls increased by 1
- errors stayed the same

Arguments:
  METHOD - The MCP method name to track (e.g., \"tools/list\")
  BODY - Forms to execute"
  (declare (indent defun) (debug t))
  `(mcp-server-lib-ert-with-metrics-tracking ((,method 1 0))
     ,@body))

(defun mcp-server-lib-ert-get-success-result (method request)
  "Process REQUEST and return the result from a successful response.
METHOD is the JSON-RPC method name for metrics verification.
This function expects the request to succeed and will fail the test if an
error is present in the response.  It verifies that the response contains no
error and that the method metrics show success before returning the result.

Arguments:
  METHOD - The MCP method name for metrics tracking (e.g., \"initialize\")
  REQUEST - The JSON-RPC request to process

Returns the \\='result field from the response.

Example:
  (let* ((request (mcp-server-lib-create-tools-list-request))
         (tools (mcp-server-lib-ert-get-success-result \"tools/list\" request)))
    ;; tools now contains the tools array from the response
    (should (arrayp tools)))"
  (mcp-server-lib-ert-verify-req-success method
    (let ((resp-obj
           (mcp-server-lib-process-jsonrpc-parsed
            request mcp-server-lib-ert-server-id)))
      (mcp-server-lib-ert--validate-jsonrpc-response
       resp-obj 'result))))

(defun mcp-server-lib-ert--get-initialize-result ()
  "Send an MCP \\='initialize request and return its result.
This is a convenience function for tests that need to send the standard
initialize request and get the result.

Returns the result field from the initialize response."
  (mcp-server-lib-ert-get-success-result
   "initialize"
   (json-encode
    `(("jsonrpc" . "2.0")
      ("method" . "initialize") ("id" . 15)
      ("params" .
       (("protocolVersion" . ,mcp-server-lib-protocol-version)
        ("capabilities" . ,(make-hash-table))))))))

(cl-defun
 mcp-server-lib-ert-assert-initialize-result
 (init-result
  tools
  resources
  &key
  instructions
  (name mcp-server-lib-ert-server-id)
  (version mcp-server-lib-ert-default-version))
 "Assert the structure of an initialize result.
INIT-RESULT is the result from an initialize request.
TOOLS is a boolean indicating if tools capability is expected.
RESOURCES is a boolean indicating if resources capability is expected.

Optional INSTRUCTIONS keyword (specified as `:instructions'):
- omitted or nil: assert the `instructions' field is not present.
- string: assert the `instructions' field equals that string.

Optional NAME keyword (`:name'): expected `serverInfo.name'.  Defaults
to `mcp-server-lib-ert-server-id'.
Optional VERSION keyword (`:version'): expected `serverInfo.version'.
Defaults to `mcp-server-lib-ert-default-version'.

This function validates:
- Protocol version matches the expected version
- Server info contains the expected server name and version
- Capabilities match the expected state for tools and resources
- The `instructions' field per :instructions"
 (cl-check-type instructions (or null string))
 (let ((protocol-version (alist-get 'protocolVersion init-result))
       (capabilities (alist-get 'capabilities init-result))
       (server-info (alist-get 'serverInfo init-result)))
   (should (string= mcp-server-lib-protocol-version protocol-version))
   (should (string= name (alist-get 'name server-info)))
   (should (string= version (alist-get 'version server-info)))
   ;; Verify capabilities match expectations
   (when tools
     (should (assoc 'tools capabilities))
     ;; Empty objects {} in JSON are parsed as nil in Elisp
     (should-not (alist-get 'tools capabilities)))
   (when resources
     (should (assoc 'resources capabilities))
     (should-not (alist-get 'resources capabilities)))
   ;; Verify exact count
   (should
    (= (+ (if tools
              1
            0)
          (if resources
              1
            0))
       (length capabilities)))
   (cond
    ((null instructions)
     (should-not (assq 'instructions init-result)))
    (t
     (should (assq 'instructions init-result))
     (should
      (string=
       instructions (alist-get 'instructions init-result)))))))

(defun mcp-server-lib-ert-get-resource-list ()
  "Get the successful response to a \\='resources/list request.
It sends a resources/list request, verifies success, and returns the
resources array.

Returns an array of resource objects.

Example:
  (let ((resources (mcp-server-lib-ert-get-resource-list)))
    (should (= 2 (length resources)))
    (should (string= \"test://resource1\"
                     (alist-get \\='uri (aref resources 0)))))"
  (let ((result
         (alist-get
          'resources
          (mcp-server-lib-ert-get-success-result
           "resources/list"
           (mcp-server-lib-create-resources-list-request)))))
    (should (arrayp result))
    result))

(defun mcp-server-lib-ert-get-resource-templates-list ()
  "Get the successful response to a \\='resources/templates/list request.
It sends a resources/templates/list request, verifies success, and returns the
resource templates array.

Returns an array of resource template objects.

Example:
  (let ((templates (mcp-server-lib-ert-get-resource-templates-list)))
    (should (= 1 (length templates)))
    (should (string= \"org://{filename}\"
                     (alist-get \\='uriTemplate (aref templates 0)))))"
  (let
      ((result
        (alist-get
         'resourceTemplates
         (mcp-server-lib-ert-get-success-result
          "resources/templates/list"
          (mcp-server-lib-create-resources-templates-list-request)))))
    (should (arrayp result))
    result))

(cl-defmacro
 mcp-server-lib-ert-with-server (&rest body)
 "Run BODY with MCP server active and initialized.
Starts the server, sends initialize request, then runs BODY.

BODY may begin with any of the following keyword/value pairs, in any
order, followed by the forms to execute:
  :tools         Boolean; non-nil to expect tools capability
  :resources     Boolean; non-nil to expect resources capability
  :instructions  Optional; if a string, asserts the `instructions' field
                 in the initialize result equals it; if omitted or nil,
                 asserts the field is absent
  :name          Optional; expected `serverInfo.name'.  Defaults to
                 `mcp-server-lib-ert-server-id'.
  :version       Optional; expected `serverInfo.version'.  Defaults to
                 `mcp-server-lib-ert-default-version'.

This macro:
1. Registers a minimal server under `mcp-server-lib-ert-server-id'
   (with the expected :name/:version/:instructions) if one is not already
   registered, tearing it down afterwards; an already-registered server is
   left untouched
2. Starts the MCP server with `mcp-server-lib-start'
3. Sends and validates the initialize request
4. Sends the initialized notification
5. Executes the remaining BODY forms
6. Stops the server with `mcp-server-lib-stop'

Unknown keywords at the head of BODY, or a trailing keyword without a
following value, signal an error at macro-expansion time."
 (declare (indent defun) (debug t))
 (let (tools
       resources
       instructions
       (name 'mcp-server-lib-ert-server-id)
       (version 'mcp-server-lib-ert-default-version))
   (while (and body (keywordp (car body)))
     (unless (cdr body)
       (error "Keyword %S has no value" (car body)))
     (let ((key (car body))
           (value (cadr body)))
       (when (keywordp value)
         (error "Keyword %S used as value for %S" value key))
       (cond
        ((eq key :tools)
         (setq tools value))
        ((eq key :resources)
         (setq resources value))
        ((eq key :instructions)
         (setq instructions value))
        ((eq key :name)
         (setq name value))
        ((eq key :version)
         (setq version value))
        (t
         (error
          "Unknown keyword %S; allowed: :tools, :resources, :instructions, :name, :version"
          key)))
       (setq body (cddr body))))
   (let ((registered-here (gensym "registered-here")))
     `(let
          ((,registered-here
            ;; A server must be registered for `initialize' to report a
            ;; serverInfo name/version.  When the caller has not already
            ;; registered one (e.g. via `mcp-server-lib-register-server'),
            ;; register a minimal one here and tear it down afterwards.
            (unless (mcp-server-lib-server-registered-p
                     mcp-server-lib-ert-server-id)
              (mcp-server-lib-register-server
               :id mcp-server-lib-ert-server-id
               :name ,name
               :version ,version
               :instructions ,instructions)
              t)))
        (unwind-protect
            (progn
              (mcp-server-lib-start)
              (mcp-server-lib-ert-assert-initialize-result
               (mcp-server-lib-ert--get-initialize-result)
               ,tools
               ,resources
               :instructions ,instructions
               :name ,name
               :version ,version)
              (should-not
               (mcp-server-lib-process-jsonrpc
                (json-encode
                 '(("jsonrpc" . "2.0")
                   ("method" . "notifications/initialized")))
                mcp-server-lib-ert-server-id))
              ,@body)
          (mcp-server-lib-stop)
          (when ,registered-here
            (mcp-server-lib-unregister-server
             mcp-server-lib-ert-server-id)))))))

(defun mcp-server-lib-ert-check-error-object
    (response expected-code expected-message)
  "Check that RESPONSE has error with EXPECTED-CODE and EXPECTED-MESSAGE."
  (let ((error-obj
         (mcp-server-lib-ert--validate-jsonrpc-response
          response 'error)))
    (should (equal expected-code (alist-get 'code error-obj)))
    (should (equal expected-message (alist-get 'message error-obj)))))

;; Private resource test helpers

(defconst mcp-server-lib-ert--resource-read-request-id 777
  "Request ID used for resource read operations in tests.")

(defun mcp-server-lib-ert--read-resource (uri)
  "Send a resources/read request for URI and return the parsed response."
  (let ((request
         (mcp-server-lib-create-resources-read-request
          uri mcp-server-lib-ert--resource-read-request-id)))
    (mcp-server-lib-process-jsonrpc-parsed
     request mcp-server-lib-ert-server-id)))

(defun mcp-server-lib-ert-verify-resource-read (uri expected-fields)
  "Verify that reading resource at URI succeeds with EXPECTED-FIELDS.
EXPECTED-FIELDS is an alist of (field . value) pairs to verify in the content."
  (mcp-server-lib-ert-verify-req-success "resources/read"
    (let* ((response (mcp-server-lib-ert--read-resource uri))
           (result
            (mcp-server-lib-ert--validate-jsonrpc-response
             response 'result
             mcp-server-lib-ert--resource-read-request-id))
           (result-keys (mapcar #'car result)))
      ;; Check result structure
      (should (= 1 (length result-keys)))
      (should (member 'contents result-keys))
      ;; Check contents array
      (let ((contents (alist-get 'contents result)))
        (should (arrayp contents))
        (should (= 1 (length contents)))
        ;; Check content item structure
        (let* ((content (aref contents 0))
               (content-keys (mapcar #'car content)))
          ;; Verify exact field count
          (should (= (length expected-fields) (length content-keys)))
          ;; Verify each expected field exists and has correct value
          (dolist (field expected-fields)
            (should (member (car field) content-keys))
            (should
             (equal
              (alist-get (car field) content) (cdr field)))))))))

(defun mcp-server-lib-ert-call-tool (tool-name params)
  "Call TOOL-NAME with PARAMS and return the text content string.
Handles all error checking and response extraction automatically.
Verifies metrics show success (+1 call, +0 errors) at method and tool levels.
Signals test failure if JSON-RPC or MCP errors occur.

Arguments:
  TOOL-NAME - String name of the tool to call
  PARAMS - Alist of parameters to pass to the tool, or nil for no parameters

Returns:
  The text content string from the tool response"
  (let ((tool-metrics-key (format "tools/call:%s" tool-name)))
    (mcp-server-lib-ert-with-metrics-tracking (("tools/call" 1 0)
                                               (tool-metrics-key 1 0))
      (let* ((request
              (mcp-server-lib-create-tools-call-request
               tool-name nil params))
             (response
              (mcp-server-lib-process-jsonrpc-parsed
               request mcp-server-lib-ert-server-id)))
        (should-not (alist-get 'error response))
        (mcp-server-lib-ert-check-text-response response)))))

(defun mcp-server-lib-ert-process-tool-response (response)
  "Process MCP tool response, handling success and error cases.
RESPONSE is the parsed JSON-RPC response from a tool call.
Returns parsed JSON on success, signals `mcp-server-lib-tool-error' on failure.

This function validates the response structure and handles the standard
MCP tool response format with isError flag and content array."
  (let* ((mcp-result
          (mcp-server-lib-ert--validate-jsonrpc-response
           response 'result))
         (is-error (eq (alist-get 'isError mcp-result) t))
         (result-text
          (mcp-server-lib-ert-check-text-response response is-error)))
    (if is-error
        ;; Tool returned an error - signal it
        (signal 'mcp-server-lib-tool-error (list result-text))
      ;; Success case - parse the JSON result
      (json-read-from-string result-text))))

(provide 'mcp-server-lib-ert)

;; Local Variables:
;; package-lint-main-file: "mcp-server-lib.el"
;; End:

;;; mcp-server-lib-ert.el ends here
