;;; mcp-server-lib-ert.el --- ERT test utilities for MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Keywords: tools, testing
;; Version: 0.2.0
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

(require 'ert)
(require 'mcp-server-lib-metrics)
(require 'mcp-server-lib-commands)

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

(defmacro mcp-server-lib-ert-verify-req-success (method &rest body)
  "Execute BODY and verify METHOD metrics show success (+1 call, +0 errors).
Captures metrics before BODY execution and asserts after that:
- calls increased by 1
- errors stayed the same

Arguments:
  METHOD - The MCP method name to track (e.g., \"tools/list\")
  BODY - Forms to execute while tracking metrics

Example:
  (mcp-server-lib-ert-verify-req-success \"tools/list\"
    (let ((response (mcp-server-lib-process-jsonrpc-parsed request)))
      (should-not (alist-get \\='error response))
      (alist-get \\='result response)))"
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
    (let ((resp-obj (mcp-server-lib-process-jsonrpc-parsed request)))
      (should-not (alist-get 'error resp-obj))
      (alist-get 'result resp-obj))))

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

(defun mcp-server-lib-ert-assert-initialize-result
    (init-result tools resources)
  "Assert the structure of an initialize result.
INIT-RESULT is the result from an initialize request.
TOOLS is a boolean indicating if tools capability is expected.
RESOURCES is a boolean indicating if resources capability is expected.

This function validates:
- Protocol version matches the expected version
- Server info contains the correct server name
- Capabilities match the expected state for tools and resources"
  (let ((protocol-version (alist-get 'protocolVersion init-result))
        (capabilities (alist-get 'capabilities init-result))
        (server-info (alist-get 'serverInfo init-result)))
    (should
     (string= mcp-server-lib-protocol-version protocol-version))
    (should
     (string= mcp-server-lib-name (alist-get 'name server-info)))
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
        (length capabilities)))))

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
 mcp-server-lib-ert-with-server
 (&rest body &key tools resources &allow-other-keys)
 "Run BODY with MCP server active and initialized.
Starts the server, sends initialize request, then runs BODY.
TOOLS and RESOURCES are booleans indicating expected capabilities.

This macro:
1. Starts the MCP server with `mcp-server-lib-start'
2. Sends and validates the initialize request
3. Sends the initialized notification
4. Executes BODY
5. Stops the server with `mcp-server-lib-stop'

Arguments:
  TOOLS - If non-nil, expects server to have tools capability
  RESOURCES - If non-nil, expects server to have resources capability
  BODY - Forms to execute with server running

Example:
  ;; Test with no capabilities expected
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (let ((response (mcp-server-lib-process-jsonrpc-parsed request)))
      (should (alist-get \\='result response))))

  ;; Test with tools capability expected
  (mcp-server-lib-ert-with-server :tools t :resources nil
    (let ((tools (mcp-server-lib-ert-get-resource-list)))
      (should (arrayp tools))))"
 (declare (indent defun) (debug t))
 `(unwind-protect
      (progn
        (mcp-server-lib-start)
        (mcp-server-lib-ert-assert-initialize-result
         (mcp-server-lib-ert--get-initialize-result)
         ,tools
         ,resources)
        ;; Send initialized notification - should return nil
        (should-not
         (mcp-server-lib-process-jsonrpc
          (json-encode
           '(("jsonrpc" . "2.0")
             ("method" . "notifications/initialized")))))
        ,@body)
    (mcp-server-lib-stop)))

(defun mcp-server-lib-ert-check-error-object
    (response expected-code expected-message)
  "Check that RESPONSE has error with EXPECTED-CODE and EXPECTED-MESSAGE."
  ;; Check that response contains only standard JSON-RPC fields plus error
  (should (equal 3 (length response))) ; jsonrpc, id, error
  (should (equal "2.0" (alist-get 'jsonrpc response)))
  (should (assq 'id response))
  (let ((error-obj (alist-get 'error response)))
    (should error-obj)
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
    (mcp-server-lib-process-jsonrpc-parsed request)))

(defun mcp-server-lib-ert-verify-resource-read (uri expected-fields)
  "Verify that reading resource at URI succeeds with EXPECTED-FIELDS.
EXPECTED-FIELDS is an alist of (field . value) pairs to verify in the content."
  (mcp-server-lib-ert-verify-req-success "resources/read"
    (let* ((response (mcp-server-lib-ert--read-resource uri))
           (response-keys (mapcar #'car response)))
      ;; Check response has exactly the expected fields
      (should (= 3 (length response-keys)))
      (should (member 'jsonrpc response-keys))
      (should (member 'id response-keys))
      (should (member 'result response-keys))
      ;; Check response field values
      (should (string= "2.0" (alist-get 'jsonrpc response)))
      (should
       (equal
        mcp-server-lib-ert--resource-read-request-id
        (alist-get 'id response)))
      ;; Check result structure
      (let* ((result (alist-get 'result response))
             (result-keys (mapcar #'car result)))
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
            (should
             (= (length expected-fields) (length content-keys)))
            ;; Verify each expected field exists and has correct value
            (dolist (field expected-fields)
              (should (member (car field) content-keys))
              (should
               (equal
                (alist-get (car field) content) (cdr field))))))))))

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
    (mcp-server-lib-ert-with-metrics-tracking
        (("tools/call" 1 0)
         (tool-metrics-key 1 0))
      (let* ((request (mcp-server-lib-create-tools-call-request 
                       tool-name nil params))
             (response (mcp-server-lib-process-jsonrpc-parsed request)))
        (should-not (alist-get 'error response))
        (mcp-server-lib-ert-check-text-response response)))))

(provide 'mcp-server-lib-ert)

;; Local Variables:
;; package-lint-main-file: "mcp-server-lib.el"
;; End:

;;; mcp-server-lib-ert.el ends here
