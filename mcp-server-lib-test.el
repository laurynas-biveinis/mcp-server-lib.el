;;; mcp-server-lib-test.el --- Tests for mcp-server-lib.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

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

;; ERT tests for mcp-server-lib.el.

;;; Code:

(require 'ert)
(require 'mcp-server-lib)
(require 'mcp-server-lib-commands)
(require 'mcp-server-lib-metrics)
(require 'mcp-server-lib-ert)
(require 'json)

;;; Test data

(defconst mcp-server-lib-test--string-list-result "item1 item2 item3"
  "Test data for string list tool.")

(defconst mcp-server-lib-test--nonexistent-tool-id "non-existent-tool"
  "Tool ID for a non-existent tool used in tests.")

(defconst mcp-server-lib-test--unregister-tool-id "test-unregister"
  "Tool ID used for testing tool unregistration.")

;;; Generic test handlers

(defun mcp-server-lib-test--return-string ()
  "Generic handler to return a test string."
  "test result")

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

(defun mcp-server-lib-test--tool-handler-two-params (first-name last-name)
  "Test handler with two parameters.
FIRST-NAME and LAST-NAME are the person's names.

MCP Parameters:
  first-name - Person's first name
  last-name - Person's last name"
  (format "Hello, %s %s!" first-name last-name))

(defun mcp-server-lib-test--tool-handler-three-params (title first-name last-name)
  "Test handler with three parameters.
TITLE, FIRST-NAME and LAST-NAME are the person's title and names.

MCP Parameters:
  title - Person's title (e.g. Mr, Ms, Dr)
  first-name - Person's first name
  last-name - Person's last name"
  (format "Hello, %s %s %s!" title first-name last-name))

;; Bytecode handler function that will be loaded during tests
(declare-function mcp-server-lib-test-bytecode-handler--handler
                  "mcp-server-lib-bytecode-handler-test")

;;; Test resource template handlers

(defun mcp-server-lib-test--template-handler-error (_params)
  "Template handler to ignore PARAMS and throw an error."
  (error "Generic error occurred"))

(defun mcp-server-lib-test--resource-template-handler-dump-params (params)
  "Generic template handler that dumps the PARAMS alist."
  (format "params: %S" params))

(defun mcp-server-lib-test--resource-template-handler-dump-params-2 (params)
  "Alternative template handler that dumps PARAMS."
  (format "Handler-2: params: %S" params))

(defun mcp-server-lib-test--resource-template-handler-nil (params)
  "Test template handler to ignore PARAMS and return nil."
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

(defmacro mcp-server-lib-test--with-undefined-function (function-symbol &rest body)
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
  `(mcp-server-lib-ert-with-server :tools nil :resources nil
     (mcp-server-lib-ert-verify-req-success ,method
       ,@body)))

(defmacro mcp-server-lib-test--register-tool (handler &rest props-and-body)
  "Register a tool with HANDLER and properties, execute body, then unregister.
This is a simpler alternative to `mcp-server-lib-test--with-tools' for cases
where you need just one tool registered without starting the server.

The macro separates PROPS-AND-BODY into the tool properties (keywords) and
the body forms (everything after the last keyword-value pair).

IMPORTANT: Tests should use this macro instead of calling
`mcp-server-lib-register-tool' directly, except when testing registration
failures or error conditions.

Arguments:
  HANDLER         Function to handle tool invocations
  PROPS-AND-BODY  Tool properties followed by body forms"
  (declare (indent 1) (debug t))
  ;; Separate properties from body
  (let ((props '())
        (body props-and-body))
    ;; Extract properties (keyword-value pairs)
    (while (and body (keywordp (car body)))
      (push (pop body) props)
      (push (pop body) props))
    (setq props (nreverse props))
    ;; Extract tool ID for unregistration
    (let ((tool-id (plist-get props :id)))
      `(unwind-protect
           (progn
             (mcp-server-lib-register-tool ,handler ,@props)
             ,@body)
         (mcp-server-lib-unregister-tool ,tool-id)))))

(defmacro mcp-server-lib-test--with-tools (tools &rest body)
  "Run BODY with MCP server active and TOOLS registered.
All tools are automatically unregistered after BODY execution.

IMPORTANT: Tests should use this macro instead of calling
`mcp-server-lib-register-tool' directly, except when testing registration
failures or error conditions.

Arguments:
  TOOLS  List of tool registration specs, each a list of arguments for
         `mcp-server-lib-register-tool': (HANDLER &rest PROPERTIES)
  BODY   Forms to execute with server running and tools registered"
  (declare (indent 1) (debug t))
  ;; Build nested mcp-server-lib-test--register-tool calls
  ;; wrapping server start and body execution
  (let ((server-and-body
         `(mcp-server-lib-ert-with-server :tools t :resources nil
            ,@body)))
    ;; Process tools in reverse order to build proper nesting
    (dolist (tool-spec (reverse tools))
      (let* ((handler (car tool-spec))
             (props (cdr tool-spec)))
        (setq server-and-body
              `(mcp-server-lib-test--register-tool ,handler ,@props
                 ,server-and-body))))
    server-and-body))

(defmacro mcp-server-lib-test--register-resource (uri handler &rest props-and-body)
  "Register a resource, execute body, then unregister.
Register a resource at URI with HANDLER and properties.
This is a simpler alternative to `mcp-server-lib-test--with-resources' for cases
where you need just one resource registered without starting the server.

The macro separates PROPS-AND-BODY into the resource properties (keywords) and
the body forms (everything after the last keyword-value pair).

IMPORTANT: Tests should use this macro instead of calling
`mcp-server-lib-register-resource' directly, except when testing registration
failures or error conditions.

Arguments:
  URI             Exact URI for the resource
  HANDLER         Function that returns the resource content
  PROPS-AND-BODY  Resource properties followed by body forms"
  (declare (indent 2) (debug t))
  ;; Separate properties from body
  (let ((props '())
        (body props-and-body))
    ;; Extract properties (keyword-value pairs)
    (while (and body (keywordp (car body)))
      (push (pop body) props)
      (push (pop body) props))
    (setq props (nreverse props))
    `(unwind-protect
         (progn
           (mcp-server-lib-register-resource ,uri ,handler ,@props)
           ,@body)
       (mcp-server-lib-unregister-resource ,uri))))

(defun mcp-server-lib-test--find-resource-by-uri (uri resources)
  "Find a resource in RESOURCES array by its URI field."
  (seq-find (lambda (r) (equal (alist-get 'uri r) uri)) resources))

(defun mcp-server-lib-test--find-resource-by-uri-template (uri-template
  resources)
  "Find a resource in RESOURCES array with URI-TEMPLATE field.
The URI-TEMPLATE is searched as uriTemplate in JSON."
  (seq-find (lambda (r) (equal (alist-get 'uriTemplate r) uri-template))
  resources))

(defun mcp-server-lib-test--is-template-resource (resource-spec)
  "Return non-nil if RESOURCE-SPEC represents a template resource.
RESOURCE-SPEC is a list where the first element is the URI or template."
  (string-match-p "{" (car resource-spec)))

(defun mcp-server-lib-test--build-resource-verification (resource-spec)
  "Build verification code for a single RESOURCE-SPEC.
RESOURCE-SPEC is a list of (URI HANDLER &rest PROPERTIES).
Returns a form that verifies the resource appears in resource-list with
expected properties."
  (let* ((uri (car resource-spec))
         (props (cddr resource-spec))
         (name (plist-get props :name))
         (description (plist-get props :description))
         (mime-type (plist-get props :mime-type))
         (is-template (string-match-p "{" uri)))
    `(let ((resource ,(if is-template
                          `(mcp-server-lib-test--find-resource-by-uri-template
                            ,uri resource-list)
                        `(mcp-server-lib-test--find-resource-by-uri
                          ,uri resource-list))))
       (should resource)
       (should (equal (alist-get ',(if is-template 'uriTemplate 'uri) resource)
                      ,uri))
       (should (equal (alist-get 'name resource) ,name))
       ,@(when description
           `((should (equal (alist-get 'description resource) ,description))))
       ,@(when mime-type
           `((should (equal (alist-get 'mimeType resource) ,mime-type)))))))

(defmacro mcp-server-lib-test--with-resources (resources &rest body)
  "Run BODY with MCP server active and RESOURCES registered.
All resources are automatically unregistered after BODY execution.

After registering all resources, automatically verifies that the resource list
contains exactly the registered resources with their expected properties.

IMPORTANT: Tests should use this macro instead of calling
`mcp-server-lib-register-resource' directly, except when testing registration
failures or error conditions.

Arguments:
  RESOURCES  List of resource registration specs, each a list of arguments for
             `mcp-server-lib-register-resource': (URI HANDLER &rest PROPERTIES)
  BODY       Forms to execute with server running and resources registered"
  (declare (indent 1) (debug t))
  ;; Build the verification code
  ;; Separate direct resources and templates
  (let* ((direct-resources (cl-remove-if #'mcp-server-lib-test--is-template-resource
                                          resources))
         (template-resources (cl-set-difference resources direct-resources :test #'equal))
         (verification-code
          `(progn
             ;; Verify direct resources in resources/list
             (let ((resource-list (mcp-server-lib-ert-get-resource-list)))
               ;; Check we have the expected number of DIRECT resources only
               (should (= ,(length direct-resources) (length resource-list)))
               ;; Verify only direct resources appear in the list
               ,@(mapcar #'mcp-server-lib-test--build-resource-verification
                         direct-resources))
             ;; Verify templates in resources/templates/list
             ,@(when template-resources
                 `((let ((resource-list (mcp-server-lib-ert-get-resource-templates-list)))
                     ;; Check we have the expected number of templates
                     (should (= ,(length template-resources) (length resource-list)))
                     ;; Verify templates appear in the template list
                     ,@(mapcar #'mcp-server-lib-test--build-resource-verification
                               template-resources)))))))
    ;; Build nested mcp-server-lib-test--register-resource calls
    ;; wrapping server start, verification, and body execution
    (let ((server-and-body
           `(mcp-server-lib-ert-with-server :tools nil :resources t
              ;; Add verification after all resources are registered
              ,verification-code
              ,@body)))
      ;; Process resources in reverse order to build proper nesting
      (dolist (resource-spec (reverse resources))
        (let* ((uri (car resource-spec))
               (handler (cadr resource-spec))
               (props (cddr resource-spec)))
          (setq server-and-body
                `(mcp-server-lib-test--register-resource ,uri ,handler ,@props
                   ,server-and-body))))
      server-and-body)))

(defun mcp-server-lib-test--check-jsonrpc-error
    (request expected-code expected-message)
  "Test that JSON-RPC REQUEST is rejected with EXPECTED-CODE and EXPECTED-MESSAGE."
  (let ((resp-obj (mcp-server-lib-process-jsonrpc-parsed request)))
    (mcp-server-lib-ert-check-error-object resp-obj expected-code expected-message)))

(defun mcp-server-lib-test--check-invalid-jsonrpc-version (version)
  "Test that JSON-RPC request with VERSION is rejected properly."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--check-jsonrpc-error
     (json-encode
      `(("jsonrpc" . ,version) ("method" . "tools/list") ("id" . 42)))
     mcp-server-lib-jsonrpc-error-invalid-request "Invalid Request: Not JSON-RPC 2.0")))

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
       (("tools/call" 1 1)
        ((format "tools/call:%s" ,tool-id) 1 1))
     (let ((request (mcp-server-lib-create-tools-call-request ,tool-id 999)))
       ,@body)))

(defun mcp-server-lib-test--get-tool-list ()
  "Get the successful response to a standard `tools/list` request."
  (let ((result
         (alist-get
          'tools
          (mcp-server-lib-ert-get-success-result
           "tools/list"
           (mcp-server-lib-create-tools-list-request)))))
    (should (arrayp result))
    result))

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
        (should (equal (alist-get (car field) resource) (cdr field)))))))

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

(defun mcp-server-lib-test--check-resource-read-request-error (params expected-code expected-message)
  "Test that a resources/read request with PARAMS returns expected error.
PARAMS is the params value to send in the JSON-RPC request.
EXPECTED-CODE is the expected error code.
EXPECTED-MESSAGE is the expected error message."
  (mcp-server-lib-test--check-jsonrpc-error
   (json-encode
    `((jsonrpc . "2.0")
      (id . 1)
      (method . "resources/read")
      (params . ,params)))
   expected-code
   expected-message))

(defun mcp-server-lib-test--read-resource-error (uri expected-code expected-message)
  "Read resource at URI expecting an EXPECTED-CODE with EXPECTED-MESSAGE.
EXPECTED-MESSAGE should be the exact error message string."
  (let ((response (mcp-server-lib-ert--read-resource uri)))
    ;; Check specific request ID for this resource read
    (should (equal mcp-server-lib-ert--resource-read-request-id (alist-get 'id response)))
    (mcp-server-lib-ert-check-error-object response expected-code expected-message)))

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
    (&optional param-name param-type param-description)
  "Verify that schema of the only tool in the tool list has correct structure.
When PARAM-NAME is nil, verifies a zero-argument tool schema.
Otherwise, verifies a one-parameter tool schema with:
PARAM-NAME as the name of the parameter to validate.
PARAM-TYPE as the expected type of the parameter.
PARAM-DESCRIPTION as the expected description of the parameter."
  (let* ((tools (mcp-server-lib-test--get-tool-list))
         (tool (aref tools 0))
         (schema (alist-get 'inputSchema tool)))
    (should (equal "object" (alist-get 'type schema)))

    (if param-name
        (progn
          ;; One parameter case - verify required and properties
          (should
           (equal (vector param-name) (alist-get 'required schema)))
          (let ((param-schema
                 (alist-get
                  (intern param-name)
                  (alist-get 'properties schema))))
            (should (equal param-type (alist-get 'type param-schema)))
            (should
             (equal
              param-description
              (alist-get 'description param-schema)))))

      ;; Zero parameter case - schema should be just {type: "object"}
      (should-not (alist-get 'required schema))
      (should-not (alist-get 'properties schema)))))

(defun mcp-server-lib-test--check-mcp-server-lib-content-format
    (result expected-text)
  "Check that RESULT follows the MCP content format with EXPECTED-TEXT."
  (let* ((response `((result . ,result)))
         (text (mcp-server-lib-ert-check-text-response response)))
    (should (string= expected-text text))))

;;; Initialization and server capabilities tests

(ert-deftest mcp-server-lib-test-initialize-no-tools-no-resources ()
  "Test initialize when no tools or resources are registered.
When no tools or resources are registered, the capabilities object
should not include tools or resources fields at all."
  (mcp-server-lib-ert-with-server :tools nil :resources nil))

(ert-deftest mcp-server-lib-test-initialize-with-tools-and-resources ()
  "Test initialize when both tools and resources are registered.
When both are registered, capabilities should include both fields."
  (mcp-server-lib-test--register-tool
   #'mcp-server-lib-test--return-string
   :id "test-tool"
   :description "Test tool"

   (mcp-server-lib-test--register-resource
    "test://resource"
    #'mcp-server-lib-test--return-string
    :name "Test Resource"
    (mcp-server-lib-ert-with-server :tools t :resources t))))


(ert-deftest mcp-server-lib-test-initialize-old-protocol-version ()
  "Test server responds with its version for older client version."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 16)
               ("params" .
                (("protocolVersion" . "2024-11-05")
                 ("capabilities" . ,(make-hash-table)))))))
           (result (mcp-server-lib-ert-get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-ert-assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-missing-protocol-version
    ()
  "Test initialize request without protocolVersion field."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 17)
               ("params" .
                (("capabilities" . ,(make-hash-table)))))))
           (result (mcp-server-lib-ert-get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-ert-assert-initialize-result result nil nil))))

(ert-deftest
    mcp-server-lib-test-initialize-non-string-protocol-version
    ()
  "Test initialize request with non-string protocolVersion."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 18)
               ("params" .
                (("protocolVersion" . 123) ; Number instead of string
                 ("capabilities" . ,(make-hash-table)))))))
           (result (mcp-server-lib-ert-get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-ert-assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-malformed-params ()
  "Test initialize request with completely malformed params."
  ;; Test with params as a string instead of object
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize")
               ("id" . 19)
               ("params" . "malformed"))))
           (result (mcp-server-lib-ert-get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-ert-assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-missing-params ()
  "Test initialize request without params field."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize")
               ("id" . 20))))
           (result (mcp-server-lib-ert-get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-ert-assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-null-protocol-version ()
  "Test initialize request with null protocolVersion."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 21)
               ("params" .
                (("protocolVersion" . :json-null)
                 ("capabilities" . ,(make-hash-table)))))))
           (result (mcp-server-lib-ert-get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-ert-assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-empty-protocol-version ()
  "Test initialize request with empty string protocolVersion."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 22)
               ("params" .
                (("protocolVersion" . "")
                 ("capabilities" . ,(make-hash-table)))))))
           (result (mcp-server-lib-ert-get-success-result
                    "initialize" init-request)))
      (mcp-server-lib-ert-assert-initialize-result result nil nil))))

(ert-deftest mcp-server-lib-test-initialize-with-valid-client-capabilities ()
  "Test initialize request with valid client capabilities (roots, sampling, experimental)."
  (mcp-server-lib-test--with-request "initialize"
    (let* ((init-request
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "initialize") ("id" . 23)
               ("params" .
                (("protocolVersion" . ,mcp-server-lib-protocol-version)
                 ("capabilities" .
                  (("roots" . ,(make-hash-table))
                   ("sampling" . ,(make-hash-table))
                   ("experimental" . ,(make-hash-table)))))))))
           (result (mcp-server-lib-ert-get-success-result
                    "initialize" init-request)))
      ;; Server should respond successfully, ignoring client capabilities
      (mcp-server-lib-ert-assert-initialize-result result nil nil))))

;;; `mcp-server-lib-register-tool' tests

(ert-deftest mcp-server-lib-test-register-tool-error-missing-id ()
  "Test that tool registration with missing :id produces an error."
  (should-error
   (mcp-server-lib-register-tool
    #'mcp-server-lib-test--return-string
    :description "Test tool without ID")
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-tool-error-missing-description
    ()
  "Test that tool registration with missing :description produces an error."
  (should-error
   (mcp-server-lib-register-tool
    #'mcp-server-lib-test--return-string
    :id "test-tool-no-desc")
   :type 'error))

(ert-deftest mcp-server-lib-test-register-tool-error-missing-handler
    ()
  "Test that tool registration with non-function handler produces an error."
  (should-error
   (mcp-server-lib-register-tool
    "not-a-function"
    :id "test-tool-bad-handler"
    :description "Test tool with invalid handler")
   :type 'error))

(ert-deftest
    mcp-server-lib-test-register-tool-error-duplicate-param-description
    ()
  "Test that duplicate parameter descriptions cause an error."
  (should-error
   (mcp-server-lib-register-tool
    #'mcp-server-lib-test--tool-handler-duplicate-param
    :id "duplicate-param-tool"
    :description "Tool with duplicate parameter")
   :type 'error))

(ert-deftest mcp-server-lib-test-register-tool-error-mismatched-param
    ()
  "Test that parameter names must match function arguments."
  (should-error
   (mcp-server-lib-register-tool
    #'mcp-server-lib-test--tool-handler-mismatched-param
    :id "mismatched-param-tool"
    :description "Tool with mismatched parameter")
   :type 'error))

(ert-deftest mcp-server-lib-test-register-tool-error-missing-param ()
  "Test that all function parameters must be documented."
  (should-error
   (mcp-server-lib-register-tool
    #'mcp-server-lib-test--tool-handler-missing-param
    :id "missing-param-tool"
    :description "Tool with missing parameter docs")
   :type 'error))

(ert-deftest mcp-server-lib-test-register-tool-error-duplicate-id ()
  "Test reference counting behavior when registering a tool with duplicate ID.
With reference counting, duplicate registrations should succeed and increment
the reference count, returning the original tool definition."
  (mcp-server-lib-ert-with-server
   :tools nil :resources nil
   (mcp-server-lib-test--register-tool
    #'mcp-server-lib-test--return-string
    :id "duplicate-test"
    :description "First registration"

    (mcp-server-lib-test--register-tool
     #'mcp-server-lib-test--return-string
     :id "duplicate-test"
     :description "Second registration - should be ignored"
     ;; Tool should be callable after registrations (ref count = 2)
     (let ((result (mcp-server-lib-test--call-tool "duplicate-test" 1)))
       (mcp-server-lib-test--check-mcp-server-lib-content-format
        result "test result")))
    
    ;; After inner macro completes, it unregisters once (ref count goes from 2 to 1)
    ;; Tool should still be callable because outer registration is still active
    (let ((result (mcp-server-lib-test--call-tool "duplicate-test" 2)))
      (mcp-server-lib-test--check-mcp-server-lib-content-format
       result "test result")))
   
   ;; After outer macro completes, it unregisters again (ref count = 0)
   ;; Tool should no longer be callable
   (mcp-server-lib-test--verify-tool-not-found "duplicate-test")))

(ert-deftest mcp-server-lib-test-register-tool-bytecode ()
  "Test schema generation for a handler loaded as bytecode.
This test verifies that MCP can correctly extract parameter information
from a function loaded from bytecode rather than interpreted elisp."
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
       "input-string"
       "string"
       "Input string parameter for bytecode testing"))

    (when (file-exists-p bytecode-file)
      (delete-file bytecode-file))))

;;; `mcp-server-lib-unregister-tool' tests

(ert-deftest mcp-server-lib-test-unregister-tool ()
  "Test that `mcp-server-lib-unregister-tool' removes a tool correctly."
  (mcp-server-lib-ert-with-server
   :tools nil :resources nil
   (mcp-server-lib-test--register-tool
    #'mcp-server-lib-test--return-string
    :id mcp-server-lib-test--unregister-tool-id
    :description "Tool for unregister test"

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
       result "test result")))

   ;; After macro cleanup, verify tool is gone
   (mcp-server-lib-test--verify-tool-list-request '())
   (mcp-server-lib-test--verify-tool-not-found
    mcp-server-lib-test--unregister-tool-id)))

(ert-deftest mcp-server-lib-test-unregister-tool-nonexistent ()
  "Test that `mcp-server-lib-unregister-tool' returns nil for missing tools."
  (mcp-server-lib-test--register-tool
   #'mcp-server-lib-test--return-string
   :id "test-other"
   :description "Other test tool"
   (should-not (mcp-server-lib-unregister-tool "nonexistent-tool"))))

(ert-deftest mcp-server-lib-test-unregister-tool-when-no-tools ()
  "Test `mcp-server-lib-unregister-tool' when no tools are registered."
  (should-not (mcp-server-lib-unregister-tool "any-tool")))

;;; Notification tests

(ert-deftest mcp-server-lib-test-notifications-cancelled ()
  "Test the MCP `notifications/cancelled` request handling."
  (mcp-server-lib-test--with-request "notifications/cancelled"
    (let* ((notifications-cancelled
            (json-encode
             `(("jsonrpc" . "2.0")
               ("method" . "notifications/cancelled"))))
           (response
            (mcp-server-lib-process-jsonrpc notifications-cancelled)))
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

(ert-deftest mcp-server-lib-test-create-resources-list-request-with-id ()
  "Test `mcp-server-lib-create-resources-list-request' with a specified ID."
  (let* ((id 42)
         (request (mcp-server-lib-create-resources-list-request id))
         (parsed (json-read-from-string request)))
    ;; Verify basic JSON-RPC structure
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "resources/list" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))))

(ert-deftest mcp-server-lib-test-create-resources-list-request-default-id ()
  "Test `mcp-server-lib-create-resources-list-request' with default ID."
  (let* ((request (mcp-server-lib-create-resources-list-request))
         (parsed (json-read-from-string request)))
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "resources/list" (alist-get 'method parsed)))
    (should (equal 1 (alist-get 'id parsed)))))

;;; `mcp-server-lib-create-resources-read-request' tests

(ert-deftest mcp-server-lib-test-create-resources-read-request-with-id ()
  "Test `mcp-server-lib-create-resources-read-request' with a specified ID."
  (let* ((id 42)
         (uri "test://resource")
         (request (mcp-server-lib-create-resources-read-request uri id))
         (parsed (json-read-from-string request)))
    ;; Verify basic JSON-RPC structure
    (should (equal "2.0" (alist-get 'jsonrpc parsed)))
    (should (equal "resources/read" (alist-get 'method parsed)))
    (should (equal id (alist-get 'id parsed)))
    ;; Verify params
    (let ((params (alist-get 'params parsed)))
      (should (equal uri (alist-get 'uri params))))))

(ert-deftest mcp-server-lib-test-create-resources-read-request-default-id ()
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
    (mcp-server-lib-ert-verify-req-success "tools/list"
      (mcp-server-lib-test--verify-tool-list-request
       '(("test-tool-1" .
          ((description . "First tool for testing")
           (inputSchema . ((type . "object")))))
         ("test-tool-2" .
          ((description . "Second tool for testing")
           (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-server-lib-test-tools-list-zero ()
  "Test the `tools/list` method returning empty array with no tools."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--verify-tool-list-request '())))

(ert-deftest mcp-server-lib-test-tools-list-schema-one-arg-handler ()
  "Test that `tools/list` schema includes parameter descriptions."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-string-arg
        :id "requires-arg"
        :description "A tool that requires an argument"))
    (mcp-server-lib-ert-verify-req-success "tools/list"
      (mcp-server-lib-test--verify-tool-schema-in-single-tool-list
       "input-string" "string" "test parameter for string input"))))

(ert-deftest mcp-server-lib-test-tools-list-schema-two-param-handler ()
  "Test that `tools/list` schema includes multiple parameter descriptions."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-two-params
        :id "two-params"
        :description "A tool that requires two arguments"))
    (mcp-server-lib-ert-verify-req-success "tools/list"
      (let* ((result (mcp-server-lib-ert-get-success-result
                      "tools/list"
                      (mcp-server-lib-create-tools-list-request)))
             (tool-list (alist-get 'tools result))
             (tool (elt tool-list 0))
             (schema (alist-get 'inputSchema tool))
             (properties (alist-get 'properties schema))
             (required (alist-get 'required schema))
             (first-name-prop (alist-get 'first-name properties))
             (last-name-prop (alist-get 'last-name properties)))
        ;; Verify the schema structure
        (should (equal "object" (alist-get 'type schema)))
        ;; Check properties exist with original parameter names
        (should first-name-prop)
        (should last-name-prop)
        ;; Check descriptions exist (don't check exact quotes due to text-quoting-style)
        (should (stringp (alist-get 'description first-name-prop)))
        (should (stringp (alist-get 'description last-name-prop)))
        (should (string-match-p "first name" (alist-get 'description first-name-prop)))
        (should (string-match-p "last name" (alist-get 'description last-name-prop)))
        ;; Check types
        (should (equal "string" (alist-get 'type first-name-prop)))
        (should (equal "string" (alist-get 'type last-name-prop)))
        ;; Check required fields (required is a vector)
        (should (seq-contains-p required "first-name"))
        (should (seq-contains-p required "last-name"))))))

(ert-deftest mcp-server-lib-test-tools-call-two-param-handler ()
  "Test invoking a tool with two parameters."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-two-params
        :id "two-params"
        :description "A tool that requires two arguments"))
    (let* ((args '((first-name . "John") (last-name . "Doe")))
           (result (mcp-server-lib-test--call-tool "two-params" 42 args)))
      (mcp-server-lib-test--check-mcp-server-lib-content-format
       result "Hello, John Doe!"))))

(ert-deftest mcp-server-lib-test-tools-call-three-param-handler ()
  "Test invoking a tool with three parameters."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-three-params
        :id "three-params"
        :description "A tool that requires three arguments"))
    (let* ((args '((title . "Dr") (first-name . "Jane") (last-name . "Smith")))
           (result (mcp-server-lib-test--call-tool "three-params" 99 args)))
      (mcp-server-lib-test--check-mcp-server-lib-content-format
       result "Hello, Dr Jane Smith!"))))

(ert-deftest mcp-server-lib-test-tools-call-missing-required-param ()
  "Test that calling a multi-param tool with missing parameters returns an error."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-two-params
        :id "two-params"
        :description "A tool that requires two arguments"))
    ;; Call with only one parameter when two are required
    (let* ((args '((first-name . "John")))  ; Missing last-name
           (request (mcp-server-lib-create-tools-call-request "two-params" 42 args))
           (response (mcp-server-lib-process-jsonrpc-parsed request))
           (error-obj (alist-get 'error response)))
      (should error-obj)
      (should (equal mcp-server-lib-jsonrpc-error-invalid-params
                     (alist-get 'code error-obj)))
      (should (string-match-p "Missing required parameter"
                               (alist-get 'message error-obj))))))

(ert-deftest mcp-server-lib-test-tools-call-too-many-params ()
  "Test that calling a tool with extra parameters returns an error."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-two-params
        :id "two-params"
        :description "A tool that requires two arguments"))
    ;; Call with three parameters when only two are expected
    (let* ((args '((first-name . "John") 
                   (last-name . "Doe")
                   (middle-name . "Extra")))  ; Extra parameter
           (request (mcp-server-lib-create-tools-call-request "two-params" 42 args))
           (response (mcp-server-lib-process-jsonrpc-parsed request))
           (error-obj (alist-get 'error response)))
      (should error-obj)
      (should (equal mcp-server-lib-jsonrpc-error-invalid-params
                     (alist-get 'code error-obj)))
      (should (string-match-p "Unexpected parameter"
                               (alist-get 'message error-obj))))))


(ert-deftest mcp-server-lib-test-tools-list-read-only-hint ()
  "Test that `tools/list` response includes readOnlyHint=true."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "read-only-tool"
        :description "A tool that doesn't modify its environment"
        :read-only t))
    (mcp-server-lib-ert-verify-req-success "tools/list"
      (mcp-server-lib-test--verify-tool-list-request
       '(("read-only-tool" .
          ((description
            . "A tool that doesn't modify its environment")
           (annotations . ((readOnlyHint . t)))
           (inputSchema . ((type . "object"))))))))))

(ert-deftest mcp-server-lib-test-tools-list-read-only-hint-false ()
  "Test that `tools/list` response includes readOnlyHint=false."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--return-string
        :id "non-read-only-tool"
        :description "Tool that modifies its environment"
        :read-only nil))
    (mcp-server-lib-ert-verify-req-success "tools/list"
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
    (mcp-server-lib-ert-verify-req-success "tools/list"
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
              (mcp-server-lib-process-jsonrpc-parsed request))
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
       mcp-server-lib-jsonrpc-error-internal "Internal error executing tool: Generic error occurred"))))

(ert-deftest mcp-server-lib-test-tools-call-no-args ()
  "Test the `tools/call` request with a tool that takes no arguments."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-string-list
        :id "string-list-tool"
        :description "A tool that returns a string with items"))
    (let ((result
           (mcp-server-lib-test--call-tool "string-list-tool" 9)))
      (mcp-server-lib-test--check-mcp-server-lib-content-format
       result mcp-server-lib-test--string-list-result))))

(ert-deftest mcp-server-lib-test-tools-call-empty-string ()
  "Test the `tools/call` request with a tool that returns an empty string."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-empty-string
        :id "empty-string-tool"
        :description "A tool that returns an empty string"))
    (mcp-server-lib-test--verify-tool-schema-in-single-tool-list)

    (let ((result
           (mcp-server-lib-test--call-tool "empty-string-tool" 10)))
      (mcp-server-lib-test--check-mcp-server-lib-content-format
       result ""))))

(ert-deftest mcp-server-lib-test-tools-call-with-string-arg ()
  "Test the `tools/call` request with a tool that takes a string argument."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--tool-handler-string-arg
        :id "string-arg-tool"
        :description "A tool that echoes a string argument"))
    (let* ((test-input "Hello, world!")
           (args `(("input-string" . ,test-input))))

      (let ((result
             (mcp-server-lib-test--call-tool "string-arg-tool"
                                             13
                                             args)))
        (mcp-server-lib-test--check-mcp-server-lib-content-format
         result (concat "Echo: " test-input))))))

(ert-deftest mcp-server-lib-test-tools-call-unregistered-tool ()
  "Test the `tools/call` request with a tool that was never registered."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
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
           (text
            (mcp-server-lib-ert-check-text-response response)))
      (should (string= "" text)))))

(ert-deftest mcp-server-lib-test-tools-call-handler-undefined ()
  "Test calling a tool whose handler function no longer exists."
  (mcp-server-lib-test--with-tools
      ((#'mcp-server-lib-test--handler-to-be-undefined
        :id "undefined-handler-tool"
        :description "A tool whose handler will be undefined"))
    (mcp-server-lib-test--with-undefined-function 'mcp-server-lib-test--handler-to-be-undefined
      (mcp-server-lib-test--check-tool-call-error "undefined-handler-tool"
        ;; Try to call the tool - should return an error
        (mcp-server-lib-test--check-jsonrpc-error
         request
         mcp-server-lib-jsonrpc-error-internal "Internal error executing tool: Symbol’s function definition is void: mcp-server-lib-test--handler-to-be-undefined")))))


;;; `mcp-server-lib-process-jsonrpc' tests

(ert-deftest mcp-server-lib-test-parse-error ()
  "Test that invalid JSON input returns a parse error."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--check-jsonrpc-error
     "This is not valid JSON" mcp-server-lib-jsonrpc-error-parse "Parse error: JSON readtable error: 84")))

(ert-deftest mcp-server-lib-test-method-not-found ()
  "Test that unknown methods return method-not-found error."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--check-jsonrpc-error
     (json-encode
      '(("jsonrpc" . "2.0")
        ("method" . "unknown/method")
        ("id" . 99)))
     mcp-server-lib-jsonrpc-error-method-not-found "Method not found: unknown/method")))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc ()
  "Test that valid JSON that is not JSON-RPC returns an invalid request error."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
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
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--check-jsonrpc-error
     (json-encode '(("jsonrpc" . "2.0") ("method" . "tools/list")))
     mcp-server-lib-jsonrpc-error-invalid-request
     "Invalid Request: Missing required 'id' field")))

(ert-deftest mcp-server-lib-test-invalid-jsonrpc-missing-method ()
  "Test that JSON-RPC request lacking the `method` key is rejected properly."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--check-jsonrpc-error
     (json-encode '(("jsonrpc" . "2.0") ("id" . 42)))
     mcp-server-lib-jsonrpc-error-invalid-request
     "Invalid Request: Missing required 'method' field")))

;;; `mcp-server-lib-process-jsonrpc-parsed' tests

(ert-deftest mcp-server-lib-test-process-jsonrpc-parsed ()
  "Test that `mcp-server-lib-process-jsonrpc-parsed' returns parsed response."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (let* ((request (mcp-server-lib-create-tools-list-request))
           (response (mcp-server-lib-process-jsonrpc-parsed request)))
      ;; Response should be a parsed alist, not a string
      (should (listp response))
      (should (alist-get 'result response))
      (should
       (arrayp (alist-get 'tools (alist-get 'result response)))))))

;;; Logging tests

(ert-deftest mcp-server-lib-test-log-io-t ()
  "Test that when `mcp-server-lib-log-io' is t, JSON-RPC messages are logged."
  (setq mcp-server-lib-log-io t)

  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (let* ((request (mcp-server-lib-create-tools-list-request))
           (response (mcp-server-lib-process-jsonrpc request)))

      (let ((log-buffer (get-buffer "*mcp-server-lib-log*")))
        (should log-buffer)

        (with-current-buffer log-buffer
          (let ((content (buffer-string))
                (expected-suffix
                 (concat
                  "-> (request) ["
                  request
                  "]\n"
                  "<- (response) ["
                  response
                  "]\n")))
            (should (string-suffix-p expected-suffix content)))))))

  (setq mcp-server-lib-log-io nil))

(ert-deftest mcp-server-lib-test-log-io-nil ()
  "Test that when `mcp-server-lib-log-io' is nil, messages are not logged."
  (setq mcp-server-lib-log-io nil)

  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (let ((request (mcp-server-lib-create-tools-list-request)))
      (mcp-server-lib-process-jsonrpc request)
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

(ert-deftest mcp-server-lib-test-install ()
  "Test script installation to temporary directory."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (mcp-server-lib-install))
          (should
           (file-exists-p (mcp-server-lib--installed-script-path)))
          (should
           (file-executable-p
            (mcp-server-lib--installed-script-path))))
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-install-overwrite ()
  "Test script installation when file already exists."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (mcp-server-lib--installed-script-path)))
    (unwind-protect
        (progn
          (write-region "existing content" nil target)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (mcp-server-lib-install))
          (should (file-exists-p target))
          (should (file-executable-p target))
          (should
           (> (file-attribute-size (file-attributes target)) 20)))
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-install-cancel ()
  "Test cancelling installation when file exists."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (mcp-server-lib--installed-script-path)))
    (unwind-protect
        (progn
          (write-region "existing content" nil target)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
            (should-error (mcp-server-lib-install) :type 'user-error))
          (should
           (string=
            "existing content"
            (with-temp-buffer
              (insert-file-contents target)
              (buffer-string)))))
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-uninstall ()
  "Test script removal from temporary directory."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (mcp-server-lib--installed-script-path)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (mcp-server-lib-install)
            (should (file-exists-p target))
            (mcp-server-lib-uninstall))
          (should-not (file-exists-p target)))
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-uninstall-missing ()
  "Test uninstalling when script doesn't exist."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir))
    (unwind-protect
        (should-error (mcp-server-lib-uninstall) :type 'user-error)
      (delete-directory temp-dir t))))

(ert-deftest mcp-server-lib-test-uninstall-cancel ()
  "Test cancelling uninstall."
  (let* ((temp-dir (make-temp-file "mcp-test-" t))
         (mcp-server-lib-install-directory temp-dir)
         (target (mcp-server-lib--installed-script-path)))
    (unwind-protect
        (progn
          (write-region "test content" nil target)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
            (mcp-server-lib-uninstall))
          (should (file-exists-p target)))
      (delete-directory temp-dir t))))

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
     (mcp-server-lib-create-tools-list-request 100))
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
    (mcp-server-lib-create-tools-list-request 200))
   (mcp-server-lib-test--call-tool "display-test-tool" 201)

   ;; Show metrics
   (mcp-server-lib-show-metrics)

   ;; Verify buffer exists and contains expected content
   (with-current-buffer "*MCP Metrics*"
     (let ((content (buffer-string)))
       (should (string-match "MCP Usage Metrics" content))
       (should (string-match "Method Calls:" content))
       ;; Should have at least 1 tools/list call from our test
       (should (string-match "tools/list\\s-+\\([0-9]+\\)" content))
       (let ((tools-list-count
              (string-to-number (match-string 1 content))))
         (should (>= tools-list-count 1)))

       (should (string-match "Tool Usage:" content))
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
  (mcp-server-lib-ert-with-server :tools nil :resources nil
                                    (mcp-server-lib-process-jsonrpc
                                     (mcp-server-lib-create-tools-list-request 100))
                                    
                                    ;; Verify metrics exist
                                    (let ((summary (mcp-server-lib-metrics-summary)))
                                      (should (stringp summary))
                                      ;; Should show at least 2 calls (initialize + tools/list)
                                      (should (string-match "[2-9][0-9]* calls\\|[0-9][0-9]+ calls" summary))))
  
  ;; Second part: start server again and verify metrics were reset
  (mcp-server-lib-ert-with-server :tools nil :resources nil
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

(ert-deftest test-mcp-server-lib-resources-list-empty ()
  "Test resources/list with no registered resources."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--check-no-resources)))

(ert-deftest test-mcp-server-lib-register-resource ()
  "Test registering a direct resource."
  (mcp-server-lib-test--with-resources
      (("test://resource1"
        #'mcp-server-lib-test--return-string
        :name "Test Resource"
        :description "A test resource"
        :mime-type "text/plain"))
    ))

(ert-deftest test-mcp-server-lib-register-resource-minimal ()
  "Test registering a resource with only required fields."
  (mcp-server-lib-test--with-resources
   (("test://minimal"
     #'mcp-server-lib-test--return-string
     :name "Minimal Resource"))
   ;; Verify resource can be read without mime-type
   (mcp-server-lib-ert-verify-resource-read
    "test://minimal"
    '((uri . "test://minimal")
      (text . "test result")))))

(ert-deftest test-mcp-server-lib-resources-read ()
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

(ert-deftest test-mcp-server-lib-resources-read-handler-nil ()
  "Test that resource handler returning nil produces valid response with empty text."
  (mcp-server-lib-test--with-resources
   (("test://nil-resource"
     #'mcp-server-lib-test--return-nil
     :name "Nil Resource"))
   ;; Read the resource
   (mcp-server-lib-ert-verify-resource-read
    "test://nil-resource"
    '((uri . "test://nil-resource")
      (text . nil)))))

(ert-deftest test-mcp-server-lib-resources-read-not-found ()
  "Test reading a non-existent resource returns error."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
   (mcp-server-lib-test--read-resource-error
    "test://nonexistent"
    mcp-server-lib-jsonrpc-error-invalid-params
    "Resource not found: test://nonexistent")))

(ert-deftest test-mcp-server-lib-register-resource-duplicate ()
  "Test registering the same resource twice increments ref count."
  (mcp-server-lib-test--with-resources
   (("test://resource1"
     #'mcp-server-lib-test--return-string
     :name "Test Resource"))
   ;; The macro automatically verifies the resource is in the list
   ;; Now register the same resource again to test ref counting
   (mcp-server-lib-test--register-resource
    "test://resource1"
    #'mcp-server-lib-test--return-string
    :name "Test Resource"
    ;; Verify it's still listed only once
    (mcp-server-lib-test--check-single-resource
     '((uri . "test://resource1")
       (name . "Test Resource"))))
   
   ;; After inner macro completes, it unregisters once (ref count goes from 2 to 1)
   ;; Resource should still exist because outer registration is still active
   (mcp-server-lib-test--check-single-resource
    '((uri . "test://resource1")
      (name . "Test Resource"))))
  
  ;; After outer macro completes, it unregisters again (ref count = 0)
  ;; Resource should no longer be listed
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--check-no-resources)))

(ert-deftest test-mcp-server-lib-register-resource-error-missing-name ()
  "Test that resource registration with missing :name produces an error."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (should-error
     (mcp-server-lib-register-resource
      "test://resource"
      #'mcp-server-lib-test--return-string
      :description "Resource without name")
     :type 'error)))

(ert-deftest test-mcp-server-lib-register-resource-error-missing-handler ()
  "Test that resource registration with non-function handler produces an error."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (should-error
     (mcp-server-lib-register-resource
      "test://resource"
      "not-a-function"
      :name "Test Resource")
     :type 'error)))

(ert-deftest test-mcp-server-lib-register-resource-error-missing-uri ()
  "Test that resource registration with nil URI produces an error."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (should-error
     (mcp-server-lib-register-resource
      nil
      #'mcp-server-lib-test--return-string
      :name "Test Resource")
     :type 'error)))

(ert-deftest test-mcp-server-lib-unregister-resource-nonexistent ()
  "Test that `mcp-server-lib-unregister-resource` returns nil for missing resources."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (should-not (mcp-server-lib-unregister-resource "test://nonexistent"))))

(ert-deftest test-mcp-server-lib-resources-list-multiple ()
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
      (let ((resource1 (mcp-server-lib-test--find-resource-by-uri "test://resource1" resources))
            (resource2 (mcp-server-lib-test--find-resource-by-uri "test://resource2" resources)))
        ;; Verify first resource
        (should resource1)
        (should (equal (alist-get 'name resource1) "First Resource"))
        (should (equal (alist-get 'description resource1) "The first test resource"))
        (should-not (alist-get 'mimeType resource1))
        ;; Verify second resource
        (should resource2)
        (should (equal (alist-get 'name resource2) "Second Resource"))
        (should-not (alist-get 'description resource2))
        (should (equal (alist-get 'mimeType resource2) "text/markdown"))))))

(ert-deftest test-mcp-server-lib-resources-read-handler-error ()
  "Test that resource handler errors return JSON-RPC error and increment error metrics."
  (mcp-server-lib-test--with-resources
   (("test://error-resource"
     #'mcp-server-lib-test--generic-error-handler
     :name "Error Resource"))
   (mcp-server-lib-test--check-resource-read-error
     "test://error-resource"
     mcp-server-lib-jsonrpc-error-internal
     "Error reading resource test://error-resource: Generic error occurred")))

(ert-deftest mcp-server-lib-test-resource-signal-error-invalid-params ()
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

(ert-deftest mcp-server-lib-test-resource-regular-error-backward-compat ()
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
   (mcp-server-lib-test--with-undefined-function 'mcp-server-lib-test--handler-to-be-undefined
     (mcp-server-lib-ert-with-metrics-tracking
      (("resources/read" 1 1))
      ;; Try to read the resource - should return an error
      ;; Note: error-message-string may use Unicode or ASCII quotes depending on locale
      (let* ((response (mcp-server-lib-ert--read-resource "test://undefined-handler"))
             (error-obj (alist-get 'error response))
             (actual-message (alist-get 'message error-obj)))
        (should (equal mcp-server-lib-jsonrpc-error-internal (alist-get 'code error-obj)))
        ;; Check message contains expected parts (quotes may vary by locale)
        (should (string-match-p "^Error reading resource test://undefined-handler: Symbol.s function definition is void: mcp-server-lib-test--handler-to-be-undefined$"
                                actual-message)))))))

(ert-deftest test-mcp-server-lib-resources-list-mixed ()
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
    '((uri . "test://direct1")
      (text . "test result")))
   (mcp-server-lib-ert-verify-resource-read
    "org://example.org"
    '((uri . "org://example.org")
      (text . "params: ((\"filename\" . \"example.org\"))")))))

;;; Resource Template Invalid Syntax Tests

(defun mcp-server-lib-test--assert-invalid-template-registration (uri)
  "Assert that registering a resource template with URI fails."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
   (should-error
    (mcp-server-lib-register-resource
     uri
     #'mcp-server-lib-test--resource-template-handler-dump-params
     :name "Test Template"))))

(defun mcp-server-lib-test--assert-invalid-handler-registration (handler
                                                                 handler-desc)
  "Check that registering a resource with invalid HANDLER & HANDLER-DESC fails."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (should-error
     (mcp-server-lib-register-resource
      "test://resource"
      handler
      :name (format "Resource with %s" handler-desc))
     :type 'error)))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-unclosed ()
  "Test resource template with unclosed variable syntax error."
  (mcp-server-lib-test--assert-invalid-template-registration "org://{filename"))

(ert-deftest
    test-mcp-server-lib-resource-template-invalid-syntax-unmatched-close ()
  "Test resource template with unmatched closing brace syntax error."
  (mcp-server-lib-test--assert-invalid-template-registration "org://filename}"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-empty ()
  "Test resource template with empty variable name syntax error."
  (mcp-server-lib-test--assert-invalid-template-registration "org://{}"))

(ert-deftest
    test-mcp-server-lib-resource-template-invalid-syntax-numeric-variable ()
  "Test resource template with numeric variable name is rejected."
  (mcp-server-lib-test--assert-invalid-template-registration
    "org://{123}/content"))

(ert-deftest
    test-mcp-server-lib-resource-template-invalid-syntax-special-chars-variable
    ()
  "Test resource template with special characters in variable name is rejected."
  (mcp-server-lib-test--assert-invalid-template-registration
    "org://{var-name}/content"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-scheme-only ()
  "Test resource template with only scheme and no path."
  (mcp-server-lib-test--assert-invalid-template-registration "org://"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-no-scheme ()
  "Test resource template without a scheme."
  (mcp-server-lib-test--assert-invalid-template-registration "{foo}"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-single-colon ()
  "Test resource template with single colon instead of ://."
  (mcp-server-lib-test--assert-invalid-template-registration "foo:bar"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-single-slash ()
  "Test resource template with :/ instead of ://."
  (mcp-server-lib-test--assert-invalid-template-registration "foo:/bar"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-no-colon ()
  "Test resource template with path but no scheme separator."
  (mcp-server-lib-test--assert-invalid-template-registration "foo/bar"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-extra-colon ()
  "Test resource template with extra colon before ://."
  (mcp-server-lib-test--assert-invalid-template-registration "foo:://bar"))

(ert-deftest
    test-mcp-server-lib-resource-template-invalid-syntax-no-scheme-prefix ()
  "Test resource template starting with ://."
  (mcp-server-lib-test--assert-invalid-template-registration "://bar"))

(ert-deftest
    test-mcp-server-lib-resource-template-invalid-syntax-double-slash-only ()
  "Test resource template with // but no scheme."
  (mcp-server-lib-test--assert-invalid-template-registration "foo//bar"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-trailing-colon
    ()
  "Test resource template with trailing colon only."
  (mcp-server-lib-test--assert-invalid-template-registration "foo:"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-colon-slash ()
  "Test resource template with :/ at end."
  (mcp-server-lib-test--assert-invalid-template-registration "foo:/"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-leading-colon
    ()
  "Test resource template starting with colon."
  (mcp-server-lib-test--assert-invalid-template-registration ":foo//bar"))

(ert-deftest
    test-mcp-server-lib-resource-template-invalid-syntax-space-in-scheme ()
  "Test resource template with space in scheme."
  (mcp-server-lib-test--assert-invalid-template-registration "foo bar://baz"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-numeric-scheme
    ()
  "Test resource template with numeric scheme."
  (mcp-server-lib-test--assert-invalid-template-registration "123://bar"))

(ert-deftest
    test-mcp-server-lib-resource-template-invalid-syntax-underscore-in-scheme ()
  "Test resource template with underscore in scheme."
  (mcp-server-lib-test--assert-invalid-template-registration "foo_bar://baz"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-hyphen-first ()
  "Test resource template with hyphen as first character in scheme."
  (mcp-server-lib-test--assert-invalid-template-registration "-foo://bar"))

(ert-deftest test-mcp-server-lib-resource-template-invalid-syntax-empty-string ()
  "Test resource template with empty string."
  (mcp-server-lib-test--assert-invalid-template-registration ""))

(ert-deftest
    test-mcp-server-lib-resource-template-invalid-syntax-whitespace-only ()
  "Test resource template with whitespace only."
  (mcp-server-lib-test--assert-invalid-template-registration "   "))

(ert-deftest test-mcp-server-lib-register-resource-nil-handler ()
  "Test resource registration with nil handler fails."
  (mcp-server-lib-test--assert-invalid-handler-registration nil "nil handler"))

(ert-deftest test-mcp-server-lib-register-resource-string-handler ()
  "Test resource registration with string handler fails."
  (mcp-server-lib-test--assert-invalid-handler-registration
   "not a function" "string handler"))

(ert-deftest test-mcp-server-lib-register-resource-number-handler ()
  "Test resource registration with number handler fails."
  (mcp-server-lib-test--assert-invalid-handler-registration 42 "number handler"))

(ert-deftest test-mcp-server-lib-resource-template-simple-variable ()
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

(ert-deftest test-mcp-server-lib-resource-template-reserved-expansion ()
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
      (text . "params: ((\"path\" . \"folder/subfolder/file.org\"))")))))

(ert-deftest test-mcp-server-lib-resource-template-multiple-variables ()
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
      (text .
            "params: ((\"filename\" . \"todo.org\") (\"path\" . \"Tasks/Urgent\"))")))))

(ert-deftest test-mcp-server-lib-register-resource-missing-name ()
  "Test error when registering template without name."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (should-error
     (mcp-server-lib-register-resource
      "test://{id}"
      #'mcp-server-lib-test--resource-template-handler-dump-params)
     :type 'error)))

(ert-deftest test-mcp-server-lib-resources-read-direct-precedence ()
  "Test that direct resources take precedence over resource templates."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    ;; Register template first
    (mcp-server-lib-test--register-resource
     "test://{id}"
     #'mcp-server-lib-test--resource-template-handler-dump-params
     :name "Template Resource"
     ;; Register direct resource with URI that would match template
     (mcp-server-lib-test--register-resource
      "test://exact"
      #'mcp-server-lib-test--return-string
      :name "Direct Resource"
      ;; Should get direct resource content
      (mcp-server-lib-ert-verify-resource-read
       "test://exact"
       '((uri . "test://exact")
         (text . "test result")))))))

(ert-deftest test-mcp-server-lib-resources-read-multiple-template-schemes ()
  "Test that resource templates with different schemes route correctly."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--register-resource
     "org://{filename}"
     #'mcp-server-lib-test--resource-template-handler-dump-params
     :name "Org Files"
     (mcp-server-lib-test--register-resource
      "doc://{docname}"
      #'mcp-server-lib-test--resource-template-handler-dump-params-2
      :name "Doc Files"
      (mcp-server-lib-ert-verify-resource-read
       "org://projects.org"
       '((uri . "org://projects.org")
         (text . "params: ((\"filename\" . \"projects.org\"))")))
      (mcp-server-lib-ert-verify-resource-read
       "doc://manual.pdf"
       '((uri . "doc://manual.pdf")
         (text . "Handler-2: params: ((\"docname\" . \"manual.pdf\"))"))))))

(ert-deftest test-mcp-server-lib-resources-read-no-template-match ()
  "Test error when no resource template matches the URI."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--register-resource
     "test://{id}"
     #'mcp-server-lib-test--resource-template-handler-dump-params
     :name "Test Template"
     ;; Try to read with non-matching URI
     (mcp-server-lib-test--read-resource-error
      "other://123"
      mcp-server-lib-jsonrpc-error-invalid-params
      "Resource not found: other://123"))))

(ert-deftest test-mcp-server-lib-resource-template-empty-parameter-value ()
  "Test resource template matching with empty parameter value."
  (mcp-server-lib-test--with-resources
   (("org://{filename}"
     #'mcp-server-lib-test--resource-template-handler-dump-params
     :name "Org file template"))
   ;; Test URI with empty filename parameter
   (mcp-server-lib-ert-verify-resource-read
    "org://"
    '((uri . "org://")
      (text . "params: ((\"filename\" . \"\"))"))))))

(ert-deftest test-mcp-server-lib-unregister-resource-multiple ()
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
   (let ((resources (mcp-server-lib-ert-get-resource-templates-list)))
     (should (= 3 (length resources))))
   ;; Unregister the middle one
   (mcp-server-lib-unregister-resource "doc://{docname}")
   ;; Verify only two remain
   (let ((resources (mcp-server-lib-ert-get-resource-templates-list)))
     (should (= 2 (length resources)))
     ;; Check the remaining ones
     (should (mcp-server-lib-test--find-resource-by-uri-template
              "org://{filename}" resources))
     (should (mcp-server-lib-test--find-resource-by-uri-template
              "test://{id}" resources))
     ;; Verify the unregistered one is gone
     (should-not (mcp-server-lib-test--find-resource-by-uri-template
                  "doc://{docname}" resources)))
   ;; Verify the remaining templates still work
   (mcp-server-lib-ert-verify-resource-read
    "org://test.org"
    '((uri . "org://test.org")
      (text . "params: ((\"filename\" . \"test.org\"))")))
   (mcp-server-lib-ert-verify-resource-read
    "test://123"
    '((uri . "test://123")
      (text . "params: ((\"id\" . \"123\"))")))
   ;; Verify the unregistered template no longer matches
   (mcp-server-lib-test--read-resource-error
    "doc://manual.pdf"
    mcp-server-lib-jsonrpc-error-invalid-params
    "Resource not found: doc://manual.pdf")))

(ert-deftest test-mcp-server-lib-resources-read-template-handler-error ()
  "Test template handler errors bumping metrics and returning JSON-RPC errors."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--register-resource
     "error://{id}"
     #'mcp-server-lib-test--template-handler-error
     :name "Error Template"
     (mcp-server-lib-test--check-resource-read-error
       "error://test"
       mcp-server-lib-jsonrpc-error-internal
       "Error reading resource error://test: Generic error occurred"))))

(ert-deftest test-mcp-server-lib-resources-read-template-handler-nil ()
  "Test nil-returning template handler produces valid response with empty text."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--register-resource
     "nil://{id}"
     #'mcp-server-lib-test--resource-template-handler-nil
     :name "Nil Template"
     ;; Read the resource
     (mcp-server-lib-ert-verify-resource-read
      "nil://test"
      '((uri . "nil://test")
        (text . nil))))))

(ert-deftest test-mcp-server-lib-resources-read-template-handler-undefined ()
  "Test reading a resource template whose handler function no longer exists."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--register-resource
     "undefined://{id}"
     #'mcp-server-lib-test--handler-to-be-undefined
     :name "Undefined Handler Template"
     (mcp-server-lib-test--with-undefined-function
      'mcp-server-lib-test--handler-to-be-undefined
       (mcp-server-lib-ert-with-metrics-tracking
        (("resources/read" 1 1))
        ;; Try to read the resource - should return an error
        (mcp-server-lib-test--read-resource-error
         "undefined://test-123"
         mcp-server-lib-jsonrpc-error-internal
         "Error reading resource undefined://test-123: Symbol’s function definition is void: mcp-server-lib-test--handler-to-be-undefined"))))))

(ert-deftest test-mcp-server-lib-resource-template-scheme-case-insensitive ()
  "Test that URI schemes should be case-insensitive per RFC 3986."
  (mcp-server-lib-test--with-resources
   (("test://{id}"
     #'mcp-server-lib-test--resource-template-handler-dump-params
     :name "Test Template"))
   ;; Test uppercase scheme should match
   (mcp-server-lib-ert-verify-resource-read
    "TEST://123"
    '((uri . "TEST://123")
      (text . "params: ((\"id\" . \"123\"))")))
   ;; Test mixed case scheme should match
   (mcp-server-lib-ert-verify-resource-read
    "Test://456"
    '((uri . "Test://456")
      (text . "params: ((\"id\" . \"456\"))")))))

(ert-deftest test-mcp-server-lib-resource-template-variable-names-case-sensitive
    ()
  "Test that variable names in templates are case-sensitive per RFC 6570."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    ;; Register template with lowercase variable
    (mcp-server-lib-test--register-resource
     "test://{username}"
     #'mcp-server-lib-test--resource-template-handler-dump-params
     :name "Lowercase Template"
     ;; Register template with uppercase variable (different template)
     (mcp-server-lib-test--register-resource
      "test://{USERNAME}"
      #'mcp-server-lib-test--resource-template-handler-dump-params-2
      :name "Uppercase Template"
      ;; Both templates should be registered
      (let ((resources (mcp-server-lib-ert-get-resource-templates-list)))
        (should (= 2 (length resources))))
      ;; Test that they extract different variables
      (mcp-server-lib-ert-verify-resource-read
       "test://john"
       '((uri . "test://john")
         (text . "params: ((\"username\" . \"john\"))")))))))

(ert-deftest test-mcp-server-lib-resource-template-path-literals-case-sensitive
    ()
  "Test that literal path segments are case-sensitive."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    ;; Register template with lowercase path
    (mcp-server-lib-test--register-resource
     "test://path/{id}"
     #'mcp-server-lib-test--resource-template-handler-dump-params
     :name "Lowercase Path Template"
     ;; Register template with uppercase path
     (mcp-server-lib-test--register-resource
      "test://PATH/{id}"
      (lambda (params)
        (format "UPPERCASE PATH: %s" (alist-get "id" params nil nil #'string=)))
      :name "Uppercase Path Template"
      ;; Both templates should be registered
      (let ((resources (mcp-server-lib-ert-get-resource-templates-list)))
        (should (= 2 (length resources))))
      ;; Test lowercase path matches only lowercase template
      (mcp-server-lib-ert-verify-resource-read
       "test://path/123"
       '((uri . "test://path/123")
         (text . "params: ((\"id\" . \"123\"))")))
      ;; Test uppercase path matches only uppercase template
      (mcp-server-lib-ert-verify-resource-read
       "test://PATH/456"
       '((uri . "test://PATH/456")
         (text . "UPPERCASE PATH: 456")))
      ;; Test mixed case path doesn't match either
      (mcp-server-lib-test--read-resource-error
       "test://Path/789"
       mcp-server-lib-jsonrpc-error-invalid-params
       "Resource not found: test://Path/789")))))

(ert-deftest test-mcp-server-lib-resource-template-unicode-in-variables ()
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

(ert-deftest test-mcp-server-lib-resource-template-percent-encoded-extraction ()
  "Test that extracted parameters remain percent-encoded."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--register-resource
     "file://{path}"
     #'mcp-server-lib-test--resource-template-handler-dump-params
     :name "File template"
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
        (text . "params: ((\"path\" . \"file%2Bwith%2Bplus.txt\"))"))))))

(ert-deftest
    test-mcp-server-lib-resource-template-reserved-expansion-passthrough ()
  "Test that {+var} allows reserved chars without encoding."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    (mcp-server-lib-test--register-resource
     "file:///{+path}"
     #'mcp-server-lib-test--resource-template-handler-dump-params
     :name "File path template"
     ;; Test mixed reserved characters
     (mcp-server-lib-ert-verify-resource-read
      "file:///path/with?query=value#section"
      '((uri . "file:///path/with?query=value#section")
        (text . "params: ((\"path\" . \"path/with?query=value#section\"))"))))))

(ert-deftest test-mcp-server-lib-resource-template-first-match-precedence ()
  "Test which template wins when multiple could match."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    ;; Register general template first
    (mcp-server-lib-test--register-resource
     "test://{id}"
     #'mcp-server-lib-test--resource-template-handler-dump-params
     :name "General template"
     ;; Register more specific template second
     (mcp-server-lib-test--register-resource
      "test://item/{id}"
      #'mcp-server-lib-test--resource-template-handler-dump-params-2
      :name "Specific template"
      ;; First registered template should win
      (mcp-server-lib-ert-verify-resource-read
       "test://item/123"
       '((uri . "test://item/123")
         (text . "params: ((\"id\" . \"item/123\"))")))
      ;; Verify both templates are registered
      (let ((resources (mcp-server-lib-ert-get-resource-templates-list)))
        (should (= 2 (length resources))))))))

(ert-deftest test-mcp-server-lib-resources-read-malformed-params ()
  "Test resources/read with invalid params structure (string instead of object)."
  (mcp-server-lib-test--with-resources
   (("test://resource" #'mcp-server-lib-test--return-string
     :name "Test Resource"))
   ;; Test with string params instead of object
   (mcp-server-lib-test--check-resource-read-request-error
    "invalid string params"
    mcp-server-lib-jsonrpc-error-internal
    "Internal error: Wrong type argument: listp, \"invalid string params\"")))

(ert-deftest test-mcp-server-lib-resources-read-missing-uri ()
  "Test resources/read without uri parameter."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    ;; Test missing uri parameter
    (mcp-server-lib-test--check-resource-read-request-error
     nil ; No uri
     mcp-server-lib-jsonrpc-error-invalid-params
     "Resource not found: nil")))

(ert-deftest test-mcp-server-lib-resources-read-numeric-uri ()
  "Test resources/read with numeric uri."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    ;; Test with number uri
    (mcp-server-lib-test--check-resource-read-request-error
     '((uri . 123))
     mcp-server-lib-jsonrpc-error-invalid-params
     "Resource not found: 123")))

(ert-deftest test-mcp-server-lib-resources-read-array-uri ()
  "Test resources/read with array uri."
  (mcp-server-lib-ert-with-server :tools nil :resources nil
    ;; Test with array uri
    (mcp-server-lib-test--check-resource-read-request-error
     '((uri . ["test" "array"]))
     mcp-server-lib-jsonrpc-error-invalid-params
     "Resource not found: [test array]")))

(ert-deftest test-mcp-server-lib-resource-template-handler-wrong-signature ()
  "Test template handler that doesn't accept params argument."
  (mcp-server-lib-test--with-resources
   (("test://{id}"
     #'mcp-server-lib-test--return-string
     :name "Wrong Signature Handler"))
   (mcp-server-lib-test--read-resource-error
    "test://123"
    mcp-server-lib-jsonrpc-error-internal
    (if (version< emacs-version "30.1")
        "Error reading resource test://123: Wrong number of arguments: ((t) nil \"Generic handler to return a test string.\" \"test result\"), 1"
      "Error reading resource test://123: Wrong number of arguments: #[nil (\"test result\") (t) nil \"Generic handler to return a test string.\"], 1"))))

(provide 'mcp-server-lib-test)

;; Local Variables:
;; package-lint-main-file: "mcp-server-lib.el"
;; End:

;;; mcp-server-lib-test.el ends here
