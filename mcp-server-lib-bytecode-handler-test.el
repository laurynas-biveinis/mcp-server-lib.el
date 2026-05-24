;;; mcp-server-lib-bytecode-handler-test.el --- Bytecode handler test -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Laurynas Biveinis

;;; Commentary:

;; This file contains a handler function that will be byte-compiled and used
;; in tests.

;;; Code:

(require 'mcp-server-lib)

(defun mcp-server-lib-test-bytecode-handler--handler (input-string)
  "Test handler function that will be loaded as bytecode.
INPUT-STRING is the string argument passed to the tool.

MCP Parameters:
  input-string - Input string parameter for bytecode testing"
  (format "Bytecode handler result: %s" input-string))

(provide 'mcp-server-lib-bytecode-handler-test)

;; Local Variables:
;; package-lint-main-file: "mcp-server-lib.el"
;; End:

;;; mcp-server-lib-bytecode-handler-test.el ends here
