;;; mcp-server-lib-metrics.el --- Metrics collection for MCP server -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Laurynas Biveinis

;; This file is part of mcp-server-lib.

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

;; This file provides metrics collection functionality for the MCP server.
;; It tracks usage statistics for all MCP operations including tools,
;; resources, and prompts.

;;; Code:

(require 'cl-lib)

;;; Data structures

(cl-defstruct
 mcp-server-lib-metrics
 "Metrics for any MCP operation.

This structure tracks usage statistics for MCP operations including
method calls (e.g., \"initialize\", \"tools/list\") and tool-specific
calls (e.g., \"tools/call:my-tool\").

Slots:
  `calls'  - Total number of times the operation was invoked
  `errors' - Number of times the operation resulted in an error"
 (calls 0 :type integer :documentation "Total number of invocations.")
 (errors
  0
  :type integer
  :documentation "Number of failed invocations."))

;;; Global state

(defvar mcp-server-lib-metrics--table (make-hash-table :test 'equal)
  "Metrics for all MCP operations.  Key: method or method:name.")

;;; Core functions

(defun mcp-server-lib-metrics--get (key)
  "Get metrics for KEY, creating if needed."
  (or (gethash key mcp-server-lib-metrics--table)
      (puthash
       key
       (make-mcp-server-lib-metrics)
       mcp-server-lib-metrics--table)))

(defun mcp-server-lib-metrics--error-rate (calls errors)
  "Calculate error rate percentage.
Compute the percentage of ERRORS relative to total CALLS.
Return a float between 0.0 and 100.0."
  (if (zerop calls)
      0.0
    (* 100.0 (/ (float errors) calls))))

(defun mcp-server-lib-metrics--track-tool-call
    (tool-name &optional is-error)
  "Track a tool call for TOOL-NAME.
If IS-ERROR is non-nil, also increment the error counter."
  (let ((metrics
         (mcp-server-lib-metrics--get
          (format "tools/call:%s" tool-name))))
    (cl-incf (mcp-server-lib-metrics-calls metrics))
    (when is-error
      (cl-incf (mcp-server-lib-metrics-errors metrics)))))

;;; Public functions

(defun mcp-server-lib-metrics-get (operation)
  "Get metrics object for a specific OPERATION.
Return a metrics object with zero values if OPERATION has not been tracked.

Arguments:
  OPERATION - The operation name (e.g., \"tools/list\",
              \"tools/call:my-tool\")

Use `mcp-server-lib-metrics-calls' and `mcp-server-lib-metrics-errors'
to access the metrics values from the returned object."
  (or (gethash operation mcp-server-lib-metrics--table)
      (make-mcp-server-lib-metrics)))

(defun mcp-server-lib-metrics-summary ()
  "Return metrics summary as a string."
  (let ((total-calls 0)
        (total-errors 0))
    (maphash
     (lambda (_key metrics)
       (cl-incf total-calls (mcp-server-lib-metrics-calls metrics))
       (cl-incf total-errors (mcp-server-lib-metrics-errors metrics)))
     mcp-server-lib-metrics--table)
    (format "MCP metrics: %d calls, %d errors (%.1f%% error rate)"
            total-calls total-errors
            (mcp-server-lib-metrics--error-rate
             total-calls total-errors))))

(provide 'mcp-server-lib-metrics)

;; Local Variables:
;; package-lint-main-file: "mcp-server-lib.el"
;; End:

;;; mcp-server-lib-metrics.el ends here
