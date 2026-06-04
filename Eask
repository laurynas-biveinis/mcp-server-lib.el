;; -*- mode: eask; lexical-binding: t -*-

(package "mcp-server-lib"
         "0.4.0"
         "Model Context Protocol server library")

(author "Laurynas Biveinis" "laurynas.biveinis@gmail.com")
(license "GPLv3")
(website-url "https://github.com/laurynas-biveinis/mcp-server-lib.el")
(keywords "comm" "tools")

(package-file "mcp-server-lib.el")
(files "mcp-server-lib-commands.el"
       "mcp-server-lib-metrics.el"
       "mcp-server-lib-ert.el"
       "emacs-mcp-stdio.sh")

(script "test" "eask test ert mcp-server-lib-test.el")

(script "org-lint" 
        (concat "emacs -Q --batch "
                "--eval \"(require 'org)\" "
                "--eval \"(require 'org-lint)\" "
                "--eval \"(let ((all-checks-passed t) "
                        "(files '(\\\"README.org\\\"))) "
                    "(dolist (file files all-checks-passed) "
                      "(with-temp-buffer "
                        "(insert-file-contents file) "
                        "(org-mode) "
                        "(let ((results (org-lint))) "
                          "(when results "
                            "(message \\\"Found issues in %s: %S\\\" file results) "
                            "(setq all-checks-passed nil))))) "
                    "(unless all-checks-passed (kill-emacs 1)))\""))

(source "gnu")

(depends-on "emacs" "27.1")
