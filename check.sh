#!/bin/bash
# check.sh - Run all quality checks, with focus on providing guardrails for LLM
# coding agents.
#
# Continue on errors but track them

#
# Linters / tests / autoformatters and their dependencies:
#
# - Elisp:
#   - Start with the syntax check (via byte compilation)
#     - Catches major issues like unbalanced parentheses
#     - LLMs tend to generate syntax errors, so check these first
#   - Run elisp-autofmt
#     - Skip if there are syntax errors to prevent incorrect runaway
#       re-indentation
#   - Run elisp-lint
#     - Skip if there are either syntax errors or autoformatting failure
#     - It may still produce formatting issues even after a successful
#       elisp-autofmt call, but much fewer of them, and more suitable for fixing
#       by the LLM.
#   - Run ERT tests
#     - Skip if there were syntax errors#
# - Org (elisp-adjacent): call Emacs org-lint on Org files
# - Shell:
#   - Check syntax with bash -n
#   - Do shellcheck static analysis
#     - Only if the syntax check passed
#   - Run the MCP stdio adapter tests
#     - Only if the syntax check passed
#   - Format all the shell scripts with shfmt
#     - Only if the syntax check passed to avoid runaway re-indentation
# - Markdown:
#   - Lint with mdl
#   - Check formatting with prettier
#   - Check terminology with textlint
# - GitHub Actions / YAML:
#   - Check GitHub workflows with actionlint
#   - Check YAML formatting with prettier

set -eu -o pipefail

readonly SHELL_FILES=(check.sh emacs-mcp-stdio.sh emacs-mcp-stdio-test.sh)

ERRORS=0
ELISP_SYNTAX_FAILED=0
SHELL_SYNTAX_FAILED=0

# Elisp

echo -n "Checking Elisp syntax... "
if eask recompile; then
	echo "OK!"
else
	echo "Elisp syntax check failed!"
	ERRORS=$((ERRORS + 1))
	ELISP_SYNTAX_FAILED=1
fi

# Only run indentation if there are no syntax errors
if [ $ELISP_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running elisp-autofmt... "
	# Note: elisp-autofmt and elisp-lint may disagree on cl-defmacro formatting
	# See https://codeberg.org/ideasman42/emacs-elisp-autofmt/issues/10
	if eask format elisp-autofmt; then
		echo "OK!"
	else
		echo "elisp-autofmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping indentation due to syntax errors"
fi

eask clean elc >/dev/null 2>&1 || true

# Only run elisp-lint if there are no errors so far
if [ $ERRORS -eq 0 ]; then
	echo -n "Running elisp-lint... "
	if eask lint elisp-lint; then
		echo "OK!"
	else
		echo "elisp-lint failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping elisp-lint due to previous errors"
fi

eask clean elc >/dev/null 2>&1 || true

# Run Eask keywords lint
echo -n "Running eask lint keywords... "
if eask lint keywords; then
	echo "OK!"
else
	echo "eask lint keywords failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running eask lint regexps... "
if eask lint regexps; then
	echo "OK!"
else
	echo "eask lint regexps failed"
	ERRORS=$((ERRORS + 1))
fi

# Only run ERT tests if there are no Elisp syntax errors
if [ $ELISP_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running all tests... "
	if eask run script test; then
		echo "OK!"
	else
		echo "ERT tests failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping ERT tests due to Elisp syntax errors"
fi

# Org

echo -n "Checking org files... README.org TODO.org "
if eask run script org-lint; then
	echo "OK!"
else
	echo "org files check failed"
	ERRORS=$((ERRORS + 1))
fi

# Shell

echo -n "Checking shell syntax... ${SHELL_FILES[*]} "
if bash -n "${SHELL_FILES[@]}"; then
	echo "OK!"
else
	echo "shell syntax check failed!"
	ERRORS=$((ERRORS + 1))
	SHELL_SYNTAX_FAILED=1
fi

if [ $SHELL_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running shellcheck... ${SHELL_FILES[*]} "
	if shellcheck "${SHELL_FILES[@]}"; then
		echo "OK!"
	else
		echo "shellcheck check failed"
		ERRORS=$((ERRORS + 1))
	fi

	echo -n "Running stdio adapter tests... "
	if ! ./emacs-mcp-stdio-test.sh; then
		echo "stdio adapter tests failed"
		ERRORS=$((ERRORS + 1))
	fi

	echo -n "Running shfmt to format all shell scripts... ${SHELL_FILES[*]} "
	if shfmt -w "${SHELL_FILES[@]}"; then
		echo "OK!"
	else
		echo "shfmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping shellcheck, stdio adapter tests, and shfmt due to previous errors"
fi

# Markdown

echo -n "Checking Markdown files... $(echo ./*.md) "
if mdl --no-verbose ./*.md; then
	echo "OK!"
else
	echo "mdl check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking Markdown formatting... $(echo ./*.md) "
if prettier --log-level warn --check ./*.md; then
	echo "OK!"
else
	echo "prettier check for Markdown failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking terminology... $(echo ./*.md) "
if textlint --rule terminology ./*.md; then
	echo "OK!"
else
	echo "textlint check failed"
	ERRORS=$((ERRORS + 1))
fi

# GitHub Actions / YAML

echo -n "Checking GitHub workflows... $(echo .github/workflows/*.yml) "
if actionlint .github/workflows/*.yml; then
	echo "OK!"
else
	echo "actionlint check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking GitHub Actions security... $(echo .github/workflows/*.yml) "
if zizmor .github/workflows/*.yml; then
	echo "OK!"
else
	echo "zizmor check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking YAML formatting... $(echo .github/workflows/*.yml) "
if prettier --log-level warn --check .github/workflows/*.yml; then
	echo "OK!"
else
	echo "prettier check failed!"
	ERRORS=$((ERRORS + 1))
fi

# Biome

echo -n "Running Biome format check... "
if npx @biomejs/biome format .; then
	echo "OK!"
else
	echo "Biome format check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running Biome lint... "
if npx @biomejs/biome lint .; then
	echo "OK!"
else
	echo "Biome lint failed!"
	ERRORS=$((ERRORS + 1))
fi

# Final result
if [ $ERRORS -eq 0 ]; then
	echo "All checks passed successfully!"
else
	echo "$ERRORS check(s) failed!"
	exit 1
fi
