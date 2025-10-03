#!/bin/bash
# emacs-mcp-stdio-test.sh - Test the MCP stdio adapter

set -eu -o pipefail

check_log_contains() {
	local -r log="$1"
	local -r pattern="$2"
	local -r err_msg="$3"

	if ! grep -q "$pattern" "$log"; then
		echo "$TEST_CASE"
		echo "FAIL: $err_msg: $log"
		exit 1
	fi
}

check_log_not_contains() {
	local -r log="$1"
	local -r pattern="$2"
	local -r err_msg="$3"

	if grep -q "$pattern" "$log"; then
		echo "$TEST_CASE"
		echo "FAIL: $err_msg: $log"
		exit 1
	fi
}

verify_handshake_sequence() {
	local -r debug_log="$1"

	# Verify from the log file that all requests were processed
	local -r init_count=$(grep -c "MCP-REQUEST.*$INIT_REQUEST" "$debug_log")
	local -r notification_count=$(grep -c "MCP-REQUEST.*$NOTIFICATION" "$debug_log")
	local -r tools_count=$(grep -c "MCP-REQUEST.*$TOOLS_REQUEST" "$debug_log")

	# All three requests should be processed in a correct implementation
	if [ "$init_count" -ne 1 ] || [ "$notification_count" -ne 1 ] || [ "$tools_count" -ne 1 ]; then
		echo "$TEST_CASE"
		echo "FAIL: Not all requests in the handshake sequence were processed"
		echo "Init request count: $init_count"
		echo "Notification count: $notification_count"
		echo "Tools request count: $tools_count"
		cat "$debug_log"
		exit 1
	fi
}

run_emacs_function() {
	local -r func_name="$1"
	local -r err_msg="$2"
	local func_output
	local func_return_code

	func_output=$(emacsclient -s "$TEST_SERVER_NAME" -e "($func_name)")
	func_return_code=$?

	if [ $func_return_code -ne 0 ] || [ "$func_output" != "t" ]; then
		echo "FAIL: $err_msg, got output: $func_output, return code: $func_return_code"
		exit 1
	fi
}

TESTS_RUN=0

readonly TEST_SERVER_NAME="mcp-test-server-$$"
readonly STDIO_CMD="./emacs-mcp-stdio.sh --socket=$TEST_SERVER_NAME"

emacs -Q --daemon="$TEST_SERVER_NAME" --load "$(pwd)/mcp-server-lib-metrics.el" --load "$(pwd)/mcp-server-lib.el" --load "$(pwd)/mcp-server-lib-commands.el" --eval "(mcp-server-lib-start)" 2>/dev/null &
readonly SERVER_PID=$!

# shellcheck disable=SC2317,SC2329  # Called by trap
cleanup() {
	[ -n "${debug_log_file:-}" ] && [ -f "$debug_log_file" ] && rm -f "$debug_log_file"
	rm -f "stdio-response.txt"
	emacsclient -s "$TEST_SERVER_NAME" -e "(kill-emacs)" >/dev/null 2>&1 || true
	wait "$SERVER_PID" 2>/dev/null || true
}
trap cleanup EXIT

readonly MAX_TRIES=50
COUNT=0
while ! emacsclient -s "$TEST_SERVER_NAME" -e 't' >/dev/null 2>&1; do
	sleep 0.2
	COUNT=$((COUNT + 1))
	if [ $COUNT -ge $MAX_TRIES ]; then
		echo "ERROR: Server failed to start after 10 seconds"
		exit 1
	fi
done

TEST_CASE="Test case 1: MCP protocol handshake sequence"

# Create a log file for debugging
debug_log_file=$(mktemp "${TMPDIR:-/tmp}/mcp-debug-$$-XXXXXX.log")
export EMACS_MCP_DEBUG_LOG="$debug_log_file"

# These are the three messages in the MCP protocol handshake
INIT_REQUEST='{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{"roots":{}},"clientInfo":{"name":"test","version":"1.0"}},"id":1}'
NOTIFICATION='{"jsonrpc":"2.0","method":"notifications/initialized"}'
TOOLS_REQUEST='{"jsonrpc":"2.0","method":"tools/list","id":2}'

# Send all three requests to the script - this should complete successfully
set +e
RESPONSE=$(printf "%s\n%s\n%s\n" "$INIT_REQUEST" "$NOTIFICATION" "$TOOLS_REQUEST" | $STDIO_CMD)
CMD_EXIT_CODE=$?
set -e
if [ $CMD_EXIT_CODE -ne 0 ]; then
	echo "STDIO command failed with exit code: $CMD_EXIT_CODE"
	echo "Debug output may help identify the issue:"
	cat "$debug_log_file"
fi

# Verify all handshake requests were processed
verify_handshake_sequence "$debug_log_file"

# Check that we got responses to both initialize and tools/list requests
# We should have two lines in the output, one for each response
LINE_COUNT=$(echo "$RESPONSE" | wc -l)
if [ "$LINE_COUNT" -ne 2 ]; then
	echo "$TEST_CASE"
	echo "FAIL: Expected 2 response lines (initialize and tools/list), got $LINE_COUNT"
	echo "Response: $RESPONSE"
	rm -f "$debug_log_file"
	exit 1
fi

# Now extract the tools/list response (second line)
TOOLS_RESPONSE=$(echo "$RESPONSE" | tail -n 1)

# Check against the expected response format for tools/list
readonly EXPECTED_TOOLS='{"jsonrpc":"2.0","id":2,"result":{"tools":[]}}'

if [ "$TOOLS_RESPONSE" != "$EXPECTED_TOOLS" ]; then
	echo "$TEST_CASE"
	echo "FAIL: tools/list response doesn't match expected"
	echo "Expected: $EXPECTED_TOOLS"
	echo "Actual: $TOOLS_RESPONSE"
	rm -f "$debug_log_file"
	exit 1
fi

# Clean up
rm -f "$debug_log_file"
TESTS_RUN=$((TESTS_RUN + 1))

TEST_CASE="Test case 2: Debug logging with init and stop functions"

readonly INIT_FUNCTION="mcp-server-lib-start"
readonly STOP_FUNCTION="mcp-server-lib-stop"

# Stop MCP for this test as we want to test explicit init/stop functions
run_emacs_function "mcp-server-lib-stop" "Failed to stop MCP"

debug_log_file=$(mktemp /tmp/mcp-debug-$$-XXXXXX.log)

# These are the three messages in the MCP protocol handshake
INIT_REQUEST='{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{"roots":{}},"clientInfo":{"name":"test","version":"1.0"}},"id":1}'
NOTIFICATION='{"jsonrpc":"2.0","method":"notifications/initialized"}'
TOOLS_REQUEST='{"jsonrpc":"2.0","method":"tools/list","id":2}'

# Send all three requests to the script with the init and stop functions
printf "%s\n%s\n%s\n" "$INIT_REQUEST" "$NOTIFICATION" "$TOOLS_REQUEST" | EMACS_MCP_DEBUG_LOG="$debug_log_file" \
	$STDIO_CMD --init-function="$INIT_FUNCTION" --stop-function="$STOP_FUNCTION" >stdio-response.txt

if [ ! -f "$debug_log_file" ]; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log file was not created: $debug_log_file"
	exit 1
fi

# Verify all handshake requests were processed
verify_handshake_sequence "$debug_log_file"

# Check that we got responses to both initialize and tools/list requests
# We should have two lines in the output, one for each response
LINE_COUNT=$(wc -l <stdio-response.txt)
if [ "$LINE_COUNT" -ne 2 ]; then
	echo "$TEST_CASE"
	echo "FAIL: Expected 2 response lines (initialize and tools/list), got $LINE_COUNT"
	echo "Response: $(cat stdio-response.txt)"
	exit 1
fi

# Now extract the tools/list response (second line)
TOOLS_RESPONSE=$(tail -n 1 stdio-response.txt)

# Check against the expected response format for tools/list
readonly EXPECTED_TOOLS_2='{"jsonrpc":"2.0","id":2,"result":{"tools":[]}}'

if [ "$TOOLS_RESPONSE" != "$EXPECTED_TOOLS_2" ]; then
	echo "$TEST_CASE"
	echo "FAIL: tools/list response doesn't match expected"
	echo "Expected: $EXPECTED_TOOLS_2"
	echo "Actual: $TOOLS_RESPONSE"
	exit 1
fi

# Check for responses from the handshake in the debug log
check_log_contains "$debug_log_file" "MCP-BASE64-RESPONSE" "Debug log doesn't contain the Base64 response from emacsclient"
check_log_contains "$debug_log_file" "MCP-RESPONSE" "Debug log doesn't contain the formatted response"

# Verify basic entries that should always exist
check_log_contains "$debug_log_file" "MCP-INFO:.*init function\|No init function specified" "Debug log doesn't contain the init function info"
check_log_contains "$debug_log_file" "MCP-INFO:.*Stopping MCP with function\|No stop function specified" "Debug log doesn't contain the stop function info"

# Verify that call/response cycle works
check_log_contains "$debug_log_file" "MCP-REQUEST:" "Debug log doesn't contain the request"
check_log_contains "$debug_log_file" "MCP-RESPONSE:" "Debug log doesn't contain the response"

if ! grep -q -E '\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\]' "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log doesn't contain timestamps: $debug_log_file"
	exit 1
fi

rm "$debug_log_file"
rm -f "stdio-response.txt"
TESTS_RUN=$((TESTS_RUN + 1))

# Start MCP again after the test
run_emacs_function "mcp-server-lib-start" "Failed to restart MCP"

TEST_CASE="Test case 3: Debug logging without init and stop functions"

# Test without init and stop functions
debug_log_file=$(mktemp /tmp/mcp-debug-$$-XXXXXX.log)

# These are the three messages in the MCP protocol handshake
INIT_REQUEST='{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{"roots":{}},"clientInfo":{"name":"test","version":"1.0"}},"id":1}'
NOTIFICATION='{"jsonrpc":"2.0","method":"notifications/initialized"}'
TOOLS_REQUEST='{"jsonrpc":"2.0","method":"tools/list","id":3}'

# Send all three requests to the script
printf "%s\n%s\n%s\n" "$INIT_REQUEST" "$NOTIFICATION" "$TOOLS_REQUEST" | EMACS_MCP_DEBUG_LOG="$debug_log_file" \
	$STDIO_CMD >stdio-response.txt

if [ ! -f "$debug_log_file" ]; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log file was not created: $debug_log_file"
	exit 1
fi

# Verify all handshake requests were processed
verify_handshake_sequence "$debug_log_file"

check_log_contains "$debug_log_file" "MCP-REQUEST" "Debug log doesn't contain the request"
check_log_contains "$debug_log_file" "MCP-BASE64-RESPONSE" "Debug log doesn't contain the Base64 response from emacsclient"
check_log_contains "$debug_log_file" "MCP-RESPONSE" "Debug log doesn't contain the formatted response"

# Verify we don't see init/stop function calls
check_log_not_contains "$debug_log_file" "MCP-INIT-CALL:" "Debug log contains init function call when it shouldn't"
check_log_not_contains "$debug_log_file" "MCP-STOP-CALL:" "Debug log contains stop function call when it shouldn't"

# Verify info messages about skipping init or logging without init
check_log_contains "$debug_log_file" "MCP-INFO:.*Skipping init function call\|No init function specified" "Debug log doesn't contain the init function skip message"

# Verify info messages about skipping stop or stopping with function
check_log_contains "$debug_log_file" "MCP-INFO:.*Skipping stop function call\|Stopping MCP with function" "Debug log doesn't contain the stop function skip/use message"

if ! grep -q -E '\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\]' "$debug_log_file"; then
	echo "$TEST_CASE"
	echo "FAIL: Debug log doesn't contain timestamps: $debug_log_file"
	exit 1
fi

# Check that we got responses to both initialize and tools/list requests
# We should have two lines in the output, one for each response
LINE_COUNT=$(wc -l <stdio-response.txt)
if [ "$LINE_COUNT" -ne 2 ]; then
	echo "$TEST_CASE"
	echo "FAIL: Expected 2 response lines (initialize and tools/list), got $LINE_COUNT"
	echo "Response: $(cat stdio-response.txt)"
	exit 1
fi

rm "$debug_log_file"
rm -f "stdio-response.txt"
TESTS_RUN=$((TESTS_RUN + 1))

TEST_CASE="Test case 4: Debug logging with invalid path"

INIT_REQUEST='{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{"roots":{}},"clientInfo":{"name":"test","version":"1.0"}},"id":1}'

if echo "$INIT_REQUEST" | EMACS_MCP_DEBUG_LOG="/non-existent-dir/mcp-debug.log" \
	$STDIO_CMD >stdio-response.txt 2>/dev/null; then
	echo "$TEST_CASE"
	echo "FAIL: Script should exit with error when log path is invalid"
	exit 1
fi
rm -f "stdio-response.txt"
TESTS_RUN=$((TESTS_RUN + 1))

TEST_CASE="Test case 5: Special character escaping test"

emacsclient -s "$TEST_SERVER_NAME" -e "
(progn
  (defun mcp-test--quote-string ()
    \"Return a test string with a double quote and newline.\"
    \"\\\"\n\")

  (mcp-server-lib-register-tool #'mcp-test--quote-string
    :id \"test-quote-string\"
    :description \"Returns a test string with special characters\")
)
" >/dev/null

INIT_REQUEST='{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{"roots":{}},"clientInfo":{"name":"test","version":"1.0"}},"id":1}'
NOTIFICATION='{"jsonrpc":"2.0","method":"notifications/initialized"}'
TOOLS_CALL="{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":4,\"params\":{\"name\":\"test-quote-string\"}}"

printf "%s\n%s\n%s\n" "$INIT_REQUEST" "$NOTIFICATION" "$TOOLS_CALL" | $STDIO_CMD >stdio-response.txt

if ! grep -q '"text":"\\"\\n"' stdio-response.txt; then
	echo "$TEST_CASE"
	echo "FAIL: Final response doesn't have properly unescaped quote and newline"
	exit 1
fi
rm -f "stdio-response.txt"
TESTS_RUN=$((TESTS_RUN + 1))

TEST_CASE="Test case 6: Original failing payload test"

emacsclient -s "$TEST_SERVER_NAME" -e "
(progn
  (defun mcp-test--original-payload ()
    \"Return the exact original payload that caused the issue.\"
    \"** aaa bbbbbbbbb ccccccc ddd eeeee ffffff                                :@ggggggggg:\\n   https://hhhhhh.iii/jjjjjjjjjjj/kkkkkkkkk/llllll/387\\n   mmmmmmmmm nnn oooooą pp qqqqq\\n** rr s tt uuuu++ vvvvvv wwww xxxx                                       :@yyyyyyyyy:\\n:zzzzzzz:\\n- aaaaa \\\"bbbb\\\"       cccc \\\"dddd\\\"       [1234-56-78 eee 90:12]\\n- fffff \\\"gggg\\\"       hhhh \\\"iiii\\\"       [1234-56-78 jjj 90:12]\\n- kkkkk \\\"llll\\\"       mmmm              [3456-78-90 nnn 12:34]\\n- nnnnn \\\"oooo\\\"       pppp              [5678-90-12 qqq 34:56]\\nrrrrr: [7890-12-34 sss 56:78]--[9012-34-56 ttt 78:90] =>  1:23\\n:uuu:\\nvvvvvv wwwwww xxxxxx yyyyyyyy zzz ~aaa::bbbbb~\\n\")

  (mcp-server-lib-register-tool #'mcp-test--original-payload
    :id \"test-original-payload\"
    :description \"Returns the exact original payload that caused the issue\")
)
" >/dev/null

INIT_REQUEST='{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{"roots":{}},"clientInfo":{"name":"test","version":"1.0"}},"id":1}'
NOTIFICATION='{"jsonrpc":"2.0","method":"notifications/initialized"}'
TOOLS_CALL="{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"id\":5,\"params\":{\"name\":\"test-original-payload\"}}"

debug_log_file=$(mktemp /tmp/mcp-debug-$$-XXXXXX.log)
printf "%s\n%s\n%s\n" "$INIT_REQUEST" "$NOTIFICATION" "$TOOLS_CALL" |
	EMACS_MCP_DEBUG_LOG="$debug_log_file" $STDIO_CMD >stdio-response.txt 2>/dev/null

# Check for valid content (should have multibyte character)
# and absence of unwanted output (unknown message errors)
check_log_contains "stdio-response.txt" "oooooą pp qqqqq" "Response doesn't contain expected content with multibyte character"
check_log_not_contains "stdio-response.txt" "\*ERROR\*: Unknown message" "Base64 encoding failed to prevent unknown message errors"

rm "$debug_log_file"
rm -f "stdio-response.txt"
TESTS_RUN=$((TESTS_RUN + 1))

TEST_CASE="Test case 7: Multi-server tool isolation via --server-id"

# Setup: Register same tool ID to two different servers with different handlers
emacsclient -s "$TEST_SERVER_NAME" -e "
(progn
  (defun mcp-test--server1-handler ()
    \"Handler for server1\"
    \"result-from-server1\")

  (defun mcp-test--server2-handler ()
    \"Handler for server2\"
    \"result-from-server2\")

  (mcp-server-lib-register-tool
   #'mcp-test--server1-handler
   :id \"multi-server-tool\"
   :description \"Test tool\"
   :server-id \"stdio-server1\")

  (mcp-server-lib-register-tool
   #'mcp-test--server2-handler
   :id \"multi-server-tool\"
   :description \"Test tool\"
   :server-id \"stdio-server2\"))
" >/dev/null

# Requests
INIT='{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}},"id":1}'
NOTIF='{"jsonrpc":"2.0","method":"notifications/initialized"}'
TOOLS_LIST='{"jsonrpc":"2.0","method":"tools/list","id":2}'
CALL='{"jsonrpc":"2.0","method":"tools/call","id":3,"params":{"name":"multi-server-tool"}}'

# Test server1
printf "%s\n%s\n%s\n%s\n" "$INIT" "$NOTIF" "$TOOLS_LIST" "$CALL" |
	$STDIO_CMD --server-id=stdio-server1 >stdio-s1.txt

# Verify tools/list shows exactly 1 tool
TOOLS_RESP=$(sed -n '2p' stdio-s1.txt)
TOOL_COUNT=$(echo "$TOOLS_RESP" | grep -o '"name":"multi-server-tool"' | wc -l)
if [ "$TOOL_COUNT" -ne 1 ]; then
	echo "$TEST_CASE"
	echo "FAIL: Server1 should see exactly 1 tool, saw $TOOL_COUNT"
	cat stdio-s1.txt
	exit 1
fi

# Verify tools/call returns server1's result
if ! sed -n '3p' stdio-s1.txt | grep -q '"text":"result-from-server1"'; then
	echo "$TEST_CASE"
	echo "FAIL: Server1 should return 'result-from-server1'"
	cat stdio-s1.txt
	exit 1
fi

# Test server2
printf "%s\n%s\n%s\n%s\n" "$INIT" "$NOTIF" "$TOOLS_LIST" "$CALL" |
	$STDIO_CMD --server-id=stdio-server2 >stdio-s2.txt

# Verify tools/call returns server2's result
if ! sed -n '3p' stdio-s2.txt | grep -q '"text":"result-from-server2"'; then
	echo "$TEST_CASE"
	echo "FAIL: Server2 should return 'result-from-server2'"
	cat stdio-s2.txt
	exit 1
fi

# Cleanup
emacsclient -s "$TEST_SERVER_NAME" -e "
(progn
  (mcp-server-lib-unregister-tool \"multi-server-tool\" \"stdio-server1\")
  (mcp-server-lib-unregister-tool \"multi-server-tool\" \"stdio-server2\"))
" >/dev/null

rm -f stdio-s1.txt stdio-s2.txt
TESTS_RUN=$((TESTS_RUN + 1))

# Stop the MCP server at the end
run_emacs_function "mcp-server-lib-stop" "Failed to stop MCP at end"

echo "$TESTS_RUN tests run OK!"
exit 0
