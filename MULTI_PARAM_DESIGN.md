# Design for Multi-Parameter Tool Support in mcp-server-lib

## Problem

Currently, tools can only have:

- No parameters: `(defun my-tool ())`
- One parameter: `(defun my-tool (params))`

We need tools with multiple parameters.

## Proposed Solution

Allow tools to have multiple parameters:

```elisp
(defun org-mcp--tool-update-todo-state (resource-uri current-state new-state)
  "Update the TODO state of a headline.

MCP Parameters:
resource-uri - URI of the headline (org-headline:// or org-id://)
current-state - Current TODO state (empty string for no state)
new-state - New TODO state to set (empty string to remove state)"
  ...)
```

When MCP client calls with:

```json
{
  "resourceUri": "org-headline://file.org/Task",
  "currentState": "TODO",
  "newState": "DONE"
}
```

The library should call:

```elisp
(org-mcp--tool-update-todo-state
  "org-headline://file.org/Task"
  "TODO"
  "DONE")
```

## Benefits

- Simpler function signatures
- Direct parameter access (no `alist-get`)
- Natural Elisp style
- Each parameter gets its own documentation line

## Examples

### Current (Single Parameter)

```elisp
(defun org-mcp--tool-update-todo-state (params)
  "Update TODO state.

MCP Parameters:
params - An alist containing resourceUri, currentState, and newState"
  (let ((resource-uri (alist-get 'resourceUri params))
        (current-state (alist-get 'currentState params))
        (new-state (alist-get 'newState params)))
    ;; implementation
    ))
```

### Proposed (Multiple Parameters)

```elisp
(defun org-mcp--tool-update-todo-state (resource-uri current-state new-state)
  "Update TODO state.

MCP Parameters:
resource-uri - URI of the headline
current-state - Current TODO state
new-state - New TODO state"
  ;; Direct use of parameters, no extraction needed
  )
```

## Implementation Notes

The `mcp-server-lib` would need to:

1. Accept functions with multiple parameters
1. Map JSON object fields to function parameters by name
1. Generate schema with multiple properties from the parameter list
