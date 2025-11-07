# Workspace Isolation for gptel Requests

## Problem

When gptel makes an async request to an LLM, tool calls may happen minutes later. If the user switches to a different workspace during this time, the tools will execute in the wrong project context, causing errors or incorrect behavior.

## Solution Overview

The workspace is captured when `gptel-request` is initiated and stored in the request's FSM INFO plist. This workspace is then dynamically bound during tool execution, ensuring all workspace-dependent operations use the correct project root regardless of which workspace is currently active.

## Implementation Details

### 1. Variable Definition (helper.el:264-272)

```elisp
(defvar ++gptel-request-workspace nil
  "The workspace that initiated the current gptel request.
Used to maintain workspace context during async LLM operations.")

(defun ++workspace-current-project-root ()
  "Get project root for current workspace or gptel request workspace."
  (or (when ++gptel-request-workspace
        (persp-parameter '++workspace-project-root ++gptel-request-workspace))
      (persp-parameter '++workspace-project-root)
      ++fake-project-root))
```

- `++gptel-request-workspace`: Dynamically bound variable holding the workspace during tool execution
- `++workspace-current-project-root`: Checks `++gptel-request-workspace` first, then falls back to current workspace

### 2. Workspace Capture (llm.el:683-691)

```elisp
(defadvice! +gptel-request-capture-workspace (orig-fn &rest args)
  "Capture current workspace in the request INFO plist."
  :around #'gptel-request
  (let* ((fsm (or (plist-get args :fsm) (gptel-make-fsm)))
         (info (gptel-fsm-info fsm)))
    (plist-put info :workspace (get-current-persp))
    (setf (gptel-fsm-info fsm) info)
    (apply orig-fn (plist-put args :fsm fsm))))
```

Captures the current workspace when `gptel-request` is called and stores it in the FSM INFO plist under the `:workspace` key.

### 3. Tool Execution Binding (llm.el:694-701)

```elisp
(defadvice! +gptel--handle-tool-use-with-workspace (orig-fn fsm)
  "Wrap tool execution to use the captured workspace from FSM's info."
  :around #'gptel--handle-tool-use
  (let* ((info (gptel-fsm-info fsm))
         (++gptel-request-workspace (plist-get info :workspace)))
    (funcall orig-fn fsm)))
```

Binds `++gptel-request-workspace` dynamically when tools are executed in `gptel--handle-tool-use`. This covers:
- Synchronous tools (executed via `apply` at gptel-request.el:1749)
- Asynchronous tools (callbacks receive the workspace binding)

### 4. MCP Server Tool Binding (gptel-tools/utils.el:399-408)

```elisp
(defun +mcp-server-lib--call-gptel-tool (tool args)
  "Call TOOL with ARGS, binding workspace context from request."
  (let ((default-directory (++workspace-current-project-root))
        (++gptel-request-workspace ++gptel-request-workspace)
        (arg-values))
    ...
    (apply (gptel-tool-function tool) arg-values)))
```

Preserves the workspace binding for tools called through MCP server.

## Execution Flow

1. **Request Initiation**: User calls `gptel-send` in workspace A
2. **Workspace Capture**: `+gptel-request-capture-workspace` stores workspace A in INFO plist
3. **User Switches**: User switches to workspace B
4. **Tool Execution**: LLM calls a tool
5. **Workspace Binding**: `+gptel--handle-tool-use-with-workspace` binds workspace A
6. **Tool Runs**: Tool executes with workspace A's project root via `++workspace-current-project-root`
7. **Cleanup**: Binding is released after tool execution

## Benefits

✅ Switch workspaces freely during LLM operations  
✅ Context buffers (lints, git diffs, workspace info) generated from correct workspace  
✅ File operations target the right project root  
✅ No race conditions or mixed workspace state  
✅ Works with both direct gptel tools and MCP server tools  

## Testing

To verify the implementation works:

1. Start a gptel request in workspace A that uses tools
2. Switch to workspace B before the LLM responds
3. Verify tool calls execute in workspace A's project context
4. Check that context buffers reference workspace A's files
