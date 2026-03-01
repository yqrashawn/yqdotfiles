# Workspace Isolation for Parallel LLM Requests

## Problem

Multiple gptel LLM queries run in multiple gptel buffers across multiple workspaces in parallel. Each LLM query may call **gptel tools** or **MCP tools**. These tools need to know the correct workspace (project root) for the request that initiated them — not whichever workspace happens to be active when the tool executes.

## Two Tool Execution Paths

### Path A: gptel tools

LLM tells gptel to run the tool. gptel executes the tool function directly in Emacs.

### Path B: MCP tools

LLM tells mcp-server-lib to run the tool. The tool is registered with `mcp-server-lib` and called via HTTP JSON-RPC. gptel tools are also registered as MCP tools via `mcp-server-lib-register-gptel-tool` so external MCP clients (e.g., Claude Code CLI) can call them too.

## Key Variables and Functions

| Symbol | Location | Purpose |
|--------|----------|---------|
| `++gptel-request-workspace` | helper.el:304 | Dynamic var: the persp workspace that initiated the gptel request |
| `++workspace-current-project-root` | helper.el:308 | Resolves project root, checking all sources in priority order |
| `mcp-server-lib--request-cwd` | mcp-server-lib.el:178 | Dynamic var: CWD from the MCP HTTP request |
| `mcp-server-lib--request-session-id` | mcp-server-lib.el:172 | Dynamic var: session ID from the MCP HTTP request |
| `mcp-server-lib-default-directory-function` | mcp-server-lib.el:155 | Customizable function to resolve `default-directory` for MCP tool calls |
| `+mcp-server-lib-default-directory-function` | llm.el:829 | Our custom implementation of the above |
| `gptel-claude-code--team-cwd` | gptel-claude-code-team.el:53 | Buffer-local var: CWD for a Claude Code process session |
| `+gptel--add-workspace-context` | gptel-extra.el:14 | Sends workspace metadata to LLM in request params |

## Implementation Details

### 1. Workspace Metadata for LLM (gptel-extra.el:14)

```elisp
(defun +gptel--add-workspace-context ()
  (when gptel-mode
    (setq-local gptel--request-params
      (list :metadata
        (list :workspace_root (++workspace-current-project-root)
              :working_dir default-directory
              :project_name ...)))))
```

Called before `gptel-send` (via advice). Tells the LLM what workspace it's operating in by including `workspace_root`, `working_dir`, and `project_name` in request metadata.

### 2. Project Root Resolution (helper.el:308)

```elisp
(defun ++workspace-current-project-root ()
  (or
   mcp-server-lib--request-cwd              ; 1. MCP request CWD (during MCP tool execution)
   (when ++gptel-request-workspace          ; 2. Captured workspace (during gptel tool execution)
     (persp-parameter '++workspace-project-root ++gptel-request-workspace))
   (persp-parameter '++workspace-project-root) ; 3. Current workspace (interactive use)
   ++fake-project-root))                       ; 4. Fallback
```

This is the central function everything converges on. Priority:
1. **MCP request CWD** — bound by mcp-server-lib HTTP handler during MCP tool calls
2. **Captured workspace** — bound by gptel advice during gptel tool calls
3. **Current workspace** — for interactive (non-async) use
4. **Fallback** — fake project root for buffers outside any workspace

### 3. gptel Path: Workspace Capture and Binding

#### 3a. Capture at Request Time (llm.el:1205)

```elisp
(defadvice! +gptel-request-capture-workspace (orig-fn &rest args)
  :around #'gptel-request
  (let* ((result (apply orig-fn args))
         (info (gptel-fsm-info result)))
    (plist-put info :workspace (get-current-persp))
    result))
```

When `gptel-request` is called, the current persp workspace is stored in the FSM INFO plist under `:workspace`.

#### 3b. Bind During Tool Execution (llm.el:1215)

```elisp
(defadvice! +gptel--handle-tool-use-with-workspace (orig-fn fsm)
  :around #'gptel--handle-tool-use
  (let* ((info (gptel-fsm-info fsm))
         (++gptel-request-workspace (plist-get info :workspace)))
    (funcall orig-fn fsm)))
```

When the LLM responds with a tool call, this advice dynamically binds `++gptel-request-workspace` to the captured workspace before executing the tool. All calls to `++workspace-current-project-root` within the tool will see the correct workspace.

### 4. MCP Path: CWD and Session-based Resolution

#### 4a. HTTP Transport Binds Dynamic Vars (mcp-server-lib-http.el:108)

```elisp
(defun mcp-server-lib-http--handle-jsonrpc-request (proc request &optional session-id cwd)
  ...
  (let ((mcp-server-lib--request-session-id sid)
        (mcp-server-lib--request-cwd dir))
    ...))
```

The HTTP handler binds `mcp-server-lib--request-session-id` and `mcp-server-lib--request-cwd` from the request.

#### 4b. Tool Execution Resolves `default-directory` (mcp-server-lib.el:904)

```elisp
(defun mcp-server-lib--handle-tools-call-apply (id tool args-vals method-metrics)
  (let* ((default-directory
           (or (and mcp-server-lib-default-directory-function
                 (funcall mcp-server-lib-default-directory-function
                   mcp-server-lib--request-session-id))
               mcp-server-lib--request-cwd
               default-directory))
         (mcp-server-lib--request-cwd default-directory))
    ...))
```

Before running the tool handler, mcp-server-lib resolves `default-directory`:
1. Try `mcp-server-lib-default-directory-function` (our custom function)
2. Fall back to `mcp-server-lib--request-cwd`
3. Fall back to `default-directory`

Note: after resolution, `mcp-server-lib--request-cwd` is rebound to the resolved `default-directory`, so `++workspace-current-project-root` sees it.

#### 4c. Our Custom Directory Function (llm.el:829)

```elisp
(setq! mcp-server-lib-default-directory-function
  (defun +mcp-server-lib-default-directory-function (session-id)
    (or
     mcp-server-lib--request-cwd
     (when (fboundp 'gptel-claude-code--mcp-default-directory)
       (gptel-claude-code--mcp-default-directory session-id))
     (when (fboundp '++workspace-current-project-root)
       (++workspace-current-project-root)))))
```

Priority within this function:
1. **`mcp-server-lib--request-cwd`** — explicit CWD from the HTTP request
2. **`gptel-claude-code--mcp-default-directory`** — looks up `gptel-claude-code--team-cwd` from the session buffer (for Claude Code CLI sessions)
3. **`++workspace-current-project-root`** — fallback to workspace-based resolution

#### 4d. Claude Code Session CWD (gptel-claude-code-mcp.el:290)

```elisp
(defun gptel-claude-code--mcp-default-directory (session-id)
  (when session-id
    (gptel-claude-code--session-cwd session-id)))

(defun gptel-claude-code--session-cwd (session-id)
  (when-let* ((buf (gptel-claude-code--session-buffer session-id)))
    (when (buffer-live-p buf)
      (buffer-local-value 'gptel-claude-code--team-cwd buf))))
```

For Claude Code processes, each session buffer stores its CWD in `gptel-claude-code--team-cwd`. The session-id from the MCP request maps to the buffer, and the CWD is retrieved.

### 5. Dual Registration of Tools (gptel-tools/utils.el)

```elisp
(defun gptelt-make-tool (&rest args)
  (let ((tool (apply #'gptel-make-tool ...)))
    (mcp-server-lib-register-gptel-tool tool)  ; Register as MCP tool too
    tool))
```

Each gptel tool is also registered with mcp-server-lib via `mcp-server-lib-register-gptel-tool`. This means the same tool function can be called either way — via gptel tool use or via MCP protocol — and workspace isolation works through both paths.

## Execution Flows

### Flow A: gptel Tool Call

```
User in Workspace A → gptel-send
  → +gptel-request-capture-workspace stores Workspace A in FSM
  → LLM responds with tool_use
  → +gptel--handle-tool-use-with-workspace binds ++gptel-request-workspace = Workspace A
    → Tool calls ++workspace-current-project-root
      → Returns Workspace A's project root (via priority #2)
```

### Flow B: MCP Tool Call from External Client (e.g., Claude Code CLI)

```
Claude Code CLI → HTTP POST /mcp/v1/sessions/{session-id}/messages
  → mcp-server-lib-http binds mcp-server-lib--request-session-id, mcp-server-lib--request-cwd
  → mcp-server-lib--handle-tools-call-apply
    → +mcp-server-lib-default-directory-function(session-id)
      → Returns mcp-server-lib--request-cwd (if provided by client)
      → OR gptel-claude-code--session-cwd(session-id) → buffer-local gptel-claude-code--team-cwd
      → OR ++workspace-current-project-root (fallback)
    → default-directory is set, mcp-server-lib--request-cwd rebound
    → Tool handler runs with correct default-directory
      → If tool calls ++workspace-current-project-root, sees mcp-server-lib--request-cwd (priority #1)
```

### Flow C: MCP Tool Call from gptel (via mcp-server-lib-register-gptel-tool)

Same as Flow B, but the MCP request originates from gptel routing to mcp-server-lib internally. The workspace context flows through `mcp-server-lib--request-cwd` which is set during the HTTP handling.

## Benefits

- Switch workspaces freely during LLM operations
- Context buffers (lints, git diffs, workspace info) generated from correct workspace
- File operations target the right project root
- No race conditions or mixed workspace state
- Works with gptel tools, MCP tools, and Claude Code CLI sessions
- Same tool function works correctly regardless of call path

## Testing

To verify workspace isolation:

1. Start a gptel request in workspace A that uses tools
2. Switch to workspace B before the LLM responds
3. Verify tool calls execute in workspace A's project context
4. Check that context buffers reference workspace A's files

To verify MCP isolation:

1. Run two Claude Code CLI sessions targeting different projects
2. Both call MCP tools simultaneously
3. Verify each tool call uses the correct project's `default-directory`
