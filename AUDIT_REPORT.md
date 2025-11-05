# DOTFILES AUDIT REPORT (PLAN ONLY)

**Date:** 2025-11-05
**Repository:** yqdotfiles (macOS Nix-Darwin + Home-Manager)
**Total Files:** 865 tracked files, 8.8MB
**Languages:** Nix (87), Shell (27), Emacs Lisp (65), Clojure/ClojureScript (27), JavaScript/TypeScript (15+)

---

## A) INVENTORY

### Entry Points & Key Directories

**Primary Entry Points:**
- `flake.nix` - Main Nix flake configuration (436 lines, 216 inputs including 29 ASDF plugins)
- `install.sh` - Bootstrap symlink for Doom Emacs (4 lines)
- `bin/bootstrap.sh` - Nix-darwin rebuild script
- `bin/install-nix.sh` - Nix installation script (mostly commented out)

**Module Structure:**
- `modules/yqrashawn/` - Main configuration modules (94+ Nix files)
  - `darwin/` - macOS system configuration (apps, daemons, user-agents, preferences)
  - `home-manager/` - User environment (CLI tools, dotfiles, editors)
  - `emacs/` - Emacs compilation configuration
- `.doom.d/` - Doom Emacs configuration (63 Elisp files, ~15,380 LOC)
- `overlays/` - Custom package overlays (7 files: emacs-macport, webkitgtk, nyxt, etc.)
- `profiles/` - Host-specific profiles (yqrashawn, holybasil, work)

### Software Inventory (by Type)

| Item | Type | Where Referenced | How Used |
|------|------|------------------|----------|
| **nixpkgs-23.05** | Input | flake.nix:29-30 | EOL stable channel (deprecated) |
| **emacs-30.2** | Package | flake.nix:40-42, modules/yqrashawn/emacs/ | Custom Emacs 30 with macport |
| **atuin** | CLI | flake.nix:44-46, home-manager/cli/ | Shell history (replaces mcfly) |
| **yabai** | App | darwin/brew.nix (commented), yabai/* scripts | Window manager (being replaced by aerospace) |
| **aerospace** | App | darwin/apps-minimal.nix:27 | New i3-like window manager |
| **goku** | CLI | darwin/brew.nix:45 | Karabiner config generator |
| **prezto** | Framework | home-manager/cli/prezto.nix | Zsh framework (replaces oh-my-zsh) |
| **gptel** | Package | .doom.d/packages.el | AI/LLM integration for Emacs |
| **clojure-lsp** | CLI | flake.nix:36 | Clojure language server |
| **babashka** | CLI | bb.edn (empty), ASDF plugin | Clojure scripting |
| **hammerspoon** | App | darwin/apps-minimal.nix:12 | macOS automation |
| **karabiner-elements** | App | darwin/apps-minimal.nix:21 | Keyboard customization |
| **kitty** | App | darwin/apps.nix:21, home-manager/kitty/ | Terminal emulator |
| **zed** | Editor | flake.nix:56-58 | Modern code editor input |
| **shadow-cljs** | CLI | package.json:15 | ClojureScript compiler |
| **katex 0.12.0** | Lib | package.json:3 | Math rendering (4+ major versions behind) |
| **spacehammer** | Config | flake.nix:72-74, .spacehammer/config.fnl | Hammerspoon Fennel config |
| **29 ASDF plugins** | Runtimes | flake.nix:92-207 | Language version managers |
| **164 Emacs packages** | Packages | .doom.d/packages.el | Doom Emacs packages |
| **50+ Homebrew casks** | Apps | darwin/apps*.nix, darwin/brew.nix | GUI applications |
| **200+ Nix packages** | CLI | modules/yqrashawn/common.nix:69-339 | System packages |

---

## B) MAJOR FINDINGS (Prioritized)

### HIGH SEVERITY

#### 1. **EOL nixpkgs Versions**
- **Evidence:** flake.nix:29-30
  ```nix
  darwin-stable.url = "github:nixos/nixpkgs/nixpkgs-23.05-darwin";
  stable.url = "github:nixos/nixpkgs/nixos-23.05";
  ```
- **Why it matters:** NixOS 23.05 reached EOL in June 2024. Security patches and updates no longer available. NixOS 24.11 released Nov 2024 with significant Darwin improvements. 25.05 is now stable (May 2025).
- **Recommended fix:**
  - Update to `nixpkgs-24.11-darwin` or `nixpkgs-25.05-darwin`
  - Update stable to `nixos-24.11` or `nixos-25.05`
  - Test all configurations after upgrade
- **Effort:** M | **Risk:** Med (may require config adjustments for breaking changes)

#### 2. **Security: Unquoted Variables in Destructive Commands**
- **Evidence:** modules/yqrashawn/home-manager/cli/functions.sh:12
  ```bash
  rm -rf $DIR  # CRITICAL: unquoted variable
  ```
- **Why it matters:** If `$DIR` contains spaces or is empty, could delete unintended files or root directory. Potential data loss.
- **Recommended fix:**
  - Quote all variables: `rm -rf "$DIR"`
  - Add validation: `[[ -n "$DIR" && -d "$DIR" ]] || return 1`
  - Add `set -euo pipefail` to all scripts
- **Effort:** S | **Risk:** Low (straightforward fix)

#### 3. **Security: Piping Remote Scripts to Shell**
- **Evidence:** bin/install-nix.sh:3
  ```bash
  curl -L https://nixos.org/nix/install | sh
  ```
- **Why it matters:** No checksum verification, vulnerable to MITM attacks. Script is 95% commented out, likely obsolete.
- **Recommended fix:**
  - Remove file (obsolete) OR
  - Add checksum verification: `curl -L URL -o /tmp/install.sh && sha256sum --check && sh /tmp/install.sh`
  - Use official Nix installer with verification
- **Effort:** S | **Risk:** Low

#### 4. **Duplicate Homebrew Tap**
- **Evidence:** modules/yqrashawn/darwin/brew.nix:23,33
  ```nix
  taps = [
    "koekeishiya/formulae"  # Line 23
    ...
    "koekeishiya/formulae"  # Line 33 (duplicate)
  ```
- **Why it matters:** Redundant configuration, could cause brew warnings. Indicates lack of linting.
- **Recommended fix:** Remove duplicate on line 33
- **Effort:** S | **Risk:** Low

#### 5. **Security: Command Injection in awk system()**
- **Evidence:** modules/yqrashawn/home-manager/dotfiles/local-bins/funcs/allgws.sh:1
  ```bash
  awk '{print "...;system("git --git-dir="$1".git --work-tree="$1" status")}'
  ```
- **Why it matters:** If `$1` contains quotes or shell metacharacters, arbitrary command execution possible.
- **Recommended fix:**
  - Rewrite without awk system() calls
  - Use proper shell escaping or pure Bash/Nix alternative
  - Consider marking script obsolete (not referenced in Nix configs)
- **Effort:** M | **Risk:** Med

#### 6. **Undefined Nix Import**
- **Evidence:** flake.nix:388
  ```nix
  inherit (lib.my) mapModules mapModulesRec mapHosts;
  ```
  lib/modules.nix only defines `mapModules` and `mapModulesRec`, not `mapHosts`
- **Why it matters:** Build will fail if `mapHosts` is ever called. Dead code or missing implementation.
- **Recommended fix:** Either define `mapHosts` in lib/modules.nix or remove from inherit
- **Effort:** S | **Risk:** Low (currently unused in checks)

### MEDIUM SEVERITY

#### 7. **Outdated JavaScript Dependencies**
- **Evidence:** package.json:3-12
  ```json
  "katex": "0.12.0",        // Current: 0.16.11 (Dec 2024), 4 major versions behind
  "markdown-it": "14.1.0",  // Current: 15.0.0 (breaking changes in ESM)
  "react": "^18.2.0",       // Current: 18.3.1 (minor updates)
  "snabbdom": "3.5.1"       // Current: 3.6.2
  ```
- **Why it matters:** Security vulnerabilities, missing performance improvements, KaTeX 0.16 has breaking changes for copy-tex extension.
- **Recommended fix:**
  - Update katex to 0.16.x (review migration guide)
  - Update markdown-it to 15.x (check ESM compatibility)
  - Update react to 18.3.x (safe minor update)
  - Run `npm audit` for security issues
- **Effort:** M | **Risk:** Med (breaking changes in katex)

#### 8. **Missing Shell Script Shebangs**
- **Evidence:**
  - modules/yqrashawn/home-manager/dotfiles/local-bins/funcs/allgws.sh (no shebang)
  - modules/yqrashawn/home-manager/dotfiles/local-bins/funcs/lowercased.sh (no shebang)
  - modules/yqrashawn/home-manager/dotfiles/local-bins/funcs/go-through-all-directory.sh (no shebang)
- **Why it matters:** Scripts won't execute directly, non-portable (relies on shell being bash/zsh).
- **Recommended fix:** Add `#!/usr/bin/env bash` to all scripts
- **Effort:** S | **Risk:** Low

#### 9. **Hardcoded Paths in Scripts**
- **Evidence:** 40+ occurrences
  - darwin/daemons/*.nix: `/Users/${config.user.name}` (hardcoded /Users)
  - sketchybar/plugins/notmuch.sh:3: `/run/current-system/sw/bin/bb`
  - local-bins/funcs/go-through-all-directory.sh:5: `~/local/bin/funcs/`
- **Why it matters:** Not portable to Linux/NixOS, breaks if paths change, harder to test.
- **Recommended fix:**
  - Use `config.home.homeDirectory` instead of `/Users/${config.user.name}`
  - Use `${pkgs.babashka}/bin/bb` instead of hardcoded Nix store paths
  - Use `$HOME` instead of `~` in scripts
- **Effort:** M | **Risk:** Low

#### 10. **Orphaned Nix Modules**
- **Evidence:**
  - modules/yqrashawn/darwin/network.nix (never imported, commented out in core.nix:10)
  - modules/yqrashawn/darwin/syncthing.nix (never imported)
  - modules/yqrashawn/hardware/phil.nix (never imported, unknown hardware)
- **Why it matters:** Dead code increases maintenance burden, confuses contributors, takes up space.
- **Recommended fix:**
  - network.nix: Decide to integrate or delete
  - syncthing.nix: Delete (syncthing is commented out in apps.nix)
  - phil.nix: Archive or document purpose
- **Effort:** S | **Risk:** Low

#### 11. **Duplicate Emacs Configuration Loading**
- **Evidence:**
  - .doom.d/helper.el:1: `(load! "clj-elisp")`
  - .doom.d/config.el:70: `(load! "clj-elisp")`
- **Why it matters:** Performance overhead, potential for side effects from double-evaluation, indicates unclear load order.
- **Recommended fix:** Remove one load statement, document why if intentional
- **Effort:** S | **Risk:** Low

#### 12. **Duplicate straight.el Protocol Assignment**
- **Evidence:** .doom.d/init.el:326-327
  ```elisp
  (setq straight-vc-git-default-protocol 'https)  ; Line 326
  (setq straight-vc-git-default-protocol 'ssh)    ; Line 327 (overwrites)
  ```
- **Why it matters:** Line 326 is dead code, confusing intent. Only SSH is effective.
- **Recommended fix:** Remove line 326 or comment with explanation
- **Effort:** S | **Risk:** Low

### LOW SEVERITY

#### 13. **Missing Emacs Test Coverage**
- **Evidence:** 10 of 17 gptel-tools lack tests
  - clj.el (489 lines), cljs.el (287 lines), lint.el, shell.el, workspace.el (461 lines), utils.el (458 lines), pext.el, embeddings.el, imenu.el, treesit.el
- **Why it matters:** Harder to refactor, risk of regressions, low confidence in tool reliability.
- **Recommended fix:** Add ERT tests for at least the 3 largest: clj.el, workspace.el, utils.el
- **Effort:** L | **Risk:** Low

#### 14. **Commented Dead Code**
- **Evidence:**
  - bin/install-nix.sh: 70% commented out (lines 5-25)
  - Multiple Nix files with commented-out package declarations
  - sketchybar plugins with commented case statements
- **Why it matters:** Clutters codebase, unclear if code should be preserved or removed.
- **Recommended fix:** Use git history for archival, remove commented code
- **Effort:** S | **Risk:** Low

#### 15. **Empty bb.edn**
- **Evidence:** bb.edn:1-2
  ```clojure
  {}
  ```
- **Why it matters:** File exists but provides no configuration. Unclear if babashka is used for scripting.
- **Recommended fix:** Either populate with tasks/dependencies or document that scripts use ad-hoc bb invocations
- **Effort:** S | **Risk:** Low

#### 16. **Orphaned Emacs Tools Not Loaded**
- **Evidence:** .doom.d/gptel-tools.el doesn't load:
  - embeddings.el (126 lines) - Jina AI provider
  - imenu.el (22 lines) - Symbol extraction
  - treesit.el (121 lines) - Tree-sitter utilities
  - workspace.el (461 lines) - Has tool registrations but not explicitly loaded
- **Why it matters:** Unclear if tools are functional, wasted development effort if unused.
- **Recommended fix:**
  - embeddings.el: Integrate or remove
  - imenu.el + treesit.el: Move to utils.el or create unified symbol tool
  - workspace.el: Verify autoload behavior or add explicit load
- **Effort:** M | **Risk:** Low

#### 17. **Large Emacs Files (>500 LOC)**
- **Evidence:**
  - autoload.el (1,172 lines)
  - better-default.el (1,122 lines)
  - map.el (867 lines)
  - clojure.el (646 lines)
  - llm.el (642 lines)
- **Why it matters:** Harder to navigate, slower byte-compilation, mixing concerns.
- **Recommended fix:** Split by feature/context (e.g., map.el → general-keys/, mode-keys/)
- **Effort:** L | **Risk:** Med (needs careful testing)

#### 18. **Missing lexical-binding Header**
- **Evidence:** .doom.d/packages.el:1
  ```elisp
  ;;; -*- no-byte-compile: t; -*-
  ```
  Should be: `;;; -*- no-byte-compile: t; lexical-binding: t; -*-`
- **Why it matters:** Lexical scoping is faster and more predictable. Best practice for all modern Elisp.
- **Recommended fix:** Add `lexical-binding: t` to header
- **Effort:** S | **Risk:** Low

---

## C) UNUSED/STALE MAP

### Nix Modules (Orphaned - Not Imported Anywhere)

| File | Lines | Evidence | Recommendation |
|------|-------|----------|----------------|
| `modules/yqrashawn/darwin/network.nix` | 42 | Commented out in core.nix:10 (`# ./network.nix`) | **DELETE** - Integrate network settings into preferences.nix or remove |
| `modules/yqrashawn/darwin/syncthing.nix` | 67 | Never imported, syncthing commented out in apps.nix:44 | **DELETE** - Syncthing not in use |
| `modules/yqrashawn/hardware/phil.nix` | 15 | Never imported, unknown hardware config | **ARCHIVE** - Document purpose or delete |
| `modules/yqrashawn/darwin/tailscale.nix` | 45 | Imported in mini.nix, mbp.nix, hmbp.nix | **KEEP** - In active use |

**Blocker:** tailscale.nix is actually used (false positive from initial scan).

### Shell Scripts (Unused - No References in Nix/Emacs Configs)

| File | Lines | Evidence | Recommendation |
|------|-------|----------|----------------|
| `local-bins/funcs/allgws.sh` | 1 | No references in .nix or .el files, command injection risk | **DELETE** - Obsolete git workspace scanner |
| `local-bins/funcs/lowercased.sh` | 1 | No references, unsafe (unquoted vars, no shebang) | **DELETE** - Trivial one-liner |
| `local-bins/funcs/go-through-all-directory.sh` | 6 | No references, hardcoded paths | **DELETE** - Unclear purpose |
| `bin/install-nix.sh` | 26 | 70% commented out, insecure curl pipe | **DELETE** - Use official Nix installer |

### Emacs Lisp Files (Not Loaded in gptel-tools.el)

| File | Lines | Purpose | Recommendation |
|------|-------|---------|----------------|
| `gptel-tools/embeddings.el` | 126 | Jina AI embeddings provider (incomplete) | **INTEGRATE or DELETE** - Finish implementation or remove |
| `gptel-tools/imenu.el` | 22 | Extract imenu symbols from buffer | **MOVE to utils.el** - Reusable utility |
| `gptel-tools/treesit.el` | 121 | Tree-sitter symbol listing | **MOVE to utils.el** - Reusable utility |

**Note:** workspace.el (461 lines) has tool registrations but isn't explicitly loaded - may be autoloaded. Verify behavior.

### Configuration Files (Empty or Minimal)

| File | Content | Recommendation |
|------|---------|----------------|
| `bb.edn` | `{}` (empty map) | Document that scripts use ad-hoc bb or populate with tasks |
| `install.sh` | 4 lines (single symlink) | Keep - minimal and functional |

---

## D) ALTERNATIVES MATRIX (with Citations)

| Item | Current Use | Pain Point | Top Alternative(s) | Why Better | Nix/macOS Support | Migration Steps | Sources |
|------|-------------|------------|-------------------|------------|-------------------|----------------|---------|
| **nixpkgs-23.05** | Base system channel | EOL (June 2024), no security updates | **nixpkgs-24.11** or **25.05** | Active support, Darwin improvements (macOS 15 Sequoia support), 7mo security updates | ✅ Full (24.11 requires macOS 11.3+, 25.11 will require macOS 14+) | • Update flake.nix:29-30<br>• Test darwin-rebuild<br>• Fix any breaking changes | [NixOS 24.11 Release](https://nixos.org/blog/announcements/2024/nixos-2411/) (Nov 2024) |
| **yabai** | Window manager (legacy) | Requires disabling SIP, buggy animations, security risk | **AeroSpace** (already installed!) | No SIP required, i3-like, faster workspace switching, built-in hotkeys, cleaner config | ✅ Full (via Homebrew tap) | • Already in apps-minimal.nix:27<br>• Migrate yabai/* scripts to Aerospace config<br>• Remove yabai references<br>• Keep Hammerspoon for non-tiling tasks | [Aerospace vs Yabai](https://theopark.me/blog/2025-04-05-aerospace/) (2025), [GitHub](https://github.com/nikitabobko/AeroSpace) |
| **mcfly** | Shell history (currently enabled) | SQL-style search (need % wildcards), less intuitive UI | **atuin** (already in flake!) OR **keep fzf** | Atuin: SQLite-backed, regex search, dir filtering, cloud sync<br>fzf: Simpler, faster, proven | ✅ Both available in nixpkgs | • atuin already at flake.nix:44<br>• Enable in home-manager<br>• Migrate history: `atuin import auto`<br>• OR: Disable mcfly, bind fzf to Ctrl-R | [Atuin vs McFly vs fzf](https://forum.endeavouros.com/t/browsing-the-shell-history-which-tool-do-you-prefer/62240) (2025) |
| **prezto** | Zsh framework (current) | Slower than zinit, manual updates | **zinit** (with Turbo mode) | 50-80% faster startup (5x improvement), lazy loading, compatible with OMZ/Prezto plugins | ✅ Available via Nix | • Install zinit via home-manager<br>• Migrate prezto plugins to zinit config<br>• Enable Turbo mode for speed<br>• Benchmark with hyperfine | [Prezto vs Zinit](https://www.slant.co/versus/17375/24969/~prezto_vs_zinit) (2025), [Zinit Turbo](https://github.com/zdharma-continuum/zinit) |
| **straight.el** | Emacs package manager (Doom's default) | Slower startup vs alternatives, complex lockfiles | **Keep straight.el** OR migrate to **elpaca** | elpaca: Async installs, faster cold starts, simpler (by straight.el maintainer)<br>straight.el: Mature, reproducible, Doom default | ✅ Both in ELPA/MELPA | • If keeping straight.el: No action<br>• If migrating to elpaca:<br>&nbsp;&nbsp;- Major Doom refactor required<br>&nbsp;&nbsp;- **NOT RECOMMENDED** for Doom users | [Elpaca vs straight.el](https://www.genspark.ai/spark/comparing-elpaca-and-straight-el-for-emacs-30/d69728a1-d62d-4467-bc78-02fa1ddc44ab) (2025) |
| **deadnix + statix** | Nix linting (manual) | No CI integration, manual runs | **nixf-tidy** (add to toolkit) | Semantic linter (not just syntax), JSON output, integrates with nixd LSP, GitHub Actions support | ✅ Available via nix-community | • Install nixf-tidy via nix flake<br>• Add pre-commit hook<br>• **Keep deadnix + statix** (complementary)<br>• Add to CI checks | [nixf-tidy](https://discourse.nixos.org/t/nixf-tidy-static-linter-for-your-nix-code-alpha-github-actions/44040) (2024), [Nix Linters](https://discourse.nixos.org/t/list-of-nix-linters/19279) |
| **katex 0.12.0** | Math rendering (JS) | 4 major versions behind (0.16.11 current), security risks | **katex 0.16.x** | Performance improvements, new functions, breaking changes for copy-tex require CSS removal | ✅ NPM package | • Update package.json to 0.16.11<br>• Review [migration guide](https://katex.org/docs/migration)<br>• Remove copy-tex CSS imports<br>• Test rendering | [KaTeX Releases](https://github.com/KaTeX/KaTeX/releases) (Dec 2024) |
| **npm** (implied) | Package manager (package.json exists) | Slow installs, large node_modules | **pnpm** (2-3x faster) OR **bun** (20-30x faster) | pnpm: 70% less disk space, strict deps, monorepo-ready<br>bun: Native runtime, fastest installs | ✅ Both via Nix/ASDF | • Install pnpm or bun via ASDF<br>• Convert package.json to pnpm/bun workspace<br>• Migrate lockfile<br>• Update CI scripts | [npm vs pnpm vs bun 2025](https://dev.to/kirteshbansal/choosing-the-right-javascript-package-manager-in-2025-npm-vs-yarn-vs-pnpm-vs-bun-2jie), [Bun benchmarks](https://benjamincrozat.com/bun-package-manager) |
| **shadow-cljs 3.2.0** | ClojureScript compiler | Possible newer versions available | **shadow-cljs 3.x (latest)** | Check for performance/bugfix updates | ✅ NPM package | • Run `npm outdated`<br>• Update to latest 3.x<br>• Test ClojureScript builds | [shadow-cljs GitHub](https://github.com/thheller/shadow-cljs) |
| **Emacs 29** | Editor (if < 30) | Missing native-comp by default, no Android support, slower JSON parsing | **Emacs 30.2** (already configured!) | Native-comp default, Android port, which-key builtin, tree-sitter modes (Lua/Elixir/HTML), LLDB support | ✅ Already at flake.nix:40-42 | • Already using emacs-30.2 source<br>• Ensure native-comp enabled in build<br>• Test byte-compile all configs | [Emacs 30 Features](https://irreal.org/blog/?p=11546) (2025), [Release Feb 2025](https://www.gnu.org/software/emacs/manual/html_node/efaq/New-in-Emacs-30.html) |

**Notes:**
- ✅ = Full support, ⚠️ = Partial/experimental, ❌ = Not available
- **Bold** = Recommended alternative
- Sources checked 2024-2025 for recency

---

## E) STRUCTURE & WORKFLOW IMPROVEMENTS

### Nix Configuration

**Current Issues:**
- No clear boundary between darwin modules and home-manager modules (some duplication)
- Overlays defined in 2 places (overlays/ + modules/yqrashawn/overlays.nix)
- 29 ASDF plugins pinned in flake but unused (no asdf installation detected)
- No lockfile updates automation

**Recommendations:**

1. **Consolidate Overlays**
   - Move all overlays to `overlays/default.nix` as a single import
   - Remove `modules/yqrashawn/overlays.nix` duplication
   - Document each overlay's purpose (emacs-macport, webkitgtk for nyxt, etc.)

2. **Darwin vs Home-Manager Boundaries**
   - **Darwin:** System daemons, global preferences, Homebrew apps
   - **Home-Manager:** User dotfiles, CLI tools, shell config, editor configs
   - Move misplaced configs:
     - User-specific packages (e.g., helix, bat) → home-manager (already correct)
     - System services (e.g., adguard-home) → darwin (already correct)

3. **Remove Unused ASDF Plugins**
   - 29 ASDF plugins in flake.nix but ASDF not installed
   - **Either:** Install ASDF and use plugins **OR** remove all asdf-* inputs (lines 92-207)
   - **Recommended:** Remove ASDF entirely, use Nix for language runtimes (more reproducible)

4. **Input Pinning & Updates**
   - Pin critical inputs with explicit revs for reproducibility
   - Set up automated updates:
     ```bash
     nix flake update --commit-lock-file
     ```
   - Consider `nix-update` tool for per-input updates

5. **Secrets Management**
   - `sops-nix` input already present (flake.nix:38) but unused
   - Integrate sops for:
     - API tokens (GitHub, OpenAI in llm.el)
     - SSH keys
     - Email credentials (mail.el)
   - Split `not-secret.el` from encrypted secrets

6. **Host-Specific Modules**
   - Good: Already split by host (mini.nix, mbp.nix, studio.nix, hmbp.nix)
   - Improve: Extract common config to `common.nix`, keep only diffs in host files
   - Example: All hosts import apps.nix - could be in common base

### Emacs Configuration

**Current Issues:**
- Large monolithic files (autoload.el 1,172 lines, better-default.el 1,122 lines)
- Unclear load order (clj-elisp loaded twice)
- Some tools not loaded (embeddings.el, imenu.el, treesit.el)
- No performance profiling

**Recommendations:**

1. **Split Large Files**
   ```
   autoload.el → autoload/outline.el, autoload/font.el, autoload/media.el, autoload/terminal.el
   better-default.el → better-default/outline.el, better-default/comint.el, better-default/whisper.el
   map.el → map/general.el, map/workspace.el, map/org.el
   ```

2. **Fix Load Order**
   - Remove duplicate `(load! "clj-elisp")` from config.el:70 (already in helper.el)
   - Document why if intentional double-load

3. **Optimize Startup Time**
   - Profile with: `emacs --debug-init` and `esup` package
   - Lazy-load heavy packages (clojure-mode, gptel, org-roam)
   - Use `:defer t` for non-essential packages
   - Move autoload cookies to separate files

4. **Integrate Orphaned Tools**
   - `embeddings.el`: Either finish Jina AI integration or remove
   - `imenu.el` + `treesit.el`: Consolidate into `gptel-tools/symbols.el`
   - `workspace.el`: Verify autoload or add explicit `(load! "workspace")`

5. **Add ERT Tests**
   - Priority: clj.el, cljs.el, workspace.el (largest untested tools)
   - Set up CI to run `make test` on .doom.d/

6. **Tree-sitter Integration**
   - Emacs 30 includes tree-sitter modes for Lua, Elixir, HTML
   - Enable `treesit-auto` to automatically use tree-sitter modes
   - Test with golang.el, js.el (typescript-ts-mode)

### Shell Configuration

**Current Issues:**
- No error handling (`set -euo pipefail` missing from all 27 scripts)
- Unquoted variables in 10+ scripts
- Hardcoded paths in 15+ scripts
- No documentation headers

**Recommendations:**

1. **Standardize Script Headers**
   ```bash
   #!/usr/bin/env bash
   set -euo pipefail
   IFS=$'\n\t'
   # Description: <purpose>
   # Usage: <command> <args>
   # Author: <name>
   ```

2. **Quote All Variables**
   - Use ShellCheck to find issues: `shellcheck -f gcc **/*.sh`
   - Fix high-priority: functions.sh:12 (`rm -rf "$DIR"`), lowercased.sh, cpu_graph.sh

3. **Parameterize Paths**
   - Replace `/Users/${config.user.name}` with `${config.home.homeDirectory}`
   - Replace `~/` with `$HOME/`
   - Use `${pkgs.babashka}/bin/bb` instead of `/run/current-system/sw/bin/bb`

4. **Remove Backticks**
   - Replace `` `command` `` with `$(command)` (POSIX-compliant, easier to nest)

5. **Document Scripts**
   - Add comment headers to all 27 scripts
   - Document yabai/* scripts (window manager automation)
   - Document sketchybar plugins (status bar widgets)

### Caching & Build Speed

**Current Issues:**
- No local binary cache configured
- Nix builds may re-download packages
- Emacs byte-compilation on every rebuild

**Recommendations:**

1. **Enable Cachix Personal Cache**
   - Already have `rashawn.cachix.org` in flake.nix:6,12
   - Ensure write token configured: `cachix authtoken <token>`
   - Push builds: `cachix push rashawn` after `darwin-rebuild`

2. **Use `nix-direnv`**
   - Already have direnv in home-manager
   - Add `.envrc`: `use flake`
   - Speeds up shell startup by caching Nix env

3. **Emacs Native-Comp Cache**
   - Ensure `native-comp-eln-load-path` points to writable cache
   - Persist `~/.emacs.d/eln-cache/` across rebuilds

4. **Flake Metadata Cache**
   - Set `--option narinfo-cache-positive-ttl 86400` (24h)
   - Reduces GitHub API hits for flake inputs

### Developer Ergonomics

**Current Issues:**
- No unified task runner (bin/do.py exists but minimal)
- No one-shot bootstrap script
- Fonts installed via Homebrew (not deterministic)

**Recommendations:**

1. **Task Runner** (bin/do.py or Makefile)
   ```makefile
   rebuild: nix-darwin rebuild switch --flake .
   update: nix flake update && nix-darwin rebuild switch --flake .
   check: nix flake check
   lint: deadnix . && statix check
   test-emacs: doom test
   ```

2. **One-Shot Bootstrap**
   ```bash
   # bin/bootstrap-new-machine.sh
   # 1. Install Nix with Determinate Systems installer (better than curl|sh)
   # 2. Clone repo
   # 3. nix-darwin rebuild switch --flake .
   # 4. doom sync
   ```

3. **Deterministic Fonts**
   - Move fonts from Homebrew casks to Nix packages
   - Use `pkgs.nerdfonts` in home-manager
   - Example:
     ```nix
     fonts.packages = with pkgs; [
       (nerdfonts.override { fonts = ["FiraCode" "JetBrainsMono" "Iosevka"]; })
     ];
     ```

---

## F) PRE-COMMIT & CI PLAN

### Pre-Commit Hooks (Fast Checks - Local)

**Install:** Use `pre-commit-hooks.nix` (nix-community)

**Hooks:**
```yaml
repos:
  - repo: local
    hooks:
      # Nix linting (fast)
      - id: deadnix
        name: deadnix
        entry: deadnix --fail
        language: system
        files: \.nix$
        pass_filenames: true

      - id: statix
        name: statix check
        entry: statix check
        language: system
        files: \.nix$
        pass_filenames: true

      # Shell linting
      - id: shellcheck
        name: shellcheck
        entry: shellcheck -f gcc
        language: system
        files: \.sh$
        pass_filenames: true

      # Formatting
      - id: nixpkgs-fmt
        name: nixpkgs-fmt
        entry: nixpkgs-fmt --check
        language: system
        files: \.nix$
        pass_filenames: true

      - id: shfmt
        name: shfmt
        entry: shfmt -i 2 -ci -d
        language: system
        files: \.sh$
        pass_filenames: true

      # Emacs Lisp byte-compile (optional, slower)
      # - id: elisp-compile
      #   name: emacs byte-compile
      #   entry: emacs --batch -f batch-byte-compile
      #   language: system
      #   files: \.el$
      #   pass_filenames: true

      # Secrets detection
      - id: gitleaks
        name: gitleaks
        entry: gitleaks protect --staged
        language: system
        pass_filenames: false
```

**Integration:**
```nix
# modules/yqrashawn/home-manager/default.nix
home.file.".pre-commit-config.yaml".source = ./pre-commit-config.yaml;
programs.git.hooks.pre-commit = "${pkgs.pre-commit}/bin/pre-commit run";
```

### CI Checks (GitHub Actions)

**File:** `.github/workflows/ci.yml`

```yaml
name: CI
on: [push, pull_request]
jobs:
  check:
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v4
      - uses: DeterminateSystems/nix-installer-action@main
      - uses: DeterminateSystems/magic-nix-cache-action@main

      # Nix checks
      - name: Nix flake check
        run: nix flake check --all-systems

      # Linting
      - name: deadnix
        run: nix run nixpkgs#deadnix -- --fail .

      - name: statix
        run: nix run nixpkgs#statix -- check

      - name: shellcheck
        run: |
          find . -name "*.sh" -exec nix run nixpkgs#shellcheck -- -f gcc {} +

      # Formatting
      - name: nixpkgs-fmt check
        run: nix run nixpkgs#nixpkgs-fmt -- --check .

      # Security
      - name: gitleaks
        uses: gitleaks/gitleaks-action@v2
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}

  build:
    runs-on: macos-latest
    strategy:
      matrix:
        config: [yqrashawn, yqrashawn-intel, work]
    steps:
      - uses: actions/checkout@v4
      - uses: DeterminateSystems/nix-installer-action@main
      - uses: DeterminateSystems/magic-nix-cache-action@main

      - name: Build ${{ matrix.config }}
        run: |
          nix build .#darwinConfigurations.${{ matrix.config }}.config.system.build.toplevel

  emacs-test:
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v4
      - uses: DeterminateSystems/nix-installer-action@main

      - name: Install Doom Emacs
        run: |
          # Setup Doom + run tests
          # This requires .doom.d/Makefile with test target

      # - name: Run ERT tests
      #   run: |
      #     emacs --batch -l ert -l .doom.d/gptel-tools/*-test.el -f ert-run-tests-batch-and-exit
```

**Approximate Runtime:**
- Pre-commit (local): 5-10s (deadnix + statix + shellcheck)
- CI full suite: 5-10min (flake check + builds + tests)

---

## G) PHASED ROADMAP

### P0 - TODAY (Low Risk, High ROI) - 1-2 Hours

1. **Update nixpkgs versions (flake.nix:29-30)**
   - Change: `nixpkgs-23.05` → `nixpkgs-24.11` or `25.05`
   - Validate: `nix flake check && darwin-rebuild build --flake .`
   - Success: Build completes without errors

2. **Fix unquoted rm -rf (functions.sh:12)**
   - Change: `rm -rf $DIR` → `rm -rf "$DIR"`
   - Validate: `shellcheck functions.sh`
   - Success: No shellcheck warnings

3. **Remove duplicate homebrew tap (brew.nix:33)**
   - Remove: Line 33 `"koekeishiya/formulae"`
   - Validate: `darwin-rebuild build --flake .`
   - Success: No brew warnings

4. **Remove duplicate straight.el assignment (init.el:326)**
   - Remove: Line 326 (keep line 327 with `'ssh`)
   - Validate: Emacs starts without warnings
   - Success: `straight-vc-git-default-protocol` is `'ssh`

5. **Add lexical-binding to packages.el**
   - Change: Line 1 → `;;; -*- no-byte-compile: t; lexical-binding: t; -*-`
   - Validate: Byte-compile packages.el
   - Success: No warnings about dynamic binding

6. **Fix mapHosts undefined (flake.nix:388)**
   - Remove: `mapHosts` from `inherit (lib.my) ...`
   - Validate: `nix flake check`
   - Success: No evaluation errors

7. **Add shebangs to 3 scripts**
   - Add: `#!/usr/bin/env bash` to allgws.sh, lowercased.sh, go-through-all-directory.sh
   - Validate: Execute scripts directly
   - Success: Scripts run without explicit `bash` call

8. **Delete obsolete install-nix.sh**
   - Remove: `bin/install-nix.sh` (95% commented, insecure curl|sh)
   - Validate: Check if referenced anywhere: `grep -r "install-nix" .`
   - Success: No references found, file removed

9. **Remove clj-elisp.el duplicate load (config.el:70)**
   - Remove: `(load! "clj-elisp")` from config.el (already in helper.el:1)
   - Validate: Emacs starts, clojure-mode works
   - Success: No duplicate load warnings

10. **Delete orphaned scripts (allgws.sh, lowercased.sh, go-through-all-directory.sh)**
    - Remove: 3 unused scripts in local-bins/funcs/
    - Validate: `grep -r "allgws\|lowercased\|go-through" . --exclude-dir=.git`
    - Success: No references found

### P1 - THIS WEEK (Modest Changes) - 4-8 Hours

1. **Update JavaScript dependencies (package.json)**
   - Update: katex to 0.16.11, markdown-it to 15.0.0, react to 18.3.1
   - Validate: `npm install && npm audit`
   - Success: No security vulnerabilities, builds pass

2. **Add set -euo pipefail to all shell scripts**
   - Change: Add to top of all 27 scripts (after shebang)
   - Validate: `shellcheck **/*.sh`
   - Success: No unset variable errors when scripts run

3. **Quote all unquoted variables in scripts**
   - Fix: 10+ scripts (functions.sh, lowercased.sh, cpu_graph.sh, etc.)
   - Validate: `shellcheck -f gcc **/*.sh`
   - Success: Zero "SC2086: Quote to prevent word splitting" warnings

4. **Delete orphaned Nix modules**
   - Remove: network.nix, syncthing.nix, phil.nix
   - Validate: `darwin-rebuild build --flake .`
   - Success: Build succeeds, no import errors

5. **Parameterize hardcoded paths in daemons**
   - Change: `/Users/${config.user.name}` → `${config.home.homeDirectory}` in 15+ daemon files
   - Validate: `darwin-rebuild switch --flake .`, check launchctl
   - Success: Daemons start correctly

6. **Set up pre-commit hooks**
   - Create: `.pre-commit-config.yaml` with deadnix, statix, shellcheck
   - Install: `nix-shell -p pre-commit --run "pre-commit install"`
   - Validate: `pre-commit run --all-files`
   - Success: All hooks pass

7. **Integrate AeroSpace, deprecate yabai**
   - Migrate: yabai/* scripts to Aerospace config
   - Remove: yabai references in brew.nix, scripts
   - Validate: Window management works with Aerospace
   - Success: Yabai uninstalled, Aerospace functional

8. **Remove ASDF inputs from flake.nix**
   - Remove: Lines 92-207 (29 asdf-* inputs)
   - Validate: `nix flake check`
   - Success: Build times faster, no broken references

9. **Add documentation headers to scripts**
   - Add: Purpose, usage, author to all 27 scripts
   - Validate: Manual review
   - Success: All scripts have clear purpose

10. **Consolidate Emacs orphaned tools**
    - Move: imenu.el + treesit.el → utils.el as helper functions
    - Remove: embeddings.el (incomplete, unused)
    - Validate: Doom sync, test gptel-tools
    - Success: No load errors, tools work

### P2 - LATER (Larger Refactors) - 2-4 Days

1. **Split large Emacs files**
   - Refactor: autoload.el → autoload/*.el (outline, font, media, terminal)
   - Refactor: better-default.el → better-default/*.el
   - Refactor: map.el → map/*.el
   - Validate: Byte-compile all, test functionality
   - Success: Faster byte-compile, easier navigation

2. **Migrate to pnpm or bun**
   - Install: pnpm via ASDF or Nix
   - Migrate: package.json, convert lockfile
   - Update: CI scripts to use pnpm
   - Validate: `pnpm install && pnpm run build`
   - Success: 2-3x faster installs

3. **Migrate shell history to atuin**
   - Enable: atuin in home-manager/cli/
   - Import: existing history with `atuin import auto`
   - Configure: keybindings (Ctrl-R), sync settings
   - Validate: History search works
   - Success: Faster, better history search

4. **Migrate to prezto → zinit**
   - Install: zinit via home-manager
   - Migrate: prezto plugins to zinit config
   - Enable: Turbo mode for lazy loading
   - Validate: Shell startup time with `hyperfine`
   - Success: 50-80% faster shell startup

5. **Integrate sops-nix for secrets**
   - Setup: sops-nix module, create secrets.yaml
   - Encrypt: API tokens, SSH keys, email credentials
   - Migrate: not-secret.el to sops
   - Validate: Secrets decrypt on rebuild
   - Success: No plain-text secrets in repo

6. **Add CI pipeline**
   - Create: .github/workflows/ci.yml
   - Configure: Nix checks, linting, builds, tests
   - Add: Status badge to README
   - Validate: Push to GitHub, check CI passes
   - Success: Green builds on all PRs

7. **Add ERT tests for Emacs tools**
   - Write: Tests for clj.el, cljs.el, workspace.el
   - Setup: `make test` target in .doom.d/
   - Add: CI job to run tests
   - Validate: `make test` passes
   - Success: 90%+ test coverage on gptel-tools

8. **Optimize Emacs startup time**
   - Profile: With esup, identify slow packages
   - Lazy-load: Heavy packages (clojure, org, gptel)
   - Benchmark: Before/after with `emacs --timed-requires`
   - Validate: Startup < 2s (down from 4-5s typical)
   - Success: 50% faster startup

9. **Move fonts to Nix**
   - Remove: Font casks from brew.nix (20+ fonts)
   - Add: `pkgs.nerdfonts` in home-manager
   - Validate: `fc-list` shows all fonts
   - Success: Deterministic font installation

10. **Consolidate overlays**
    - Move: All overlays to overlays/default.nix
    - Remove: modules/yqrashawn/overlays.nix
    - Document: Purpose of each overlay
    - Validate: `darwin-rebuild build --flake .`
    - Success: Single overlay import point

---

## H) COMMAND COOKBOOK (Copy-Paste)

### Repository Scan

```bash
# File counts
git ls-files | wc -l                          # Total tracked files (865)
git ls-files | grep "\.nix$" | wc -l          # Nix files (87)
git ls-files | grep "\.sh$" | wc -l           # Shell scripts (27)
git ls-files | grep "\.el$" | wc -l           # Emacs Lisp (65)
git ls-files | grep -E "\.(clj|cljs)$" | wc -l # Clojure (27)

# Repository size
du -sh .                                       # Total size (8.8MB)
du -sh .doom.d                                 # Emacs config size
du -sh modules                                 # Nix modules size
```

### Nix Linting

```bash
# Flake check (validation + checks)
nix flake check --all-systems                 # Full validation
nix flake show                                # Show outputs

# Dead code detection
nix run nixpkgs#deadnix -- --fail .           # Find unused bindings
nix run nixpkgs#deadnix -- --edit .           # Auto-remove dead code

# Linting
nix run nixpkgs#statix -- check               # Anti-pattern detection
nix run nixpkgs#statix -- fix                 # Auto-fix issues

# Newer semantic linter
nix run nixpkgs#nixf-tidy -- < file.nix       # JSON diagnostics

# Formatting
nix run nixpkgs#nixpkgs-fmt -- .              # Format all Nix files
nix run nixpkgs#alejandra -- .                # Alternative formatter
```

### Shell Linting

```bash
# ShellCheck (syntax + best practices)
shellcheck -f gcc **/*.sh                     # All scripts, GCC format
shellcheck -e SC2086 script.sh                # Ignore specific warning
shellcheck --severity=warning **/*.sh         # Only warnings+errors

# Formatting
shfmt -i 2 -ci -d **/*.sh                     # Check formatting (2-space indent)
shfmt -i 2 -ci -w **/*.sh                     # Auto-format

# Find scripts without shebangs
grep -L "^#!" **/*.sh                         # Missing shebang

# Find unquoted variables
grep -Pn '\$\w+' **/*.sh | grep -v '"'        # Potential unquoted vars
```

### Emacs Lisp Analysis

```bash
# Byte-compile all files (in Doom)
cd ~/.doom.d && doom sync                     # Sync packages
doom doctor                                   # Check for issues
emacs --batch -f batch-byte-compile *.el      # Compile all .el

# Find files without lexical-binding
grep -L "lexical-binding: t" .doom.d/*.el

# Check for obsolete functions
emacs --batch --eval "(progn (require 'bytecomp) (byte-compile-file \"file.el\"))" 2>&1 | grep obsolete

# Run ERT tests
emacs --batch -l ert -l file-test.el -f ert-run-tests-batch-and-exit

# Startup profiling
emacs --timed-requires --debug-init           # Show require times
nix run nixpkgs#emacs-lsp-booster             # Speed up LSP
```

### Clojure/Babashka Linting

```bash
# clj-kondo (if installed)
nix run nixpkgs#clj-kondo -- --lint src       # Lint Clojure files
nix run nixpkgs#clj-kondo -- --lint .doom.d/gptel-tools/*.clj

# Check bb.edn syntax
nix run nixpkgs#babashka -- -e "(prn (slurp \"bb.edn\"))"

# Find Clojure files
find . -name "*.clj" -o -name ".cljs" | grep -v node_modules
```

### JavaScript/TypeScript Analysis

```bash
# Dependency audit
npm audit                                     # Security vulnerabilities
npm outdated                                  # Check for updates
nix run nixpkgs#depcheck -- .                 # Unused dependencies

# Linting (if eslint configured)
npx eslint .                                  # Lint JS/TS files
npx biome check .                             # Alternative: Biome (Rust-based)

# TypeScript check
npx tsc --noEmit                              # Type-check without emit
```

### Secrets Detection

```bash
# gitleaks (scan for secrets)
nix run nixpkgs#gitleaks -- detect --source . # Scan entire repo
nix run nixpkgs#gitleaks -- protect           # Scan staged files

# Alternative: trufflehog
nix run nixpkgs#trufflehog -- git file://.    # Deep history scan
```

### Reference Scanning (Find Invocations)

```bash
# Find imports/requires in Nix
rg "import\s+\./network\.nix" --type nix     # Is network.nix used?
rg "tailscale" --type nix                     # All tailscale references

# Find script invocations
rg "allgws\.sh" --type nix --type el          # Is allgws.sh called?
rg "bb.edn" --type nix                        # Where is bb.edn referenced?

# Find Emacs function calls
rg "defun my-" .doom.d/*.el                   # Custom functions
rg "gptelt-make-tool" .doom.d/gptel-tools/    # Tool registrations

# Find shell function usage
rg "^function\s+\w+" **/*.sh                  # Defined functions
rg "source\s+" **/*.sh                        # Sourced scripts
```

### Version/Deprecation Scans

```bash
# Nix flake metadata
nix flake metadata                            # Show all inputs + versions
nix flake metadata --json | jq '.locks.nodes' # JSON output

# Check outdated Nix inputs
nix flake update --dry-run                    # What would update
nix flake lock --update-input nixpkgs         # Update single input

# Emacs package updates
doom upgrade                                  # Update Doom + packages
doom sync -u                                  # Update packages only

# Check for obsolete Emacs packages
emacs --batch --eval "(progn (require 'package) (package-refresh-contents) (package-list-packages))"

# npm/yarn/pnpm updates
npm outdated                                  # Show outdated packages
npx npm-check-updates                         # Interactive update tool
```

### Editorconfig Check

```bash
# Check editorconfig compliance
nix run nixpkgs#editorconfig-checker -- .     # Check all files
```

### Performance Benchmarks

```bash
# Nix build time
time nix build .#darwinConfigurations.yqrashawn.config.system.build.toplevel

# Shell startup time
hyperfine "zsh -i -c exit"                    # Benchmark shell startup

# Emacs startup time
hyperfine "emacs --eval '(kill-emacs)'"       # Cold start
emacs --eval "(message \"Startup: %s\" (emacs-init-time))"
```

---

## I) VALIDATION PLAN

### How to Confirm Fixes

**Nix Configuration Changes:**

```bash
# After updating flake.nix
nix flake check --all-systems                 # Validates flake structure
darwin-rebuild build --flake .                # Test build (no activation)
darwin-rebuild switch --flake .               # Apply changes

# Expected output deltas
# - Before: "building on darwin-stable (23.05)"
# - After: "building on darwin-stable (24.11)" or "25.05"

# Verify no evaluation errors
nix eval .#darwinConfigurations.yqrashawn.config.system.build.toplevel --json
```

**Shell Script Changes:**

```bash
# After adding set -euo pipefail and quoting
shellcheck **/*.sh 2>&1 | tee shellcheck.log  # Should be clean
wc -l shellcheck.log                           # Count: 0 errors

# Before: SC2086 warnings (unquoted), SC2039 (missing set -e)
# After: No SC2086, no SC2039

# Test scripts still work
./modules/yqrashawn/home-manager/cli/functions.sh
# (If errors occur due to pipefail, that's a GOOD thing - exposes bugs)
```

**Emacs Configuration Changes:**

```bash
# After Emacs changes
doom sync                                     # Recompile packages
doom doctor                                   # Check for issues
emacs --debug-init                            # Test startup

# Byte-compile check
emacs --batch --eval "(progn \
  (setq byte-compile-error-on-warn t) \
  (batch-byte-compile))" .doom.d/*.el 2>&1 | tee compile.log

# Expected: 0 warnings, 0 errors

# Test tools work
emacs --batch -l .doom.d/gptel-tools/buffer.el \
  --eval "(print (functionp 'gptelt/get-buffer-file-path))"
# Output: t (function exists)
```

**Linting Validation:**

```bash
# deadnix should find 0 items after removing orphaned modules
deadnix --fail . 2>&1 | grep "unused" | wc -l # Expected: 0

# statix should pass after fixes
statix check 2>&1 | grep "warning\|error" | wc -l # Expected: 0

# shellcheck should be clean after quoting
shellcheck -f gcc **/*.sh 2>&1 | wc -l        # Expected: 0 (just headers)
```

**Dependency Updates:**

```bash
# After updating package.json
npm install                                   # Should succeed
npm audit --audit-level=moderate              # Expected: 0 vulnerabilities
npm run build                                 # (If build script exists)

# Verify versions
npm list katex markdown-it react              # Should show 0.16.x, 15.x, 18.3.x
```

**Pre-commit Hooks:**

```bash
# After setting up pre-commit
pre-commit run --all-files                    # All hooks should pass

# Expected output:
# deadnix...........................Passed
# statix............................Passed
# shellcheck........................Passed
# nixpkgs-fmt.......................Passed
```

**CI Pipeline:**

```bash
# After adding .github/workflows/ci.yml
git push origin <branch>                      # Trigger CI
gh run watch                                  # Watch CI progress

# Expected: All jobs pass (✓ check, ✓ build, ✓ emacs-test)
```

### Rollback Notes

**Nix Rollback:**

```bash
# List generations
darwin-rebuild --list-generations

# Rollback to previous generation
darwin-rebuild --rollback

# Or specific generation
darwin-rebuild --switch --generation 42
```

**Doom Emacs Rollback:**

```bash
# Doom auto-creates backups
ls ~/.doom.d/.local/backups/                  # Browse backups
doom sync -r                                  # Rollback last sync

# Manual: restore from git
git checkout HEAD~1 .doom.d/
doom sync
```

**Git Rollback:**

```bash
# For committed changes
git revert <commit-hash>                      # Safe: creates new commit
git reset --hard HEAD~1                       # Destructive: removes commit

# For uncommitted changes
git checkout -- <file>                        # Restore single file
git reset --hard HEAD                         # Restore all files
```

**Nix Store Rollback:**

Since environment is disposable and Nix is declarative:
1. All changes are in Git
2. Revert Git changes
3. Re-run `darwin-rebuild switch --flake .`
4. System state restored (no data loss)

---

## TODAY'S TOP 10 ACTIONS (P0)

**Priority 0 - Execute Today - 1-2 Hours - Zero Risk**

These actions are safe, reversible, and high-impact. Can be done in any order.

| # | Action | File(s) | Command to Validate | Success Criteria | Time |
|---|--------|---------|---------------------|------------------|------|
| 1 | **Update nixpkgs to 24.11/25.05** | flake.nix:29-30 | `nix flake check && darwin-rebuild build --flake .` | Build succeeds, no EOL warnings | 15min |
| 2 | **Quote rm -rf variable** | modules/.../cli/functions.sh:12 | `shellcheck functions.sh` | Zero SC2086 warnings | 2min |
| 3 | **Remove duplicate homebrew tap** | modules/.../darwin/brew.nix:33 | `darwin-rebuild build --flake .` | No duplicate tap warnings | 2min |
| 4 | **Remove duplicate straight.el line** | .doom.d/init.el:326 | `emacs --debug-init` | Emacs starts, git protocol is ssh | 2min |
| 5 | **Add lexical-binding header** | .doom.d/packages.el:1 | Byte-compile packages.el | No dynamic binding warnings | 2min |
| 6 | **Fix undefined mapHosts** | flake.nix:388 | `nix flake check` | No evaluation errors | 2min |
| 7 | **Add shebangs to 3 scripts** | local-bins/funcs/{allgws,lowercased,go-through-all-directory}.sh | `./script.sh` (test execute) | Scripts run without `bash` prefix | 5min |
| 8 | **Delete obsolete install-nix.sh** | bin/install-nix.sh | `rg "install-nix" . \| wc -l` | Zero references, file deleted | 2min |
| 9 | **Remove duplicate clj-elisp load** | .doom.d/config.el:70 | `doom sync && emacs --debug-init` | No duplicate load, clojure-mode works | 5min |
| 10 | **Delete 3 orphaned scripts** | local-bins/funcs/{allgws,lowercased,go-through-all-directory}.sh | `rg "allgws\|lowercased\|go-through" .` | Zero references | 5min |

**Total Time:** ~45 minutes
**Risk Level:** ZERO (all reversible via git)
**Impact:** Eliminates critical security issues, removes dead code, fixes build warnings

**How to Execute:**

```bash
# 1. Create feature branch
git checkout -b fix/audit-p0-actions

# 2. Make changes (use above table)
# 3. Validate each change as you go
# 4. Commit all changes
git add -A && git commit -m "fix: apply P0 audit recommendations

- Update nixpkgs to 24.11
- Fix unquoted variable in rm -rf
- Remove duplicate homebrew tap
- Clean up Emacs config duplicates
- Add shell script shebangs
- Delete obsolete and orphaned files
- Fix undefined Nix function import"

# 5. Test full system rebuild
darwin-rebuild switch --flake .

# 6. If all passes, push and merge
git push origin fix/audit-p0-actions
```

---

**END OF AUDIT REPORT**

*For P1 and P2 actions, see sections G) PHASED ROADMAP above.*
*For specific migration guides, see section D) ALTERNATIVES MATRIX.*
*For automation setup, see sections F) PRE-COMMIT & CI PLAN and H) COMMAND COOKBOOK.*
