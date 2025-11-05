# 📊 Comprehensive Dotfiles Audit Report (Plan Only)

## Summary

Comprehensive audit of yqdotfiles repository (865 files, 8.8MB) covering Nix, Shell, Emacs Lisp, Clojure/ClojureScript, and JavaScript/TypeScript configurations.

**This is a PLAN-ONLY report with NO code changes.** Review findings and prioritize actions.

## 📋 What's Included

### A) Inventory
- Complete mapping of 865 tracked files
- Entry points: flake.nix, .doom.d/, modules/
- Software inventory: 300+ packages across Nix, Homebrew, ASDF, npm

### B) Major Findings (18 Total)

**HIGH Severity (6):**
- 🔴 EOL nixpkgs 23.05 (no security updates since June 2024)
- 🔴 Unquoted `rm -rf $DIR` (data loss risk)
- 🔴 Insecure `curl | sh` pattern
- 🔴 Duplicate Homebrew tap
- 🔴 Command injection in awk system()
- 🔴 Undefined Nix function import

**MEDIUM Severity (6):**
- 🟡 JavaScript deps 4 versions behind (katex 0.12→0.16)
- 🟡 Missing shell script shebangs (3 files)
- 🟡 40+ hardcoded paths (not portable)
- 🟡 Orphaned Nix modules (network.nix, syncthing.nix, phil.nix)
- 🟡 Duplicate Emacs config loading
- 🟡 Duplicate straight.el protocol assignment

**LOW Severity (6):**
- Missing Emacs test coverage (10 of 17 tools)
- Commented dead code
- Empty bb.edn
- Orphaned Emacs tools not loaded
- Large Emacs files (>500 LOC)
- Missing lexical-binding header

### C) Unused/Stale Map
- **3 orphaned Nix modules** (network.nix, syncthing.nix, phil.nix)
- **4 unused shell scripts** (allgws.sh, lowercased.sh, go-through-all-directory.sh, install-nix.sh)
- **3 Emacs tools not loaded** (embeddings.el, imenu.el, treesit.el)

### D) Alternatives Matrix
Research-backed recommendations with citations (2024-2025 sources):
- nixpkgs 23.05 → **24.11/25.05** (Nov 2024 release, Darwin improvements)
- yabai → **AeroSpace** (no SIP required, already installed!)
- mcfly → **atuin** (better history, already in flake!)
- prezto → **zinit** (50-80% faster startup)
- katex 0.12 → **0.16.11** (4 major versions behind)
- npm → **pnpm** or **bun** (2-30x faster installs)

### E) Structure Improvements
- Consolidate overlays (2 locations → 1)
- Remove 29 ASDF plugins (unused)
- Darwin vs Home-Manager boundaries
- Emacs startup optimization
- Shell error handling standardization

### F) Pre-Commit & CI Plan
Ready-to-copy `.pre-commit-config.yaml` and `.github/workflows/ci.yml` with:
- deadnix, statix, shellcheck, nixpkgs-fmt, gitleaks
- Estimated local runtime: 5-10s
- CI runtime: 5-10min

### G) Phased Roadmap

**P0 - TODAY (45min, zero risk):**
Top 10 immediate actions - all safe, reversible, high-impact

**P1 - THIS WEEK (4-8 hours):**
10 modest changes - dependency updates, parameterization, pre-commit setup

**P2 - LATER (2-4 days):**
Large refactors - file splits, package manager migration, CI setup

### H) Command Cookbook
Copy-paste commands for:
- Nix linting (deadnix, statix, nixf-tidy)
- Shell linting (shellcheck, shfmt)
- Emacs analysis (byte-compile, ERT tests)
- Secrets detection (gitleaks)
- Version scans, benchmarks

### I) Validation Plan
How to confirm fixes:
- Expected output deltas
- Rollback procedures (generations, git revert)
- Success criteria for each change

## 🎯 Top 10 Actions (P0 - Execute Today)

| # | Action | Time | Risk |
|---|--------|------|------|
| 1 | Update nixpkgs to 24.11/25.05 | 15min | Low |
| 2 | Quote `rm -rf` variable | 2min | Zero |
| 3 | Remove duplicate homebrew tap | 2min | Zero |
| 4 | Remove duplicate straight.el line | 2min | Zero |
| 5 | Add lexical-binding header | 2min | Zero |
| 6 | Fix undefined mapHosts | 2min | Zero |
| 7 | Add shebangs to 3 scripts | 5min | Zero |
| 8 | Delete obsolete install-nix.sh | 2min | Zero |
| 9 | Remove duplicate clj-elisp load | 5min | Zero |
| 10 | Delete 3 orphaned scripts | 5min | Zero |

**Total:** 45 minutes, zero risk, all reversible via git

## 📊 Key Statistics

- **865 tracked files** (87 Nix, 27 Shell, 65 Emacs Lisp, 27 Clojure)
- **300+ software components** installed
- **7 Darwin configurations** (yqrashawn, mini, studio, mbp, holybasil, yqrashawn-intel, work)
- **164 Emacs packages**, 17 custom gptel-tools
- **50+ Homebrew apps**, 200+ Nix packages, 29 ASDF plugins

## 🔗 Related

- GitHub Dependabot found **30 vulnerabilities** (6 critical, 7 high) - aligns with audit findings
- Full report: `AUDIT_REPORT.md` (1,248 lines)

## ✅ Next Steps

1. Review report thoroughly
2. Prioritize actions based on risk/ROI
3. Execute P0 actions (45min)
4. Schedule P1 actions for this week
5. Plan P2 refactors

---

**This PR contains only documentation (plan).** No code changes are included per requirements for senior dotfiles audit.
