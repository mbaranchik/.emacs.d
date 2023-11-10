#!/usr/bin/env python3

import argparse
import os
import sys
import shutil
import subprocess

script_dir = os.path.dirname(os.path.abspath(__file__))

def elisp_bool(val):
    if val == "on":
        return 't'
    else:
        return 'nil'

def elisp_string(val):
    if not val:
        return ""
    return val

def get_modes_list(modes):
    mode_map = {
        'c': ['c-mode', 'c-ts-mode'],
        'cpp': ['c++-mode', 'c++-ts-mode'],
        'python': ['python-mode', 'python-ts-mode'],
        'bash': ['sh-mode', 'bash-ts-mode']
    }
    return [mode_map[x] for x in modes]

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Prepare config.el to your liking")
    parser.add_argument("--lsp",
                        choices=["eglot", "lsp-bridge"],
                        default=None,
                        help="use this as LSP client, default=none")
    parser.add_argument("--c-server",
                        choices=["ccls", "clangd"],
                        default="clangd",
                        help="use this as LSP server for C/C++")
    parser.add_argument("--py-server",
                        choices=["pylsp", "pyright"],
                        default="pylsp",
                        help="use this as LSP server for Python")
    parser.add_argument("--lsp-enable-modes",
                        choices=["c", "cpp", "python", "bash"],
                        nargs="+",
                        default=["c", "cpp", "python", "bash"],
                        help="enable LSP for these modes, default=all")
    parser.add_argument("--lsp-autoformat-enable-modes",
                        choices=["c", "cpp", "python", "bash"],
                        nargs="+",
                        default=[],
                        help="enable auto-format buffer on save for these modes")
    parser.add_argument("--diag",
                        choices=["flycheck", "flymake"],
                        default=None,
                        help="use this as diagnostics framework, default=none")
    parser.add_argument("--prog-comp",
                        choices=["corfu", "company"],
                        default=None,
                        help="use this as completion framework, default=none")
    parser.add_argument("--mode-line",
                        choices=["doom", "mood"],
                        default="doom",
                        help="use this as mode-line")
    parser.add_argument("--treesit",
                        choices=["on", "off"],
                        default="off",
                        help="enable treesit and install plugins")
    parser.add_argument("--treesit-install-grammers",
                        action="store_true",
                        help="install treesit grammers")
    parser.add_argument("--theme",
                        choices=["doom-vibrant", "doom-moonlight", "doom-xcode"],
                        default="doom-xcode",
                        help="use this as theme")
    parser.add_argument("--auto-server",
                        choices=["on", "off"],
                        default="off",
                        help="automatically enable server")
    parser.add_argument("--force",
                        action="store_true",
                        default=False,
                        help="ignores warnings")

    args = parser.parse_args()

    if args.lsp == "lsp-bridge" and (args.prog_comp != None or args.diag != None):
        sys.stderr.write("ERROR: LSP client 'lsp-bridge' uses proprietary completion and diagnostics, fix arguments\n")
        sys.exit(1)

    if not shutil.which(args.c_server):
        sys.stderr.write(f"WARNING: LSP server '{args.c_server}' not found in PATH!\n")
        if not args.force:
            sys.stderr.write("ERROR: Bad option supplied to --c-server, use --force to use anyway")
            sys.exit(1)

    if not shutil.which(args.py_server):
        sys.stderr.write(f"WARNING: LSP server '{args.py_server}' not found in PATH!")
        if not args.force:
            sys.stderr.write("ERROR: Bad option supplied to --py-server, use --force to use anyway")
            sys.exit(1)

    if args.treesit_install_grammers:
        subprocess.check_call(f"cd {script_dir} && \
        {shutil.which('emacs')} -Q \
        --batch --eval \
        '(progn (load-file \"lisp.d/treesit-sources.el\") (load-file \"lisp.d/treesit-install.el\"))'", shell=True)

    with open(os.path.join(script_dir, 'config.el'), 'w') as f:
              f.write(f""";;; -*- lexical-binding: t -*-

;; <Server Enable>
;; t:   Auto start server if not already running
;; nil: Do nothing
(set-config-bool-var "start-server" {elisp_bool(args.auto_server)})

;; <LSP Client>
;; "eglot":      Use EGLOT (built-in)
;; "lsp-bridge": Use lsp-bridge
;;               Notice: Uses proprietary completion and diagnostics framework
(set-config-var "lsp" "{elisp_string(args.lsp)}")

;; <C/C++ LSP Server>
;; "ccls":   Use ccls
;; "clangd": Use clangd
(set-config-var "lsp/cpp-backend" "{elisp_string(args.c_server)}")

;; <Python LSP Server>
;; "pylsp":   Use python-lsp-server (see https://github.com/python-lsp/python-lsp-server)
;; "pyright": Use pyright (see https://github.com/microsoft/pyright)
(set-config-var "lsp/py-backend" "{elisp_string(args.py_server)}")

;; <LSP Enabled Modes>
;; "c":      Enable for c-mode/c-ts-mode
;; "cpp":    Enable for c++-mode/c++-ts-mode
;; "python": Enable for python-mode/python-ts-mode
;; "bash":   Enable for sh-mode/bash-ts-mode
(set-config-var "lsp/enable-modes" '({" ".join(args.lsp_enable_modes)}))

;; <LSP AutoFormat Enabled Modes>
;; "c":      Enable for c-mode/c-ts-mode
;; "cpp":    Enable for c++-mode/c++-ts-mode
;; "python": Enable for python-mode/python-ts-mode
;; "bash":   Enable for sh-mode/bash-ts-mode
(set-config-var "lsp/autoformat-enable-modes" '({" ".join(args.lsp_autoformat_enable_modes)}))

;; <Programming Completion>
;; "corfu":   Enable corfu completion framework
;; "company": Enable company completion framework
(set-config-var "autocomplete" "{elisp_string(args.prog_comp)}")

;; <Diagnostics>
;; "flymake":  Use flymake as diagnostics framework
;; "flycheck": Use flycheck as diagnostics framework
(set-config-var "code-diag" "{elisp_string(args.diag)}")

;; <Mode-Line>
;; "doom": Use doom mode-line
;; "mood": Use mood mode-line
(set-config-var "modeline" "{elisp_string(args.mode_line)}")

;; <Theme>
;; "doom-xcode"
;; "doom-vibrant"
;; "doom-moonlight"
;; *: Use any available theme
(set-config-var "theme-name" "{args.theme}" "EMACS_USE_THEME")
(set-config-quote-var "theme-sym" '{args.theme} "EMACS_USE_THEME")

;; <Treesit>
(set-config-bool-var "use-treesit" {elisp_bool(args.treesit)})

;; <UI>
(set-config-bool-var "use-idle-highlight" nil) ;; Enable idle symbol highlight
(set-config-bool-var "use-visual-line-mode" nil) ;; Enable visual line mode
(set-config-bool-var "use-indent-guide" nil) ;; Enable indent-guide
(set-config-bool-var "use-which-function" nil) ;; Enable which-function in mode-line
(set-config-bool-var "use-diff-hl" t) ;; Enable version-control diff in gutter

;; <Copyright>
(set-config-var "auto-insert-copyright" "REPLACE_WITH_COPYRIGHT" "EMACS_COPYRIGHT_COMPANY")
(set-config-var "auto-insert-name" "REPLACE_WITH_NAME" "EMACS_COPYRIGHT_NAME")

(provide 'my/config)
""")
