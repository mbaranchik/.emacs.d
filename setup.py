#!/usr/bin/env python3

import argparse
import platform
import os
import sys
import shutil
import subprocess
import termcolor
import urllib.request
import tarfile
import zipfile
from io import BytesIO
import socket
import json

hostname = socket.gethostname()
script_dir = os.path.dirname(os.path.abspath(__file__))

def fatal(*args, **kwargs):
    print(f"{termcolor.colored('[FATAL]:', 'red', attrs=['bold'])}", *args, **kwargs)
    sys.stdout.flush()

def error( *args, **kwargs):
    print(f"{termcolor.colored('[ERROR]:', 'red', attrs=['bold'])}", *args, **kwargs)
    sys.stdout.flush()

def warn(*args, **kwargs):
    print(f"{termcolor.colored('[WARN]:', 'yellow', attrs=['bold'])}", *args, **kwargs)
    sys.stdout.flush()

def info(*args, **kwargs):
    print(f"{termcolor.colored('[INFO]:', 'grey', attrs=['bold'])}", *args, **kwargs)
    sys.stdout.flush()

def log(*args, **kwargs):
    print(f"{termcolor.colored('[LOG]:', 'white', attrs=['bold'])}", *args, **kwargs)
    sys.stdout.flush()

def is_linux():
    return sys.platform in ["linux", "linux2"]

def is_macos():
    return sys.platform == "darwin"

def is_macos_intel():
    return is_macos() and 'i386' in platform.processor()

def is_macos_arm():
    return is_macos() and not is_macos_intel()

def platform_name():
    if is_linux():
        return 'linux'
    if is_macos_intel():
        return 'macos-intel'
    if is_macos_arm():
        return 'macos-arm'
    if is_windows():
        return 'windows'

def get_home_dir():
    return os.path.expanduser('~')

def get_home_bin():
    return os.path.expanduser('~/bin')

def download(url):
    info(f"Downloading {url}")
    return urllib.request.urlopen(url)

def unzip(file, where):
    with zipfile.ZipFile(BytesIO(file.read())) as t:
        log(f"Extracting files {t.namelist()}")
        t.extractall(path=where)

def untar(file):
    with tarfile.open(name=None, fileobj=BytesIO(file.read())) as t:
        members = t.getmembers()
        log(f"Extracting files {[x.name for x in members]}")
        t.extractall(path=where, members=members)

emacs_lsp_booster_releases = {
    'linux': 'https://github.com/blahgeek/emacs-lsp-booster/releases/download/v0.2.1/emacs-lsp-booster_v0.2.1_x86_64-unknown-linux-musl.zip',
    'macos-intel': 'https://github.com/blahgeek/emacs-lsp-booster/releases/download/v0.2.1/emacs-lsp-booster_v0.2.1_x86_64-apple-darwin.zip',
    'macos-arm': 'https://github.com/blahgeek/emacs-lsp-booster/releases/download/v0.2.1/emacs-lsp-booster_v0.2.1_x86_64-apple-darwin.zip'
}

def elisp_bool(val):
    if val == "on" or val == True:
        return 't'
    else:
        return 'nil'

def bool_var(val):
    if val == "on" or val == True:
        return True
    elif val == "off" or val == False:
        return False
    else:
        raise Exception(f"Value '{val}' is not a boolean (or on/off)")

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

def merge(source, destination):
    for key, value in source.items():
        if isinstance(value, dict):
            # get node or create one
            node = destination.setdefault(key, {})
            merge(value, node)
        else:
            destination[key] = value

if __name__ == "__main__":
    cfg = {
        "server": False,
        "lsp": {
            "package": None,
            "c": {
                "server": "clangd",
                "enable": True
            },
            "cpp": {
                "server": "clangd",
                "enable": True
            },
            "python": {
                "server": "pylsp",
                "enable": True
            },
            "bash": {
                "server": None,
                "enable": True
            }
        },
        "code-diag": {
            "package": None,
            "c": {
                "enable": False
            },
            "cpp": {
                "enable": False
            },
            "python": {
                "enable": False
            },
            "bash": {
                "enable": False
            }
        },
        "autoformat": {
            "package": "apheleia",
            "c": {
                "enable": False
            },
            "cpp": {
                "enable": False
            },
            "python": {
                "enable": False
            },
            "bash": {
                "enable": False
            }
        },
        "autocomplete": None,
        "treesit": False,
        "ui": {
            "theme": "doom-xcode",
            "modeline": "doom",
            "idle-highlight": False,
            "visual-line-mode": False,
            "indent-guide": False,
            "which-function": False,
            "diff-hl": True
        },
        "indent": {
            "basic-offset": 4,
            "indent-tabs": False
        },
        "copyright": {
            "header": "",
            "name": ""
        }
    }

    cfg_fname = os.path.join(script_dir, 'config.json')
    if os.path.os.path.isfile(cfg_fname):
        print("INFO: Pre-loading existing configuration from config.json")
        with open(cfg_fname, 'r') as f:
            existing_cfg = json.load(f)
            merge(existing_cfg, cfg)

    def get_enabled_modes(opt):
        return [x for x in cfg[opt].keys() if x != "package" and cfg[opt][x]["enable"]]

    parser = argparse.ArgumentParser(description="Prepare config.el to your liking",
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("--lsp",
                        choices=["eglot", "lsp-bridge", ""],
                        default=cfg["lsp"]["package"],
                        help="use this as LSP client, default=none")
    parser.add_argument("--c-server",
                        choices=["ccls", "clangd"],
                        default=cfg["lsp"]["c"]["server"],
                        help="use this as LSP server for C/C++")
    parser.add_argument("--py-server",
                        choices=["pylsp", "pyright"],
                        default=cfg["lsp"]["python"]["server"],
                        help="use this as LSP server for Python")
    parser.add_argument("--bash-server",
                        choices=["bash-language-server"],
                        default=cfg["lsp"]["bash"]["server"],
                        help="use this as LSP server for BASH")
    parser.add_argument("--lsp-enable-modes",
                        choices=["c", "cpp", "python", "bash"],
                        nargs="+",
                        default=get_enabled_modes("lsp"),
                        help="enable LSP for these modes, default=all")
    parser.add_argument("--autoformat-enable-modes",
                        choices=["c", "cpp", "python", "bash"],
                        nargs="+",
                        default=get_enabled_modes("autoformat"),
                        help="enable auto-format buffer on save using LSP for these modes")
    parser.add_argument("--diag-enable-modes",
                        choices=["c", "cpp", "python", "bash"],
                        nargs="+",
                        default=get_enabled_modes("code-diag"),
                        help="enable diagnostics from LSP for these modes")
    parser.add_argument("--diag",
                        choices=["flycheck", "flymake", ""],
                        default=cfg["code-diag"]["package"],
                        help="use this as diagnostics framework, default=none")
    parser.add_argument("--prog-comp",
                        choices=["corfu", "company", ""],
                        default=cfg["autocomplete"],
                        help="use this as completion framework, default=none")
    parser.add_argument("--mode-line",
                        choices=["doom", "mood"],
                        default=cfg["ui"]["modeline"],
                        help="use this as mode-line")
    parser.add_argument("--treesit",
                        choices=["on", "off"],
                        default=cfg["treesit"],
                        help="enable treesit and install plugins")
    parser.add_argument("--treesit-install-grammers",
                        action="store_true",
                        help="install treesit grammers")
    parser.add_argument("--theme",
                        default=cfg["ui"]["theme"],
                        help="use this as theme")
    parser.add_argument("--auto-server",
                        choices=["on", "off"],
                        default=cfg["server"],
                        help="automatically enable server")
    parser.add_argument("--force",
                        action="store_true",
                        default=False,
                        help="ignores warnings")
    args = parser.parse_args()

    # Update config with user values
    cfg["lsp"]["package"] = args.lsp
    cfg["lsp"]["c"]["server"] = cfg["lsp"]["cpp"]["server"] = args.c_server
    cfg["lsp"]["python"]["server"] = args.py_server
    cfg["lsp"]["bash"]["server"] = args.bash_server
    for mode in ['c', 'cpp', 'python', 'bash']:
        cfg["lsp"][mode]["enable"] = mode in args.lsp_enable_modes
        cfg["autoformat"][mode]["enable"] = mode in args.autoformat_enable_modes
        cfg["code-diag"][mode]["enable"] = mode in args.diag_enable_modes
    cfg["code-diag"]["package"] = args.diag
    cfg["autocomplete"] = args.prog_comp
    cfg["treesit"] = args.treesit
    cfg["ui"]["modeline"] = args.mode_line
    cfg["ui"]["theme"] = args.theme
    cfg["server"] = args.auto_server

    if cfg["lsp"]["package"] == "lsp-bridge" and (cfg["autocomplete"] != None or cfg["code-diag"]["package"] != None):
        sys.stderr.write("WARNING: LSP client 'lsp-bridge' uses proprietary completion and diagnostics, fix arguments\n")
        if not args.force:
            sys.stderr.write("ERROR: Bad options supplied, use --force to use anyway")
            sys.exit(1)

    if not shutil.which(cfg["lsp"]["c"]["server"]):
        sys.stderr.write(f"WARNING: LSP server '{cfg["lsp"]["c"]["server"]}' not found in PATH!\n")
        if not args.force:
            sys.stderr.write("ERROR: Bad option supplied to --c-server, use --force to use anyway")
            sys.exit(1)

    if not shutil.which(cfg["lsp"]["python"]["server"]):
        sys.stderr.write(f"WARNING: LSP server '{cfg["lsp"]["python"]["server"]}' not found in PATH!")
        if not args.force:
            sys.stderr.write("ERROR: Bad option supplied to --py-server, use --force to use anyway")
            sys.exit(1)

    with open(cfg_fname, 'w') as f:
        print("INFO: Saving configuration to config.json")
        json.dump(cfg, f, indent=2)

    if args.treesit_install_grammers:
        subprocess.check_call(f"cd {script_dir} && \
        {shutil.which('emacs')} -Q \
        --batch --eval \
        '(progn (load-file \"lisp.d/treesit-sources.el\") (load-file \"lisp.d/treesit-install.el\"))'", shell=True)

    if cfg["lsp"]["package"] == "eglot":
        # Install eglot booster
        unzip(download(emacs_lsp_booster_releases[platform_name()]), get_home_bin())

    with open(os.path.join(script_dir, 'config.el'), 'w') as f:
              f.write(f""";;; -*- lexical-binding: t -*-

;; <Server Enable>
;; t:   Auto start server if not already running
;; nil: Do nothing
(set-config-bool-var "start-server" {elisp_bool(cfg['server'])})

;; <LSP Client>
;; "eglot":      Use EGLOT (built-in)
;; "lsp-bridge": Use lsp-bridge
;;               Notice: Uses proprietary completion and diagnostics framework
(set-config-var "lsp" "{elisp_string(cfg["lsp"]["package"])}" "EMACS_LSP_SERVER")

;; <C/C++ LSP Server>
;; "ccls":   Use ccls
;; "clangd": Use clangd
(set-config-var "lsp/cpp-backend" "{elisp_string(cfg["lsp"]["c"]["server"])}")

;; <Python LSP Server>
;; "pylsp":   Use python-lsp-server (see https://github.com/python-lsp/python-lsp-server)
;; "pyright": Use pyright (see https://github.com/microsoft/pyright)
(set-config-var "lsp/py-backend" "{elisp_string(cfg["lsp"]["python"]["server"])}")

;; <LSP Enabled Modes>
;; "c":      Enable for c-mode/c-ts-mode
;; "cpp":    Enable for c++-mode/c++-ts-mode
;; "python": Enable for python-mode/python-ts-mode
;; "bash":   Enable for sh-mode/bash-ts-mode
(set-config-var "lsp/enable-modes" '({" ".join(get_enabled_modes("lsp"))}))

;; <LSP AutoFormat Enabled Modes>
;; "c":      Enable for c-mode/c-ts-mode
;; "cpp":    Enable for c++-mode/c++-ts-mode
;; "python": Enable for python-mode/python-ts-mode
;; "bash":   Enable for sh-mode/bash-ts-mode
(set-config-var "ide/autoformat-enable-modes" '({" ".join(get_enabled_modes("autoformat"))}))

;; <LSP Diagnostics Enabled Modes>
;; "c":      Enable for c-mode/c-ts-mode
;; "cpp":    Enable for c++-mode/c++-ts-mode
;; "python": Enable for python-mode/python-ts-mode
;; "bash":   Enable for sh-mode/bash-ts-mode
(set-config-var "ide/diagnostics-enable-modes" '({" ".join(get_enabled_modes("code-diag"))}))

;; <Programming Completion>
;; "corfu":   Enable corfu completion framework
;; "company": Enable company completion framework
(set-config-var "autocomplete" "{elisp_string(cfg['autocomplete'])}" "EMACS_COMPLETE")

;; <Diagnostics>
;; "flymake":  Use flymake as diagnostics framework
;; "flycheck": Use flycheck as diagnostics framework
(set-config-var "code-diag" "{elisp_string(cfg['code-diag']['package'])}")

;; <Mode-Line>
;; "doom": Use doom mode-line
;; "mood": Use mood mode-line
(set-config-var "modeline" "{elisp_string(cfg['ui']['modeline'])}")

;; <Theme>
;; "doom-xcode"
;; "doom-vibrant"
;; "doom-moonlight"
;; *: Use any available theme
(set-config-var "theme-name" "{cfg['ui']['theme']}" "EMACS_USE_THEME")

;; <Treesit>
(set-config-bool-var "use-treesit" {elisp_bool(cfg['treesit'])})

;; <UI>
(set-config-bool-var "use-idle-highlight" {elisp_bool(cfg['ui']['idle-highlight'])}) ;; Enable idle symbol highlight
(set-config-bool-var "use-visual-line-mode" {elisp_bool(cfg['ui']['visual-line-mode'])}) ;; Enable visual line mode
(set-config-bool-var "use-indent-guide" {elisp_bool(cfg['ui']['indent-guide'])}) ;; Enable indent-guide
(set-config-bool-var "use-which-function" {elisp_bool(cfg['ui']['which-function'])}) ;; Enable which-function in mode-line
(set-config-bool-var "use-diff-hl" {elisp_bool(cfg['ui']['diff-hl'])}) ;; Enable version-control diff in gutter

;; <General>
(set-config-var "basic-indent-offset" {cfg['indent']['basic-offset']}) ;; Default indent offset for all modes, nil to ignore this
(set-config-bool-var "use-indent-tabs" {elisp_bool(cfg['indent']['indent-tabs'])}) ;; Default value for indent-tabs-mode

;; <Copyright>
(set-config-var "auto-insert-copyright" "{cfg['copyright']['header']}" "EMACS_COPYRIGHT_COMPANY")
(set-config-var "auto-insert-name" "{cfg['copyright']['name']}" "EMACS_COPYRIGHT_NAME")

(provide 'my/config)
""")
