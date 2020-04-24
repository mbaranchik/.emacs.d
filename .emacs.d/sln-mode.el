;;; sln-mode.el --- sample major mode for editing SLN. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2015, by you

;; Author: your name ( your email )
;; Version: 2.0.13
;; Created: 26 Jun 2015
;; Keywords: languages
;; Homepage: http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; short description here

;; full doc on how to use here


;;; Code:

;; define several category of keywords
;; (setq sln-keywords '("break" "default" "do" "else" "for" "if" "return" "state" "while") )
;; (setq sln-types '("float" "integer" "key" "list" "rotation" "string" "vector"))
;; (setq sln-constants '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
;; (setq sln-events '("at_rot_target" "at_target" "attach"))
;; (setq sln-functions '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList"))

;; ;; generate regex string for each category of keywords
;; (setq sln-keywords-regexp (regexp-opt sln-keywords 'words))
;; (setq sln-type-regexp (regexp-opt sln-types 'words))
;; (setq sln-constant-regexp (regexp-opt sln-constants 'words))
;; (setq sln-event-regexp (regexp-opt sln-events 'words))
;; (setq sln-functions-regexp (regexp-opt sln-functions 'words))

;; ;; create the list for font-lock.
;; ;; each category of keyword is given a particular face
;; (setq sln-font-lock-keywords
;;       `(
;;         (,sln-type-regexp . font-lock-type-face)
;;         (,sln-constant-regexp . font-lock-constant-face)
;;         (,sln-event-regexp . font-lock-builtin-face)
;;         (,sln-functions-regexp . font-lock-function-name-face)
;;         (,sln-keywords-regexp . font-lock-keyword-face)
;;         ;; note: order above matters, because once colored, that part won't change.
;;         ;; in general, longer words first
;;         ))


;; =================================================
;; SLN FONTLOCK SUPPORT
;; =================================================

;; (defconst sln-font-lock-keywords nil
;;   "Default highlighting for Sln mode.")

;; (defconst sln-font-lock-keywords-1 nil
;;   "Subdued level highlighting for Sln mode.")

;; (defconst sln-font-lock-keywords-2 nil
;;   "Medium level highlighting for Sln mode.
;; See also `sln-font-lock-extra-types'.")

;; (defconst sln-font-lock-keywords-3 nil
;;   "Gaudy level highlighting for Sln mode.
;; See also `sln-font-lock-extra-types'.")

(defvar sln-types-keywords_
       '(
         "bit" "bits" "bool" "byte" "bytes" "file" "pointer" "string" "uint"
         "channel" "lock" "memory"
         "it" "index" "result" "on" "off"
         "TRUE" "FALSE" "UNDEF" "NULL"
         )
      )


(defvar sln-function-keywords_
  (concat "\\<\\("
          (regexp-opt
           '(
             "abs" "add" "add0" "all_indices" "and_all" "append" "appendf" "apply"
             "average" "bin" "bitwise_op" "check" "check_test" "clear" "count"
             "csv_check_empty" "csv_check_exists" "csv_check_col_exists" "csv_column" "csv_to_table"
             "crc_32" "crc_32_flip" "crc_8" "date_time" "dec" "deep_compare"
             "deep_compare_physical" "deep_copy" "delete" "div_round_up"
             "dut_error" "enabled" "even" "exists" "extract" "fast_delete" "field" "finalize"
             "finalize_test" "first" "first_index" "get_config" "get_indicies"
             "get_keep" "get_symbol" "has" "hex" "ilog10" "ilog2" "init" "insert"
             "ipow" "is_a_permutation" "is_empty" "isqrt" "key" "key_exists"
             "key_index" "last" "last_index" "max" "max_index" "max_value" "min"
             "min_index" "min_value" "odd" "or_all" "out" "outf" "output_from"
             "output_from_check" "pack" "pop" "pop0" "post_generate" "pre_generate"
             "product" "push" "push0" "quit" "quote" "read_config" "resize"
             "reverse" "run" "run_test" "set_config" "set_keep" "setup" "size"
             "sort" "sort_by_field" "spawn" "spawn_check" "sln" "split"
             "start_test" "stop_run" "str_chop" "str_empty" "str_exactly"
             "str_expand_dots" "str_insensitive" "str_join" "str_len" "str_lower"
             "str_match" "str_pad" "str_replace" "str_split" "str_split_all"
             "str_sub" "str_upper" "sum" "system" "top" "top0" "to_string" "unique"
             "unpack" "write_config" "pre_gen_code"
             )
           'symbols)
          "\\)\\>")
  )



(defvar sln-keywords_
       '(
         ;; the non-type keywords start here
         "a" "add" "address_all" "also" "always" "and" "as" "as_a" "basic"
         "before" "break" "by" "C" "call_case" "case" "change" "check"
         "clock" "code" "compute" "computed" "continue" "cover" "cross" "cycle"
         "cycles" "default" "define" "delay" "delayed" "detach" "do" "DOECHO"
         "down" "each" "ECHO" "edges" "else" "emit" "empty" "end" "ended" "error"
         "event" "events" "exec" "exit" "expect" "extend" "fail" "fall" "fill"
         "finish" "first" "for" "force" "forever" "from" "gen" "generated" "high" "idle"
         "if" "illegal" "import" "in" "initial" "initialize" "int" "intersects"
         "is" "item" "keep" "keeping" "key" "kind" "length" "like" "line"
         "list" "log" "low" "matches" "matching" "max" "me" "min" "mode" "nand"
         "negedge" "network" "new" "next" "no" "non" "nor" "normal" "not"
         "nxor" "of" "on" "only" "or" "others" "out" "outf" "packing" "pass"
         "por" "posedge" "print" "range" "ranges" "read" "release"
         "report" "return" "reverse" "rise" "routine" "sample" "script"
         "select" "sequence" "soft" "start" "step" "struct" "sync" "task"
         "terminal" "testgroup" "text" "that" "then" "time" "to"
         "traceable" "transition" "true" "try" "type" "undefined" "unit"
         "until" "untraceable" "using" "value" "var" "verilog" "vhdl" "wait"
         "when" "while" "with" "within" "write" "write_once" "xor"
         ;; sln specific keywords
         "abstract" "action" "bind" "body" "child" "component" "compound"
         "conditional" "constraint" "declaration" "default" "definition" "disable"
         "enabled" "exec" "file" "message" "platform" "run_end" "run_start"
         "scenario_start" "schedule" "table" "trace_table" "token" "wait_condition" "yield"
         ;; common operator names
         "chain" "if" "plan" "serial" "try" "group" "select" "select_consistent"
         "select_consistent_parallel" "select_multiset" "select_multiset_consistent"
         "select_multiset_consistent_weighted" "filter_inconsistent" "fill" "replicate"
         "replicate_foreach_comp" "resource_partition" "parallel" "permute" "solve_barrier"
         "runtime_do_while" "runtime_if" "runtime_loop" "runtime_select" "runtime_repeat" "runtime_while"
         ;; older common operator names - to be removed in the future
         "as_is" "choose" "choose_legal" "choose_legal_same_time" "choose_multiset"
         "choose_multiset_legal" "filter_illegal" "pack_fill" "repeat" "repeat_on_comps"
         "same_time" "run_dispatcher" "run_serial" "runtime_dispatcher"
         )
       )


(defconst sln-variable-definition-regexp
 "\\(\\w+\\)\\s-*:\\s-*\\(\\w+\\)"
 "Regexp that identifies variable definitions (arg 1)")


(defun sln-within-ex-comment ()
  "Return point if within ex-comment region, else nil."
  (save-excursion
    (if (re-search-backward "\\('>\\)\\|\\(<'\\)" nil 'm)
	(if (match-beginning 2) nil
	  (point))
      (point))))

(defun sln-within-exec-block ()
  "Return point if within ex-comment region, else nil."
  (save-excursion
    (if (re-search-backward "\\('>\\)\\|\\(<'\\)" nil 'm)
	(if (match-beginning 2) nil
	  (point))
      (point))))

(defun sln-start-comment (limit)
  "Return point before comment ends if before LIMIT, else nil."
  (when (re-search-forward "^[ \t]*\'\>" limit t)
    (match-beginning 0)))

(defun sln-end-comment (limit)
  "Return point after comment starts if before LIMIT, else nil."
  (re-search-forward "^[ \t]*\<'" limit t))

(defun sln-match-ex-code-regions (limit)
  "Match a non code block, setting match-data and returning t, else nil."
  (when (< (point) limit)
    (let ((start (or (sln-within-ex-comment)
		     (sln-start-comment limit)))
	  (case-fold-search t))
      (when start
	(let ((end (or (sln-end-comment limit) limit)))
	  (set-match-data (list start end))
	  (goto-char end))))))


(defvar sln-font-lock-keywords-1
              (list
               ;; Additionally fontify functions
               (cons (concat "\\<\\(" sln-function-keywords_ "\\)\\>")
                     'font-lock-function-name-face)
               )
      )

(defvar sln-font-lock-keywords-2
              `(
                ;; Fontify user defined methods
		 ;;; Comment this out as it is horrendously expensive:
                ;;(cons sln-function-name-regexp '(1 font-lock-function-name-face))
                ;; Fontify user defined variables
                (,sln-variable-definition-regexp . font-lock-variable-name-face)

                )
      )

(defvar sln-font-lock-keywords-3
              (list
               ;; Fontify as comments things at the start and end of file
               ;;((sln-match-ex-code-regions 0) . font-lock-comment-face)
               '(sln-match-ex-code-regions (0 'font-lock-comment-face t))
               )
      )

(defvar sln-font-lock-defaults
      `((
       ;; Fontify comments
       ("\\(//.*$\\)\\|\\(--.*$\\)" . font-lock-comment-face)
       ("//.*$" . font-lock-comment-face)
       ;; Fontify all types
       (,(regexp-opt sln-types-keywords_ 'symbols) . font-lock-type-face)
       ;; Fontify macros
       ("#\\(ifn?def\\|else\\)" . font-lock-type-face)
       ;; Fontify all builtin keywords
       (,(regexp-opt sln-keywords_ 'symbols) . font-lock-keyword-face)
       ;; Fontiy keywords that include spaces (!)
       (,(concat
              "\\<"
              "\\(all[ \t]+of\\)\\|"
              "\\(check[ \t]+that\\)\\|"
              "\\(each[ \t]+file\\)\\|"
              "\\(each[ \t]+line\\)\\|"
              "\\(first[ \t]+of\\)\\|"
              "\\(in[ \t]+range\\)\\|"
              "\\(is[ \t]+a\\)\\|"
              "\\(is[ \t]+also\\)\\|"
              "\\(is[ \t]+c[ \t]+routine\\)\\|"
              "\\(is[ \t]+empty\\)\\|"
              "\\(is[ \t]+first\\)\\|"
              "\\(is[ \t]+inline\\)\\|"
              "\\(is[ \t]+not[ \t]+a\\)\\|"
              "\\(is[ \t]+not[ \t]+empty\\)\\|"
              "\\(is[ \t]+only\\)\\|"
              "\\(is[ \t]+undefined\\)\\|"
              "\\(state[ \t]+machine\\)\\|"
              "\\(using[ \t]+index\\)\\|"
              "\\(verilog[ \t]+code\\)\\|"
              "\\(verilog[ \t]+function\\)\\|"
              "\\(verilog[ \t]+import\\)\\|"
              "\\(verilog[ \t]+simulator\\)\\|"
              "\\(vhdl[ \t]+simulator\\)"
              "\\>"
              )
        . font-lock-keyword-face)
       ;;sln-font-lock-keywords-1
       ;;sln-font-lock-keywords-2
       ;;sln-font-lock-keywords-3
       ;; (,((list
       ;;         ;; Additionally fontify functions
       ;;     ((concat "\\<\\(" sln-function-keywords_"\\)\\>") . font-lock-function-name-face)
       ;;     )))
       ;; (,sln-variable-definition-regexp . font-lock-variable-name-face)
       ;; (list
       ;;  '(sln-match-ex-code-regions (0 'font-lock-comment-face t))
       ;;  )
       )))

;; (defun sln-disable-rainbow-ex-comments (depth match loc)
;;   (unless (sln-within-ex-comment)
;;     (rainbow-delimiters-default-pick-face depth match loc)))

;; (setq rainbow-delimiters-pick-face-function #'sln-disable-rainbow-ex-comments)


(define-derived-mode sln-mode prog-mode "SLN script"
  "SLN mode is a major mode for editing SLN files"
  ;; you again used quote when you had '((sln-hilite))
  ;; I just updated the variable to have the proper nesting (as noted above)
  ;; and use the value directly here
  (setq font-lock-defaults sln-font-lock-defaults)

  ;; when there's an override, use it
  ;; otherwise it gets the default value
  ;; (when sln-tab-width
  ;;   (setq tab-width sln-tab-width))

  ;; for comments
  ;; overriding these vars gets you what (I think) you want
  ;; they're made buffer local when you set them
  (setq comment-start "\\<(>')?\\>")
  (setq comment-end "\\<(<')?\\>")

  ;;(modify-syntax-entry ?# "< b" sln-mode-syntax-table)
  ;;(modify-syntax-entry ?\n "> b" sln-mode-syntax-table)

  ;; Note that there's no need to manually call `sln-mode-hook'; `define-derived-mode'
  ;; will define `sln-mode' to call it properly right before it exits
  )

(provide 'sln-mode)
;;; sln-mode ends here
