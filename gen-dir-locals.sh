#!/bin/bash

if [[ $3 == "C++" ]]; then
    C_STR="(c-mode . ((mode . c++)))"
fi

echo ";;; Directory Local Variables
;;; For more information see (info \"(emacs) Directory Variables\")
((nil . ((indent-tabs-mode . ${TAB_STR})))
${C_STR})" >> $1

