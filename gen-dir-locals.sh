#!/bin/bash

FIND=find
if [[ $(uname) == "Darwin" ]]; then
    FIND=gfind
fi

# Includes
TEMP_FILE=~/$$.temp
TEMP_FILE_2=${TEMP_FILE}_headers_list
$FIND -name "*.h" -exec sh -c "dirname {}" \; > ${TEMP_FILE}
cat  ${TEMP_FILE} | grep -v Tests | grep -v .cquery_cached_index | grep -v SystemC | grep -v virtualizer | sort | uniq > ${TEMP_FILE_2}

IFS=$'\n' read -d '' -r -a INC_LIST < ${TEMP_FILE_2}

rm ${TEMP_FILE} ${TEMP_FILE_2}

if [[ $3 == "C++" ]]; then
    C_STR="(c-mode . ((mode . c++)))"
fi


echo ";;; Directory Local Variables
;;; For more information see (info \"(emacs) Directory Variables\")
(${C_STR}
 (nil .
         ((eval . (let ((root (projectile-project-root)))
                    (setq-local company-c-headers-path-user
                        (list" > $1

for DIR in "${INC_LIST[@]}"; do
    echo "                                    (concat root \"$DIR\")" >> $1
done

echo "                                ))" >> $1
echo "
                    (setq-local company-clang-arguments
                                (list " >> $1

for DIR in "${INC_LIST[@]}"; do
    echo "                                    (concat \"-I\" root \"$DIR\")" >> $1
done

echo "                                ))" >> $1
echo "
                    (setq-local flycheck-clang-include-path
                                (list " >> $1

for DIR in "${INC_LIST[@]}"; do
    echo "                                    (concat root \"$DIR\")" >> $1
done

echo "                                ))" >> $1
echo "
                    (setq-local flycheck-gcc-include-path
                                (list " >> $1

for DIR in "${INC_LIST[@]}"; do
    echo "                                    (concat root \"$DIR\")" >> $1
done

echo "                                ))" >> $1

if [[ $2 == "Tabs" ]]; then
    TAB_STR="t"
else
    TAB_STR="nil"
fi

echo "
                    ))
       (indent-tabs-mode . ${TAB_STR})
       )
      ))" >> $1

echo ";;((nil . ((indent-tabs-mode . t)
;;         (fill-column . 80)))
;; (c-mode . ((c-file-style . \"BSD\")
;;            (subdirs . nil)))
;; (\"src/imported\"
;;  . ((nil . ((change-log-default-name
;;              . \"ChangeLog.local\"))))))" >> $1
