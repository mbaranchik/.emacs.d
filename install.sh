#!/bin/bash

EMACS=emacs
BASE_DIR=~/.emacs.d
CLEAN=

usage() {
    echo "Usage: $0 [--dir <git dir, default is $PWD>] [--emacs <executable, default is 'emacs'] [--clean (clean git dir)]"
}

while [[ $# -gt 0 ]]
do
    key="$1"
    case "$key" in
        -h|--help)
            usage
            exit 0
            ;;
        --clean)
            CLEAN=1
            shift
            ;;
        --emacs=*)
            EMACS="${key#*=}"
            shift
            ;;
        --emacs)
            EMACS="${2}"
            shift
            shift
            ;;
        --dir=*)
            BASE_DIR="${key#*=}"
            shift
            ;;
        --dir)
            BASE_DIR="${2}"
            shift
            shift
            ;;
        ?*)
            # Any character
            echo "Couldn't Idenitfy option $1"
            usage
            exit 2
            shift
            ;;
        *)
            # Will reach here at end of line
            break
            ;;
    esac
done

if [[ ! -d ${BASE_DIR} ]]; then
    echo "ERROR: Directory ${BASE_DIR} doesn't exist"
    exit 1
fi

if [[ ! -z ${CLEAN} ]]; then
    echo "INFO: Removing ignored and untracked files from git"
    git -C ${BASE_DIR} clean -fdX
fi

exec ${BASE_DIR}/update.sh --emacs ${EMACS} --dir ${BASE_DIR}
