# README #

My configuration of Emacs

### How do I get set up? ###

* To grab (in root user directory):

    ```
    cd ~
    git clone https://github.com/mbaranchik/emacs.init.git
    ```

* Install :

    ```
    $HOME/emacs.init/install.sh --dir $HOME/emacs.init
    ```

    ```
    # See more options:
    $HOME/emacs.init/install.sh --help
    ```

* Update Packages :

    ```
    git -C $HOME/emacs.init pull --rebase
    $HOME/emacs.init/update.sh --dir $HOME/emacs.init
    ```

    ```
    # See more options:
    $HOME/emacs.init/update.sh --help
    ```

### Tips ###

* Faster startup time:

    * **Recommended** See https://github.com/mbaranchik/emacs_pool.py for client as standalone emacs feeling

    * **Or** Edit using emacsclient to avoid long loading time, e.g. in cshell:

        * tcsh, add to ~/.cshrc (or other startup file):

            ```
            alias emacs 'emacsclient -c \!*'
            setenv ALTERNATE_EDITOR ""
            setenv VISUAL 'emacsclient -c'
            setenv EDITOR 'emacsclient -t'
            ```

        * bash/zsh, add to ~/.bashrc or ~/.zshrc:

            ```
            emacs() {
                emacsclient -c $@
            }
            export ALTERNATE_EDITOR=""
            export VISUAL='emacsclient -c'
            export EDITOR='emacsclient -t'
            ```

* Configuration can be altered by setting environment vars, see $HOME/emacs.init/config.el:

    ```
    ;; For example
    (set-config-var 'start-server t "EMACS_START_SERVER")
    ;; To disbale automatic server init, set EMACS_START_SERVER=n
    ;; etc...
    ```

