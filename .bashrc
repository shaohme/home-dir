# return if non-interactive
[[ $- == *i* ]] || return

# return if not executed in terminal
if ! [ -t 1 ] ; then
    return
fi

HISTCONTROL=ignoreboth
HISTSIZE=1000
HISTFILESIZE=2000

shopt -s histappend
shopt -s checkwinsize


if [ "$(id -u)" -eq 0 ]; then
  PS1='\w # '
else
  PS1='\w $ '
fi

if ! [ -z "${ANDROID_NDK_HOME}" ]; then
    LATEST_NDK_DIR=$(ls -td ${ANDROID_NDK_HOME}/*/ | head -1)
    if ! [ -z "${LATEST_NDK_DIR}" ] && ! [ "${LATEST_NDK_DIR}" = "/tmp" ]; then
        PATH="$PATH:${LATEST_NDK_DIR}"
    fi
fi

export GPG_TTY="$( tty )"

case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

if type "kubectl" > /dev/null; then
    source <(kubectl completion bash)
fi

if type "aws_completer" > /dev/null; then
    complete -C 'aws_completer' aws
fi

if [ -f $HOME/.local/bin/bashmarks.sh ]; then
    . $HOME/.local/bin/bashmarks.sh
fi

if [ -f /usr/share/virtualenvwrapper/virtualenvwrapper.sh ]; then
    . /usr/share/virtualenvwrapper/virtualenvwrapper.sh
fi

case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${PWD}\007"'

    # Show the currently running command in the terminal title:
    # http://www.davidpashley.com/articles/xterm-titles-with-bash.html
    show_command_in_title_bar()
    {
        case "$BASH_COMMAND" in
            *\033]0*)
                # The command is trying to set the title bar as well;
                # this is most likely the execution of $PROMPT_COMMAND.
                # In any case nested escapes confuse the terminal, so don't
                # output them.
                ;;
            *)
                echo -ne "\033]0;${BASH_COMMAND}\007"
                ;;
        esac
    }
    trap show_command_in_title_bar DEBUG
    ;;
*)
    ;;
esac


if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [ -f ${HOME}/.bash_aliases ]; then
    . ${HOME}/.bash_aliases
fi
