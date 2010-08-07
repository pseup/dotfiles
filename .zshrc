

# ------------------------------
# Options {{{

autoload -Uz compinit
compinit

# History
HISTFILE=~/.zsh_history
HISTSIZE=2000
SAVEHIST=2000

# Prompt
PROMPT=$'[%{\e[34m%}%n%{\e[0m%}][%{\e[32m%}%~%{\e[0m%}] %{\e[34m%}>> %{\e[0m%}'

# UID specific cursor color
if [[ $TERM != "linux" ]]; then
  if [[ $UID -ge 1000 ]]; then # normal user
    precmd () { print -n "\033]12;2\007" }
  elif [[ $UID -eq 0 ]]; then # root
    precmd () { print -n "\033]12;1\007" }
  fi
fi

# Vars
export EDITOR="vim"
export PAGER="vimpager"
alias less="$PAGER"
export GREP_COLOR='34'

# LS Colors
[[ -r ~/.dircolors && -x /bin/dircolors ]] && eval $(dircolors -b ~/.dircolors)

# Setopts
setopt autocd
setopt autopushd pushdminus pushdsilent pushdtohome
setopt cdablevars
setopt nohup nocheckjobs # do not kill & spawned apps
setopt hist_ignore_dups hist_reduce_blanks
setopt nohashdirs nohashcmds

# }}}
# ------------------------------
# Completion {{{

zmodload zsh/complist
zstyle :compinstall filename '${HOME}/.zshrc'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
zstyle ':completion:*:*:killall:*' menu yes select
zstyle ':completion:*:killall:*'   force-list always

# }}}
# ------------------------------
# Aliases {{{
alias c='clear'
alias cp='cp -v'
alias grep='grep --color=auto'
alias ll='ls --color --group -lh'
alias ls='ls --color --group -F'
alias mv='mv -v'
alias rm='rm -v'

alias irc='weechat-curses'
alias vim='vim -p'
alias gvim='gvim -p'

alias emptytrash='rm -r ~/.local/share/Trash/*'

# Extensions
alias -s gif="feh"
alias -s jpg="feh"
alias -s png="feh"
alias -s avi="mplayer"
alias -s flv="mplayer"
alias -s mkv="mplayer"
alias -s mp4="mplayer"

# }}}
# ------------------------------
# Functions {{{

# Download and make AUR package
slurp() {
  cd ~/Packages && slurpy -d "$1" && cd "$1" && makepkg
}

# All in one archive extract
extract () {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2) tar xjf $1 ;;
      *.tar.gz) tar xzf $1 ;;
      *.bz2) bunzip2 $1 ;;
      *.rar) unrar x $1 ;;
      *.gz) gunzip $1 ;;
      *.tar) tar xf $1 ;;
      *.tbz2) tar xjf $1 ;;
      *.tgz) tar xzf $1 ;;
      *.zip) unzip "$1" ;;
      *.Z) uncompress $1 ;;
      *.7z) 7z x $1 ;;
      *) echo "'$1', filetype unsupported by extract()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# }}}
# ------------------------------
# Key Bindings {{{

bindkey -v
typeset -g -A key
#bindkey '\e[3~' delete-char
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
#bindkey '\e[2~' overwrite-mode
bindkey '^?' backward-delete-char
bindkey '^[[1~' beginning-of-line
bindkey '^[[5~' up-line-or-history
bindkey '^[[3~' delete-char
bindkey '^[[4~' end-of-line
bindkey '^[[6~' down-line-or-history
bindkey '^[[A' up-line-or-search
bindkey '^[[D' backward-char
bindkey '^[[B' down-line-or-search
bindkey '^[[C' forward-char
# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line

bindkey "^[Od" backward-word
bindkey "^[Oc" forward-word

# }}}
# ------------------------------
# TTY Colors {{{
if [[ $TERM == "linux" ]]; then
    echo -en "\e]P01e2320" #zen-black (norm. black)
    echo -en "\e]P8709080" #zen-bright-black (norm. darkgrey)
    echo -en "\e]P1705050" #zen-red (norm. darkred)
    echo -en "\e]P9dca3a3" #zen-bright-red (norm. red)
    echo -en "\e]P260b48a" #zen-green (norm. darkgreen)
    echo -en "\e]PAc3bf9f" #zen-bright-green (norm. green)
    echo -en "\e]P3dfaf8f" #zen-yellow (norm. brown)
    echo -en "\e]PBf0dfaf" #zen-bright-yellow (norm. yellow)
    echo -en "\e]P4506070" #zen-blue (norm. darkblue)
    echo -en "\e]PC94bff3" #zen-bright-blue (norm. blue)
    echo -en "\e]P5dc8cc3" #zen-purple (norm. darkmagenta)
    echo -en "\e]PDec93d3" #zen-bright-purple (norm. magenta)
    echo -en "\e]P68cd0d3" #zen-cyan (norm. darkcyan)
    echo -en "\e]PE93e0e3" #zen-bright-cyan (norm. cyan)
    echo -en "\e]P7dcdccc" #zen-white (norm. lightgrey)
    echo -en "\e]PFffffff" #zen-bright-white (norm. white)
fi
# }}}
