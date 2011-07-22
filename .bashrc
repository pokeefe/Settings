export PS1="[\t][\u@\h \W]$ "

alias m="mate"

alias p="python "
export PYTHONSTARTUP=~/.pythonrc

alias 556="cd ~/Dropbox/Winter2011/556"
alias 564="cd ~/Dropbox/Winter2011/564"
alias proj="cd ~/Projects/SMRT\ Labs/Sparrow/exerciseAlpha"

alias o="open ."
alias po="popd"
alias pu="pushd"
alias d="dirs -v"

alias g="grep --color='auto' -n"
alias ls='ls -G'

alias hgs="hg status"
alias hgf="hg fetch"
alias hgc="hg ci"
alias hgp="hg push"

alias gl="git log --pretty=format:\"%h %ad | %s%d [%an]\" --graph --date=short"

alias ga='git add '
alias gb='git branch '
alias gc='git commit'
alias gd='git diff'
alias go='git checkout '
alias gx='gitx --all'
alias gs='git status'
alias got='git '
alias get='git '

alias ll='ls -lah'
alias c='clear'

alias emacs='open -a /Applications/Emacs.app '
alias e=emacs

export EDITOR='open -a /Applications/Emacs.app '
export CVSEDITOR=e
export SVN_EDITOR=e

export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"

manp()
{
  man -t "${1}" | open -f -a Skim
}

catskim()
{
  cat "${1}" | enscript -p - | open -f -a Skim
}

#Color man pages
# export LESS_TERMCAP_mb=$'\E[01;31m'      # begin blinking
# export LESS_TERMCAP_md=$'\E[01;38;5;74m' # begin bold
# export LESS_TERMCAP_me=$'\E[0m'          # end mode
# export LESS_TERMCAP_se=$'\E[0m'          # end standout-mode                 
# export LESS_TERMCAP_so=$'\E[01;44;33m'   # begin standout-mode - info box                              
# export LESS_TERMCAP_ue=$'\E[0m'          # end underline
# export LESS_TERMCAP_us=$'\E[01;32m'      # begin underline