export PS1="[\t][\u@\h \W]$ "

alias m="mate"

alias p="python"
alias ip="ipython --pylab"
alias ipq="ipython qtconsole --pylab"
export PYTHONPATH=/usr/local/lib/python:$PYTHONPATH
export PYTHONPATH=/Library/Python/2.7/site-packages:$PYTHONPATH
export PYTHONPATH=/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages:$PYTHONPATH
export PYTHONPATH=/Users/patokeefe/Projects/OpenSource/marlib:$PYTHONPATH

alias remotematlab='ssh -X pokeefe@login.engin.umich.edu'

alias 556="cd ~/Dropbox/Winter2011/556"
alias 564="cd ~/Dropbox/Winter2011/564"
alias proj="cd ~/Projects/SMRT\ Labs/Sparrow/exerciseAlpha"

alias o="open ."
alias po="popd"
alias pu="pushd"
alias d="dirs -v"
alias l="less "

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
alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n '

export EDITOR='open -a /Applications/Emacs.app '
export EDITOR='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient '
export CVSEDITOR=e
export SVN_EDITOR=e

export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"
