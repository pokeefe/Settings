export PS1="[\t][\u@\h \W]$ "

alias m="mate"

alias p="python"
alias ip="ipython --pylab"
alias ipq="ipython qtconsole --pylab"
export PYTHONPATH=/usr/local/lib/python:$PYTHONPATH
export PYTHONPATH=/Library/Python/2.7/site-packages:$PYTHONPATH
export PYTHONPATH=/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages:$PYTHONPATH
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
# export PYTHONPATH=/Users/patokeefe/Projects/OpenSource/marlib:$PYTHONPATH
# export PYTHONPATH=/Users/patokeefe/EsslResearch/CVTestSuite:$PYTHONPATH
# export PYTHONPATH=/Users/patokeefe/EsslResearch/CVTestSuite/Utils:$PYTHONPATH
# export PYTHONPATH=/Users/patokeefe/EsslResearch/CVTestSuite/PoseEstimation:$PYTHONPATH

export OSG_DEPENDENCIES=/Libraries/osx-x64-release 

alias remotematlab='ssh -X pokeefe@login.engin.umich.edu'

alias 545="cd ~/Insync/patokeefe1\@gmail.com/545 "
alias os="cd ~/Projects/OpenSource "

alias o="open ."
alias po="popd"
alias pu="pushd"
alias d="dirs -v"
alias l="less "

alias multiepstopdf="find . -name \"*.eps\" -exec epstopdf {} \\;"

alias g="grep --color='auto' -n"
alias ls='ls -G'

alias hgs="hg status"
alias hgf="hg fetch"
alias hgc="hg ci"
alias hgp="hg push"
alias hgl="hg glog"

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
alias gp='git push'

alias ll='ls -lah'
alias c='clear'

alias ..='cd ..'
alias ....='cd ../..'
alias ......='cd ../../..'

alias emacs='open -a /Applications/Emacs.app '
alias e=emacs
alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n '

export EDITOR='open -a /Applications/Emacs.app '
export EDITOR='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient '
export CVSEDITOR=e
export SVN_EDITOR=e

export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"


export CLASSPATH=/Users/patokeefe/Projects/Archived/Graduate/568-ps1-ps2/java/lcm.jar
export CLASSPATH=/Users/patokeefe/Projects/Archived/Graduate/568-ps1-ps2/java/april.jar:$CLASSPATH
export DYLD_LIBRARY_PATH=/Users/patokeefe/Projects/Archived/Graduate/568-ps1-ps2/lib

# export CLASSPATH=$CLASSPATH:/usr/share/java/jogl.jar:/usr/share/java/gluegen-rt.jar:/usr/local/share/java/lcm.jar:$HOME/april/java/april.jar:./
# export CLASSPATH=/Users/patokeefe/Projects/OpenSource/april/java/april.jar:/usr/local/share/java/lcm.jar:./
# export DYLD_LIBRARY_PATH=/Users/patokeefe/Projects/OpenSource/april/lib
alias java='java -ea -server'

# export CLASSPATH=/Users/patokeefe/Downloads/eecs568/java/april.jar
# export CLASSPATH=/Users/patokeefe/Downloads/eecs568/java/lcm.jar:$CLASSPATH
# export DYLD_LIBRARY_PATH=/Users/patokeefe/Projects/Archived/Graduate/568-ps1-ps2/java/jni:$DYLD_LIBRARY_PATH
# export DYLD_LIBRARY_PATH=/Users/patokeefe/Downloads/eecs568/java/jni:$DYLD_LIBRARY_PATH

export OPENCV_TEST_DATA_PATH='/Users/patokeefe/OpenSource/ecto_opencv/src/rgbd/testdata'
