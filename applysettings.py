#!/usr/bin/env python

import os
import sys
import errno
import shutil

scriptDir           = sys.path[0]
homeDir             = os.getenv('HOME')

#These home dir files will be links to version controlled files
bashrc          = os.path.join(homeDir, '.bashrc')
bashProfile     = os.path.join(homeDir, '.bash_profile')
emacsd          = os.path.join(homeDir, '.emacs.d')
emacs           = os.path.join(homeDir, '.emacs')
hgrc            = os.path.join(homeDir, '.hgrc')
gitconfig       = os.path.join(homeDir, '.gitconfig')
ipythonconfig   = os.path.join(homeDir, '.ipython', 'profile_default', 'ipython_config.py')
ipythonqtconfig = os.path.join(homeDir, '.ipython', 'profile_default', 'ipython_qtconsole_config.py')
gitignoreglobal = os.path.join(homeDir, '.gitignore_global')

#These will be the actual version controlled files (the sources)
bashrcSource          = os.path.join(scriptDir, '.bashrc')
bashProfileSource     = os.path.join(scriptDir, '.bash_profile')
emacsdSource          = os.path.join(scriptDir, 'emacs-settings', '.emacs.d')
emacsSource           = os.path.join(scriptDir, 'emacs-settings', '.emacs')
hgrcSource            = os.path.join(scriptDir, '.hgrc')
gitconfigSource       = os.path.join(scriptDir, '.gitconfig')
ipythonconfigSource   = os.path.join(scriptDir, 'python-settings', 'ipython_config.py')
ipythonqtconfigSource = os.path.join(scriptDir, 'python-settings', 'ipython_qtconsole_config.py')
gitignoreglobalSource = os.path.join(scriptDir, '.gitignore_global')

#make a list of tupple pairs
sourceToDestination = { bashrcSource : bashrc,
                        bashProfileSource : bashProfile,
                        emacsdSource : emacsd,
                        emacsSource : emacs,
                        hgrcSource : hgrc,
                        gitconfigSource : gitconfig,
                        ipythonqtconfigSource : ipythonqtconfig,
                        ipythonconfigSource : ipythonconfig,
                        gitignoreglobalSource : gitignoreglobal }


def createLink(src, dest):
    os.symlink(src, dest)

def removeFile(path):
    try:
        os.remove(path)
    except OSError, err:
        if (err.errno == errno.EISDIR or
            err.errno == errno.ENOTEMPTY):
            printInfo('\tRemoving dir ' + path)
            shutil.rmtree(path)
        else:
            pass

def makeDir(directory):
    try:
        os.mkdir(directory)
    except OSError, err: #do nothing if directory exists
        if err.errno == errno.EEXIST:
            pass

def printInfo(string):
    print 'INFO: ' + string

def main():

    # for d in requiredDirs:
        # makeDir(d)

    for src, dest in sourceToDestination.items():

        printInfo("Creating link: " + dest)
        removeFile(dest)
        createLink(src, dest)
        
    return 0
    
if __name__ == '__main__':

    sys.exit(main())

