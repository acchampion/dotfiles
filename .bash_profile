# Commands to mount Android file image
######################################################################
# function mountAndroid  { hdiutil attach ~/android.dmg.sparseimage  -mountpoint /Volumes/android; }
# function mountCyanogen { hdiutil attach ~/cyanogenmod.dmg.sparseimage -mountpoint /Volumes/cyanogenmod; }
# function mountCmN1 { hdiutil attach ~/cyanogenmod_n1.dmg.sparseimage -mountpoint /Volumes/cyanogenmod_n1; }
######################################################################

# set the number of open files to be 8192
ulimit -S -n 8192

# Import bash configuration files
source ~/.aliases
source ~/.exports

# Uncomment these lines only if we need Perl or PHP
# source ~/perl5/perlbrew/etc/bashrc
# source ~/.phpbrew/bashrc

if [ -f $(brew --prefix)/share/bash-completion/bash_completion ]; then
    . $(brew --prefix)/share/bash-completion/bash_completion
fi

# Modify PROMPT_COMMAND each time a command is run.
# Source: https://gist.github.com/phette23/5270658
if [ $ITERM_SESSION_ID ]; then
  export PROMPT_COMMAND='echo -ne "\033];${PWD##*/}\007"; ':"$PROMPT_COMMAND";
fi

# Less Colors for Man Pages
export LESS_TERMCAP_mb=$'\E[01;31m' # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m' # begin bold
export LESS_TERMCAP_me=$'\E[0m' # end mode
export LESS_TERMCAP_se=$'\E[0m' # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m' # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

# Powerline
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
source /usr/local/lib/python3.7/site-packages/powerline/bindings/bash/powerline.sh
