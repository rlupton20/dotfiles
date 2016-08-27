# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

# Export paths for local cabal (so I can use sandboxes)
export PATH=$HOME/.cabal/bin:$PATH

# Add a custom scripts folder to the path
export PATH=$HOME/.scripts/:$PATH

# Also find locally installed packages and add them to the PATH
# (pip for instance will install packages here)
export PATH=$HOME/.local/bin:$PATH

# Set terminal colours to 256
# export TERM=xterm-256color

export EDITOR="emacsclient -c -nw"

# An ls alias so we have colour
alias ls='ls --color'

# Alias top to htop
alias top='htop'

# Add stack auto-completion
eval "$(stack --bash-completion-script stack)"

# Define a custom function for making a folder and
# then cd-ing straight in to it
newdir() {
    mkdir $1
    cd $1
}

# Define a custom command to launch the default
# editor to edit a file
edit() {
    $EDITOR $1
}

# Define a utility function to rebuild the user
# nix environment
nix-user-rebuild() {
    nix-env -f ~/.nix-user/packages.nix -ir
}
