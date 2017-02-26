# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
else
	PS1='[\u@\h \W]\$ '
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# An ls alias so we have colour
alias ls='ls --color'

# Alias top to htop
alias htop='TERM=xterm htop'
alias top='htop'

# Make emacs connect to emacs daemon
alias emacs='emacsclient -c'

# Use virsh-sys to refer to system qemu:/// setup
alias virsh-sys='virsh -c qemu:///system'
alias vkill='virsh -c qemu:///system destroy'
alias vstart='virsh -c qemu:///system start'

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
