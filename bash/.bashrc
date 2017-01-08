# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# An ls alias so we have colour
alias ls='ls --color'

# Define a utility function to rebuild the user
# nix environment
nix-user-rebuild() {
    nix-env -f ~/.nix-user/packages.nix -ir
}
