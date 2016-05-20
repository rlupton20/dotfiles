#!/bin/bash

############################################################
# tmux session launcher  # # # # # # # # # # # # # # # # # #
############################################################

############################################################
# These variables configure a) the virtual (graphical)     #
# terminal in use, along with the appropriate switch       #
# to open the terminal running a particular                #
# application, and b) the incantation for dmenu to         #
# allow a particular tmux instance to be used. I use       #
# dmenu2 which allows more configuration of appearances;   #
# if this doesn't work, try setting SELECTOR to just       #
# dmenu and then customize.                                #
############################################################
XTERM="urxvt -e"
SELECTOR='dmenu -h 30 -y 350 -nb #24251f -nf #66d9ef -sb #272822 -sf #f92672 -fn LiberationMono-10:bold'

# Make a choice of an existing session, or naming a new session
CHOICE=$(tmux ls | awk '{ print $1 }' | sed 's\:.*\\' | $SELECTOR)

# If not choice is made exit cleanly
if [ "$CHOICE" = "" ]; then
    exit
fi

# Did we choose a session that already exists?
EXISTS=$(tmux ls | awk "/$CHOICE/ { print NR }")

# If not, make a new one with this name, and in the
# other case, attach to that session.
if [ "$EXISTS" = "" ]; then
    $XTERM tmux new -s $CHOICE
else
    $XTERM tmux attach-session -t $CHOICE
fi

