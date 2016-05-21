#!/bin/bash

################################################################
# tmux session launcher  # # # # # # # # # # # # # # # # # # # #
################################################################

################################################################
# The variables here select the programs which are used        #
# in the background.                                           #
#                                                              #
#   XTERM :        the X virtual terminal (with appropriate    #
#                  switches) to launch a new terminal process  #
#                  in X and execute a parameter.               #
#                                                              #
#   XSELECTOR :    The program to use for selection of the     #
#                  tmux seession to launch. This is set to     #
#                  dmenu2 which has some aesthetic             #
#                  improvements over ordinary dmenu. If using  #
#                  dmenu, perhaps start with just "dmenu" and  #
#                  customize from there.                       #
#                                                              #
#   TERMSELECTOR : Which selection program to use in a         #
#                  pure terminal environment. This uses        #
#                  percol (install with pip). Percol does      #
#                  not return unmatched user input, so         #
#                  (at the moment) the pure terminal           #
#                  version of this script will not launch      #
#                  a new session on a failed match :(.         #
#                                                              #
################################################################
XTERM="urxvt -e"
XSELECTOR="dmenu -h 30 -y 350 \
             -nb #24251f -nf #66d9ef -sb #272822 -sf #f92672 \
             -fn LiberationMono-10:bold"
TERMSELECTOR="percol"


################################################################
# We need to know whether we have an X display available, or   #
# whether we are in a pure terminal environment. We set        #
# some program variables accordingly.                          #
################################################################
if [ -z "$DISPLAY" ]; then
    SELECTOR="$TERMSELECTOR"
    RUN=""
else
    SELECTOR="$XSELECTOR"
    RUN="$XTERM"
fi

################################################################
# Now we are set up, we can make a choice and act accordingly. #
################################################################

# Make a choice of an existing session, or naming a new session
CHOICE=$(tmux ls | awk '{ print $1 }' | sed 's\:.*\\' | $SELECTOR)

# If no choice is made, exit now
if [ -z "$CHOICE" ]; then
    exit
fi

# Did we choose a session that already exists?
EXISTS=$(tmux ls | awk "/$CHOICE/ { print NR }")

# If not, make a new one with this name, and in the
# other case, attach to that session.
if [ -z "$EXISTS" ]; then
    $RUN tmux new -s $CHOICE
else
    $RUN tmux attach-session -t $CHOICE
fi

