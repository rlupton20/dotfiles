#!/usr/bin/env bash

# DEPENDENCIES
# rofi, maim, slop, xclip

SCREENSHOT_DIR=~/.screenshots

# Options are separated by ;, with each field separated by :
# FIELD 0: name
# FIELD 1: maim switches
# FIELD 2: save to file
# FIELD 3: put on clipboard
OPTIONS='select:"-c 1,0,0,0.6 -s":y:y;window:"-c 1,0,0,0.6 -st 9999999":y:y;screen:"":y:y;clip:"-c 1,0,0,0.6 -s":n:y'

function get_maim_switches {
    echo $(echo $OPTIONS | tr ';' '\n' | grep ^${1} | awk -F ':' '{print $2}' | xargs echo)
}

function get_save {
    echo $(echo $OPTIONS | tr ';' '\n' | grep ^${1} | awk -F ':' '{print $3}' | xargs echo)
}

function get_clip {
    echo $(echo $OPTIONS | tr ';' '\n' | grep ^${1} | awk -F ':' '{print $4}' | xargs echo)
}


# Get user choice
CHOICE=$(echo $OPTIONS | tr ';' '\n' | awk -F ':' '{print $1}' | rofi -dmenu)

# Extract maim switches
MAIM_SWITCHES=$(get_maim_switches $CHOICE)

# Determine where to save the screenshot (possibly /dev/null)
TO_SAVE=$(get_save $CHOICE)
if [[ $TO_SAVE == y ]]
then
    SAVE_FILE=${SCREENSHOT_DIR}/$(date +%F-%T).png;
else
    SAVE_FILE=/dev/null
fi

# Determine whether to add to clipboard
TO_CLIP=$(get_clip $CHOICE)
if [[ $TO_CLIP == y ]]
then
    CLIP="-selection clipboard -t image/png -i"
else
    CLIP="-h" # Hack hack hack (pipe into help)
fi

# Do the screenshot
maim $MAIM_SWITCHES --format png /dev/stdout | tee $SAVE_FILE | xclip $CLIP
