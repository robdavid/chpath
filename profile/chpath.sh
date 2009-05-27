#!/bin/sh
chpath () {
    SCRIPT=$(chpath-bin -s "$@")
    if [ -n "$SCRIPT" ] && [ -e $SCRIPT ]
    then
	. $SCRIPT
    fi
}
