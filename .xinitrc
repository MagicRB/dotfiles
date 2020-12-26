#!/usr/bin/env bash

dunst &
picom &
setxkbmap -layout us,sk -variant ,qwerty -option 'grp:lalt_lshift_toggle' -option ctrl:nocaps

export EXWM=1
exec emacs
