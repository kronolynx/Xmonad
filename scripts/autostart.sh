#!/usr/bin/env bash
function run {
   if (command -v $1 >/dev/null 2>&1 && ! pgrep $1 >/dev/null 2>&1); then
     $@ &
   fi
}


# $HOME/.scripts/monitor.sh &

if [ "$DESKTOP_SESSION" == "xmonad" ]; then

    ## detect screen layout automatically
    autorandr -c &>/dev/null

    if (command -v start-pulseaudio-x11 && ! pgrep pulseaudio); then
        start-pulseaudio-x11 &
    fi

    if (command -v xfce4-power-manager && ! pgrep xfce4-power-man) ; then
        xfce4-power-manager &
    fi
    run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
    run /usr/lib/xfce4/notifyd/xfce4-notifyd;
    run thunar --daemon;
    run nm-applet;
    #run pasystray;
    run volumeicon

    feh --bg-fill ~/.wallpapers/river.jpg

    run xautolock -time 10 -locker lock -notify 30 -notifier "notify-send 'Locker' 'Locking screen in 30 seconds'";
elif [ "$DESKTOP_SESSION" == "xfce" ]; then 
    feh --bg-fill ~/.wallpapers/river.jpg
fi

# killall xembedsniproxy

# run greenclip daemon;
run picom --experimental-backends;


# set x cursor
# https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Setting_the_X_cursor
xsetroot -cursor_name left_ptr &
xset s 500 &

if [ -f ~/.scripts/autostart_work.sh ]; then
  ~/.scripts/autostart_work.sh
fi

keyboard # set keyboard

xbacklight = 20


# xautolock -time 10 -locker lock -notify 30 -notifier "notify-send 'Locker' 'Locking screen in 30 seconds'" -killtime 7 -killer "systemctl suspend" &
