#!/usr/bin/env bash
function run {
   if (command -v $1 >/dev/null 2>&1 && ! pgrep $1 >/dev/null 2>&1); then
     $@ &
   fi
}

if (command -v start-pulseaudio-x11 && ! pgrep pulseaudio); then
    start-pulseaudio-x11 &
fi

if (command -v xfce4-power-manager && ! pgrep xfce4-power-man) ; then
    xfce4-power-manager &
fi

run polkit-gnome-authentication-agent-1

$HOME/.scripts/keyboard.sh &
$HOME/.scripts/monitor.sh &

run /usr/lib/xfce4/notifyd/xfce4-notifyd;
run greenclip daemon;
run thunar --daemon;
run feh --bg-scale ~/.wallpapers/girl-anime.jpg;
run nm-applet;
run volumeicon;
run picom -C -b;

# set x cursor
# https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Setting_the_X_cursor
xsetroot -cursor_name left_ptr &
xset s 500 &

if [ -f ~/.scripts/autostart_work.sh ]; then
  ~/.scripts/autostart_work.sh
fi

run xautolock -time 10 -locker lock -notify 30 -notifier "notify-send 'Locker' 'Locking screen in 30 seconds'";

# xautolock -time 10 -locker lock -notify 30 -notifier "notify-send 'Locker' 'Locking screen in 30 seconds'" -killtime 7 -killer "systemctl suspend" &
