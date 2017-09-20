setxkbmap -option "caps:hyper"
xmodmap -e "remove Mod4 = Hyper_L" -e "add Mod3 = Hyper_L"
xcape -e "Hyper_L=Escape;Shift_L=parenleft;Shift_R=parenright"
