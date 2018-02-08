#NoEnv ; recommended for performance and compatibility with future autohotkey releases.
#UseHook
#InstallKeybdHook
#SingleInstance force

SendMode Input

;; Media keys
NumpadAdd & Numpad5:: Send {Media_Play_Pause}
NumpadAdd & Numpad4:: Send {Media_Prev}
NumpadAdd & Numpad6:: Send {Media_Next}

; Change audio source
F24::
    Run, c:\windows\system32\control.exe mmsys.cpl
    WinWaitActive, Sound
    WinSet, AlwaysOnTop, On, Sound
    Send, {DOWN 5}
    Sleep, 100
    ControlGet, MyState, Enabled, , Button2
    If (MyState = 1){
        ControlClick, Button2, A
    } Else {
        Send, {DOWN}
        ControlClick, Button2, A
    }
    WinClose, Sound
return

; Volume control for spotify
#IfWinExists ahk_class SpotifyMainWindow
NumpadAdd & Numpad8::
{
    ControlSend, ahk_parent, ^{Up}, ahk_class SpotifyMainWindow
    return
}

NumpadAdd & Numpad2::
{
    ControlSend, ahk_parent, ^{Down}, ahk_class SpotifyMainWindow
    return
}