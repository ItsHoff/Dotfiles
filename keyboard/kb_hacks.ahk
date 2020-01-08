#NoEnv ; recommended for performance and compatibility with future autohotkey releases.
#UseHook
#InstallKeybdHook
#SingleInstance force

SendMode Input

;; deactivate capslock completely
SetCapslockState, AlwaysOff

;; vim navigation with caps
~Capslock & h:: Send {Blind}{Left}
~Capslock & l:: Send {Blind}{Right}
~Capslock & k:: Send {Blind}{Up}
~Capslock & j:: Send {Blind}{Down}

;; Map Caps-Enter to Ctrl-Alt-Enter
~Capslock & Enter:: Send {Blind} ^!{Enter}

~Capslock & BS:: Send {Blind}{Del}
; Plain Caps sends esc
~Capslock::
    KeyWait, Capslock
    if (A_PriorKey = "Capslock") {
        Send {Blind}{Esc}
    }
return

;; AppsKey can be mapped to hyper in emacs
#IfWinActive Emacs
~Capslock::
    Send {Blind}{AppsKey DownTemp}
    KeyWait, Capslock
    Send {Blind}{AppsKey Up}
    ; Plain press sends esc
    if (A_PriorKey = "Capslock") {
        Send {Blind}{Esc}
    }
return
