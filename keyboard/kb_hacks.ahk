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

;; Emacs specifics
#IfWinActive emacs
;; AppsKey can be mapped to hyper in emacs
~Capslock::
    Send {Blind}{AppsKey DownTemp}
    KeyWait, Capslock
    Send {Blind}{AppsKey Up}
    ; Plain press sends esc
    if (A_PriorKey = "Capslock") {
        Send {Blind}{Esc}
    }
return
