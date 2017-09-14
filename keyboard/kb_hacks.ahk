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

;; Make plain shifts insert brackets
~LShift::
    Send {LShift Down}
    KeyWait, LShift, T0.5
    if !ErrorLevel and (A_PriorKey = "LShift") {
        Send {(}
    } else if !ErrorLevel and (A_PriorKey = "RShift") {
        ;; Tried to add both quickly so do that
        Send {(}
        Send {)}
    } else {
        KeyWait, LShift
    }
    Send {LShift Up}
return

~RShift::
    Send {RShift Down}
    KeyWait, RShift, T0.5
    ;; Send ) if we don't timeout otherwise wait some more
    if !ErrorLevel and (A_PriorKey = "RShift") {
        Send {)}
    } else {
        KeyWait, RShift
    }
    Send {RShift Up}
return
