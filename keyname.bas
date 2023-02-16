Function GetName$ (KeyCode)
    If KeyCode = 100308 Then: Result$ = "LAlt"
    If KeyCode = 100307 Then: Result$ = "RAlt"
    If KeyCode = 100306 Then: Result$ = "LCtrl"
    If KeyCode = 100305 Then: Result$ = "RCtrl"
    If KeyCode = 100304 Then: Result$ = "LShift"
    If KeyCode = 100303 Then: Result$ = "RShift"
    If KeyCode = 100301 Then: Result$ = "CapsLock"
    If KeyCode = 100300 Then: Result$ = "NumLock"
    If KeyCode = 34304 Then: Result$ = "F12"
    If KeyCode = 34048 Then: Result$ = "F11"
    If KeyCode = 21248 Then: Result$ = "Delete"
    If KeyCode = 20992 Then: Result$ = "Insert"
    If KeyCode = 20736 Then: Result$ = "PageDown"
    If KeyCode = 20480 Then: Result$ = "Down"
    If KeyCode = 20224 Then: Result$ = "End"
    If KeyCode = 19712 Then: Result$ = "Right"
    If KeyCode = 19200 Then: Result$ = "Left"
    If KeyCode = 18688 Then: Result$ = "PageUp"
    If KeyCode = 18432 Then: Result$ = "Up"
    If KeyCode = 18176 Then: Result$ = "Home"
    If KeyCode = 17408 Then: Result$ = "F10"
    If KeyCode = 17152 Then: Result$ = "F9"
    If KeyCode = 16896 Then: Result$ = "F8"
    If KeyCode = 16640 Then: Result$ = "F7"
    If KeyCode = 16384 Then: Result$ = "F6"
    If KeyCode = 16128 Then: Result$ = "F5"
    If KeyCode = 15872 Then: Result$ = "F4"
    If KeyCode = 15616 Then: Result$ = "F3"
    If KeyCode = 15360 Then: Result$ = "F2"
    If KeyCode = 15104 Then: Result$ = "F1"
    If KeyCode >= 123 And KeyCode <= 126 Then: Result$ = Chr$(KeyCode)
    If KeyCode >= 97 And KeyCode <= 122 Then: Result$ = LCase$(Chr$(KeyCode)) 'a-z
    If KeyCode >= 91 And KeyCode <= 96 Then: Result$ = Chr$(KeyCode)
    If KeyCode >= 65 And KeyCode <= 90 Then: Result$ = UCase$(Chr$(KeyCode)) 'A-Z
    If KeyCode >= 33 And KeyCode <= 64 Then: Result$ = Chr$(KeyCode) ': ; < = > ? @ 0 - 9 ! " # $ % & ' ( ) * + , - . /
    If KeyCode = 32 Then: Result$ = "Space"
    If KeyCode = 27 Then: Result$ = "Escape"
    If KeyCode = 13 Then: Result$ = "Enter"
    If KeyCode = 9 Then: Result$ = "Tab"
    If KeyCode = 8 Then: Result$ = "BackSpace"
    GetName$ = Result$
End Function
