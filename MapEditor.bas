Screen 13
_FullScreen

Dim KeyPress As String
Dim KeyAscii As Long
Dim CurrentColor As Integer
Dim CurrentChar As String
Dim X As Integer
Dim Y As Integer
Dim Toggle As Integer
Dim Shared MapChar(40, 25) As String * 1
Dim Shared MapColor(40, 25) As Integer
Dim Null As String * 1
Null = Chr$(0)

Dim File As Long
Dim FileName As String
Const FilePath = "C:\Game\Map\"

Const XMax = 40
Const YMax = 25

Const keyAltC = 11776
Const keyAltQ = 4096
Const keyNum1 = 20224
Const keyNum2 = 20480
Const keyNum3 = 20736
Const keyNum4 = 19200
Const keyNum5 = 19456
Const keyNum6 = 19712
Const keyNum7 = 18176
Const keyNum8 = 18432
Const keyNum9 = 18688
Const keyESC = 27

Const keyAltA = 7680
Const keyAltH = 8960
Const keyAltS = 7936
Const keyAltL = 9728

Const keySpace = 32
Const keyEnter = 13
Const keyHome = 18176
Const keyCtrlHome = 30464
Const keyAltHome = 38656

Const keyInsert = 20992
Const keyCtrlInsert = 37376
Const keyAltInsert = 41472
Const keyDelete = 21248
Const keyCtrlDelete = 37632
Const keyAltDelete = 41728
Const keyBack = 8
Const keyAltBack = 3584

Dim TileChars(2, 255) As Integer
Dim TileColors(2, 255) As Integer

For n = 0 To 255
    TileChars(1, n) = n
    TileChars(2, n) = 7
Next n

For n = 0 To UBound(TileColors, 2)
    TileColors(1, n) = 254
    TileColors(2, n) = n
Next n

CurrentColor = 7
X = 20
Y = 12
Toggle = 1
Do
    Toggle = Toggle * -1
    T1 = Timer

    KeyPress = InKey$
    If Len(KeyPress) > 0 Then
        KeyAscii = ASCII_16(KeyPress)
    Else
        KeyAscii = 0
    End If

    X(1) = X
    Y(1) = Y

    If KeyAscii = keyAltH Then
        DisplayHelp
        DrawMap
    ElseIf KeyAscii = keyAltS Then
        File = FreeFile

        Cls
        Print: Print
        Input "File name to save > ", FileName
        FileName = FileName + ".map"

        Dim Tmp(XMax, YMax) As Integer
        For X = 1 To XMax
            For Y = 1 To YMax
                Tmp(X, Y) = Asc(MapChar(X, Y))
            Next Y
        Next X

        Print "Will write to: "; FilePath + FileName
        Print "File Number: "; File
        Do: Loop Until InKey$ <> ""

        Open FilePath + FileName For Binary As #File
        Put #File, , Tmp()
        Put #File, , MapColor()
        Close #File

        Print: Print "Map saved to file: "; FileName
        Do: Loop Until InKey$ <> ""
        DrawMap
        X = 20
        Y = 12

    ElseIf KeyAscii = keyAltL Then
        File = FreeFile

        Cls
        Print: Print
        Input "File name to load > ", FileName
        FileName = FileName + ".map"

        Open FilePath + FileName For Binary As #File
        Get #File, , Tmp()
        Get #File, , MapColor()
        Close #File

        For X = 1 To XMax
            For Y = 1 To YMax
                MapChar(X, Y) = Chr$(Tmp(X, Y))
            Next Y
        Next X

        Print: Print "Map loaded from file: "; FileName
        Do: Loop Until InKey$ <> ""
        DrawMap
        X = 20
        Y = 12

    ElseIf KeyAscii = keyEnter Then
        X = 1
        Y = Y + 1
    ElseIf KeyAscii = keyDelete Then
        MapChar(X, Y) = Null
        MapColor(X, Y) = 0
        X = X + 1
    ElseIf KeyAscii = keyBack Then
        MapChar(X, Y) = Null
        MapColor(X, Y) = 0
        X = X - 1
    ElseIf KeyAscii = keyCtrlDelete Then
        If MapChar(X, Y) <> Null Then: CurrentChar = MapChar(X, Y)
        If MapColor(X, Y) <> 0 Then: CurrentColor = MapColor(X, Y)
    ElseIf KeyAscii = keyInsert Then
        MapChar(X, Y) = CurrentChar
        MapColor(X, Y) = CurrentColor
        X = X + 1
    ElseIf KeyAscii = keyCtrlInsert Then
        MapColor(X, Y) = CurrentColor
        X = X + 1
    ElseIf KeyAscii = keyAltA Then
        CurrentChar = Chr$(TileSelect(TileChars(), Asc(CurrentChar)))
        DrawMap
    ElseIf KeyAscii = keyAltC Then
        CurrentColor = TileSelect(TileColors(), CurrentColor)
        DrawMap
    Else
        If KeyAscii = keyAltBack Then
            KeyPress = Chr$(keyBack)
            KeyAscii = keyBack
        End If
        If Len(KeyPress) = 1 Then
            MapChar(X, Y) = KeyPress
            MapColor(X, Y) = CurrentColor
            X = X + 1
            CurrentChar = KeyPress
        End If
    End If

    If (KeyAscii = keyNum1) Or (KeyAscii = keyNum2) Or (KeyAscii = keyNum3) Then: Y = Y + 1
    If (KeyAscii = keyNum3) Or (KeyAscii = keyNum6) Or (KeyAscii = keyNum9) Then: X = X + 1
    If (KeyAscii = keyNum9) Or (KeyAscii = keyNum8) Or (KeyAscii = keyNum7) Then: Y = Y - 1
    If (KeyAscii = keyNum7) Or (KeyAscii = keyNum4) Or (KeyAscii = keyNum1) Then: X = X - 1
    If (KeyAscii = keyNum5) Then
        X = 20
        Y = 12
    End If

    If KeyAscii = keyAltHome Then
        X = 1
        Y = 1
    ElseIf KeyAscii = keyCtrlHome Then
        Locate 11, 1
        Print Tab(5); "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
        Print Tab(5); "³ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ³"
        Print Tab(5); "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        Locate 12, 6
        Color 14
        Input "Erase entire map (Y/N) ", A$
        If UCase$(Left$(A$, 1)) = "Y" Then
            For R = 1 To 40
                For c = 1 To 25
                    MapChar(c, R) = Null
                    MapColor(c, R) = 0
                Next c
            Next R
        End If
        X = 20
        Y = 12
        CurrentColor = 7
        CurrentChar = ""
        DrawMap
    End If

    If X < 1 Then: X = 1
    If X > 40 Then: X = 40
    If Y < 1 Then: Y = 1
    If Y > 25 Then: Y = 25

    Locate Y, X
    If Int(T1 * 5) Mod 2 = 0 Then
        If (MapChar(X, Y) = " ") Or (MapChar(X, Y) = Chr$(0)) Then
            Color 15
            Print "þ";
        Else
            Color MapColor(X, Y)
            Print MapChar(X, Y);
        End If
    Else
        Color CurrentColor
        Print "þ";
    End If

    Color 7

    If X <> X(1) Or Y <> Y(1) Then
        Locate Y(1), X(1)
        Color MapColor(X(1), Y(1))
        Print MapChar(X(1), Y(1));
    End If
Loop

Sub DisplayHelp
    Cls
    Screen 12
    Color 3
    Locate 3, 1
    Print Tab(10); "ALT+S"; Tab(30); "Save file": Print
    Print Tab(10); "ALT+L"; Tab(30); "Load file": Print
    Print Tab(10); "Alt+H"; Tab(30); "Open Help": Print
    Print Tab(10); "ALT+A"; Tab(30); "Select character": Print
    Print Tab(10); "ALT+C"; Tab(30); "Select Color": Print
    Print Tab(10); "Delete"; Tab(30); "Delete to the right": Print
    Print Tab(10); "BackSpace"; Tab(30); "Delete to the left": Print
    Print Tab(10); "ALT+BackSpace"; Tab(30); "Paste original BackSpace character": Print
    Print Tab(10); "CTRL+Delete"; Tab(30); "Copy character & color": Print
    Print Tab(10); "Insert"; Tab(30); "Past current character & color": Print
    Print Tab(10); "CTRL+Insert"; Tab(30); "Overwrite with current color": Print
    Print Tab(10); "Enter"; Tab(30); "Go to Next line": Print
    Print Tab(10); "ALT+Home"; Tab(30); "Go to start": Print
    Print Tab(10); "CTRL+Home"; Tab(30); "Clear Map": Print

    Color 7
    Print Tab(20); "( any key to continue )"
    Do: Loop Until InKey$ <> ""
    Screen 13
End Sub

Sub DrawMap
    For r = 1 To 25
        Locate r, 1
        For C = 1 To 40
            Color MapColor(C, r)
            Print MapChar(C, r);
        Next C
    Next r
    Color 7
End Sub

Function ASCII_16 (K As String)
    Dim A As Long
    A = Asc(Mid$(K, 1, 1))
    If Len(K) = 2 Then
        A = A + (Asc(Mid$(K, 2, 1)) * 256)
    End If
    ASCII_16 = A
End Function

Function TileSelect (T() As Integer, Default As Integer)
    Dim Index As Integer
    Dim Col As Integer
    Dim Row As Integer
    Dim Rows As Integer

    Dim SelectIndex As Integer
    Dim SelectRow As Integer
    Dim SelectCol As Integer
    Dim SelectChar As Integer
    Dim SelectKey As String
    Dim SelectAscii As Long

    Rows = Int(UBound(T, 2) / 16)

    Do
        Cls
        Color 3
        Print Tab(4); "Enter to select, Escape to cancel": Print
        Index = 0
        Row = 0

        _ControlChr Off

        Do While Index <= UBound(T, 2)
            Col = 0

            If Row = SelectRow Then
                Color 14
                Print Tab((SelectCol * 2) + 4); "ÚÄ¿"
            End If

            Print Tab(4);

            Do While (Col < 16) And (Index <= UBound(T, 2))

                If Row = SelectRow And Col = SelectCol Then
                    Color 14
                    Print "³";
                ElseIf Row = SelectRow And (Col - 1) = SelectCol Then
                    Print "";
                Else
                    Print " ";
                End If

                Color T(2, Index)
                Print Chr$(T(1, Index));

                If Row = SelectRow And Col = SelectCol Then
                    Color 14
                    Print "³";
                End If

                Col = Col + 1
                Index = Index + 1
            Loop
            Print

            If Row = SelectRow Then
                Color 14
                Print Tab((SelectCol * 2) + 4); "ÀÄÙ"
            End If


            Row = Row + 1
        Loop
        Color 3
        Print: Print Tab(5); "["; SelectIndex; "]"
        Do
            SelectKey = InKey$
        Loop Until SelectKey <> ""

        SelectAscii = ASCII_16(SelectKey)
        If SelectAscii = keyNum8 Then
            SelectRow = SelectRow - 1
        ElseIf SelectAscii = keyNum2 Then
            SelectRow = SelectRow + 1
        ElseIf SelectAscii = keyNum4 Then
            SelectCol = SelectCol - 1
        ElseIf SelectAscii = keyNum6 Then
            SelectCol = SelectCol + 1
        End If

        If SelectCol < 0 Then SelectCol = 0
        If SelectCol > 15 Then SelectCol = 15
        If SelectRow < 0 Then SelectRow = 0
        If SelectRow > Rows Then SelectRow = Rows

        SelectIndex = (SelectRow * 16) + SelectCol

    Loop Until SelectAscii = keyEnter Or SelectAscii = keyESC

    If (SelectAscii = keyEnter) And (SelectIndex <= UBound(T, 2)) Then
        TileSelect = SelectIndex
    Else
        TileSelect = Default
    End If

    _ControlChr On

End Function








