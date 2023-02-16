'Window Changes:
'========================================================
Screen 13
_Title "QB64 Game: v0.6"
_FullScreen
_MouseHide
'Items:
'========================================================
Type Item
    Num As Integer
    Name As String
    Amount As Integer
    Category As String
End Type
ArraySize = 20
Dim Shared Items(ArraySize) As Item
Restore Items
Do
    Read Num, Name$, Amount, Category$
    Items(Num).Num = Num
    Items(Num).Name = Name$
    Items(Num).Amount = Amount
    Items(Num).Category = Category$
Loop Until Num = 0
Crafting:
'=========================================================
ArraySize = 20
Dim Shared Materials(ArraySize, ArraySize) As Integer
Dim Shared CraftName(ArraySize) As String
Dim Shared CraftItemNum(ArraySize) As Integer
Dim Shared M(ArraySize) As Integer
Restore Recipes
Do
    Read Num, Name$, ItemNum, M(1), M(2), M(3), M(4), M(5), M(6), M(7), M(8), M(9), M(10), M(11), M(12), M(13), M(14)
    CraftName(Num) = Name$
    CraftItemNum(Num) = ItemNum
    For A = 1 To UBound(M)
        Materials(Num, A) = M(A)
    Next A
Loop Until Num = 0
'Locations:
'=========================================================
Dim Shared MapChar(40, 25) As String * 1
Dim Shared MapColor(40, 25) As Integer
Dim Shared PlayerColor As Integer
Dim Shared PlayerName As String
Dim Shared PlayerX As Integer
Dim Shared PlayerY As Integer
Dim Shared MapX As Integer
Dim Shared MapY As Integer
Dim Shared MapZ As Integer
PlayerName = "Default"
PlayerColor = 15
PlayerX = 20
PlayerY = 12
MapX = 2
MapY = 2
MapZ = 2
'Files:
'=========================================================
Dim Shared File As Long
Dim Shared FileName As String
Const FilePath = "C:\Game\"
'Gathering:
'=========================================================
Dim Shared GatherTimer As Integer
GatherTimer = 50
'Keycodes:
'=========================================================
ArraySize = 20
Dim Shared Controls(ArraySize) As Integer
Const Up = 1
Const Left = 2
Const Down = 3
Const Right = 4
Const Escape = 5
Const Interact = 6
Const Inventory = 7
Const Crafting = 8
Controls(Up) = 18432
Controls(Left) = 19200
Controls(Down) = 20480
Controls(Right) = 19712
Controls(Escape) = 27
Controls(Interact) = 13
Controls(Inventory) = 105
Controls(Crafting) = 99
'Main Loop
'==========================================================
Do: _Limit 1000
    'Loop:
    LoopCounter = (LoopCounter + 1) Mod 1000
    'Controls:
    If LoopCounter Mod 100 = 0 Then
        If _KeyDown(Controls(Up)) Then: PlayerY = PlayerY - 1: Call Limits("Up") 'Move Up
        If _KeyDown(Controls(Left)) Then: PlayerX = PlayerX - 1: Call Limits("Left") 'Move Left
        If _KeyDown(Controls(Down)) Then: PlayerY = PlayerY + 1: Call Limits("Down") 'Move Down
        If _KeyDown(Controls(Right)) Then: PlayerX = PlayerX + 1: Call Limits("Right") 'Move Right
        'Gathering:
        If GatherTimer <= 50 Then: GatherTimer = GatherTimer + 1
        'Printing:
        Locate PlayerY, PlayerX: Print " ";
        Call SaveLoad("loadmap")
        Color PlayerColor: Locate PlayerY, PlayerX: Print "þ";
        _Display
    End If
    If _KeyDown(Controls(Escape)) Then: Call DisplayMenu("mainmenu") 'Main Menu
    If _KeyDown(Controls(Inventory)) Or _KeyDown(Asc(UCase$(Chr$(Controls(Inventory))))) Then: Call DisplayMenu("inventory") 'Inventory
    If _KeyDown(Controls(Interact)) And GatherTimer >= 50 Then: Call DisplayMenu("gatherresource") 'Gather Resource
    If _KeyDown(Controls(Crafting)) Or _KeyDown(Asc(UCase$(Chr$(Controls(Crafting))))) Then: Call DisplayMenu("crafting") 'Crafting
Loop
Items:
'=========================================================
Data 1,Wooden Pickaxe,0,craft
Data 2,Diamond,0,mining
Data 3,Leaves,0,felling
Data 4,Sticks,0,felling
Data 5,Stone,0,mining
Data 6,Fish,0,fishing
Data 7,Logs,0,felling
Data 8,Rope,0,resources
Data 9,Coal,0,mining
Data 10,Iron,0,mining
Data 11,Gold,0,mining
Data 12,Torch,0,tools
Data 13,Wooden Axe,0,craft
Data 14,Fishing Rod,0,craft
Data 0,,,
Recipes:
'=========================================================
Data 1,Wooden Pickaxe,1,0,0,0,3,0,0,0,2,0,0,0,0,0,0
Data 2,Fishing Rod,14,0,0,0,0,0,0,2,5,0,0,0,0,0,0
Data 3,Wooden Axe,13,0,0,0,4,0,0,0,3,0,0,0,0,0,0
Data 4,Torch,12,0,0,0,3,0,0,0,0,2,0,0,0,0,0
Data 5,Rope,8,0,0,2,0,0,0,0,0,0,0,0,0,0,0
Data 0,,,,,,,,,,,,,,,,
'Position Limits:
'=========================================================
Sub Limits (LastKey$)
    If PlayerY < 1 Then: MapZ = MapZ - 1: PlayerY = 25
    If PlayerX < 1 Then: MapX = MapX - 1: PlayerX = 40
    If PlayerX > 40 Then: MapX = MapX + 1: PlayerX = 1
    If PlayerY > 25 Then: MapZ = MapZ + 1: PlayerY = 1
    Call SaveLoad("loadmap")
    If MapColor(PlayerX, PlayerY) = 39 Then
        If MapY = 1 Then
            MapY = MapY + 1
            PlayerX = 16
            PlayerY = 10
        ElseIf MapY = 2 Then
            MapY = MapY - 1
            PlayerX = 20
            PlayerY = 7
        End If
        Exit Sub
    End If
    If MapColor(PlayerX, PlayerY) <> 0 And MapChar(PlayerX, PlayerY) <> "X" Then
        If LastKey$ = "Up" Then: PlayerY = PlayerY + 1
        If LastKey$ = "Left" Then: PlayerX = PlayerX + 1
        If LastKey$ = "Right" Then: PlayerX = PlayerX - 1
        If LastKey$ = "Down" Then: PlayerY = PlayerY - 1
    End If
    If MapX < 1 Then: MapX = 1: PlayerX = 1
    If MapX > 3 Then: MapX = 3: PlayerX = 40
    If MapZ < 1 Then: MapZ = 1: PlayerY = 1
    If MapZ > 3 Then: MapZ = 3: PlayerY = 25
End Sub
'Display Menus:
'==========================================================
Sub DisplayMenu (Menu$)
    _AutoDisplay
    TmpX = MapX
    TmpY = MapY
    TmpZ = MapZ
    MapY = 0
    If Menu$ = "mainmenu" Then 'Main Menu
        Do: Loop Until Not _KeyDown(Controls(Escape))
        MapX = 0: MapZ = 1
        Call SaveLoad("loadmap")
        Do: _Limit 100
            PSelected = Selected
            If _KeyDown(Controls(Up)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Up)): Selected = Selected - 1
            If _KeyDown(Controls(Down)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Down)): Selected = Selected + 1
            If _KeyDown(Controls(Interact)) Then
                Do: _Limit 100: Loop Until Not _KeyDown(Controls(Interact))
                If Selected = 0 Then: Exit Do
                If Selected = 1 Then
                    PlayerX = 20
                    PlayerY = 12
                    MapX = 2
                    MapY = 2
                    MapZ = 2
                    GatherTimer = 1
                    For A = 1 To UBound(Items)
                        Items(A).Amount = 0
                    Next A
                    Cls
                    Exit Sub
                End If
                If Selected = 2 Then: SaveLoad ("savegame")
                If Selected = 3 Then: SaveLoad ("loadgame")
                If Selected = 4 Then: DisplayMenu ("settings"): SaveLoad ("loadmap")
                If Selected = 5 Then: Cls: System
            End If
            If PSelected <> Selected Then: SaveLoad ("loadmap")
            If Selected < 0 Then: Selected = 5
            If Selected > 5 Then: Selected = 0
            Line (125, 119 + 8 * Selected)-(196, 119 + 8 * Selected), 7
        Loop
    ElseIf Menu$ = "inventory" Then 'Inventory
        Do: Loop Until Not _KeyDown(Controls(Inventory))
        Category$ = "felling"
        Selected = 0
        Bypass = 1
        MapX = 0: MapZ = 0
        SaveLoad ("loadmap")
        Do: _Limit 1000
            PSelected = Selected
            If _KeyDown(Controls(Escape)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Escape)): Exit Do
            If _KeyDown(Controls(Left)) Then: Selected = Selected - 1: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Left))
            If _KeyDown(Controls(Right)) Then: Selected = Selected + 1: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Right))
            If Selected < 0 Then: Selected = 4
            If Selected > 4 Then: Selected = 0
            If Selected <> PSelected Or Bypass = 1 Then
                Bypass = 0
                Color 15
                If Selected = 0 Then: Category$ = "felling"
                If Selected = 1 Then: Category$ = "mining"
                If Selected = 2 Then: Category$ = "fishing"
                If Selected = 3 Then: Category$ = "resources"
                If Selected = 4 Then: Category$ = "tools"
                Line (249, 192)-(310, 192), 7
                Locate 3, 26: Print TmpX: Locate 3, 32: Print TmpY: Locate 3, 38: Print TmpZ
                Locate 4, 26: Print PlayerX: Locate 4, 32: Print PlayerY
                B = 0
                For A = 1 To UBound(Items)
                    Locate 6 + B, 4: Print "                 ";
                    B = B + 1
                Next A
                Color 0: Locate 4, 4: Print Title$
                Color 15
                Title$ = UCase$(Left$(Category$, 1)) + Mid$(Category$, 2, Len(Category$)) + ":"
                Locate 4, 4: Print Title$
                Locate 5, 3: Print "®-------------¯"
                Locate 3, 26: Print TmpX: Locate 3, 32: Print TmpY: Locate 3, 38: Print TmpZ
                B = 0
                For A = 1 To UBound(Items)
                    If Items(A).Category = Category$ Then
                        Locate 6 + B, 4: Print Items(A).Name, Items(A).Amount
                        Locate 6 + B, 14: Print "-"
                        B = B + 1
                    End If
                Next A
            End If
            LoopCounter = (LoopCounter + 1) Mod 1000
            If LoopCounter Mod 100 = 0 Then
                If GatherTimer <= 50 Then: GatherTimer = GatherTimer + 1
                For A = 1 To Int(GatherTimer / 6.25)
                    Locate 8, 23 + A: Color 44: Print "Û";
                Next
            End If
        Loop Until _KeyDown(Controls(Interact))
        Do: Loop Until Not _KeyDown(Controls(Interact))
    ElseIf Menu$ = "gatherresource" Then 'Gather Resource
        Do: _Limit 100: Loop Until Not _KeyDown(Controls(Interact))
        For A = -1 To 1
            For B = -1 To 1
                If PlayerX - B <= 40 And PlayerY - A <= 25 Then
                    If MapChar(PlayerX - B, PlayerY - A) = "Û" Or MapChar(PlayerX - B, PlayerY - A) = "Ý" Or MapChar(PlayerX - B, PlayerY - A) = "Þ" Or MapChar(PlayerX - B, PlayerY - A) = "Ü" Or MapChar(PlayerX - B, PlayerY - A) = "ß" Then
                        Resource$ = "mining"
                        If MapColor(PlayerX - B, PlayerY - A) = 43 Then: Ore$ = "Gold"
                        If MapColor(PlayerX - B, PlayerY - A) = 15 Then: Ore$ = "Iron"
                        If MapColor(PlayerX - B, PlayerY - A) = 18 Then: Ore$ = "Coal"
                        If MapColor(PlayerX - B, PlayerY - A) = 77 Then: Ore$ = "Diamond"
                        If Ore$ = "" Then: Ore$ = "Stone"
                        Checked = -1
                    End If
                    If MapChar(PlayerX - B, PlayerY - A) = "°" Or MapChar(PlayerX - B, PlayerY - A) = "±" Then Resource$ = "felling": Checked = -1
                    If MapChar(PlayerX - B, PlayerY - A) = "÷" Or MapChar(PlayerX - B, PlayerY - A) = "²" Then Resource$ = "fishing": Checked = -1
                    If Checked = -1 Then Exit For: Exit For
                End If
            Next B
        Next A
        If Resource$ = "" Then: MapY = TmpY: Exit Sub
        MapX = 1: MapZ = 0
        Call SaveLoad("loadmap")
        Line (249, 192)-(310, 192), 7
        Title$ = UCase$(Left$(Resource$, 1)) + Mid$(Resource$, 2, Len(Resource$))
        If Title$ = "Mining" And Ore$ <> "" Then: Title$ = "Mining " + Ore$
        Color 15
        Locate 3, 1: Print Title$; ":"
        Dim GatheredAmount(UBound(Items)) As Integer
        Randomize (Timer)
        B = 0
        For A = 1 To UBound(Items)
            If Items(A).Category = Resource$ Then
                GatheredAmount(A) = Int(Rnd * 5) + 1
                If Resource$ = "mining" Then
                    If Items(A).Name = "Stone" And Ore$ <> "Stone" Or Ore$ = Items(A).Name Then
                        Locate 7 + B, 2: Print Items(A).Name, GatheredAmount(A)
                        Locate 7 + B, 12: Print "-"
                        B = B + 1
                        Items(A).Amount = Items(A).Amount + GatheredAmount(A)
                    End If
                Else
                    Locate 7 + B, 2: Print Items(A).Name, GatheredAmount(A)
                    Locate 7 + B, 12: Print "-"
                    B = B + 1
                    Items(A).Amount = Items(A).Amount + GatheredAmount(A)
                End If
            End If
        Next A
        Do: _Limit 100
            If _KeyDown(Controls(Escape)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Escape)): Exit Do
        Loop Until _KeyDown(Controls(Interact))
        Do: _Limit 100: Loop Until Not _KeyDown(Controls(Interact))
        GatherTimer = 1
    ElseIf Menu$ = "crafting" Then 'Crafting
        Do: Loop Until Not _KeyDown(Controls(Crafting))
        ArraySize = 20
        Dim NeededAmount(ArraySize) As Integer
        Dim NeededNum(ArraySize) As Integer
        MapX = 1: MapZ = 1
        Multiplier = 1
        Bypass = 1
        For A = 1 To UBound(CraftItemNum)
            If CraftName(A) <> "" Then: CraftableItems = CraftableItems + 1
        Next A
        Do: _Limit 100
            PSelected = Selected
            PMSelected = MSelected
            If _KeyDown(Controls(Escape)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Escape)): Exit Do
            If _KeyDown(Controls(Up)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Up)): Selected = Selected - 1
            If _KeyDown(Controls(Down)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Down)): Selected = Selected + 1
            If _KeyDown(Controls(Left)) And Selected <> CraftableItems Then: MSelected = MSelected - 1: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Left))
            If _KeyDown(Controls(Right)) And Selected <> CraftableItems Then: MSelected = MSelected + 1: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Right))
            If _KeyDown(Controls(Interact)) Then
                Do: _Limit 100: Loop Until Not _KeyDown(Controls(Interact))
                If Selected <> CraftableItems Then
                    B = 1
                    For A = 1 To UBound(M)
                        If Materials(Selected + 1, A) <> 0 Then
                            NeededAmount(B) = 0
                            NeededNum(B) = 0
                            NeededAmount(B) = Materials(Selected + 1, A)
                            NeededNum(B) = A
                            B = B + 1
                        End If
                    Next A
                    Check = 0
                    For A = 1 To 3
                        If Items(NeededNum(A)).Amount >= (NeededAmount(A) * Multiplier) Then: Check = Check + 1
                    Next A
                    If Check = 3 Then
                        For A = 1 To 3
                            Items(NeededNum(A)).Amount = Items(NeededNum(A)).Amount - (NeededAmount(A) * Multiplier)
                        Next A
                        Items(CraftItemNum(Selected + 1)).Amount = Items(CraftItemNum(Selected + 1)).Amount + Multiplier
                        Color 15: Locate 22, 5: Print "Item Crafted!"
                        _Delay 0.4
                        Color 0: Locate 22, 5: Print "Item Crafted!";
                    Else
                        Color 15: Locate 22, 1: Print "Insufficent Resources!"
                        _Delay 0.4
                        Color 0: Locate 22, 1: Print "Insufficent Resources!";
                    End If
                Else
                    Exit Do
                End If
            End If
            If PSelected <> Selected Or PMSelected <> MSelected Or Bypass = 1 Then
                Bypass = 0
                If Selected < 0 Then: Selected = 0
                If Selected > CraftableItems Then: Selected = CraftableItems
                SaveLoad ("loadmap")
                If MSelected < 0 Then: MSelected = 0
                If MSelected > 4 Then: MSelected = 4
                If MSelected = 0 Then: Multiplier = 1: Line (69 + (MSelected * 16), 192)-(82 + (MSelected * 16), 192), 7
                If MSelected = 1 Then: Multiplier = 5: Line (69 + (MSelected * 16), 192)-(82 + (MSelected * 16), 192), 7
                If MSelected = 2 Then: Multiplier = 10: Line (101, 192)-(122, 192), 7
                If MSelected = 3 Then: Multiplier = 25: Line (125, 192)-(146, 192), 7
                If MSelected = 4 Then: Multiplier = 50: Line (149, 192)-(170, 192), 7
                Color 15
                For A = 1 To UBound(CraftItemNum)
                    If CraftName(A) <> "" Then
                        Locate 3 + A, 2: Print CraftName(A)
                    End If
                Next A
                Locate 4, 24: Print CraftName(Selected + 1) + ":"
                Locate 21, 33: Print Multiplier
                B = 0
                For A = 1 To UBound(M)
                    If Materials(Selected + 1, A) <> 0 Then
                        Locate 6 + B, 25: Print Materials(Selected + 1, A) * Multiplier; Items(A).Name
                        B = B + 1
                    End If
                Next A
                If Selected = CraftableItems Then
                    Line (249, 192)-(310, 192), 7
                    For B = 1 To 18
                        For A = 1 To 22
                            Locate A, B + 22: Print " ";
                        Next A
                    Next B
                End If
                If Selected <> CraftableItems Then: Line (7, 31 + 8 * Selected)-(158, 31 + 8 * Selected), 7
            End If
        Loop
        Do: _Limit 100: Loop Until Not _KeyDown(Controls(Interact))
    ElseIf Menu$ = "settings" Then 'Settings
        TmpX = MapX
        TmpY = MapY
        TmpZ = MapZ
        MapX = 1
        MapY = 0
        MapZ = 2
        Bypass = 1
        Do: _Limit 100
            PSelected = Selected
            If _KeyDown(Controls(Up)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Up)): Selected = Selected - 1: SaveLoad ("loadmap")
            If _KeyDown(Controls(Down)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Down)): Selected = Selected + 1: SaveLoad ("loadmap")
            If _KeyDown(Controls(Left)) And Selected = 14 Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Left)): PlayerColor = PlayerColor - 1: Bypass = 1
            If _KeyDown(Controls(Right)) And Selected = 14 Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Right)): PlayerColor = PlayerColor + 1: Bypass = 1
            If _KeyDown(Controls(Interact)) Then
                Do: _Limit 100: Loop Until Not _KeyDown(Controls(Interact))
                Line (8, 39 + 8 * Selected)-(160, 39 + 8 * Selected), 0
                If Selected >= 0 And Selected < 8 Then
                    Do: _Limit 100: Loop Until InKey$ = ""
                    Do: _Limit 100: Key$ = InKey$: Loop Until Key$ <> ""
                    Controls(Selected + 1) = Asc(Key$)
                End If
                If Selected = 13 Then
                    Do: _Limit 100: Loop Until InKey$ = ""
                    Locate 18, 9: Print "          "
                    Locate 18, 9: Input ">", Temp$
                    Do: _Limit 100: Loop Until InKey$ = ""
                    Do: Loop Until Not _KeyDown(Controls(Interact))
                    PlayerName = Left$(Temp$, 10)
                End If
                Bypass = 1
                If Selected = 15 Then: Exit Do
            End If
            If PSelected <> Selected Or Bypass = 1 Then
                If Selected < 0 Then: Selected = 0
                If Selected = 8 Then: Selected = 13
                If Selected = 12 Then: Selected = 7
                If Selected > 15 Then: Selected = 15
                If PlayerColor < 1 Then: PlayerColor = 15
                If PlayerColor > 15 Then: PlayerColor = 1
                Bypass = 0
                SaveLoad ("loadmap")
                Color 15
                For A = 0 To UBound(Controls)
                    If Controls(A) <> 0 Then
                        If Len(GetName$(Controls(A))) = 1 Then
                            Locate 5 + (A - 1), 15: Print UCase$(GetName$(Controls(A)))
                        Else
                            Locate 5 + (A - 1), 15: Print GetName$(Controls(A))
                        End If
                    End If
                Next A
                Locate 18, 9: Print PlayerName
                Color PlayerColor
                If Selected <> 14 Then: Locate 19, 10: Print "þ"
                If Selected = 14 Then
                    Color 15: Locate 19, 10: Print "®"
                    Color PlayerColor: Locate 19, 12: Print "þ"
                    Color 15: Locate 19, 14: Print "¯"
                End If
                Color 15
                If Selected <> 15 Then: Line (8, 39 + 8 * Selected)-(160, 39 + 8 * Selected), 7: Line (249, 192)-(310, 192), 0
                If Selected = 15 Then: Line (249, 192)-(310, 192), 7: Line (8, 39 + 8 * PSelected)-(160, 39 + 8 * PSelected), 0
            End If
        Loop
        Do: _Limit 100: Loop Until Not _KeyDown(Controls(Interact))
    End If
    MapX = TmpX
    MapY = TmpY
    MapZ = TmpZ
End Sub
'Save & Load System:
'==========================================================
Sub SaveLoad (FileSystem$)
    If FileSystem$ = "loadmap" Then 'Load Map
        Dim Tmp(40, 25) As Integer
        File = FreeFile
        FileName = LTrim$((Str$(MapX) + (Str$(MapZ))) + (".map"))
        Open LTrim$((FilePath + "Map\") + (Mid$(Str$(MapY), 2, Len(Str$(MapY))) + "\") + (FileName)) For Binary As #File
        Get #File, , Tmp()
        Get #File, , MapColor()
        Close #File
        For X = 1 To 40
            For Y = 1 To 25
                MapChar(X, Y) = Chr$(Tmp(X, Y))
                Color MapColor(X, Y): Locate Y, X: Print MapChar(X, Y);
            Next Y
        Next X
    ElseIf FileSystem$ = "savegame" Then 'Save Game
        Beep
        FileName = "save.txt"
        File = FreeFile
        Open (FilePath + "Save\") + FileName For Output As #File
        For A = 1 To UBound(Items)
            Write #File, Items(A).Amount
        Next A
        Close #File
    ElseIf FileSystem$ = "loadgame" Then 'Load Game
        Beep
        FileName = "save.txt"
        File = FreeFile
        Open (FilePath + "Save\") + FileName For Input As #File
        For A = 1 To UBound(Items)
            Input #File, Amount
            Items(A).Amount = Amount
        Next A
        Close #File
    End If
End Sub
'$include: 'KeyName.bas'
