'Window Changes:
'========================================================
Screen 13
_Title "QB64 Game: v0.5"
_FullScreen
_MouseHide
'Items:
'========================================================
ArraySize = 10
Dim Shared ItemName(ArraySize) As String
Dim Shared ItemAmount(ArraySize) As Integer
Dim Shared ItemCategory(ArraySize) As String
Restore Items
Do
    Read Num, Name$, Amount, Category$
    ItemName(Num) = Name$
    ItemAmount(Num) = Amount
    ItemCategory(Num) = Category$
Loop Until Num = 0
'Crafting:
'=========================================================
ArraySize = 20
Dim Shared CraftMaterialAmount(ArraySize) As Integer
Dim Shared CraftMaterialNum(ArraySize) As Integer
Dim Shared CraftMaterialName(ArraySize) As String
Dim Shared CraftItemNum(ArraySize) As Integer
Dim Shared CraftItemName(ArraySize) As String
Restore Crafting
Do
    Read Num, Name$, NameNum, MatName$, MatNum, MatAmount
    CraftItemName(Num) = Name$
    CraftItemNum(Num) = NameNum
    CraftMaterialName(Num) = MatName$
    CraftMaterialNum(Num) = MatNum
    CraftMaterialAmount(Num) = MatAmount
Loop Until Num = 0
'Locations:
'=========================================================
Dim Shared MapChar(40, 25) As String * 1
Dim Shared MapColor(40, 25) As Integer
Dim Shared PlayerX As Integer
Dim Shared PlayerY As Integer
Dim Shared MapX As Integer
Dim Shared MapY As Integer
Dim Shared MapZ As Integer
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
Dim Shared GatherTimer(2) As Integer
GatherTimer(1) = 50
'Keycodes:
'=========================================================
Const Up = 18432
Const Left = 19200
Const Down = 20480
Const Right = 19712
Const Escape = 27
Const Enter = 13
Const Inventory = 105, UInventory = 73
Const Crafting = 99, UCrafting = 67
'Main Loop
'==========================================================
Do: _Limit 1000
    'Loop:
    LoopCounter = LoopCounter + 1
    If LoopCount = 10 Then: LoopCounter = 0

    'Controls:
    If LoopCounter Mod 100 = 0 Then
        If _KeyDown(Up) Then: PlayerY = PlayerY - 1: Call Limits("Up") 'Move Up
        If _KeyDown(Left) Then: PlayerX = PlayerX - 1: Call Limits("Left") 'Move Left
        If _KeyDown(Down) Then: PlayerY = PlayerY + 1: Call Limits("Down") 'Move Down
        If _KeyDown(Right) Then: PlayerX = PlayerX + 1: Call Limits("Right") 'Move Right
        Locate PlayerY, PlayerX: Print " ";
        'Gathering:
        If GatherTimer(1) > 50 Then: GatherTimer(2) = 1
        If GatherTimer(2) = 0 Then: GatherTimer(1) = GatherTimer(1) + 1
        'Printing:
        Call SaveLoad("loadmap")
        Color 15: Locate PlayerY, PlayerX: Print "þ";
        _Display
    End If

    If _KeyDown(Escape) Then: Call DisplayMenu("mainmenu") 'Main Menu
    If _KeyDown(Inventory) Or _KeyDown(UInventory) Then: Call DisplayMenu("inventory") 'Inventory
    If _KeyDown(Enter) And GatherTimer(2) = 1 Then: Call DisplayMenu("gatherresource") 'Gather Resource
    If _KeyDown(Crafting) Or _KeyDown(UCrafting) Then: Call DisplayMenu("crafting") 'Crafting
Loop
Items:
'=========================================================
Data 1,Diamond,0,mining
Data 2,Leaves,0,felling
Data 3,Sticks,0,felling
Data 4,Stone,0,mining
Data 5,Fish,0,fishing
Data 6,Logs,0,felling
Data 7,Rope,0,crafting
Data 8,Coal,0,mining
Data 9,Iron,0,mining
Data 10,Gold,0,mining
Data 0,,,
Crafting:
'=========================================================
Data 1,Rope,6,Leaves,2,2
Data 0,,,,,
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
        MapX = 0: MapZ = 1
        Call SaveLoad("loadmap")
        Do: _Limit 100
            PSelected = Selected
            If _KeyDown(Up) Then: Do: _Limit 100: Loop Until Not _KeyDown(Up): Selected = Selected - 1
            If _KeyDown(Down) Then: Do: _Limit 100: Loop Until Not _KeyDown(Down): Selected = Selected + 1
            If _KeyDown(Enter) Then
                Do: _Limit 100: Loop Until Not _KeyDown(Enter)
                If Selected = 0 Then: Exit Do
                If Selected = 1 Then
                    PlayerX = 20
                    PlayerY = 12
                    MapX = 2
                    MapY = 2
                    MapZ = 2
                    GatherTimer(2) = 1
                    For A = 1 To UBound(ItemAmount)
                        ItemAmount(A) = 0
                    Next A
                    Cls
                    Exit Sub
                End If
                If Selected = 2 Then: SaveLoad ("loadgame")
                If Selected = 3 Then: SaveLoad ("savegame")
                If Selected = 4 Then: Cls: System
            End If
            If PSelected <> Selected Then: Line (125, 95 + 8 * PSelected)-(196, 95 + 8 * PSelected), 0
            If Selected < 0 Then: Selected = 0
            If Selected > 4 Then: Selected = 4
            Line (125, 95 + 8 * Selected)-(196, 95 + 8 * Selected), 7
        Loop
    ElseIf Menu$ = "inventory" Then 'Inventory
        Category$ = "felling"
        Selected = 0
        First = 1
        MapX = 0: MapZ = 0
        SaveLoad ("loadmap")
        Do: _Limit 1000
            PSelected = Selected
            If _KeyDown(Left) Then: Selected = Selected - 1: Do: _Limit 100: Loop Until Not _KeyDown(Left)
            If _KeyDown(Right) Then: Selected = Selected + 1: Do: _Limit 100: Loop Until Not _KeyDown(Right)
            If Selected < 0 Then: Selected = 0
            If Selected > 2 Then: Selected = 2
            If Selected <> PSelected Or First = 1 Then
                First = 0
                Color 15
                If Selected = 0 Then: Category$ = "felling"
                If Selected = 1 Then: Category$ = "mining"
                If Selected = 2 Then: Category$ = "fishing"
                Line (249, 192)-(310, 192), 7
                Locate 3, 26: Print TmpX: Locate 3, 32: Print TmpY: Locate 3, 38: Print TmpZ
                Locate 4, 26: Print PlayerX: Locate 4, 32: Print PlayerY
                B = 0
                For A = 1 To UBound(ItemCategory)
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
                For A = 1 To UBound(ItemCategory)
                    If ItemCategory(A) = Category$ Then
                        Locate 6 + B, 4: Print ItemName(A), ItemAmount(A)
                        Locate 6 + B, 14: Print "-"
                        B = B + 1
                    End If
                Next A
            End If
            LoopCounter = LoopCounter + 1
            If LoopCount = 10 Then: LoopCounter = 0
            If LoopCounter Mod 100 = 0 Then
                If GatherTimer(1) > 50 Then: GatherTimer(2) = 1:
                If GatherTimer(2) = 0 Then: GatherTimer(1) = GatherTimer(1) + 1
                For A = 1 To Int(GatherTimer(1) / 6.25)
                    Locate 8, 23 + A: Color 44: Print "Û";
                Next
            End If
        Loop Until _KeyDown(Enter)
        Do: Loop Until Not _KeyDown(Enter)
    ElseIf Menu$ = "gatherresource" Then 'Gather Resource
        Do: _Limit 100: Loop Until Not _KeyDown(Enter)
        For A = -1 To 1
            For B = -1 To 1
                If PlayerX - B <= 40 And PlayerY - A <= 25 Then
                    If MapChar(PlayerX - B, PlayerY - A) = "Û" Or MapChar(PlayerX - B, PlayerY - A) = "Ý" Or MapChar(PlayerX - B, PlayerY - A) = "Þ" Or MapChar(PlayerX - B, PlayerY - A) = "Ü" Or MapChar(PlayerX - B, PlayerY - A) = "ß" Then
                        Resource$ = "mining"
                        If MapColor(PlayerX - B, PlayerY - A) = 43 Then: Ore$ = "Gold": Checked = -1
                        If MapColor(PlayerX - B, PlayerY - A) = 15 Then: Ore$ = "Iron": Checked = -1
                        If MapColor(PlayerX - B, PlayerY - A) = 18 Then: Ore$ = "Coal": Checked = -1
                        If MapColor(PlayerX - B, PlayerY - A) = 77 Then: Ore$ = "Diamond": Checked = -1
                        If Ore$ = "" Then: Ore$ = "Stone"
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
        Dim GatheredAmount(UBound(ItemAmount)) As Integer
        Randomize (Timer)
        B = 0
        For A = 1 To UBound(ItemCategory)
            If ItemCategory(A) = Resource$ Then
                GatheredAmount(A) = Int(Rnd * 5) + 1
                If Resource$ = "mining" Then
                    If ItemName(A) = "Stone" And Ore$ <> "Stone" Then
                        Locate 7 + B, 2: Print ItemName(A), GatheredAmount(A)
                        Locate 7 + B, 12: Print "-"
                        B = B + 1
                        ItemAmount(A) = ItemAmount(A) + GatheredAmount(A)
                    End If
                    If Ore$ = ItemName(A) Then
                        Locate 7 + B, 2: Print ItemName(A), GatheredAmount(A)
                        Locate 7 + B, 12: Print "-"
                        B = B + 1
                        ItemAmount(A) = ItemAmount(A) + GatheredAmount(A)
                    End If
                Else
                    Locate 7 + B, 2: Print ItemName(A), GatheredAmount(A)
                    Locate 7 + B, 12: Print "-"
                    B = B + 1
                    ItemAmount(A) = ItemAmount(A) + GatheredAmount(A)
                End If
            End If
        Next A
        Do: _Limit 100: Loop Until _KeyDown(Enter)
        Do: _Limit 100: Loop Until Not _KeyDown(Enter)
        GatherTimer(1) = 0
        GatherTimer(2) = 0
    ElseIf Menu$ = "crafting" Then 'Crafting
        Cls
        Color 15
        Locate 1, 1: Print "Crafting:"
        Locate 2, 1: Print "----------"
        Locate 24, 32: Print "Continue";
        For A = 1 To UBound(CraftItemNum)
            If CraftItemName(A) <> "" Then: CraftableItems = CraftableItems + 1
        Next A
        Do: _Limit 100
            PSelected = Selected
            If _KeyDown(Up) Then: Do: _Limit 100: Loop Until Not _KeyDown(Up): Selected = Selected - 1
            If _KeyDown(Down) Then: Do: _Limit 100: Loop Until Not _KeyDown(Down): Selected = Selected + 1
            If _KeyDown(Enter) Then
                Do: _Limit 100: Loop Until Not _KeyDown(Enter)
                If Selected <> CraftableItems Then
                    If ItemAmount(CraftMaterialNum(Selected + 1)) >= CraftMaterialAmount(Selected + 1) Then
                        ItemAmount(CraftItemNum(Selected + 1)) = ItemAmount(CraftItemNum(Selected + 1)) + 1
                        ItemAmount(CraftMaterialNum(Selected + 1)) = ItemAmount(CraftMaterialNum(Selected + 1)) - CraftMaterialAmount(Selected + 1)
                        Locate 24, 2: Print "1 - "; CraftItemName(Selected + 1); " Crafted!";
                        _Delay 0.4
                    Else
                        Locate 24, 2: Print "Insufficent Resources!";
                        _Delay 0.4
                    End If
                    Locate 24, 2: Print "                        ";
                End If
                If Selected = CraftableItems Then: Exit Do
            End If
            For A = 1 To UBound(CraftItemNum)
                If CraftItemName(A) <> "" Then
                    Locate 3 + A, 2: Print CraftItemName(A)
                    Locate 3 + A, 10: Print "-"; CraftMaterialAmount(A); CraftMaterialName(A)
                End If
            Next A
            If PSelected <> Selected Then: Line (7, 31 + 8 * PSelected)-(158, 31 + 8 * PSelected), 0
            If Selected < 0 Then: Selected = 0
            If Selected = CraftableItems Then: Line (249, 192)-(310, 192), 7: Line (7, 31 + 8 * Selected)-(158, 31 + 8 * Selected), 0
            If Selected > CraftableItems Then: Selected = CraftableItems
            If Selected <> CraftableItems Then: Line (7, 31 + 8 * Selected)-(158, 31 + 8 * Selected), 7: Line (249, 192)-(310, 192), 0
        Loop
        Do: _Limit 100: Loop Until Not _KeyDown(Enter)
        MapY = 2
        Exit Sub
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
        FileName = LTrim$((Str$(MapX) + Str$(MapZ)) + ".map")
        Open LTrim$((FilePath + "Map\") + Mid$(Str$(MapY), 2, Len(Str$(MapY))) + "\") + FileName For Binary As #File
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
        For A = 1 To UBound(ItemAmount)
            Write #File, ItemAmount(A)
        Next A
        Close #File
    ElseIf FileSystem$ = "loadgame" Then 'Load Game
        Beep
        FileName = "save.txt"
        File = FreeFile
        Open (FilePath + "Save\") + FileName For Input As #File
        For A = 1 To UBound(ItemAmount)
            Input #File, Amount
            ItemAmount(A) = Amount
        Next A
        Close #File
    End If
End Sub
