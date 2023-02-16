'Window Changes:
'========================================================
Screen 13
_Title "The Wilderness: v0.7"
_FullScreen
_MouseHide
'Items:
'========================================================
Type Item
    Num As Integer
    Name As String
    Amount As Integer
    Category As String
    Strength As Integer
End Type
ArraySize = 20
Dim Shared Items(ArraySize) As Item
Restore Items
Do
    Read Num, Name$, Amount, Category$, Strength
    Items(Num).Num = Num
    Items(Num).Name = Name$
    Items(Num).Amount = Amount
    Items(Num).Category = Category$
    Items(Num).Strength = Strength
Loop Until Num = 0
'Crafting:
'=========================================================
Type Recipe
    ItemNum As Integer
    Num1 As Integer
    Num2 As Integer
    Name As String
    Amount1 As Integer
    Amount2 As Integer
    Strength As Integer
End Type
ArraySize = 20
Dim Shared Recipes(ArraySize) As Recipe
Dim Shared TmpAmount(ArraySize) As Integer
Dim Shared TmpName(ArraySize) As String
Restore Recipes
Do
    Read Num, ItemNum, Level, Name$, TmpName(1), TmpAmount(1), TmpName(2), TmpAmount(2)
    Recipes(Num).ItemNum = ItemNum
    Recipes(Num).Strength = Level
    Recipes(Num).Name = Name$
    Recipes(Num).Num1 = GetItemNum(TmpName(1))
    Recipes(Num).Num2 = GetItemNum(TmpName(2))
    Recipes(Num).Amount1 = TmpAmount(1)
    Recipes(Num).Amount2 = TmpAmount(2)
Loop Until Num = 0
'Player, And Locations:
'=========================================================
Dim Shared ToolLevels(3) As Integer
Dim Shared MapChar(40, 25) As String * 1
Dim Shared MapColor(40, 25) As Integer
Dim Shared GatherTimer As Integer
Dim Shared PlayerColor As Integer
Dim Shared PlayerName As String
Dim Shared Developer As String
Dim Shared PlayerX As Integer
Dim Shared PlayerZ As Integer
Dim Shared MapX As Integer
Dim Shared MapY As Integer
Dim Shared MapZ As Integer
Dim Shared Timer1 As Long
Dim Shared GatheredAmount(UBound(Items)) As Integer
Dim Shared GatheredNum(UBound(Items)) As Integer
Dim Shared GatheredItems As Integer
Const PickaxeLevel = 1
Const AxeLevel = 2
Const FishingRodLevel = 3
'Files:
'=========================================================
Dim Shared File As Long
Dim Shared FileName As String
Const FilePath = "C:\Game\"
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
Const Commands = 9
Call InitializeGame
'Main Loop
'==========================================================
Do: _Limit 1000
    LoopCounter = (LoopCounter + 1) Mod 1000
    If LoopCounter Mod 100 = 0 Then
        If _KeyDown(Controls(Up)) Then: PlayerZ = PlayerZ - 1: Call Limits("Up"): 'Move Up
        If _KeyDown(Controls(Left)) Then: PlayerX = PlayerX - 1: Call Limits("Left"): 'Move Left
        If _KeyDown(Controls(Down)) Then: PlayerZ = PlayerZ + 1: Call Limits("Down"): 'Move Down
        If _KeyDown(Controls(Right)) Then: PlayerX = PlayerX + 1: Call Limits("Right"): 'Move Right
        DrawMap
        Color PlayerColor: Locate PlayerZ, PlayerX: Print "þ";
        If Timer <= (Timer1 + 1) Then
            Call DisplayGathered(GatheredItems, GatheredAmount(), GatheredNum())
        End If
        If GatherTimer <= 50 Then
            GatherTimer = GatherTimer + 1
            Line ((((PlayerX - 1) * 8) - 1), ((PlayerZ - 1) * 8) - 1)-((((PlayerX - 1) * 8) + (GatherTimer / 7)), ((PlayerZ - 1) * 8) - 1), 44
        End If
        _Display
    End If
    If _KeyDown(Controls(Escape)) Then: Call DisplayMenu("mainmenu") 'Main Menu
    If _KeyDown(Controls(Commands)) And Developer = "true" Then: Call CMDStuff 'Commands
    If _KeyDown(Controls(Inventory)) Or _KeyDown(Asc(UCase$(Chr$(Controls(Inventory))))) Then: Call DisplayMenu("inventory") 'Inventory
    If _KeyDown(Controls(Interact)) And GatherTimer >= 50 Then: Call DisplayMenu("gatherresource") 'Gather Resource
    If _KeyDown(Controls(Crafting)) Or _KeyDown(Asc(UCase$(Chr$(Controls(Crafting))))) Then: Call DisplayMenu("crafting") 'Crafting
Loop
Items:
'=========================================================
'Num,Name,Amount,Category,Strength
Data 1,Diamond,0,mining,3
Data 2,Leaves,0,felling,0
Data 3,Sticks,0,felling,0
Data 4,Stone,0,mining,1
Data 5,Fish,0,fishing,1
Data 6,Logs,0,felling,1
Data 7,Rope,0,resource,0
Data 8,Coal,0,mining,2
Data 9,Iron,0,mining,2
Data 10,Gold,0,mining,3
Data 11,Torch,0,tool,0
Data 12,Wooden Pickaxe,0,tool,0
Data 13,Stone Pickaxe,0,tool,0
Data 14,Iron Pickaxe,0,tool,0
Data 15,Diamond Pickaxe,0,tool,0
Data 16,Wooden Axe,0,tool,0
Data 17,Stone Axe,0,tool,0
Data 18,Iron Axe,0,tool,0
Data 19,Diamond Axe,0,tool,0
Data 20,Fishing Rod,0,tool,0
Data 0,,,,
Recipes:
'=========================================================
'Num,ItemNum,Level,Name,Resources:(Name, Amount)
Data 1,12,1,"Wooden Pickaxe","logs",2,"rope",2
Data 2,13,2,"Stone Pickaxe","stone",4,"logs",3
Data 3,14,3,"Iron Pickaxe","rope",3,"iron",4
Data 4,15,4,"Diamond Pickaxe","diamond",4,"iron",2
Data 5,16,1,"Wooden Axe","sticks",4,"rope",3
Data 6,17,2,"Stone Axe","stone",3,"rope",3
Data 7,18,3,"Iron Axe","logs",4,"iron",3
Data 8,19,4,"Diamond Axe","diamond",3,"iron",3
Data 9,20,0,"Fishing Rod","logs",2,"rope",5
Data 10,11,0,"Torch","sticks",3,"coal",2
Data 11,7,0,"Rope","leaves",2,,
Data 0,,,,,,,
'Position Limits:
'=========================================================
Sub Limits (LastKey$)
    If PlayerZ < 1 Then: MapZ = MapZ - 1: PlayerZ = 25: Call SaveLoad("loadmap")
    If PlayerX < 1 Then: MapX = MapX - 1: PlayerX = 40: Call SaveLoad("loadmap")
    If PlayerX > 40 Then: MapX = MapX + 1: PlayerX = 1: Call SaveLoad("loadmap")
    If PlayerZ > 25 Then: MapZ = MapZ + 1: PlayerZ = 1: Call SaveLoad("loadmap")
    If MapColor(PlayerX, PlayerZ) = 39 Then
        If MapY = 1 Then
            _Delay 0.5
            MapY = MapY + 1
            PlayerX = 16
            PlayerZ = 10
        ElseIf MapY = 2 Then
            _Delay 0.5
            MapY = MapY - 1
            PlayerX = 20
            PlayerZ = 7
        End If
        Call SaveLoad("loadmap")
        Call DrawMap
        Exit Sub
    End If
    If MapColor(PlayerX, PlayerZ) <> 0 And MapChar(PlayerX, PlayerZ) <> "X" Then
        If LastKey$ = "Up" Then: PlayerZ = PlayerZ + 1
        If LastKey$ = "Left" Then: PlayerX = PlayerX + 1
        If LastKey$ = "Right" Then: PlayerX = PlayerX - 1
        If LastKey$ = "Down" Then: PlayerZ = PlayerZ - 1
    End If
    If MapX < 1 Then: MapX = 1: PlayerX = 1
    If MapX > 3 Then: MapX = 3: PlayerX = 40
    If MapZ < 1 Then: MapZ = 1: PlayerZ = 1
    If MapZ > 3 Then: MapZ = 3: PlayerZ = 25
End Sub
'Display Menus:
'==========================================================
Sub DisplayMenu (Menu$)
    _AutoDisplay
    TmpX = MapX
    TmpY = MapY
    TmpZ = MapZ
    MapY = 0
    If Menu$ = "mainmenu" Then
        Do: Loop Until Not _KeyDown(Controls(Escape))
        MapX = 0: MapZ = 1
        Bypass = 1
        Call SaveLoad("loadmap")
        Do: _Limit 100
            PSelected = Selected
            If _KeyDown(Controls(Up)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Up)): Selected = Selected - 1
            If _KeyDown(Controls(Down)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Down)): Selected = Selected + 1
            If PSelected <> Selected Or Bypass = 1 Then
                Bypass = 0
                DrawMap
                If Selected < 0 Then: Selected = 5
                If Selected > 5 Then: Selected = 0
                Line (125, 119 + 8 * Selected)-(196, 119 + 8 * Selected), 7
            End If
            If _KeyDown(Controls(Interact)) Then
                Do: _Limit 100: Loop Until Not _KeyDown(Controls(Interact))
                If Selected = 0 Then: Exit Do
                If Selected = 1 Then: InitializeGame: Exit Do
                If Selected = 2 Then: Call SaveLoad("savegame")
                If Selected = 3 Then: Call SaveLoad("loadgame")
                If Selected = 4 Then: DisplayMenu ("settings"): DrawMap:
                If Selected = 5 Then: Cls: System
                Bypass = 1
            End If
        Loop
    ElseIf Menu$ = "inventory" Then 'Inventory
        Do: Loop Until Not _KeyDown(Controls(Inventory))
        Selected = 0
        Bypass = 1
        MapX = 0: MapZ = 0
        Call SaveLoad("loadmap")
        DrawMap
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
                If Selected = 3 Then: Category$ = "resource"
                If Selected = 4 Then: Category$ = "tool"
                Line (249, 192)-(310, 192), 7
                Locate 3, 26: Print TmpX: Locate 3, 32: Print TmpY: Locate 3, 38: Print TmpZ
                Locate 4, 26: Print PlayerX: Locate 4, 32: Print PlayerZ
                B = 0
                For A = 1 To UBound(Items)
                    Locate 6 + B, 4: Print "                   ";
                    B = B + 1
                Next A
                Color 0: Locate 4, 4: Print Title$
                Color 15
                Title$ = UCase$(Left$(Category$, 1)) + Mid$(Category$, 2, Len(Category$)) + ":"
                Locate 4, 4: Print Title$
                Locate 5, 3: Print "®-------------¯"
                Locate 3, 26: Print TmpX: Locate 3, 32: Print TmpY: Locate 3, 38: Print TmpZ
                If Category$ <> "tool" Then
                    B = 0
                    For A = 1 To UBound(Items)
                        If Items(A).Category = Category$ Then
                            Locate 6 + B, 4: Print Left$(Items(A).Name, 10), Items(A).Amount
                            Locate 6 + B, 14: Print "-"
                            B = B + 1
                        End If
                    Next A
                Else
                    Locate 6, 4: Print "Pickaxe": Locate 6, 14: Print "- "; ToolName$(ToolLevels(PickaxeLevel), "pick")
                    Locate 7, 4: Print "Axe": Locate 7, 14: Print "- "; ToolName$(ToolLevels(AxeLevel), "axe")
                    Locate 8, 4: Print "Fishing Rod": Locate 8, 14: Print "- "; ToolName$(ToolLevels(FishingRodLevel), "fish")
                End If
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
                If PlayerX - B <= 40 And PlayerZ - A <= 25 Then
                    If MapChar(PlayerX - B, PlayerZ - A) = "Û" Or MapChar(PlayerX - B, PlayerZ - A) = "Ý" Or MapChar(PlayerX - B, PlayerZ - A) = "Þ" Or MapChar(PlayerX - B, PlayerZ - A) = "Ü" Or MapChar(PlayerX - B, PlayerZ - A) = "ß" Then
                        Resource$ = "mining"
                        If MapColor(PlayerX - B, PlayerZ - A) = 43 Then: Ore$ = "Gold"
                        If MapColor(PlayerX - B, PlayerZ - A) = 15 Then: Ore$ = "Iron"
                        If MapColor(PlayerX - B, PlayerZ - A) = 18 Then: Ore$ = "Coal"
                        If MapColor(PlayerX - B, PlayerZ - A) = 77 Then: Ore$ = "Diamond"
                        If Ore$ = "" Then: Ore$ = "Stone"
                        Checked = -1
                    End If
                    If MapChar(PlayerX - B, PlayerZ - A) = "°" Or MapChar(PlayerX - B, PlayerZ - A) = "±" Then Resource$ = "felling": Checked = -1
                    If MapChar(PlayerX - B, PlayerZ - A) = "÷" Or MapChar(PlayerX - B, PlayerZ - A) = "²" Then Resource$ = "fishing": Checked = -1
                    If Checked = -1 Then Exit For: Exit For
                End If
            Next B
        Next A
        If Resource$ = "" Then: MapY = TmpY: Exit Sub
        GatherTimer = 1
        Randomize (Timer)
        GatheredItems = 0
        If Resource$ = "mining" Then: ToolType = PickaxeLevel
        If Resource$ = "felling" Then: ToolType = AxeLevel
        If Resource$ = "fishing" Then: ToolType = FishingRodLevel
        For A = 1 To UBound(Items)
            If Items(A).Strength <= ToolLevels(ToolType) Then
                If Items(A).Category = Resource$ Then
                    If Resource$ = "mining" Then
                        If Items(A).Name = "Stone" And Ore$ <> "Stone" Or Ore$ = Items(A).Name Then
                            GatheredAmount(GatheredItems + 1) = Int(Rnd * 5) + 1
                            Items(A).Amount = Items(A).Amount + GatheredAmount(GatheredItems + 1)
                            GatheredNum(GatheredItems + 1) = A
                            GatheredItems = GatheredItems + 1
                        End If
                    Else
                        GatheredAmount(GatheredItems + 1) = Int(Rnd * 5) + 1
                        Items(A).Amount = Items(A).Amount + GatheredAmount(GatheredItems + 1)
                        GatheredNum(GatheredItems + 1) = A
                        GatheredItems = GatheredItems + 1
                    End If
                End If
            End If
        Next A
        Timer1 = Int(Timer) + 1
        Call DisplayGathered(GatheredItems, GatheredAmount(), GatheredNum())
    ElseIf Menu$ = "crafting" Then 'Crafting
        Do: Loop Until Not _KeyDown(Controls(Crafting))
        ArraySize = 20
        Dim NeededAmount(ArraySize) As Integer
        Dim NeededNum(ArraySize) As Integer
        MapX = 1: MapZ = 1
        Multiplier = 1
        Bypass = 1
        Call SaveLoad("loadmap")
        For A = 1 To UBound(Recipes)
            If Recipes(A).Name <> "" Then: CraftableItems = CraftableItems + 1
        Next A
        Do: _Limit 1000
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
                    NeededAmount(1) = Recipes(Selected + 1).Amount1
                    NeededAmount(2) = Recipes(Selected + 1).Amount2
                    NeededNum(1) = Recipes(Selected + 1).Num1
                    NeededNum(2) = Recipes(Selected + 1).Num2
                    If Items(NeededNum(1)).Amount >= (NeededAmount(1) * Multiplier) Then: Check = Check + 1
                    If Items(NeededNum(2)).Amount >= (NeededAmount(2) * Multiplier) Then: Check = Check + 1
                    If Check = 2 Then
                        Items(NeededNum(1)).Amount = Items(NeededNum(1)).Amount - (NeededAmount(1) * Multiplier)
                        Items(NeededNum(2)).Amount = Items(NeededNum(2)).Amount - (NeededAmount(2) * Multiplier)
                        Items(Recipes(Selected + 1).ItemNum).Amount = Items(Recipes(Selected + 1).ItemNum).Amount + Multiplier
                        Call AddItemLevel(Recipes(Selected + 1).ItemNum)
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
                DrawMap
                If MSelected < 0 Then: MSelected = 0
                If MSelected > 4 Then: MSelected = 4
                If MSelected = 0 Then: Multiplier = 1: Line (69 + (MSelected * 16), 192)-(82 + (MSelected * 16), 192), 7
                If MSelected = 1 Then: Multiplier = 5: Line (69 + (MSelected * 16), 192)-(82 + (MSelected * 16), 192), 7
                If MSelected = 2 Then: Multiplier = 10: Line (101, 192)-(122, 192), 7
                If MSelected = 3 Then: Multiplier = 25: Line (125, 192)-(146, 192), 7
                If MSelected = 4 Then: Multiplier = 50: Line (149, 192)-(170, 192), 7
                Color 15
                For A = 1 To UBound(Recipes)
                    If Recipes(A).Name <> "" Then
                        Locate 3 + A, 2: Print Recipes(A).Name
                    End If
                Next A
                Locate 4, 24: Print Recipes(Selected + 1).Name + ":"
                Locate 21, 33: Print Multiplier
                Locate 6, 25: Print Recipes(Selected + 1).Amount1 * Multiplier; Items(Recipes(Selected + 1).Num1).Name
                If Recipes(Selected + 1).Num2 <> 0 Then: Locate 7, 25: Print Recipes(Selected + 1).Amount2 * Multiplier; Items(Recipes(Selected + 1).Num2).Name
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
            LoopCounter = (LoopCounter + 1) Mod 1000
            If LoopCounter Mod 100 = 0 Then: If GatherTimer <= 50 Then: GatherTimer = GatherTimer + 1
        Loop
        Do: _Limit 100: Loop Until Not _KeyDown(Controls(Interact))
    ElseIf Menu$ = "settings" Then 'Settings
        MapX = 1: MapZ = 2
        Bypass = 1
        Rest = 114
        Call SaveLoad("loadmap")
        Do: _Limit 100
            PSelected = Selected
            If _KeyDown(Controls(Up)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Up)): Selected = Selected - 1: DrawMap
            If _KeyDown(Controls(Down)) Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Down)): Selected = Selected + 1: DrawMap
            If _KeyDown(Controls(Left)) And Selected = 15 Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Left)): PlayerColor = PlayerColor - 1: Bypass = 1
            If _KeyDown(Controls(Right)) And Selected = 15 Then: Do: _Limit 100: Loop Until Not _KeyDown(Controls(Right)): PlayerColor = PlayerColor + 1: Bypass = 1
            If _KeyDown(Rest) Or _KeyDown(Asc(UCase$(Chr$(Rest)))) Then: InitControls: Bypass = 1
            If _KeyDown(Controls(Interact)) Then
                Do: _Limit 100: Loop Until Not _KeyDown(Controls(Interact))
                Line (8, 39 + 8 * Selected)-(160, 39 + 8 * Selected), 0
                If Selected >= 0 And Selected < 8 Then
                    _KeyClear
                    Do: _Limit 100
                        Key$ = InKey$
                        If Key$ <> "" Then
                            KeyAscii = Asc(Mid$(Key$, 1, 1))
                            If Len(Key$) = 2 Then: KeyAscii = KeyAscii + (Asc(Mid$(Key$, 2, 1)) * 256)
                            If KeyAscii <> Controls(Selected + 1) Then: Exit Do
                        End If
                    Loop
                    _KeyClear
                    Controls(Selected + 1) = KeyAscii
                End If
                If Selected = 14 Then
                    Do: _Limit 100: Loop Until InKey$ = ""
                    Locate 19, 9: Print "          "
                    Locate 19, 9: Input ">", Temp$
                    Do: _Limit 100: Loop Until InKey$ = ""
                    Do: Loop Until Not _KeyDown(Controls(Interact))
                    PlayerName = Left$(Temp$, 10)
                End If
                Bypass = 1
                If Selected = 16 Then: Exit Do
            End If
            If PSelected <> Selected Or Bypass = 1 Then
                If Selected < 0 Then: Selected = 0
                If Selected = 8 Then: Selected = 14
                If Selected = 13 Then: Selected = 7
                If Selected > 16 Then: Selected = 16
                If PlayerColor < 1 Then: PlayerColor = 15
                If PlayerColor > 15 Then: PlayerColor = 1
                Bypass = 0
                DrawMap
                Color 15
                For A = 0 To 8
                    If Controls(A) <> 0 Then
                        If Len(GetName$(Controls(A))) = 1 Then
                            Locate 5 + (A - 1), 15: Print UCase$(GetName$(Controls(A)))
                        Else
                            Locate 5 + (A - 1), 15: Print GetName$(Controls(A))
                        End If
                    End If
                Next A
                Locate 19, 9: Print PlayerName
                Color PlayerColor
                If Selected <> 14 Then: Locate 20, 10: Print "þ"
                If Selected = 14 Then
                    Color 15: Locate 20, 10: Print "®   ¯"
                    Color PlayerColor: Locate 20, 12: Print "þ"
                End If
                If Selected <> 16 Then: Line (8, 39 + 8 * Selected)-(160, 39 + 8 * Selected), 7: Line (249, 192)-(310, 192), 0
                If Selected = 16 Then: Line (249, 192)-(310, 192), 7: Line (8, 39 + 8 * PSelected)-(160, 39 + 8 * PSelected), 0
            End If
        Loop
        Do: _Limit 100: Loop Until Not _KeyDown(Controls(Interact))
        Bypass = 1
    End If
    MapX = TmpX
    MapY = TmpY
    MapZ = TmpZ
    Call SaveLoad("loadmap")
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
            Next Y
        Next X
    ElseIf FileSystem$ = "savegame" Then 'Save Game
        Beep
        File = FreeFile
        Open (FilePath + "Save\") + FileName For Output As #File
        Write #File, "Item Amounts:"
        For A = 1 To UBound(Items)
            Write #File, Items(A).Amount
        Next A
        Write #File, "Name/Color:"
        Write #File, PlayerName
        Write #File, PlayerColor
        Write #File, "Controls:"
        For A = 1 To UBound(Controls)
            Write #File, Controls(A)
        Next A
        Close #File
    ElseIf FileSystem$ = "loadgame" Then 'Load Game
        Beep
        File = FreeFile
        Open (FilePath + "Save\") + FileName For Input As #File
        Input #File, Tmp$
        For A = 1 To UBound(Items)
            Input #File, Items(A).Amount
        Next A
        Input #File, Tmp$
        Input #File, PlayerName
        Input #File, PlayerColor
        Input #File, Tmp$
        For A = 1 To UBound(Controls)
            Input #File, Controls(A)
        Next A
        Close #File
    End If
End Sub
Sub DrawMap
    For X = 1 To 40
        For Y = 1 To 25
            Color MapColor(X, Y): Locate Y, X: Print MapChar(X, Y);
        Next Y
    Next X
End Sub
'Initalize Game:
'==========================================================
Sub InitializeGame
    PlayerX = 20
    PlayerZ = 12
    MapX = 2
    MapY = 2
    MapZ = 2
    GatherTimer = 50
    PlayerName = "Default"
    PlayerColor = 15
    Developer = "true"
    InitControls
    Call SaveLoad("loadmap")
    DrawMap
    For A = 1 To UBound(Items)
        Items(A).Amount = 0
    Next A
End Sub
Sub InitControls
    Controls(Up) = 18432
    Controls(Left) = 19200
    Controls(Down) = 20480
    Controls(Right) = 19712
    Controls(Escape) = 27
    Controls(Interact) = 13
    Controls(Inventory) = 105
    Controls(Crafting) = 99
    Controls(Commands) = 47
End Sub
'Command Stuff:
'========================================================
Sub CMDStuff
    _AutoDisplay
    Do: _Limit 100: Loop Until Not _KeyDown(Controls(Commands))
    Do: _Limit 100
        _KeyClear
        For A = 1 To 40
            Locate 25, A: Color MapColor(A, 25): Print MapChar(A, 25);
        Next A
        Color 15
        Locate 25, 1: Print ">"; CommandInput$;
        Locate 25, (Len(CommandInput$) + 2): Print "_";
        Do: _Limit 100: Key$ = InKey$: Loop Until Key$ <> ""
        If Len(CommandInput$) <= 37 Then
            If Asc(Key$) >= 65 And Asc(Key$) <= 90 Then: CommandInput$ = CommandInput$ + Key$ 'A - Z
            If Asc(Key$) >= 97 And Asc(Key$) <= 122 Then: CommandInput$ = CommandInput$ + Key$ 'a - z
            If Asc(Key$) >= 48 And Asc(Key$) <= 57 Then: CommandInput$ = CommandInput$ + Key$ '0 - 9
            If Asc(Key$) = 32 Then: CommandInput$ = CommandInput$ + Chr$(32) 'Space
            If Asc(Key$) = 8 Then: CommandInput$ = Left$(CommandInput$, (Len(CommandInput$) - 1)) 'Escape
        End If
        If Asc(Key$) = 13 Then: Exit Do 'Enter
        _KeyClear
    Loop
    CommandInput$ = LCase$(CommandInput$ + Chr$(32))
    MainCommand$ = TextParser$(CommandInput$, 1)
    CommandInput$ = LTrim$(Right$(CommandInput$, (Len(CommandInput$) - Len(MainCommand$))))
    If MainCommand$ = "give" Or MainCommand$ = "take" Then
        Amount = AmountParser(CommandInput$, 1)
        CommandInput$ = LTrim$(Right$(CommandInput$, (Len(CommandInput$) - Len(LTrim$(Str$(Amount))))))
        Item$ = TextParser(CommandInput$, 1)
        CommandInput$ = LTrim$(Right$(CommandInput$, (Len(CommandInput$) - Len(Item$))))
        ItemNum = GetItemNum(Item$)
        If MainCommand$ = "give" Then: Items(ItemNum).Amount = Items(ItemNum).Amount + Amount
        If MainCommand$ = "take" Then: Items(ItemNum).Amount = Items(ItemNum).Amount - Amount
    ElseIf MainCommand$ = "tp" Then
        XCor = AmountParser(CommandInput$, 1)
        CommandInput$ = LTrim$(Right$(CommandInput$, (Len(CommandInput$) - Len(LTrim$(Str$(XCor))))))
        ZCor = AmountParser(CommandInput$, 1)
        CommandInput$ = LTrim$(Right$(CommandInput$, (Len(CommandInput$) - Len(LTrim$(Str$(ZCor))))))
        If CheckX(XCor) Then
            If CheckZ(ZCor) Then
                PlayerX = XCor: PlayerZ = ZCor
            End If
        End If
    ElseIf MainCommand$ = "map" Then
        MapX = (AmountParser(CommandInput$, 1))
        CommandInput$ = LTrim$(Right$(CommandInput$, (Len(CommandInput$) - Len(LTrim$(Str$(MapX))))))
        MapY = (AmountParser(CommandInput$, 1))
        CommandInput$ = LTrim$(Right$(CommandInput$, (Len(CommandInput$) - Len(LTrim$(Str$(MapY))))))
        MapZ = (AmountParser(CommandInput$, 1))
        SaveLoad ("loadmap")
        DrawMap
    End If
End Sub
'Tools:
'===========
Function ToolLevel (Type$)
    If Type$ = "Wooden" Then level = 1
    If Type$ = "Stone" Then: level = 2
    If Type$ = "Iron" Then: level = 3
    If Type$ = "Diamond" Then: level = 4
    If Type$ = "Fishing" Then: level = 1
    ToolLevel = level
End Function
Function ToolName$ (Level, Type$)
    If Type$ <> "fish" Then
        If Level = 0 Then: ToolName$ = "No Tool"
        If Level = 1 Then: ToolName$ = "Wooden"
        If Level = 2 Then: ToolName$ = "Stone"
        If Level = 3 Then: ToolName$ = "Iron"
        If Level = 4 Then: ToolName$ = "Diamond"
    Else
        If Level = 0 Then: ToolName$ = "No Tool"
        If Level = 1 Then: ToolName$ = "Has Tool"
    End If
End Function
Function GetItemNum (ItemName$)
    For A = 1 To UBound(Items)
        If ItemName$ = LCase$(Items(A).Name) Then: GetItemNum = Items(A).Num
    Next A
End Function
Sub AddItemLevel (ItemNum)
    ItemName$ = Items(ItemNum).Name
    Parsered$ = TextParser(ItemName$, 1)
    ItemName$ = LTrim$(Right$(ItemName$, (Len(ItemName$) - Len(Parsered$))))
    level = ToolLevel(Parsered$)
    If ItemName$ = "Pickaxe" Then
        If ToolLevels(PickaxeLevel) < level Then: ToolLevels(PickaxeLevel) = level
    ElseIf ItemName$ = "Axe" Then
        If ToolLevels(AxeLevel) < level Then: ToolLevels(AxeLevel) = level
    ElseIf ItemName$ = "Rod" Then
        If ToolLevels(FishingRodLevel) < level Then: ToolLevels(FishingRodLevel) = level
    End If
End Sub
'Quick Draw:
'============
Sub DisplayGathered (GatheredItems, GatheredAmount( 30) As Integer, GatheredNum( 30) As Integer)
    Color 14
    For A = 1 To GatheredItems
        Locate (25 + 1) - A, 28: Print "            ";
        Locate (25 + 1) - A, 28: Print "+"; LTrim$(Str$(GatheredAmount(A)));
        Locate (25 + 1) - A, 32: Print "-";
        Locate (25 + 1) - A, ((40 + 1) - Len(Items(GatheredNum(A)).Name)): Print Items(GatheredNum(A)).Name;
    Next A
End Sub

'$include: 'keyname.bas'
'$Include: 'cmdstuff.bas'

