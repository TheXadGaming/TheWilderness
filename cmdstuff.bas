Function TextParser$ (Text$, B)
    For A = 1 To Len(Text$)
        Portion$ = Mid$(Text$, A, 1)
        If Portion$ = Chr$(32) Then: Exit For
    Next A
    TextParser$ = RTrim$(Mid$(Text$, B, A))
End Function
Function AmountParser (Text$, A)
    B = 0
    Do: _Limit 100
        B = B + 1
        Portion$ = Mid$(Text$, B, 1)
        If Portion$ = Chr$(32) Then: Exit Do
    Loop
    Amount = Val((Mid$(Text$, A, B)))
    AmountParser = AmountCheck(Amount)
End Function
Function AmountCheck (Amount)
    If Amount >= 1 And Amount <= 999 Then
        Result = Amount
    Else
        Result = 1
    End If
    AmountCheck = Result
End Function
Function CheckX (X)
    If X >= 1 And X <= 40 Then: CheckX = X
End Function
Function CheckZ (Z)
    If Z >= 1 And Z <= 25 Then: CheckZ = Z
End Function
Function CheckMapX (X)
    If X >= 1 And X <= 3 Then: CheckMapX = X
End Function
Function CheckMapY (Y)
    If Y >= 2 And Y <= 3 Then: CheckMapY = Y
End Function
Function CheckMapZ (Z)
    If Z >= 1 And Z <= 3 Then: CheckMapZ = Z
End Function
