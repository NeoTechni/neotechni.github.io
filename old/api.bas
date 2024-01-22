Option Explicit

'Public Const vbTab As String = Chr(9)
Public TwoToTheN(1 To 32) As Long 'for large exponents
Public Const MaxInt As Integer = 32767
Public Const MinInt As Integer = -MaxInt - 1
Public Const MaxLong As Long = 2147483647
Public Const MinLong As Long = -MaxLong - 1
Public Const MaxCurr As Currency = 922337203685477#
Public Const MinCurr As Currency = -MaxCurr
Public Const MaxSng As Single = 3.4028235E+38
Public Const MinSng As Single = -MaxSng
Private Const INFINITE As Long = -1
Public Const vbSelected As String = "[SELECTEDITEM]"
Public Const vbUnselected As String = "[UNSELECTEDITEM]"

'for executing files
Public Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)
Private Type SHELLEXECUTEINFO
    cbSize As Long
    fMask As Long
    hWnd As Long
    lpVerb As Long
    lpFile As Long
    lpParameters As Long
    lpDirectory As Long
    nShow As Long
    hInstApp As Long
    lpIDList As Long
    lpClass As Long
    hkeyClass As Long
    dwHotKey As Long
    hIcon_Or_hMonitor As Long
    hProcess As Long
End Type
Private Type STARTUPINFO
   cb As Long
   lpReserved As String
   lpDesktop As String
   lpTitle As String
   dwX As Long
   dwY As Long
   dwXSize As Long
   dwYSize As Long
   dwXCountChars As Long
   dwYCountChars As Long
   dwFillAttribute As Long
   dwFlags As Long
   wShowWindow As Integer
   cbReserved2 As Integer
   lpReserved2 As Long
   hStdInput As Long
   hStdOutput As Long
   hStdError As Long
End Type
Private Type PROCESS_INFORMATION
   hProcess As Long
   hThread As Long
   dwProcessID As Long
   dwThreadID As Long
End Type
Private Declare Function ShellExecuteEx Lib "Shell32.dll" Alias "ShellExecuteExW" (pExecInfo As SHELLEXECUTEINFO) As Long
Private Declare Function ShellExecute Lib "Shell32.dll" Alias "ShellExecuteA" (ByVal hWnd As Long, ByVal lpszOp As String, ByVal lpszFile As String, ByVal lpszParams As String, ByVal LpszDir As String, ByVal FsShowCmd As Long) As Long
Private Declare Function WaitForSingleObject Lib "kernel32.dll" (ByVal hHandle As Long, ByVal dwMilliseconds As Long) As Long
Private Declare Function CreateProcessA Lib "kernel32" (ByVal lpApplicationName As String, ByVal lpCommandLine As String, ByVal lpProcessAttributes As Long, ByVal lpThreadAttributes As Long, ByVal bInheritHandles As Long, ByVal dwCreationFlags As Long, ByVal lpEnvironment As Long, ByVal lpCurrentDirectory As String, lpStartupInfo As STARTUPINFO, lpProcessInformation As PROCESS_INFORMATION) As Long
Private Declare Function GetExitCodeProcess Lib "kernel32" (ByVal hProcess As Long, lpExitCode As Long) As Long
Private Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long
Private Const NORMAL_PRIORITY_CLASS = &H20&

'graphics
Public Declare Function GetPixel Lib "gdi32" (ByVal hDC As Long, ByVal X As Long, ByVal Y As Long) As Long
Public Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal X As Long, ByVal Y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long 'use vbSrcCopy for dwRop
Public Declare Function SetPixelV Lib "gdi32" (ByVal hDC As Long, ByVal X As Long, ByVal Y As Long, ByVal crColor As Long) As Long
Public Declare Function TransparentBlt Lib "msimg32" (ByVal hdcDest As Long, ByVal nXOriginDest As Long, ByVal nYOriginDest As Long, ByVal nWidthDest As Long, ByVal hHeightDest As Long, ByVal hdcSrc As Long, ByVal nXOriginSrc As Long, ByVal nYOriginSrc As Long, ByVal nWidthSrc As Long, ByVal nHeightSrc As Long, ByVal crTransparent As Long) As Boolean
Public Declare Function AlphaBlend Lib "msimg32" (ByVal hdcDest As Long, ByVal nXOriginDest As Long, ByVal nYOriginDest As Long, ByVal nWidthDest As Long, ByVal hHeightDest As Long, ByVal hdcSrc As Long, ByVal nXOriginSrc As Long, ByVal nYOriginSrc As Long, ByVal nWidthSrc As Long, ByVal nHeightSrc As Long, ByVal blendFunc As Long) As Boolean
Public Const AlphaMultiplier As Long = 65536 ' blenfunc = Alpha * 65536 (alpha = 0 to 255)

Public Type POINTAPI
    X As Long
    Y As Long
End Type
Public Type RECT
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type

'file exists
Public Declare Function OpenFile Lib "kernel32" (ByVal lpFileName As String, lpReOpenBuff As OFSTRUCT, ByVal wStyle As Long) As Long
Const OF_READ = &H0
Const OF_WRITE = &H1
Const OF_READWRITE = &H2
Const OF_SHARE_COMPAT = &H0
Const OF_SHARE_EXCLUSIVE = &H10
Const OF_SHARE_DENY_WRITE = &H20
Const OF_SHARE_DENY_READ = &H30
Const OF_SHARE_DENY_NONE = &H40
Const OF_PARSE = &H100
Const OF_DELETE = &H200
Const OF_VERIFY = &H400
Const OF_CANCEL = &H800
Const OF_CREATE = &H1000
Const OF_PROMPT = &H2000
Const OF_EXIST = &H4000
Const OF_REOPEN = &H8000&
Const OFS_MAXPATHNAME As Long = 128
Type OFSTRUCT ' OpenFile() Structure
        cBytes As Byte
        fFixedDisk As Byte
        nErrCode As Integer
        Reserved1 As Integer
        Reserved2 As Integer
        szPathName(0 To OFS_MAXPATHNAME - 1) As Byte
End Type

'path relativity
Private Declare Function PathCanonicalize Lib "shlwapi.dll" Alias "PathCanonicalizeA" (ByVal pszBuf As String, ByVal pszPath As String) As Long

'trim WS
Private Declare Function lstrlen Lib "kernel32" Alias "lstrlenW" (ByVal lpString As Long) As Long
Private Declare Function StrTrim Lib "shlwapi" Alias "StrTrimW" (ByVal pszSource As Long, ByVal pszTrimChars As Long) As Long

' Hook/Unhook Msgbox
Private piHookHwnd As Long, YesButton As String, NoButton As String, CancelButton As String ' Holds Msgbox handle
Private Declare Function SetWindowsHookEx Lib "user32" Alias "SetWindowsHookExA" (ByVal idHook As Long, ByVal lpfn As Long, ByVal hMod As Long, ByVal dwThreadID As Long) As Long
Private Declare Function UnhookWindowsHookEx Lib "user32" (ByVal hHook As Long) As Long
Private Declare Function SetDlgItemText Lib "user32" Alias "SetDlgItemTextA" (ByVal hDlg As Long, ByVal nIDDlgItem As Long, ByVal lpString As String) As Long
Private Declare Function MessageBoxTimeout Lib "user32.dll" Alias "MessageBoxTimeoutA" (ByVal hWnd As Long, ByVal lpText As String, ByVal lpCaption As String, ByVal uType As Long, ByVal wLanguageId As Long, ByVal dwMilliseconds As Long) As Long
Private Const WH_CBT = 5 ' For SetWindowsHookEx()
Private Const HCBT_ACTIVATE = 5
Private Const MB_SETFOREGROUND& = &H10000
Public Const VBTIMEDOUT As Long = &H7D00&

'Set environment variable
Private Declare Function SHSetValue Lib "shlwapi.dll" Alias "SHSetValueA" (ByVal hKey As Long, ByVal pszSubKey As String, ByVal pszValue As String, ByVal dwType As Long, pvData As String, ByVal cbData As Long) As Long
Private Declare Function SendMessageTimeout Lib "user32" Alias "SendMessageTimeoutA" (ByVal hWnd As Long, ByVal Msg As Long, ByVal wParam As Long, ByVal lParam As String, ByVal fuFlags As Long, ByVal uTimeout As Long, lpdwResult As Long) As Long
Private Declare Function SetEnvironmentVariable Lib "kernel32" Alias "SetEnvironmentVariableA" (ByVal lpName As String, ByVal lpValue As String) As Long
Private Const REG_SZ = 1
Private Const REG_EXPAND_SZ = 2
Private Const HWND_BROADCAST = &HFFFF&
Private Const WM_WININICHANGE = &H1A
Private Const HKEY_CURRENT_USER = &H80000001
Private Const SHREGSET_FORCE_HKCU = &H1
Private Const SMTO_ABORTIFHUNG = &H2

'listview stuff
Private Type LVITEM
    Mask As Long
    iItem As Long
    iSubItem As Long
    State As Long
    stateMask As Long
    pszText As String
    cchTextMax As Long
    iImage As Long
    lParam As Long
    iIndent As Long
End Type
Private Type LVHITTESTINFO
    pt As POINTAPI
    Flags As Long
    iItem As Long
    iSubItem As Long
End Type
Private Declare Function ScreenToClient Lib "user32" (ByVal hWnd As Long, lpPoint As POINTAPI) As Long
Private Const LVM_FIRST As Long = &H1000
Private Const LVM_GETCOLUMNWIDTH As Long = (LVM_FIRST + 29)
Private Const LVM_GETTOPINDEX As Long = (LVM_FIRST + 39)
Private Const LVM_GETSUBITEMRECT As Long = (LVM_FIRST + 56)
Private Const LVM_SUBITEMHITTEST As Long = (LVM_FIRST + 57)
Private Const LVHT_ONITEMICON As Long = &H2
Private Const LVHT_ONITEMLABEL As Long = &H4
Private Const LVHT_ONITEMSTATEICON As Long = &H8
Private Const LVHT_ONITEM As Long = (LVHT_ONITEMICON Or LVHT_ONITEMLABEL Or LVHT_ONITEMSTATEICON)
Private Const LVIR_LABEL As Long = 2
Private bDoingSetup As Boolean, Dirty As Boolean, dwLastSubitemEdited As Long
Public itmClicked As ListItem

'KVP manager
Public Type KVP
    Key As String
    Value As Variant
End Type
Public Type KVPs
    CaseSensitive As Boolean
    Name As String
    Data() As KVP
    Count As Long
End Type

Public Declare Function SetForegroundWindow Lib "user32" (ByVal hWnd As Long) As Long
Public Declare Function WindowFromPoint Lib "user32" (ByVal xPoint As Long, ByVal yPoint As Long) As Long
Public Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long

Public Function ListView_MouseDown(LST As ListView, X As Single, Y As Single, Optional Relative As Boolean = True) As RECT
    'this routine:                                          https://bytes.com/topic/visual-basic/answers/14065-listview-edit
    '1. sets the last change if the dirty flag is set
    '2. sets a flag to prevent setting the dirty flag
    '3. determines the item or subitem clicked
    '4. calc's the position for the text box
    '5. moves and shows the text box
    '6. clears the dirty flag
    '7. clears the DoingSetup flag
    Dim HTI As LVHITTESTINFO
    Dim fpx As Single
    Dim fpy As Single
    Dim fpw As Single
    Dim fph As Single
    Dim RC As RECT
    Dim topindex As Long

    bDoingSetup = True 'prevent the textbox change event from registering as dirty when the text is assigned to the textbox
    
    'if a pending dirty flag is set, update the last edited item before moving on
    'If dirty And dwLastSubitemEdited > 0 Then itmClicked.SubItems(dwLastSubitemEdited) = Text1.Text
    'Text1.Visible = False'hide the textbox
        
    With HTI 'get the position of the click
        .pt.X = (X / Screen.TwipsPerPixelX)
        .pt.Y = (Y / Screen.TwipsPerPixelY)
        .Flags = LVHT_ONITEM
    End With
    Call SendMessage(LST.hWnd, LVM_SUBITEMHITTEST, 0, HTI) 'find out which subitem was clicked
    
    'if on an item (HTI.iItem <> -1) and the click occurred on the subitem column of interest (HTI.iSubItem = 2 - which is column 3 (0-based)) move and show the textbox
    If HTI.iItem <> -1 And HTI.iSubItem > 0 Then
        LST.LabelEdit = lvwManual 'prevent the listview label editing from occurring if the control has full row select set

        'determine the bounding rectangle of the subitem column
        RC.Left = LVIR_LABEL
        RC.Top = HTI.iSubItem
        Call SendMessage(LST.hWnd, LVM_GETSUBITEMRECT, HTI.iItem, RC)

        'we need to keep track of which item was clicked so the item can be updated later position the text box
        Set itmClicked = LST.ListItems(HTI.iItem + 1)
        itmClicked.Selected = True

        topindex = SendMessage(LST.hWnd, LVM_GETTOPINDEX, 0&, ByVal 0&) 'get the current top index

        'establish the bounding rect for the subitem in VB terms (the x and y coordinates, and the height and width of the item
        fpx = LST.Left + (RC.Left * Screen.TwipsPerPixelX) ' + 80
        fpy = LST.Top + (HTI.iItem + 1 - topindex) + (RC.Top * Screen.TwipsPerPixelY)
        fph = 15 * Screen.TwipsPerPixelY 'a hard-coded height for the text box
        fpw = SendMessage(LST.hWnd, LVM_GETCOLUMNWIDTH, HTI.iSubItem, ByVal 0&) 'get the column width for the subitem
        fpw = (fpw * Screen.TwipsPerPixelX) ' - 40 'calc the required width of the textbox to fit in the column
        With ListView_MouseDown
            .Left = fpx / Screen.TwipsPerPixelX
            .Top = fpy / Screen.TwipsPerPixelY
            If Relative Then
                .Left = .Left + LST.Left
                .Top = .Top + LST.Top
            End If
            .Right = .Left + (fpw / Screen.TwipsPerPixelX)
            .Bottom = .Top + (fph / Screen.TwipsPerPixelY)
        End With
        
        'assign the current subitem value to the textbox
        'With Text1
        '    .Text = itmClicked.SubItems(HTI.iSubItem)
        '    dwLastSubitemEdited = HTI.iSubItem

            'position it over the subitem, make visible and assure the text box appears overtop the listview
        '    .Move fpx, fpy, fpw, fph
        '    .Visible = True
        '    .ZOrder 0
        '    .SetFocus
        'End With

        'clear the setup flag to allow the textbox change event to set the "dirty" flag, and clear that flag in preparation for editing
        bDoingSetup = False
        Dirty = False
    End If
End Function
'Private Sub ListView1_MouseUp(Button As Integer, 'Shift As Integer, X As Single, Y As Single)
'
''if showing the text box, set focus to it and select any text in the control
'If Text1.Visible = True Then
'
'With Text1
'.SetFocus
'.SelStart = 0
'.SelLength = Len(.Text)
'End With
'
'End If
'End Sub

Public Function DeleteFile(Filename As String) As Boolean
    On Error Resume Next
    Kill Filename
    DeleteFile = True
End Function

Public Function AllEnvironmentVariables(Optional Delimiter As String = "|") As String
    Dim temp As Long, Name As String, tempstr As String, tempstr2() As String, temp2 As Long
    For temp = 1 To 255
        tempstr = Environ(temp)
        If Len(tempstr) = 0 Then Exit For
        AllEnvironmentVariables = AllEnvironmentVariables & IIf(Len(AllEnvironmentVariables) = 0, "", Delimiter) & tempstr
        Name = GetSide(tempstr, "=", True)
        If EndsWith(Name, "path", vbTextCompare) And TextContains(tempstr, ";") Then
            tempstr = GetSide(tempstr, "=", False)
            tempstr2 = Split(tempstr, ";")
            For temp2 = 0 To UBound(tempstr2)
                AllEnvironmentVariables = AllEnvironmentVariables & vbNewLine & Name & temp2 & "=" & tempstr2(temp2)
            Next
        End If
    Next
End Function

Public Function SetEnvironmentVar(EnvName As String, EnvValue As Variant) As String
    Dim resApi As Long
    SetEnvironmentVariable EnvName, CStr(EnvValue)    ' Set Environment Variable for the current process
    resApi = SHSetValue(HKEY_CURRENT_USER, "Environment", EnvName, REG_EXPAND_SZ, ByVal CStr(EnvValue), CLng(LenB(StrConv(CStr(EnvValue), vbFromUnicode)) + 1)) ' Set Environment Variable via the registry for all other processes
    SendMessageTimeout HWND_BROADCAST, WM_WININICHANGE, 0, "Environment", SMTO_ABORTIFHUNG, 5000, resApi ' Make sure all other processes are aware of the Environment Variable Change
    SaveSettingString HKEY_LOCAL_MACHINE, "SYSTEM\CurrentControlSet\Control\Session Manager\Environment", EnvName, CStr(EnvValue) 'HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment
    SetEnvironmentVar = Environ(EnvName)
End Function

Public Function ExecCmd(cmdline As String, Optional MaxTimeMilliSeconds As Long = INFINITE) As Long
    Dim proc As PROCESS_INFORMATION, Start As STARTUPINFO, RET As Long
    Start.cb = Len(Start)
    RET& = CreateProcessA(vbNullChar, cmdline$, 0&, 0&, 1&, NORMAL_PRIORITY_CLASS, 0&, vbNullChar, Start, proc)
    If MaxTimeMilliSeconds <> 0 Then
        RET& = WaitForSingleObject(proc.hProcess, MaxTimeMilliSeconds)
        Call GetExitCodeProcess(proc.hProcess, RET&)
        Call CloseHandle(proc.hThread)
        Call CloseHandle(proc.hProcess)
    End If
    ExecCmd = RET&
End Function

'verbs: edit, find, open, print, properties, runas, a delay of 0 will not lock the current program
Public Function RunAndWait(hWnd As Long, Optional sPath As String, Optional CommandLineArguments As String, Optional Verb As String, Optional MaxTimeMilliSeconds As Long = INFINITE, Optional ForceTermination As Boolean = True) As Boolean
    On Error GoTo ErrorHandler
    Dim sei     As SHELLEXECUTEINFO
    Dim pos     As Long
    Dim iExit   As Long
    Dim sDir    As String
    
    If Not FileExists(sPath) Or Len(sPath) = 0 Then Exit Function
    If ForceTermination Then
        ExecCmd Trim(sPath & " " & CommandLineArguments), MaxTimeMilliSeconds
        RunAndWait = True
        Exit Function
    End If

    With sei
        .cbSize = LenB(sei)
        .fMask = SEE_MASK_NOCLOSEPROCESS
        .hWnd = hWnd
        .lpVerb = StrPtr(Verb) 'runas
        .lpFile = StrPtr(sPath)
        .lpParameters = StrPtr(CommandLineArguments)
        pos = InStrRev(sPath, "\")
        If pos <> 0 Then sDir = Left$(sPath, pos - 1)
        .lpDirectory = StrPtr(sDir)
        .nShow = SW_NORMAL
    End With

    If ShellExecuteEx(sei) Then
        If sei.hProcess <> 0 And MaxTimeMilliSeconds <> 0 Then
            iExit = WaitForSingleObject(sei.hProcess, MaxTimeMilliSeconds)
            CloseHandle sei.hProcess
        End If
    Else
        RunAndWait = -1
    End If

    RunAndWait = iExit = 0
    Exit Function
ErrorHandler:
    Debug.Print err; err.Description; "RunAndWait"; sPath
    RunAndWait = False
End Function

Public Function CopyFiles(SourcePath As String, DestinationPath As String, ParamArray Files() As Variant) As Boolean
    Dim temp As Long
    For temp = 0 To UBound(Files)
        CopyFiles = CopyFile(ChkDir(SourcePath, CStr(Files(temp))), ChkDir(DestinationPath, CStr(Files(temp)))) Or CopyFiles
    Next
End Function

Public Function CopyFile(ByVal Source As String, Destination As String, ParamArray Replacements() As Variant) As Boolean
    On Error Resume Next
    If Not FileExists(Source) Then Exit Function 'source not found!
    If DirExists(Destination) Then 'is a directory
        Destination = Destination & GetFilename(Source)
    Else 'is not a directory
        MakeDir GetDirectory(Destination)
    End If
    If FileExists(Destination) Then
        If FileModifiedCurr(Destination) > FileModifiedCurr(Source) Then
            Exit Function 'only copy if it doesn't exist and is younger
        End If
        'deletefile Destination
    End If
    If IsMissing(Replacements) Then
        FileCopy Source, Destination
    Else
        Dim temp As Long
        Source = File_Get_Contents(Source)
        For temp = LBound(Replacements) To UBound(Replacements) Step 2
            Source = Replace(Source, Replacements(temp), Replacements(temp + 1), , , vbTextCompare)
        Next
        File_Put_Contents Destination, Source
    End If
    CopyFile = FileExists(Destination)
End Function

Public Function Start(Filename As String) As String
    ShellExecute 0, vbNullChar, Filename, vbNullChar, vbNullChar, vbNormalFocus
End Function

Public Function GetFromListView(LST As ListView, Key As String, Optional SubIndex As Integer = 1, Optional NewValue As Variant) As String
    Dim Item As ListItem
    Set Item = FindListViewItem(LST, Key, , False)
    If Not Item Is Nothing Then
        With Item
            If SubIndex > 0 Then
                If Not IsMissing(NewValue) Then .SubItems(SubIndex) = NewValue
                GetFromListView = .SubItems(SubIndex)
            ElseIf IsMissing(NewValue) Then
                Select Case SubIndex
                    Case -1: GetFromListView = .Tag
                    Case 0: GetFromListView = .Text
                End Select
            Else
                Select Case SubIndex
                    Case -1: .Tag = NewValue
                    Case 0: .Text = NewValue
                End Select
            End If
        End With
    End If
End Function

Public Function ListItemClicked(LST As ListView, X As Single, Y As Single, Optional TrueForXFalseForY As Boolean = True) As Long
    Dim HTI As LVHITTESTINFO
    ListItemClicked = -1
    With HTI 'get the position of the click
        .pt.X = (X \ Screen.TwipsPerPixelX)
        .pt.Y = (Y \ Screen.TwipsPerPixelY)
        .Flags = LVHT_ONITEM
    End With
    Call SendMessage(LST.hWnd, LVM_SUBITEMHITTEST, 0, HTI) 'find out which subitem was clicked
    If (HTI.iItem > -1) Then
        If TrueForXFalseForY Then
            ListItemClicked = HTI.iSubItem
        Else
            ListItemClicked = HTI.iItem + 1
        End If
    End If
End Function

Public Sub AutoSizeColumnHeader(LST As ListView, Optional Index As Integer, Optional ByVal SizeToHeader As Boolean = True)
    On Error Resume Next
    If Index = 0 Then
        For Index = 1 To LST.ColumnHeaders.Count
            AutoSizeColumnHeader LST, Index
        Next
    Else
        Call SendMessage(LST.hWnd, LVM_FIRST + 30, LST.ColumnHeaders.Item(Index).Index - 1, IIf(SizeToHeader, -2, -1))
    End If
End Sub
Public Function AddListItem(LST As ListView, ParamArray Text() As Variant) As ListItem
    Dim temp As Long
    Set AddListItem = FindListViewItem(LST, CStr(Text(0)), CStr(Text(0)), True)
    For temp = 1 To UBound(Text)
        If Text(temp) = vbSelected Then
            Set LST.SelectedItem = AddListItem
        ElseIf Not Text(temp) = vbUnselected Then
            AddListItem.SubItems(temp) = Text(temp)
        End If
    Next
    AutoSizeColumnHeader LST, , True
End Function
Public Function FindListViewItem(LST As ListView, ByVal Key As String, Optional Text As String = vbNullChar, Optional MakeIfNotFound As Boolean = True, Optional SelectIt As Boolean) As ListItem
    On Error GoTo err:
    Key = LCase(Key)
    Set FindListViewItem = LST.ListItems(Key)
    If Not Text = vbNullChar Then
        FindListViewItem.Text = Text
        If SelectIt Then Set LST.SelectedItem = FindListViewItem
    End If
    Exit Function
err:
    If MakeIfNotFound Then
        If IsNumeric(Key) Then
            Set FindListViewItem = LST.ListItems.Add(, "num" & Key, Text)
        Else
            Set FindListViewItem = LST.ListItems.Add(, Key, Text)
        End If
        If SelectIt Then Set LST.SelectedItem = FindListViewItem
    End If
End Function

Public Function Append(ByVal Text As String, ByVal TextToAppend As String, Optional Delimiter As String = "|", Optional TrimFirst As Boolean) As String
    If TrimFirst Then
        Text = TrimS(Text, Delimiter)
        TextToAppend = TrimS(TextToAppend, Delimiter)
    End If
    If Len(Text) = 0 Then
        Append = TextToAppend
    ElseIf Len(TextToAppend) > 0 Then
        Append = Text & Delimiter & TextToAppend
    Else
        Append = Text
    End If
End Function
Public Function TrimS(ByVal Text As String, TextToTrim As String) As String
    Dim TrimAgain As Boolean
    TrimS = TrimWS(Text)
    If StartsWith(TrimS, TextToTrim) Then TrimS = Right(TrimS, Len(TrimS) - Len(TextToTrim)): TrimAgain = True
    If EndsWith(TrimS, TextToTrim) Then TrimS = Left(TrimS, Len(TrimS) - Len(TextToTrim)): TrimAgain = True
    If TrimAgain Then TrimS = TrimWS(TrimS)
End Function

Public Function IfTrue(Expression As Boolean, IsTrue, Optional IsFalse = "")
    If Expression Then IfTrue = IsTrue Else IfTrue = IsFalse
End Function

Public Function DebugKVP(Data As KVPs) As String
    Dim temp As Long
    Debug.Print "Debug KVPs: " & IIf(Len(Data.Name) = 0, "[Unnamed]", Data.Name)
    For temp = 0 To Data.Count - 1
        DebugKVP = Data.Data(temp).Value
        If TextContains(DebugKVP, vbNewLine) Then DebugKVP = GetSide(DebugKVP, vbNewLine, True) & "..."
        Debug.Print (temp + 1) & " of " & Data.Count, Data.Data(temp).Key, DebugKVP
    Next
End Function
'Public Function CopyKVPs(Data As KVPs, INIFilename As String, Section As String)
'    Dim tempstr() As String, Count As Long, temp As Long
'    tempstr = GetINIkeys(INIFilename, Section, Count)
'    For temp = 1 To Count
'        Debug.Print tempstr(1, temp), SetKVP(Data, tempstr(1, temp), tempstr(2, temp))
'    Next
'End Function

Public Function LoadKVP(ByRef Data As KVPs, INIFilename As String, Section As String) As Long
    Dim tempstr() As String, Count As Long, temp As Long
    tempstr = GetINIkeys(INIFilename, Section, Count, False, Data.CaseSensitive)
    For temp = 1 To Count
        SetKVP Data, tempstr(1, temp), tempstr(2, temp)
    Next
    LoadKVP = Count
End Function
Public Function FindKVP(ByRef Data As KVPs, ByRef Key As String) As Long
    Dim temp As Long
    If Not Data.CaseSensitive Then Key = LCase(Key)
    FindKVP = -1
    For temp = 0 To Data.Count - 1
        If Len(Key) = 0 Then
            Debug.Print temp & " of  " & Data.Count, Data.Data(temp).Key, Data.Data(temp).Value
        ElseIf Data.Data(temp).Key = Key Then
            FindKVP = temp
            Exit For
        End If
    Next
End Function
Public Function GetKVP(ByRef Data As KVPs, Optional ByVal Key As String, Optional Default As Variant = "") As Variant
    Dim temp As Long
    If Len(Key) = 0 Then
        Key = Clean(CStr(Default))
        For temp = 0 To Data.Count - 1
            GetKVP = Append(GetKVP, IIf(Key = "key", Data.Data(temp).Key, Data.Data(temp).Value))
        Next
    Else
        GetKVP = Default
        temp = FindKVP(Data, Key)
        If temp > -1 Then GetKVP = Data.Data(temp).Value
    End If
End Function
Public Function SetKVP(ByRef Data As KVPs, ByVal Key As String, Value As Variant) As Long
    SetKVP = FindKVP(Data, Key)
    If SetKVP = -1 Then
        SetKVP = Data.Count
        Data.Count = Data.Count + 1
        ReDim Preserve Data.Data(Data.Count)
        With Data.Data(SetKVP)
            If Data.CaseSensitive Then
                .Key = Key
            Else
                .Key = LCase(Key)
            End If
            .Value = Value
        End With
    Else
        Data.Data(SetKVP).Value = Value
    End If
End Function
Public Function DeleteKVP(ByRef Data As KVPs, Optional Key As String) As Long
    Dim temp As Long, temp2 As Long
    If Len(Key) = 0 Then 'delete all
        DeleteKVP = Data.Count
        Data.Count = 0
    Else
        temp = FindKVP(Data, Key)
        If temp > -1 Then
            For temp2 = temp To Data.Count - 2
                Data.Data(temp2).Key = Data.Data(temp2 + 1).Key
                Data.Data(temp2).Value = Data.Data(temp2 + 1).Value
            Next
            DeleteKVP = 1
            Data.Count = Data.Count - 1
        End If
    End If
    If DeleteKVP > 0 Then
        If Data.Count = 0 Then
            ReDim Data.Data(0)
        Else
            ReDim Preserve Data.Data(Data.Count)
        End If
    End If
End Function

Public Function CustomMSGBOX(Title As String, Message As String, Yes As String, Optional NO As String, Optional Cancel As String, Optional dwMilliseconds As Long, Optional ReturnAsString As Boolean = True) As String
    Dim Param As VbMsgBoxStyle, Result As VbMsgBoxResult
    YesButton = Yes
    NoButton = NO
    CancelButton = Cancel
    piHookHwnd = SetWindowsHookEx(WH_CBT, AddressOf HookProc, 0, App.ThreadID)
    Param = vbOKCancel
    If Len(NO) > 0 Then
        Param = vbYesNo
        If Len(Cancel) > 0 Then Param = vbYesNoCancel
    End If
    If dwMilliseconds < -1 Then
        Result = MsgBox(Message, MB_SETFOREGROUND Or Param, Title)
    Else
        Result = MessageBoxTimeout(0&, Message, Title, MB_SETFOREGROUND Or Param, 0&, dwMilliseconds)
    End If
    CustomMSGBOX = Result
    If ReturnAsString Then
        Select Case Result
            Case vbOK, vbYes: CustomMSGBOX = Yes
            Case vbNo: CustomMSGBOX = NO
            Case vbCancel: CustomMSGBOX = Cancel
            Case vbAbort: CustomMSGBOX = "[abort]" 'closed with the [x] button
            Case VBTIMEDOUT: CustomMSGBOX = "[timedout]"
        End Select
    End If
End Function

Public Function ScanForDirs(ByVal Directory As String) As String
    Dim tempstr() As String, temp As Long
    tempstr = Split(ScanDir(Directory, , vbDirectory), "|")
    For temp = 0 To UBound(tempstr)
        If Len(tempstr(temp)) > 0 Then
            If DirExists(Directory & "\" & tempstr(temp)) Then
                ScanForDirs = Append(ScanForDirs, tempstr(temp), "|")
            End If
        End If
    Next
End Function

Public Function EnumDirs(ByVal INIFilename As String, Section As String, ByVal KeyPrefix As String) As String
    Dim tempstr() As String, Count As Long, temp As Long
    tempstr = GetINIkeys(INIFilename, Section, Count, False, True)
    For temp = 1 To Count
        If StartsWith(tempstr(1, temp), KeyPrefix, vbTextCompare) Then
            EnumDirs = Append(EnumDirs, tempstr(2, temp))
        End If
    Next
End Function

Public Function RandomFile(ByVal Directories As String, Optional Filter As String = "*", Optional Recursive As Boolean, Optional Randomization As Long) As String
    Dim tempstr() As String, Count As Long
    RandomFile = ScanDir(Directories, Filter, , Recursive, IIf(Randomization < 1, -1, Randomization))
    tempstr = Split(RandomFile, "|")
    Count = ArrCount(tempstr)
    If Count > 0 Then
        RandomFile = tempstr(Random(0, Count - 1))
        If Not TextContains(RandomFile, "\") And Not TextContains(Directories, "|") Then
            RandomFile = ChkDir(Directories, RandomFile)
        End If
    End If
End Function

Public Function ScanDir(ByVal Directory As String, Optional Filter As String = "*", Optional Attributes As VbFileAttribute = vbNormal, Optional Recursive As Boolean, Optional ByRef Randomization As Long = -1, Optional IncludePath As Boolean, Optional Delimiter As String = "|") As String
    On Error Resume Next
    Dim sFileName As String, sFull As String, DIRS() As String, DIRcount As Long, oRand As Long, temp As Long, oFilter As String, OnlyDirs As Boolean
    oRand = Randomization
    IncludePath = IncludePath Or Randomization > -1 Or Recursive
    Select Case LCase(Filter)
        Case "images": Filter = "*.jpg;*.jpeg;*.png;*.gif"
    End Select
    If TextContains(Filter, ";") Then oFilter = Filter
    If TextContains(Directory, "|") Then
        IncludePath = True
        DIRS = Split(Directory, "|")
        DIRcount = ArrCount(DIRS) - 1
        Directory = DIRS(DIRcount)
    End If
    If Not EndsWith(Directory, "\") Then Directory = Directory & "\"
    If Recursive Then Attributes = Attributes Or vbDirectory
    sFileName = Dir(Directory & IfTrue(Len(oFilter) = 0, Filter), Attributes)
    
    Do While Len(sFileName) > 0
        If Not (sFileName = "." Or sFileName = "..") Then
            sFull = sFileName
            If IncludePath Then sFull = ChkDir(Directory, sFileName)
            If DirExists(sFull) Then
                If Recursive Then
                    DIRcount = DIRcount + 1
                    ReDim Preserve DIRS(DIRcount)
                    DIRS(DIRcount - 1) = sFull
                End If
                If Attributes = vbDirectory Then
                    ScanDir = Append(ScanDir, sFull, Delimiter)
                End If
            ElseIf Len(oFilter) = 0 Or IsLike(sFileName, oFilter) Then
                If Randomization = -1 Then
                    ScanDir = Append(ScanDir, sFull, Delimiter)
                Else
                    temp = Random(0, Randomization)
                    Debug.Print "Checking odds " & temp & " (" & IIf(temp = 0, "YES", "NO") & ") of " & sFull
                    If temp = 0 Then
                        ScanDir = sFull
                        Exit Function
                    End If
                    Randomization = Randomization - 1
                End If
            End If
        End If
        If Randomization = -1 Or Randomization > 0 Then sFileName = Dir()
    Loop
    
    If Randomization = -1 Then
        Do While DIRcount > 0 'dirs must be scanned after the files
            DIRcount = DIRcount - 1
            Directory = DIRS(DIRcount)
            sFileName = ScanDir(Directory, Filter, Attributes, True, Randomization, IncludePath, Delimiter)
            ScanDir = Append(ScanDir, sFileName, Delimiter)
        Loop
    ElseIf Len(ScanDir) = 0 Then
        If DIRcount > 0 Then
            ScanDir = ScanDir(DIRS(Random(0, DIRcount - 1)), Filter, Attributes, True, Randomization, IncludePath, Delimiter)
        ElseIf Randomization > 0 Then
            ScanDir = ScanDir(Directory, Filter, Attributes, Recursive, Randomization * 0.75, IncludePath, Delimiter)
        End If
    End If
End Function

Public Function IsPattern(Text As String) As Boolean
    IsPattern = TextContains(Text, "*") Or TextContains(Text, "?") Or TextContains(Text, "!") Or TextContains(Text, ";")
End Function

Public Function IsLike(ByVal Text As String, Optional ByVal Pattern As String) As Boolean
    Dim temp As Long, tempstr() As String
    If Len(Pattern) = 0 Then Exit Function
    tempstr = Split(LCase(Pattern), ";")
    Text = LCase(Text)
    If TextContains(Text, "!") Then
        For temp = LBound(tempstr) To UBound(tempstr)
            Pattern = tempstr(temp)
            If TextContains(Pattern, "!") Then
                Pattern = Replace(Pattern, "!", "")
                If IsLike(Text, Pattern) Then Exit Function
            End If
        Next
    End If
    For temp = LBound(tempstr) To UBound(tempstr)
        Pattern = tempstr(temp)
        If Pattern = "*" Then
            IsLike = True
        ElseIf IsPattern(Pattern) Then
            If Text Like Pattern Then
                IsLike = True
            End If
        ElseIf StartsWith(Pattern, "=") And EndsWith(Pattern, "=") Then
            IsLike = Text = Mid(Pattern, 2, Len(Pattern) - 2)
        Else
            IsLike = TextContains(Text, Pattern)
        End If
        If IsLike Then Exit Function
    Next
End Function

Public Function AppendIfNotExists(Existing() As String, ByVal Text As String, Key As String) As String
    Dim tempstr() As String, temp As Long
    If TextContains(Key, "|") Then
        tempstr = Split(Key, "|")
        For temp = 0 To UBound(tempstr)
            Text = AppendIfNotExists(Existing, Text, tempstr(temp))
        Next
    Else
        temp = IndexOf(Existing, Key, , vbTextCompare)
        If temp = -1 Then Text = Append(Text, Key)
    End If
    AppendIfNotExists = Text
End Function

Public Function TrimWS(ByVal Text As String) As String
    Const WHITE_SPACE As String = " " & vbTab & vbCr & vbLf
    If StrTrim(StrPtr(Text), StrPtr(WHITE_SPACE)) Then
        TrimWS = Left$(Text, lstrlen(StrPtr(Text)))
    Else
        TrimWS = Text
    End If
End Function

Public Function RemoveExtension(ByVal Filename As String, Optional NewExtension As String) As String
    Dim Extension As String
    Extension = GetExtension(Filename)
    If Len(Extension) > 0 Then Filename = Left(Filename, Len(Filename) - Len(Extension) - 1)
    If Len(NewExtension) > 0 Then Filename = Filename & "." & Replace(NewExtension, ".", "")
    RemoveExtension = Filename
End Function

Public Function MakeDir(Directory As String) As Boolean
    On Error Resume Next 'recursive!
    Dim tempstr() As String, temp As Long, tempdir As String
    MakeDir = DirExists(Directory)
    If Not MakeDir Then
        tempstr = Split(Directory, "\")
        tempdir = tempstr(0)
        For temp = 1 To UBound(tempstr)
            tempdir = ChkDir(tempdir, tempstr(temp))
            MakeDir = DirExists(tempdir)
            If Not MakeDir Then
                MkDir tempdir
                MakeDir = DirExists(tempdir)
            End If
        Next
    End If
End Function

Public Function CleanFilename(ByVal Filename As String, Optional RemoveExtensionAndFolder As Boolean = True) As String
    Dim Words() As String, temp As Long
    If RemoveExtensionAndFolder Then
        If TextContains(Filename, "\") Then Filename = GetExtension(Filename, "\", 0)
        Filename = RemoveExtension(Filename)
    End If
    Filename = RemoveBrackets(Trim(Filename))
    Filename = RemoveBrackets(Filename, "[", "]")
    Filename = Replace(Replace(Filename, ".", " "), "_", " ")
    If Len(FilterWordList) > 0 Then
        Words = Split(FilterWordList, " ")
        For temp = 0 To UBound(Words)
            Filename = Replace(Replace(Filename, " " & Words(temp), "", , , vbTextCompare), Words(temp) & " ", "", , , vbTextCompare)
        Next
    End If
    CleanFilename = Replace(Replace(Filename, " -", ""), " -", "")
End Function

Public Function RemoveBrackets(ByVal Text As String, Optional LBracket As String = "(", Optional RBracket As String = ")") As String
    Dim tempstr As String
    Do While TextContains(Text, LBracket) And TextContains(Text, RBracket)
        tempstr = LBracket & GetBetween(Text, LBracket, RBracket) & RBracket
        Text = Replace(Text, tempstr, "")
    Loop
    RemoveBrackets = Text
End Function

Public Sub BubbleSort1(ByRef pvarArray As Variant)
    Dim I As Long
    Dim iMin As Long
    Dim iMax As Long
    Dim varSwap As Variant
    Dim blnSwapped As Boolean
    
    iMin = LBound(pvarArray)
    iMax = UBound(pvarArray) - 1
    Do
        blnSwapped = False
        For I = iMin To iMax
            If pvarArray(I) > pvarArray(I + 1) Then
                varSwap = pvarArray(I)
                pvarArray(I) = pvarArray(I + 1)
                pvarArray(I + 1) = varSwap
                blnSwapped = True
            End If
        Next
        iMax = iMax - 1
    Loop Until Not blnSwapped
End Sub


Public Function TrimQuotes(ByVal Text As String, Optional OfWhat As String = """") As String
    If StartsWith(Text, OfWhat) And EndsWith(Text, OfWhat) Then Text = Mid(Text, 2, Len(Text) - 2)
    TrimQuotes = Text
End Function

Public Function IIFindex(Index As Long, ParamArray Values() As Variant) As String
    If Index = -1 Then Index = Random(LBound(Values), UBound(Values))
    If Index >= LBound(Values) And Index <= UBound(Values) Then IIFindex = Values(Index)
End Function

Public Function Copy(Text As String) As String
    On Error Resume Next
    Copy = Text
    Clipboard.Clear
    Clipboard.SetText Text
End Function


Public Function IsInIDE() As Boolean
    IsInIDE = App.LogMode = 0 'Or DebugMode
End Function

Public Function TextEquals(STR1 As String, STR2 As String, Optional Method As VbCompareMethod = vbTextCompare) As Boolean
    TextEquals = StrComp(STR1, STR2, Method) = 0
End Function

Function File_Get_Contents(Filename As String) As String
    On Error Resume Next
    Dim handle As Integer
    If FileExists(Filename) Then
        handle = FreeFile
        Open Filename For Input As #handle
            File_Get_Contents = Input$(LOF(handle), handle)
        Close #handle
    End If
End Function

Function File_Put_Contents(Filename As String, ByVal Contents As String, Optional AppendIt As Boolean) As String
    On Error Resume Next
    Dim handle As Integer
    handle = FreeFile
    If AppendIt Then
        Open Filename For Append As #handle
    Else
        Open Filename For Output As #handle
    End If
    Contents = TrimWS(Contents)
    Print #handle, Contents
    Close #handle
    File_Put_Contents = Filename
    If Len(Contents) = 0 Then File_Put_Contents = File_Put_Contents & " [EMPTY!]"
End Function

'Public Function FileExists(ByVal Fname As String, Optional MustNotBeEmpty As Boolean = True) As Boolean
'    Dim lRetVal As Long, OfSt As OFSTRUCT
'    Const HFILE_ERROR = &HFFFFFFFF
'    If Len(Fname) > 0 Then
'        lRetVal = OpenFile(Fname, OfSt, OF_EXIST)
'        FileExists = lRetVal <> HFILE_ERROR
'        If FileExists And MustNotBeEmpty Then FileExists = FileLen(Fname) > 0
'    End If
'End Function

Public Function DirExists(ByVal Directory As String) As Boolean
    On Error Resume Next
    If Len(Directory) > 0 Then DirExists = (GetAttr(Directory) And vbDirectory) <> 0
End Function

Public Function UnfoldRelativePath(ByVal sPath As String) As String
    Dim sBuff As String
    sBuff = Space$(261)
    If PathCanonicalize(sBuff, sPath) Then
        UnfoldRelativePath = Left$(sBuff, InStr(sBuff, vbNullChar) - 1)
    Else
        UnfoldRelativePath = sPath
    End If
End Function

Public Function InjectFilename(Filename As String, What2Inject As String, Optional NewExtension As String) As String
    Dim Extension As String
    Extension = GetExtension(Filename)
    If Len(Extension) = 0 Then
        InjectFilename = Filename & What2Inject
        If Len(NewExtension) > 0 Then InjectFilename = InjectFilename & "." & NewExtension
    Else
        If Len(NewExtension) = 0 Then NewExtension = Extension
        InjectFilename = Left(Filename, Len(Filename) - Len(Extension) - 1) & What2Inject & "." & NewExtension
    End If
End Function

Public Function ChkDir(Dir As String, Optional Filename As String) As String
    If IsFilename(Filename) Then
        ChkDir = Filename
    Else
        If Len(Dir) = 0 Then Dir = App.Path
        ChkDir = Replace(Dir & "\" & Filename, "\\", "\")
        If TextContains(ChkDir, "..") Then
            Dim sBuff As String, sPath As String
            sPath = ChkDir
            sBuff = Space$(261)
            If PathCanonicalize(sBuff, sPath) Then
                ChkDir = Left$(sBuff, InStr(sBuff, vbNullChar) - 1)
            End If
        End If
    End If
End Function

Public Sub TransBLT(SrcHDC As Long, xSrc As Long, ySrc As Long, Width As Long, Height As Long, DestHDC As Long, X As Long, Y As Long, Optional MaskHDC As Long, Optional Xmsk As Long = -1, Optional Ymsk As Long = -1)
    Const SRCPAINT = &HEE0086 'Assumes the mask matches the source's coordinates
    If MaskHDC = 0 Then
        MaskHDC = SrcHDC
    Else
        If Xmsk = -1 Then Xmsk = xSrc
        If Ymsk = -1 Then Ymsk = ySrc
    End If
    BitBlt DestHDC, X, Y, Width, Height, MaskHDC, Xmsk, Ymsk, SRCPAINT
    BitBlt DestHDC, X, Y, Width, Height, SrcHDC, xSrc, ySrc, vbSrcAnd
End Sub


Public Sub TransparentBlit(DestHDC As Long, DestX As Long, DestY As Long, Width As Long, Height As Long, SrcHDC As Long, SrcX As Long, SrcY As Long, Optional BufferBox As PictureBox, Optional Alpha As Byte = 255, Optional NewWidth As Long, Optional NewHeight As Long, Optional TransColor As Long = 8388863) 'rgb(255,0,128) <0 =no transparency
    If NewWidth = 0 Then NewWidth = Width
    If NewHeight = 0 Then NewHeight = Height
    If TransColor < 0 Then
        AlphaBlend DestHDC, DestX, DestY, NewWidth, NewHeight, SrcHDC, SrcX, SrcY, Width, Height, Alpha * 65536
    Else
        If Alpha = 255 Then
            TransparentBlt DestHDC, DestX, DestY, NewWidth, NewHeight, SrcHDC, SrcX, SrcY, Width, Height, TransColor
        ElseIf Alpha > 0 Then
            BufferBox.Width = NewWidth
            BufferBox.Height = NewHeight
            
            BitBlt BufferBox.hDC, 0, 0, NewWidth, NewHeight, DestHDC, DestX, DestY, vbSrcCopy
            TransparentBlt BufferBox.hDC, 0, 0, NewWidth, NewHeight, SrcHDC, SrcX, SrcY, Width, Height, TransColor
            AlphaBlend DestHDC, DestX, DestY, NewWidth, NewHeight, BufferBox.hDC, 0, 0, NewWidth, NewHeight, Alpha * 65536
        End If
    End If
End Sub



Public Function Dec(Hexadecimal As String) As Long
    On Error Resume Next
    Dec = MinLong
    If StartsWith(Hexadecimal, "0x", vbTextCompare) Then Hexadecimal = Right(Hexadecimal, Len(Hexadecimal) - 2)
    Dec = CLng("&H" & Hexadecimal)
End Function

Public Function Bytes2GBA(ParamArray Values() As Variant) As String
    Dim temp As Long
    For temp = 0 To UBound(Values)
        Bytes2GBA = Pad(Hex(Values(temp)), 2) & Bytes2GBA
    Next
    Bytes2GBA = "0x" & Bytes2GBA
End Function

Public Function FromHTMLColor(ByVal HTMLColor As String) As Long
    Dim R As Long, G As Long, b As Long
    HTMLColor = Replace(LCase(HTMLColor), "#", "")
    R = Dec(Mid(HTMLColor, 1, 2))
    G = Dec(Mid(HTMLColor, 3, 2))
    b = Dec(Mid(HTMLColor, 5, 2))
    FromHTMLColor = RGB(R, G, b)
End Function
Public Function ToHTMLColor(Color As Long, Optional Channel As Boolean) As String
    If Channel Then
        ToHTMLColor = Hex(Color)
        If Len(ToHTMLColor) = 1 Then ToHTMLColor = "0" & ToHTMLColor
    Else
        ToHTMLColor = "#" & ToHTMLColor(Red(Color), True) & ToHTMLColor(Green(Color), True) & ToHTMLColor(Blue(Color), True)
    End If
End Function

Public Function Red(Color As Long) As Long
    Red = Color Mod 256
End Function

Public Function Green(Color As Long) As Long
    Green = ((Color And &HFF00) / 256) Mod 256
End Function

Public Function Blue(Color As Long) As Long
    Blue = (Color And &HFF0000) / 65536
End Function

Public Function AlterBrightness(ByVal Color As OLE_COLOR, brightness As Long, Optional ForceChange As Boolean) As Long
    Dim R As Long, G As Long, b As Long
    Color = SysToLNG(Color)
    R = MinMax(Red(Color) + brightness, 0, 255, ForceChange)
    G = MinMax(Green(Color) + brightness, 0, 255, ForceChange)
    b = MinMax(Blue(Color) + brightness, 0, 255, ForceChange)
    AlterBrightness = RGB(R, G, b)
End Function

Public Function MinMax(Number As Long, Minimum As Long, Maximum As Long, Optional ForceChange As Boolean) As Long
    MinMax = Number
    If ForceChange Then
        If Number < Minimum Then MinMax = Number + Maximum
        If Number > Maximum Then MinMax = Number Mod Maximum
    Else
        If Number < Minimum Then MinMax = Minimum
        If Number > Maximum Then MinMax = Maximum
    End If
End Function


Public Function GetWidth(R As RECT) As Long
    GetWidth = R.Right - R.Left
End Function
Public Function GetHeight(R As RECT) As Long
    GetHeight = R.Bottom - R.Top
End Function
Public Function GetWidthHeight(R As RECT, ByRef Width As Long, ByRef Height As Long, Optional VectorName As String)
    If Len(VectorName) > 0 Then R = GetDimensions(VectorName)
    Width = R.Right - R.Left
    Height = R.Bottom - R.Top
End Function

Public Function getFile(strFileName As String) As String
    Dim strFile As String, nFile As Long
    nFile = FreeFile
    Open strFileName For Binary As #nFile
    strFile = String(LOF(nFile), " ")
    Get #nFile, , strFile
    Close #nFile
    getFile = strFile
End Function

Public Function GetBetween(Text As String, Start As String, Finish As String, Optional CompareMethod As VbCompareMethod = vbTextCompare, Optional IncludeStartEnd As Boolean) As String
    Dim StartL As Long, FinishL As Long
    StartL = InStr(1, Text, Start, CompareMethod)
    If StartL > 0 Then
        StartL = StartL + Len(Start)
        FinishL = InStr(StartL, Text, Finish, CompareMethod)
        If FinishL > 0 Then
            GetBetween = Mid(Text, StartL, FinishL - StartL)
            If IncludeStartEnd Then GetBetween = Start & GetBetween & Finish
        End If
    End If
End Function
Public Function GetBetweenReverse(Text As String, Finish As String, Start As String) As String
    Dim FinishL As Long, StartL As Long
    FinishL = InStr(Text, Finish)
    If FinishL > 0 Then
        StartL = InStrRev(Text, Start, FinishL)
        If StartL > 0 Then
            GetBetweenReverse = Mid(Text, StartL, FinishL - StartL)
        End If
    End If
End Function
Public Function EnumBetween(ByVal Text As String, Start As String, Finish As String, Optional ByRef Count As Long, Optional CompareMethod As VbCompareMethod = vbTextCompare, Optional IncludeStartEnd As Boolean) As String()
    Dim tempstr As String, stringarray() As String
    Count = 0
    tempstr = GetBetween(Text, Start, Finish, CompareMethod)
    Do While Len(Trim(tempstr)) > 0
        Count = Count + 1
        ReDim Preserve stringarray(Count)
        If IncludeStartEnd Then
            stringarray(Count - 1) = Start & tempstr & Finish
        Else
            stringarray(Count - 1) = tempstr
        End If
        Text = Replace(Text, Start & tempstr & Finish, "")
        tempstr = GetBetween(Text, Start, Finish, CompareMethod)
    Loop
    EnumBetween = stringarray
End Function

Public Function GetDirectory(ByVal Filename As String) As String
    If Right(Filename, 1) = "\" Then Filename = Left(Filename, Len(Filename) - 1)
    GetDirectory = GetSide(Filename, "\", True, True)
End Function
Public Function GetFilename(ByVal Filename As String, Optional IncludeExtension As Boolean = True) As String
    Dim Extension As String
    GetFilename = Filename
    If TextContains(Filename, "\") Then
        GetFilename = GetExtension(Filename, "\", 0)
    End If
    If Not IncludeExtension Then
        Extension = GetExtension(GetFilename, , 0)
        If Len(Extension) > 0 Then
            GetFilename = Replace(GetFilename, "." & Extension, "")
        End If
    End If
End Function

Public Function ReplaceExtension(ByVal Filename As String, NewExtension As String) As String
    ReplaceExtension = GetExtension(Filename)
    ReplaceExtension = Left(Filename, Len(Filename) - Len(ReplaceExtension)) & NewExtension
End Function

Public Function GetExtension(ByVal Filename As String, Optional LastChar As String = "/\.", Optional TextCase As Long = -1) As String
    Dim temp As Long, temp2 As Long, Done As Boolean
    If TextContains(Filename, Right(LastChar, 1)) Then
        For temp2 = 1 To Len(LastChar)
            temp = InStrRev(Filename, Mid(LastChar, temp2, 1))
            If temp > 0 Then
                Filename = Right(Filename, Len(Filename) - temp)
                Done = True
            End If
        Next
        If Not Done Then Exit Function
        If TextCase = -1 Then Filename = LCase(Filename)
        If TextCase = 1 Then Filename = UCase(TextCase)
        GetExtension = Filename
    End If
End Function

Public Function Ceil(ByVal Number) As Long
    If Number >= 0 Then
        If Number = Int(Number) Then
            Ceil = Number
        Else
            Ceil = Int(Number) + 1
        End If
    ElseIf Number < 0 Then
        Ceil = Int(Number)
    End If
End Function
Public Function Floor(ByVal Number) As Long
    Floor = Fix(Number)
End Function

Public Function ArrayIsInitialized(ARR) As Boolean
  Dim memVal As Long
  CopyMemory memVal, ByVal VarPtr(ARR) + 8, ByVal 4 'get pointer to array
  CopyMemory memVal, ByVal memVal, ByVal 4  'see if it points to an address...
  ArrayIsInitialized = (memVal <> 0)        '...if it does, array is intialized
End Function

Public Function ArrCount(ARR) As Long
    On Error Resume Next
    If ArrayIsInitialized(ARR) Then
        If LBound(ARR) = 0 Then
            ArrCount = UBound(ARR) + 1
        Else
            ArrCount = UBound(ARR)
        End If
    End If
End Function

Public Function TextContains(Text As String, ToLookFor As String, Optional CompareMethod As VbCompareMethod = vbBinaryCompare) As Boolean
    If Len(ToLookFor) > 0 Then TextContains = InStr(1, Text, ToLookFor, CompareMethod) > 0
End Function

Public Function GetSide(ByVal Text As String, Delimiter As String, Optional LeftSide As Boolean, Optional FromEnd As Boolean) As String
    Dim pos As Long
    If FromEnd Then
        pos = InStrRev(Text, Delimiter)
    Else
        pos = InStr(Text, Delimiter)
    End If
    If pos > 0 Then
        If LeftSide Then
            GetSide = Left(Text, pos - 1)
        Else
            GetSide = Right(Text, Len(Text) - pos - (Len(Delimiter) - 1))
        End If
    End If
End Function

Public Function GetVer() As Long
    GetVer = App.Major & Format(App.Minor, "000") & App.Revision
End Function

Public Function StartsWith(Text As String, ToLookFor As String, Optional CompareMethod As VbCompareMethod = vbBinaryCompare) As Boolean
    StartsWith = TextEquals(Left(Text, Len(ToLookFor)), ToLookFor, CompareMethod)
End Function
Public Function EndsWith(Text As String, ToLookFor As String, Optional CompareMethod As VbCompareMethod = vbBinaryCompare) As Boolean
    EndsWith = TextEquals(Right(Text, Len(ToLookFor)), ToLookFor, CompareMethod)
End Function
Public Function Max(ParamArray Numbers() As Variant) As Currency
    Dim largest As Currency, temp As Long
    For temp = 0 To UBound(Numbers)
        If IsNumeric(Numbers(temp)) Then
            If temp = 0 Or Numbers(temp) > largest Then largest = Numbers(temp)
        End If
    Next
    Max = largest
End Function
Public Function Min(ParamArray Numbers() As Variant) As Currency
    Dim Smallest As Currency, temp As Long
    For temp = 0 To UBound(Numbers)
        If temp = 0 Or Numbers(temp) < Smallest Then Smallest = Numbers(temp)
    Next
    Min = Smallest
End Function

Public Function GetMouseLoc() As POINTAPI
     Dim RECT As POINTAPI
     Call GetCursorPos(RECT)
     GetMouseLoc = RECT
End Function
Public Function GetHwndUnderMouse() As Long
    Dim LOC As POINTAPI
    LOC = GetMouseLoc()
    GetHwndUnderMouse = WindowFromPoint(LOC.X, LOC.Y)
End Function

Public Function LoadMenu(Menu As Object, CSV As String) As Long
    Dim tempstr() As String, temp As Long, Section As String, Value As String, sType As String
    For temp = Menu.UBound To 1 Step -1
        Unload Menu(temp)
    Next
    If Not TextContains(CSV, ",") And Not TextContains(CSV, "|") Then
        Section = LCase(CSV)
        Select Case Section
            'Case "android": CSV = "TRACK PREV,TRACK PLAY,TRACK NEXT,ANSWER,FIND"
            
        End Select
    End If
    tempstr = Split(Replace(CSV, "|", ","), ",")
    For temp = 0 To UBound(tempstr)
        Value = Trim(tempstr(temp))
        If Len(Value) > 0 Then
            If LoadMenu > 0 Then Load Menu(LoadMenu)
            With Menu(LoadMenu)
                sType = "bool"
                If TextContains(Value, "[") And TextContains(Value, "]") Then
                    sType = GetBetween(Value, "[", "]")
                    Value = Replace(Value, "[" & sType & "]", "")
                    sType = Trim(LCase(sType))
                End If
                .Tag = sType
                .Caption = Replace(Value, "@", "")
                .Checked = TextContains(Value, "@")
                Select Case Section
                    'Case "settings"
                    '    Select Case LCase(sType)
                    '        Case "bool"
                    '            If GetINISettingBool(INIFILE, "settings", .Caption) Then
                    '                mnu_settings_Click CInt(temp)
                    '            End If
                    '        Case Else
                    '            mnu_settings_Click CInt(temp)
                    '    End Select
                    'Case "modes"
                    '    If TextEquals(.Caption, "clock") Then .Checked = True
                End Select
            End With
            LoadMenu = LoadMenu + 1
        End If
    Next
End Function






Public Function RemoveListItem(LIST As ListBox, Item As String) As Long
    Dim temp As Long
    RemoveListItem = FindListItem(LIST, Item, vbTextCompare, False)
    If RemoveListItem > -1 Then LIST.RemoveItem RemoveListItem
End Function
Public Function AddListItems(LIST As ListBox, Items As String)
    Dim tempstr() As String, temp As Long
    If Len(Items) > 0 Then
        tempstr = Split(Replace(Items, ",", "|"), "|")
        For temp = 0 To UBound(tempstr)
            If Len(tempstr(temp)) > 0 Then LIST.AddItem tempstr(temp) ',  tempstr(temp)
        Next
    End If
End Function

Public Function FindListItem(LIST As ListBox, Item As String, Optional CompareMethod As VbCompareMethod = vbBinaryCompare, Optional AddIfNotFouind As Boolean) As Long
    Dim temp As Long
    FindListItem = -1
    If Len(Item) > 0 Then
        For temp = 0 To LIST.ListCount
            If StrComp(Item, LIST.LIST(temp), CompareMethod) = 0 Then
                FindListItem = temp
                Exit For
            End If
        Next
    End If
    If AddIfNotFouind Then
        LIST.AddItem Item
        FindListItem = FindListItem(LIST, Item, vbBinaryCompare, False)
    End If
End Function

Private Sub SetUpTwoToTheN()
    Dim temp As Long, temp2 As Long
    If TwoToTheN(1) = 0 Then
        temp2 = 1
        For temp = 1 To 28
            TwoToTheN(temp) = temp2
            temp2 = temp2 * 2
        Next
        TwoToTheN(30) = 536870912 'for some reason, VB wont make numbers this high at runtime
        TwoToTheN(31) = 1073741824
        TwoToTheN(32) = -2147483648# 'since this is the maximum value, all bits are 1. including the one that makes it a negative number '4294967296#
    End If
End Sub

Private Function GetButton(Buttons As Long, Button As Long) As Boolean
    Dim Value As Long
    Value = TwoToTheN(Button)
    If Value > 0 Then GetButton = (Buttons And Value) = Value
End Function

Public Function DecimalToBinary(ByVal DecimalNum As Long, Optional MaxDigits As Long = 32) As String
    Dim temp As Long 'MOST SIGNIFICANT DIGIT ON LEFT to LOWEST SIGNIFICANT DIGIT ON RIGHT
    SetUpTwoToTheN
    For temp = 1 To MaxDigits
        DecimalToBinary = IIf(GetButton(DecimalNum, temp), 1, 0) & DecimalToBinary
    Next
End Function
Public Function BinaryToDecimal(Binary As String) As Long
    Dim temp As Long, Index As Long 'MOST SIGNIFICANT DIGIT ON LEFT to LOWEST SIGNIFICANT DIGIT ON RIGHT
    SetUpTwoToTheN
    For temp = Len(Binary) To 1 Step -1
        Index = Index + 1
        If Mid(Binary, temp, 1) = "1" Then
            BinaryToDecimal = BinaryToDecimal + TwoToTheN(Index)
        End If
    Next
End Function

Public Function FileModified(Filename As String) As String
    FileModified = "0000-00-00 00:00:00"
    If FileExists(Filename) Then FileModified = Format(FileDateTime(Filename), "yyyy-MM-dd HH:nn:ss")
End Function
Public Function FileModifiedCurr(Filename As String) As Currency
    If FileExists(Filename) Then FileModifiedCurr = Format(FileDateTime(Filename), "yyyyMMddHHnnss")
End Function
Public Function DateTime2Curr(Optional DateTime As String) As Currency
    If Len(DateTime) = 0 Then
        DateTime2Curr = Format(Now, "yyyyMMddHHnnss")
    Else
        DateTime2Curr = Replace(Replace(Replace(DateTime, "-", ""), ":", ""), " ", "")
    End If
End Function
