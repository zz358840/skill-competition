VERSION 5.00
Begin VB.Form frmMain 
   Caption         =   "104學年度  工業類科學生技藝競賽  電腦修護職種  第二站 崗位號碼：001"
   ClientHeight    =   8208
   ClientLeft      =   192
   ClientTop       =   420
   ClientWidth     =   13536
   FillColor       =   &H000000FF&
   FillStyle       =   0  '實心
   BeginProperty Font 
      Name            =   "細明體"
      Size            =   14.4
      Charset         =   136
      Weight          =   700
      Underline       =   -1  'True
      Italic          =   0   'False
      Strikethrough   =   -1  'True
   EndProperty
   ForeColor       =   &H000000FF&
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   ScaleHeight     =   8208
   ScaleMode       =   0  '使用者自訂
   ScaleWidth      =   13525.13
   StartUpPosition =   2  '螢幕中央
   Begin VB.Timer color_and_runhorse_timer 
      Interval        =   1
      Left            =   2040
      Top             =   1080
   End
   Begin VB.Timer vbbutton_color_timer 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   2400
      Top             =   3960
   End
   Begin VB.Timer vbbutton_runhorse_timer 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   2400
      Top             =   4440
   End
   Begin VB.Timer adc_timer 
      Interval        =   1000
      Left            =   3240
      Top             =   5400
   End
   Begin VB.CommandButton button 
      Caption         =   "S8"
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Index           =   18
      Left            =   3288
      TabIndex        =   18
      Top             =   4080
      Width           =   732
   End
   Begin VB.CommandButton button 
      Caption         =   "S7"
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Index           =   17
      Left            =   4177
      TabIndex        =   17
      Top             =   4080
      Width           =   732
   End
   Begin VB.CommandButton button 
      Caption         =   "S6"
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Index           =   16
      Left            =   5066
      TabIndex        =   16
      Top             =   4080
      Width           =   732
   End
   Begin VB.CommandButton button 
      Caption         =   "S5"
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Index           =   15
      Left            =   5955
      TabIndex        =   15
      Top             =   4080
      Width           =   732
   End
   Begin VB.CommandButton button 
      Caption         =   "S4"
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Index           =   14
      Left            =   6844
      TabIndex        =   14
      Top             =   4080
      Width           =   732
   End
   Begin VB.CommandButton button 
      Caption         =   "S3"
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Index           =   13
      Left            =   7733
      TabIndex        =   13
      Top             =   4080
      Width           =   732
   End
   Begin VB.CommandButton button 
      Caption         =   "S2"
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Index           =   12
      Left            =   8622
      TabIndex        =   12
      Top             =   4080
      Width           =   732
   End
   Begin VB.CommandButton button 
      Caption         =   "S1"
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Index           =   11
      Left            =   9516
      TabIndex        =   11
      Top             =   4080
      Width           =   732
   End
   Begin VB.TextBox vrefvalue 
      Alignment       =   2  '置中對齊
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   9360
      TabIndex        =   8
      Top             =   5280
      Width           =   3012
   End
   Begin VB.TextBox stationnumbervalue 
      Alignment       =   2  '置中對齊
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   9360
      TabIndex        =   7
      Text            =   "001"
      Top             =   6120
      Width           =   1215
   End
   Begin VB.TextBox keyvalue 
      Alignment       =   2  '置中對齊
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   5040
      TabIndex        =   5
      Top             =   6240
      Width           =   1215
   End
   Begin VB.CommandButton button 
      Caption         =   "EXIT"
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Index           =   3
      Left            =   8160
      TabIndex        =   2
      Top             =   7080
      Width           =   2055
   End
   Begin VB.TextBox advalue 
      Alignment       =   2  '置中對齊
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   5040
      TabIndex        =   0
      Top             =   5280
      Width           =   1215
   End
   Begin VB.Shape led 
      BorderStyle     =   0  '透明
      FillColor       =   &H00FFFFFF&
      Height          =   696
      Index           =   9
      Left            =   960
      Shape           =   3  '圓形
      Top             =   7560
      Width           =   732
   End
   Begin VB.Shape led 
      BorderStyle     =   0  '透明
      FillColor       =   &H00FFFFFF&
      Height          =   696
      Index           =   8
      Left            =   0
      Shape           =   3  '圓形
      Top             =   7440
      Width           =   732
   End
   Begin VB.Label nowtime 
      Alignment       =   2  '置中對齊
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  '單線固定
      BeginProperty Font 
         Name            =   "細明體"
         Size            =   28.2
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1692
      Left            =   3102
      TabIndex        =   10
      Top             =   1080
      Width           =   7332
   End
   Begin VB.Label vreftext 
      Caption         =   "參考電壓值"
      BeginProperty Font 
         Name            =   "新細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   372
      Left            =   7440
      TabIndex        =   9
      Top             =   5400
      Width           =   1572
   End
   Begin VB.Label stationnumbertext 
      Caption         =   "Station Number"
      BeginProperty Font 
         Name            =   "新細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   372
      Left            =   7080
      TabIndex        =   6
      Top             =   6240
      Width           =   2172
   End
   Begin VB.Label keytext 
      Caption         =   "key"
      BeginProperty Font 
         Name            =   "新細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   372
      Left            =   4320
      TabIndex        =   4
      Top             =   6240
      Width           =   1092
   End
   Begin VB.Shape led 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  '實心
      Height          =   696
      Index           =   0
      Left            =   3288
      Shape           =   3  '圓形
      Top             =   3024
      Width           =   732
   End
   Begin VB.Shape led 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  '實心
      Height          =   696
      Index           =   1
      Left            =   4177
      Shape           =   3  '圓形
      Top             =   3024
      Width           =   732
   End
   Begin VB.Shape led 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  '實心
      Height          =   696
      Index           =   2
      Left            =   5066
      Shape           =   3  '圓形
      Top             =   3036
      Width           =   732
   End
   Begin VB.Shape led 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  '實心
      Height          =   696
      Index           =   3
      Left            =   5955
      Shape           =   3  '圓形
      Top             =   3036
      Width           =   732
   End
   Begin VB.Shape led 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  '實心
      Height          =   696
      Index           =   4
      Left            =   6844
      Shape           =   3  '圓形
      Top             =   3036
      Width           =   732
   End
   Begin VB.Shape led 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  '實心
      Height          =   696
      Index           =   5
      Left            =   7733
      Shape           =   3  '圓形
      Top             =   3036
      Width           =   732
   End
   Begin VB.Shape led 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  '實心
      Height          =   696
      Index           =   6
      Left            =   8622
      Shape           =   3  '圓形
      Top             =   3036
      Width           =   732
   End
   Begin VB.Shape led 
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  '實心
      Height          =   696
      Index           =   7
      Left            =   9516
      Shape           =   3  '圓形
      Top             =   3036
      Width           =   732
   End
   Begin VB.Label usbstate 
      Alignment       =   2  '置中對齊
      BackColor       =   &H0000FFFF&
      BorderStyle     =   1  '單線固定
      Caption         =   "DEVICE OFF"
      BeginProperty Font 
         Name            =   "新細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   372
      Left            =   1320
      TabIndex        =   3
      Top             =   6240
      Width           =   1932
   End
   Begin VB.Label adtext 
      Caption         =   "ADC值"
      BeginProperty Font 
         Name            =   "新細明體"
         Size            =   14.4
         Charset         =   136
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   372
      Left            =   3876
      TabIndex        =   1
      Top             =   5400
      Width           =   1572
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim i, j As Integer
Public button_index, vb_write_ic_color_button As Integer 'index傳給a 由a判斷功能 /vb_write_ic_color是vb控制ic顏色
Dim vbclean, icclean As Integer '清除vb和板子的led的暫存判斷變數
Dim runhorse_temp As Integer '跑馬燈暫存變數
Dim vbbutton_ledcolor(9) As Integer 'led顏色選擇
Sub vbclean_all_led() '清除vb所有led
    For i = 0 To 7
        led(i).FillColor = RGB(0, 0, 0)
    Next i
End Sub
Sub icclean_all_led() '清除板子所有led
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    If (Result) Then
        OutDataEightByte 0, 0, 0, 0, 0, 0, 0, 0
        OutDataEightByte 0, 1, 0, 0, 0, 0, 0, 0
        OutDataEightByte 0, 2, 0, 0, 0, 0, 0, 0
        OutDataEightByte 0, 3, 0, 0, 0, 0, 0, 0
        OutDataEightByte 0, 4, 0, 0, 0, 0, 0, 0
        OutDataEightByte 0, 5, 0, 0, 0, 0, 0, 0
        OutDataEightByte 0, 6, 0, 0, 0, 0, 0, 0
        OutDataEightByte 0, 7, 0, 0, 0, 0, 0, 0
        CloseUsbDevice
    End If
End Sub
Sub icclean_button_nowled() '清除板子目前閃爍的led(給按鈕用)
    Dim Result As Boolean
    Delay (1)
    Result = OpenUsbDevice(VendorID, ProductID)
    If (Result) Then
        OutDataEightByte 0, vb_write_ic_color_button, 0, 0, 0, 0, 0, 0
        CloseUsbDevice
    End If
End Sub
Sub change_mode() '板子切換回開機模式
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    If (Result) Then
        OutDataEightByte 0, 0, 0, 0, 0, 0, 0, 1
        OutDataEightByte 0, 1, 0, 0, 0, 0, 0, 1
        OutDataEightByte 0, 2, 0, 0, 0, 0, 0, 1
        OutDataEightByte 0, 3, 0, 0, 0, 0, 0, 1
        OutDataEightByte 0, 4, 0, 0, 0, 0, 0, 1
        OutDataEightByte 0, 5, 0, 0, 0, 0, 0, 1
        OutDataEightByte 0, 6, 0, 0, 0, 0, 0, 1
        OutDataEightByte 0, 7, 0, 0, 0, 0, 0, 1
        CloseUsbDevice
    End If
End Sub
Private Sub Form_Load() '程式啟動
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    
    If (Result) Then
        For i = 0 To 7
            OutDataEightByte 0, i, 0, 0, 0, 0, 0, 0
        Next i
        CloseUsbDevice
    End If
    vbclean_all_led
    runhorse_temp = 1
    vb_write_ic_color_button = 7
End Sub
Sub vbclean_s1() '清除vb目前閃爍的led(給vb按鈕1用)
    Dim Result As Boolean
    Delay (1)
    Result = OpenUsbDevice(VendorID, ProductID)
    If (Result) Then
        OutDataEightByte 0, vb_write_ic_color_button, 0, 0, 0, 0, 0, 0
        CloseUsbDevice
    End If
    led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0)
End Sub
Sub vbclean_s2() '清除vb目前閃爍的led(給vb按鈕2用)
    Dim Result As Boolean
    Delay (1)
    Result = OpenUsbDevice(VendorID, ProductID)
    If (Result) Then
        OutDataEightByte 0, vb_write_ic_color_button, 0, 0, 0, 0, 0, 0
        CloseUsbDevice
    End If
    led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0)
End Sub
Sub vbclean_s3() '清除vb目前閃爍的led(給vb按鈕3用)
    Dim Result As Boolean
    Delay (1)
    Result = OpenUsbDevice(VendorID, ProductID)
    If (Result) Then
        OutDataEightByte 0, vb_write_ic_color_button, 0, 0, 0, 0, 0, 0
        CloseUsbDevice
    End If
    led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0)
End Sub
Sub vbclean_s4() '清除vb目前閃爍的led(給vb按鈕4用)
    Dim Result As Boolean
    Delay (1)
    Result = OpenUsbDevice(VendorID, ProductID)
    If (Result) Then
        OutDataEightByte 0, vb_write_ic_color_button, 0, 0, 0, 0, 0, 0
        CloseUsbDevice
    End If
    led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0)
End Sub
Sub vbclean_s5() '清除vb目前閃爍的led(給vb按鈕5用)
    Dim Result As Boolean
    Delay (1)
    Result = OpenUsbDevice(VendorID, ProductID)
    If (Result) Then
        CloseUsbDevice
    End If
End Sub
Sub vbclean_s6() '清除vb目前閃爍的led(給vb按鈕6用)
    Dim Result As Boolean
    Delay (1)
    Result = OpenUsbDevice(VendorID, ProductID)
    If (Result) Then
        OutDataEightByte 0, vb_write_ic_color_button, 0, 0, 0, 0, 0, 0
        OutDataEightByte 0, Abs(vb_write_ic_color_button - 7), 0, 0, 0, 0, 0, 0
        CloseUsbDevice
    End If
    led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0)
    led(Abs(vb_write_ic_color_button - 7)).FillColor = RGB(0, 0, 0)
End Sub
Sub vbclean_s7() '清除vb目前閃爍的led(給vb按鈕7用)
    Dim Result As Boolean
    Delay (1)
    Result = OpenUsbDevice(VendorID, ProductID)
    If (Result) Then
        OutDataEightByte 0, Abs(vb_write_ic_color_button), 0, 0, 0, 0, 0, 0
        CloseUsbDevice
    End If
    led(Abs(vb_write_ic_color_button)).FillColor = RGB(0, 0, 0)
End Sub
Sub vbclean_s8() '清除vb目前閃爍的led(給vb按鈕8用)
    Dim Result As Boolean
    Delay (1)
    Result = OpenUsbDevice(VendorID, ProductID)
    If (Result) Then
        OutDataEightByte 0, vb_write_ic_color_button, 0, 0, 0, 0, 0, 0
        OutDataEightByte 0, vb_write_ic_color_button - 1, 0, 0, 0, 0, 0, 0
        CloseUsbDevice
    End If
End Sub
Private Sub button_click(Index As Integer) '取按鈕的index
    button_index = Index
End Sub
Private Sub adc_timer_Timer() 'ADC的計時器
    Dim Result As Boolean
'    Dim Voltage As Single
    Dim Voltage As Double
        
'    Const VREF = 2.56
    Const VREF = 5
    Dim adc_temp As Integer
    
    Result = OpenUsbDevice(VendorID, ProductID)
    
    If (Result) Then
        ReadData ReadKeyData
        adc_temp = (256 * ReadKeyData(8) + ReadKeyData(7)) ' / 4.01 '除以4.01可測試是否等於0~255
        advalue.Text = adc_temp
        Voltage = (256 * ReadKeyData(8) + ReadKeyData(7)) * VREF / 1024  '先計算
        Voltage = Format(Voltage, "0.00")
        vrefvalue.Text = Voltage & "V" '將運算值給此元件 並加上V文字
        usbstate.Caption = "DEVICE ON"
    Else
        advalue.Text = " "
        vrefvalue.Text = " "
        usbstate.Caption = "DEVICE OFF"
    End If
    CloseUsbDevice
End Sub
Private Sub color_and_runhorse_timer_Timer()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    
    If (Result) Then
        ReadData ReadKeyData
        CloseUsbDevice
'VB的key該顯示還是清除
        If (ReadKeyData(6) = 0) Then
            keyvalue.Text = " "
        ElseIf (ReadKeyData(6) > 0) Then
            keyvalue.Text = "S" & ReadKeyData(6)
        End If
        
        
        Select Case (ReadKeyData(5))
        Case 0: '板子開機模式 VB同步
            For i = 0 To 7
                led(i).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
            Next i
        Case 1: '板子按鈕模式 VB同步
            led(ReadKeyData(1)).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
        Case 2: '板子跑馬燈 VB同步
'因為key7功能是右閃到左後左閃到右 所以下方遞減數字要轉換
            If (ReadKeyData(1) = 7) Then
                vbclean_all_led
                runhorse_temp = 1
            End If
            If (ReadKeyData(1) = 0) Then
                runhorse_temp = -1
            End If
'讀key值讓vb同步
            Select Case (ReadKeyData(6))
                Case 1:
                    led(ReadKeyData(1)).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
                    led(ReadKeyData(1) + 1).FillColor = RGB(0, 0, 0)
                Case 2:
                    led(ReadKeyData(1)).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
                    led(ReadKeyData(1) + 2).FillColor = RGB(0, 0, 0)
                Case 3:
                    If (ReadKeyData(1) = 6) Then
                        vbclean_all_led
                    End If
                    led(ReadKeyData(1)).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
                    led(ReadKeyData(1) + 2).FillColor = RGB(0, 0, 0)
                Case 4:
                    led(ReadKeyData(1)).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
                    led(Abs(ReadKeyData(1) - 1)).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
                    led(ReadKeyData(1) + 1).FillColor = RGB(0, 0, 0)
                Case 5:
                    led(ReadKeyData(1)).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
                Case 6:
                    led(ReadKeyData(1)).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
                    led(Abs(ReadKeyData(1) - 7)).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
                    led(Abs(ReadKeyData(1) + 1)).FillColor = RGB(0, 0, 0)
                    led(Abs(Abs(ReadKeyData(1) - 7) - 1)).FillColor = RGB(0, 0, 0)
                Case 7:
                    led(ReadKeyData(1)).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
                    led(Abs(ReadKeyData(1) + runhorse_temp)).FillColor = RGB(0, 0, 0)
                Case 8:
                    led(ReadKeyData(1)).FillColor = RGB(ReadKeyData(2), ReadKeyData(3), ReadKeyData(4))
                    led(Abs(ReadKeyData(1) + runhorse_temp)).FillColor = RGB(0, 0, 0)
            End Select
        End Select

    
    Else
        keyvalue.Text = " "
        For i = 0 To 7
            led(i).FillColor = RGB(255, 255, 255)
        Next i
    End If

    nowtime.Caption = "                    " & "Current Time :" & Time$
'If (button_index = 11 Or button_index = 12 Or button_index = 13 Or button_index = 14 Or button_index = 15 Or button_index = 16 Or button_index = 17 Or button_index = 18) Then vbbutton_color_timer.Enabled = True
If (button_index = 11 Or button_index = 12 Or button_index = 13 Or button_index = 14 Or button_index = 15 Or button_index = 16 Or button_index = 17 Or button_index = 18) Then vbbutton_runhorse_timer.Enabled = True
If button_index = 3 Then
change_mode
End
End If
End Sub
Private Sub vbbutton_color_timer_Timer() 'vb按鈕單一顏色設定計時器
Select Case (button_index)
Case 11:
color_s1
Case 12:
color_s2
Case 13:
color_s3
Case 14:
color_s4
Case 15:
color_s5
Case 16:
color_s6
Case 17:
color_s7
Case 18:
color_s8
End Select
End Sub
Private Sub vbbutton_runhorse_timer_Timer() 'vb按鈕跑馬燈計時器
Select Case (button_index)
Case 11:
runhorse2_s1
Case 12:
runhorse2_s2
Case 13:
runhorse2_s3
Case 14:
runhorse2_s4
Case 15:
runhorse2_s5
Case 16:
runhorse2_s6
Case 17:
runhorse2_s7
Case 18:
runhorse2_s8
End Select
End Sub
Sub color_s1()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    vbbutton_ledcolor(0) = vbbutton_ledcolor(0) + 1 '遞增1 每次必從1開始
    vbbutton_ledcolor(0) = vbbutton_ledcolor(0) Mod 9 '累加到9後歸0(即黑色)
    If (Result) Then
        Select Case (vbbutton_ledcolor(0))
        Case 1
        red
        Case 2
        green
        Case 3
        blue
        Case 4
        yellow
        Case 5
        cyan
        Case 6
        purple
        Case 7
        white
        Case 8
        gray
        Case 0
        black
        End Select

        OutDataEightByte 0, 7, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(7).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        button_index = 0
        vbbutton_color_timer.Enabled = False
    End If
End Sub
Sub color_s2()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    vbbutton_ledcolor(1) = vbbutton_ledcolor(1) + 1 '遞增1 每次必從1開始
    vbbutton_ledcolor(1) = vbbutton_ledcolor(1) Mod 9 '累加到9後歸0(即黑色)
    If (Result) Then
        Select Case (vbbutton_ledcolor(1))
        Case 1
        red
        Case 2
        green
        Case 3
        blue
        Case 4
        yellow
        Case 5
        cyan
        Case 6
        purple
        Case 7
        white
        Case 8
        gray
        Case 0
        black
        End Select

        OutDataEightByte 0, 6, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(6).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        button_index = 0
        vbbutton_color_timer.Enabled = False
    End If
End Sub
Sub color_s3()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    vbbutton_ledcolor(2) = vbbutton_ledcolor(2) + 1 '遞增1 每次必從1開始
    vbbutton_ledcolor(2) = vbbutton_ledcolor(2) Mod 9 '累加到9後歸0(即黑色)
    If (Result) Then
        Select Case (vbbutton_ledcolor(2))
        Case 1
        red
        Case 2
        green
        Case 3
        blue
        Case 4
        yellow
        Case 5
        cyan
        Case 6
        purple
        Case 7
        white
        Case 8
        gray
        Case 0
        black
        End Select

        OutDataEightByte 0, 5, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(5).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        button_index = 0
        vbbutton_color_timer.Enabled = False
    End If
End Sub
Sub color_s4()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    vbbutton_ledcolor(3) = vbbutton_ledcolor(3) + 1 '遞增1 每次必從1開始
    vbbutton_ledcolor(3) = vbbutton_ledcolor(3) Mod 9 '累加到9後歸0(即黑色)
    If (Result) Then
        Select Case (vbbutton_ledcolor(3))
        Case 1
        red
        Case 2
        green
        Case 3
        blue
        Case 4
        yellow
        Case 5
        cyan
        Case 6
        purple
        Case 7
        white
        Case 8
        gray
        Case 0
        black
        End Select

        OutDataEightByte 0, 4, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(4).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        button_index = 0
        vbbutton_color_timer.Enabled = False
    End If
End Sub
Sub color_s5()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    vbbutton_ledcolor(4) = vbbutton_ledcolor(4) + 1 '遞增1 每次必從1開始
    vbbutton_ledcolor(4) = vbbutton_ledcolor(4) Mod 9 '累加到9後歸0(即黑色)
    If (Result) Then
        Select Case (vbbutton_ledcolor(4))
        Case 1
        red
        Case 2
        green
        Case 3
        blue
        Case 4
        yellow
        Case 5
        cyan
        Case 6
        purple
        Case 7
        white
        Case 8
        gray
        Case 0
        black
        End Select

        OutDataEightByte 0, 3, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(3).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        button_index = 0
        vbbutton_color_timer.Enabled = False
    End If
End Sub
Sub color_s6()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    vbbutton_ledcolor(5) = vbbutton_ledcolor(5) + 1 '遞增1 每次必從1開始
    vbbutton_ledcolor(5) = vbbutton_ledcolor(5) Mod 9 '累加到9後歸0(即黑色)
    If (Result) Then
        Select Case (vbbutton_ledcolor(5))
        Case 1
        red
        Case 2
        green
        Case 3
        blue
        Case 4
        yellow
        Case 5
        cyan
        Case 6
        purple
        Case 7
        white
        Case 8
        gray
        Case 0
        black
        End Select

        OutDataEightByte 0, 2, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(2).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        button_index = 0
        vbbutton_color_timer.Enabled = False
    End If
End Sub
Sub color_s7()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    vbbutton_ledcolor(6) = vbbutton_ledcolor(6) + 1 '遞增1 每次必從1開始
    vbbutton_ledcolor(6) = vbbutton_ledcolor(6) Mod 9 '累加到9後歸0(即黑色)
    If (Result) Then
        Select Case (vbbutton_ledcolor(6))
        Case 1
        red
        Case 2
        green
        Case 3
        blue
        Case 4
        yellow
        Case 5
        cyan
        Case 6
        purple
        Case 7
        white
        Case 8
        gray
        Case 0
        black
        End Select

        OutDataEightByte 0, 1, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(1).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        button_index = 0
        vbbutton_color_timer.Enabled = False
    End If
End Sub
Sub color_s8()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)
    vbbutton_ledcolor(7) = vbbutton_ledcolor(7) + 1 '遞增1 每次必從1開始
    vbbutton_ledcolor(7) = vbbutton_ledcolor(7) Mod 9 '累加到9後歸0(即黑色)
    If (Result) Then
        Select Case (vbbutton_ledcolor(7))
        Case 1
        red
        Case 2
        green
        Case 3
        blue
        Case 4
        yellow
        Case 5
        cyan
        Case 6
        purple
        Case 7
        white
        Case 8
        gray
        Case 0
        black
        End Select

        OutDataEightByte 0, 0, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(0).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        button_index = 0
        vbbutton_color_timer.Enabled = False
    End If
End Sub
Sub runhorse_s1()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        red

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        icclean_button_nowled '清除板子目前閃爍的led
        led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0) '清除vb目前閃爍的led
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = -1 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse_s2()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        green

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        icclean_button_nowled '清除板子目前閃爍的led
        led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0) '清除vb目前閃爍的led
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = -1 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse_s3()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        blue

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        icclean_button_nowled '清除板子目前閃爍的led
        led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0) '清除vb目前閃爍的led
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = -1 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub

Sub runhorse_s4()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        yellow

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        icclean_button_nowled '清除板子目前閃爍的led
        led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0) '清除vb目前閃爍的led
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = -1 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse_s5()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        cyan

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        icclean_button_nowled '清除板子目前閃爍的led
        led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0) '清除vb目前閃爍的led
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = -1 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse_s6()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        purple

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        icclean_button_nowled '清除板子目前閃爍的led
        led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0) '清除vb目前閃爍的led
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = -1 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse_s7()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        white

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        icclean_button_nowled '清除板子目前閃爍的led
        led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0) '清除vb目前閃爍的led
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = -1 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse_s8()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        gray

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        icclean_button_nowled '清除板子目前閃爍的led
        led(vb_write_ic_color_button).FillColor = RGB(0, 0, 0) '清除vb目前閃爍的led
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = -1 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Sub runhorse2_s1()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        red

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        vbclean_s1
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = -1 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse2_s2()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        green

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        vbclean_s2
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 2
 
    If vb_write_ic_color_button = -1 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse2_s3()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
        vb_write_ic_color_button = 6
    End If

    If (Result) Then

        blue

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        vbclean_s3
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 2
 
    If vb_write_ic_color_button = -2 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse2_s4()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        yellow

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        OutDataEightByte 0, vb_write_ic_color_button - 1, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        led(vb_write_ic_color_button - 1).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        vbclean_s4
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = 0 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub

Sub runhorse2_s5()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        cyan

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        vbclean_s5
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = -1 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse2_s6()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        purple

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        OutDataEightByte 0, Abs(vb_write_ic_color_button - 7), color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        led(Abs(vb_write_ic_color_button - 7)).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        vbclean_s6
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = 3 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse2_s7()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
        runhorse_temp = 1
    End If
    If (vb_write_ic_color_button = 0) Then
        runhorse_temp = -1
    End If

    If (Result) Then

        white

        OutDataEightByte 0, Abs(vb_write_ic_color_button), color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(Abs(vb_write_ic_color_button)).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        vbclean_s7
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = -8 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
Sub runhorse2_s8()
    Dim Result As Boolean
    Result = OpenUsbDevice(VendorID, ProductID)

    If vb_write_ic_color_button = 7 Then
'        icclean_all_led
        vbclean_all_led
    End If

    If (Result) Then

        gray

        OutDataEightByte 0, vb_write_ic_color_button, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        OutDataEightByte 0, vb_write_ic_color_button - 1, color(0), color(1), color(2), 0, 0, 0 '顏色寫入到板子
        led(vb_write_ic_color_button).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        led(vb_write_ic_color_button - 1).FillColor = RGB(color(0), color(1), color(2)) 'vb顯示顏色
        CloseUsbDevice
        vbclean_s4
    End If
        vb_write_ic_color_button = vb_write_ic_color_button - 1
 
    If vb_write_ic_color_button = 0 Then
        icclean_all_led
        vbclean_all_led
        button_index = 0
        vb_write_ic_color_button = 7
        vbbutton_runhorse_timer.Enabled = False
    End If
End Sub
