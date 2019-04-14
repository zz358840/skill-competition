Attribute VB_Name = "COMBAPI"

Public Declare Function OpenUsbDevice Lib "USBHIO.dll" (ByVal MyUsbVendorID As Integer, ByVal MyUsbProductID As Integer) As Boolean
Public Declare Sub OutDataCtrl Lib "USBHIO.dll" (ByVal OutData As Byte, ByVal OutControl As Byte)
Public Declare Sub OutDataEightByte Lib "USBHIO.dll" (ByVal OutData1 As Byte, ByVal OutData2 As Byte, ByVal OutData3 As Byte, _
                                                      ByVal OutData4 As Byte, ByVal OutData5 As Byte, ByVal OutData6 As Byte, _
                                                      ByVal OutData7 As Byte, ByVal OutData8 As Byte)
Public Declare Sub ReadData Lib "USBHIO.dll" (ByRef ReadBuffer() As Byte)
Public Declare Sub CloseUsbDevice Lib "USBHIO.dll" ()

Public ReadKeyData(8) As Byte

Public Const VendorID = &H1234
Public Const ProductID = &H2468

Public color(2) As Integer
Public Sub Delay(ByVal Sec As Single) '延遲(秒)(可輸入小數)
    Dim sgnThisTime As Single, sgnCount As Single
    sgnThisTime = Timer
    Do While sgnCount < Sec
        sgnCount = Timer - sgnThisTime
        DoEvents
    Loop
End Sub
Sub red()
color(0) = 255
color(1) = 0
color(2) = 0
End Sub
Sub green()
color(0) = 0
color(1) = 255
color(2) = 0
End Sub
Sub blue()
color(0) = 0
color(1) = 0
color(2) = 255
End Sub
Sub yellow()
color(0) = 255
color(1) = 255
color(2) = 0
End Sub
Sub cyan() '青
color(0) = 0
color(1) = 255
color(2) = 255
End Sub
Sub purple()
color(0) = 255
color(1) = 0
color(2) = 255
End Sub
Sub white()
color(0) = 255
color(1) = 255
color(2) = 255
End Sub
Sub gray()
color(0) = 96
color(1) = 96
color(2) = 96
End Sub
Sub black()
color(0) = 0
color(1) = 0
color(2) = 0
End Sub


