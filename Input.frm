VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form Form2 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Options"
   ClientHeight    =   4335
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   3855
   Icon            =   "Input.frx":0000
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4335
   ScaleWidth      =   3855
   StartUpPosition =   3  'Windows Default
   Begin VB.CheckBox Check7 
      Caption         =   "Fill Smoke"
      Height          =   255
      Left            =   2640
      TabIndex        =   22
      Top             =   3960
      Width           =   1095
   End
   Begin VB.CheckBox Check8 
      Caption         =   "Fill Explosions"
      Height          =   255
      Left            =   1200
      TabIndex        =   23
      Top             =   3960
      Width           =   1335
   End
   Begin VB.CheckBox Check6 
      Caption         =   "Fill Asteroids and bullets"
      Height          =   255
      Left            =   1200
      TabIndex        =   21
      Top             =   3720
      Width           =   2655
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Sounds Window"
      Height          =   495
      Left            =   120
      TabIndex        =   20
      Top             =   3720
      Width           =   975
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   0
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.TextBox Text3 
      Height          =   285
      Left            =   1560
      TabIndex        =   18
      Text            =   "1000"
      Top             =   3360
      Width           =   2175
   End
   Begin MSComctlLib.Slider Slider2 
      Height          =   255
      Left            =   1320
      TabIndex        =   16
      Top             =   3000
      Width           =   2535
      _ExtentX        =   4471
      _ExtentY        =   450
      _Version        =   393216
      LargeChange     =   1
      Min             =   1
      Max             =   8192
      SelStart        =   1
      Value           =   1000
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Save Config"
      Height          =   255
      Left            =   120
      TabIndex        =   15
      Top             =   2640
      Width           =   1455
   End
   Begin VB.CheckBox Check5 
      Caption         =   "Warp powerup"
      Height          =   255
      Left            =   1680
      TabIndex        =   14
      Top             =   2640
      Value           =   1  'Checked
      Width           =   2175
   End
   Begin MSComctlLib.Slider Slider1 
      Height          =   255
      Left            =   1560
      TabIndex        =   13
      Top             =   1560
      Width           =   2295
      _ExtentX        =   4048
      _ExtentY        =   450
      _Version        =   393216
      LargeChange     =   1
      Max             =   100
   End
   Begin VB.CheckBox Check4 
      Caption         =   "White lines at high speed"
      Height          =   255
      Left            =   1680
      TabIndex        =   11
      Top             =   960
      Value           =   1  'Checked
      Width           =   2175
   End
   Begin VB.CheckBox Check3 
      Caption         =   "Explosive Asteroids"
      Height          =   255
      Left            =   1680
      TabIndex        =   10
      Top             =   2280
      Width           =   2175
   End
   Begin VB.CheckBox Check2 
      Caption         =   "Huge Asteroids"
      Height          =   255
      Left            =   1680
      TabIndex        =   9
      Top             =   1920
      Width           =   2175
   End
   Begin VB.OptionButton Option3 
      Caption         =   "No flame or smoke"
      Height          =   255
      Left            =   1680
      TabIndex        =   8
      Top             =   600
      Width           =   2175
   End
   Begin VB.OptionButton Option2 
      Caption         =   "Flame only"
      Height          =   255
      Left            =   1680
      TabIndex        =   7
      Top             =   360
      Width           =   2175
   End
   Begin VB.OptionButton Option1 
      Caption         =   "Both flame and smoke"
      Height          =   255
      Left            =   1680
      TabIndex        =   6
      Top             =   120
      Value           =   -1  'True
      Width           =   2175
   End
   Begin VB.TextBox Text2 
      Height          =   285
      Left            =   120
      TabIndex        =   5
      Text            =   "1"
      Top             =   2280
      Width           =   1455
   End
   Begin VB.CheckBox Check1 
      Caption         =   "Show Stars"
      Height          =   255
      Left            =   120
      TabIndex        =   3
      Top             =   1560
      Value           =   1  'Checked
      Width           =   1455
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Ok"
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   1080
      Width           =   1455
   End
   Begin VB.TextBox Text1 
      Height          =   285
      Left            =   120
      TabIndex        =   1
      Text            =   "1"
      Top             =   720
      Width           =   1455
   End
   Begin VB.Label Label5 
      Caption         =   "Distance from stars:"
      Height          =   255
      Left            =   120
      TabIndex        =   19
      Top             =   3360
      Width           =   1455
   End
   Begin VB.Label Label4 
      Caption         =   "Stars to display:"
      Height          =   255
      Left            =   120
      TabIndex        =   17
      Top             =   3000
      Width           =   1215
   End
   Begin VB.Label Label3 
      Caption         =   "Star Blur Level:"
      Height          =   255
      Left            =   1680
      TabIndex        =   12
      Top             =   1320
      Width           =   2055
   End
   Begin VB.Label Label2 
      Caption         =   "Magnification:"
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   1920
      Width           =   1455
   End
   Begin VB.Label Label1 
      Caption         =   "Speed (Milliseconds between refresh):"
      Height          =   495
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   1455
   End
End
Attribute VB_Name = "Form2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
On Error GoTo errorsolveForm2OK
If 0 = 1 Then
errorsolveForm2OK:
MsgBox ("Warning: An unexpected error has occoured and your gameplay may be affected! Asteroids will attempt to continue to play. Error:" + Str$(Err.Number) + ": " + Err.Description)
End If
Form1.Timer1.Interval = Val(Text1.Text)
End Sub

Private Sub Command2_Click()
On Error GoTo errorsolveForm2Save
If 0 = 1 Then
errorsolveForm2Save:
MsgBox ("Warning: An unexpected error has occoured and your gameplay may be affected! Asteroids will attempt to continue to play. Error:" + Str$(Err.Number) + ": " + Err.Description)
End If
Form1.Timer1.Interval = Val(Text1.Text)
Open "asteroids.cfg" For Random As #1
temps$ = Form2.Text1.Text
Put #1, 1, temps$
temps$ = Form2.Text2.Text
Put #1, 2, temps$
temps$ = Form2.Text3.Text
Put #1, 3, temps$
temp = Form2.Check1.Value
Put #1, 4, temp
temp = Form2.Check2.Value
Put #1, 5, temp
temp = Form2.Check3.Value
Put #1, 6, temp
temp = Form2.Check4.Value
Put #1, 7, temp
temp = Form2.Check5.Value
Put #1, 8, temp
If Form2.Option1.Value = True Then
temp = 0
End If
If Form2.Option2.Value = True Then
temp = 1
End If
If Form2.Option3.Value = True Then
temp = 2
End If
Put #1, 9, temp
temp = Form2.Slider1.Value
Put #1, 10, temp
temp = Form2.Slider2.Value
Put #1, 11, temp
temp = Form6.Check1.Value
Put #1, 12, temp
temp = Form6.Check2.Value
Put #1, 13, temp
temp = Form6.Check3.Value
Put #1, 14, temp
temp = Form2.Check6.Value
Put #1, 15, temp
temp = Form2.Check7.Value
Put #1, 16, temp
temp = Form2.Check8.Value
Put #1, 17, temp
Close #1
End Sub

Private Sub Command3_Click()
Form6.Visible = True
End Sub

Private Sub Form_Unload(Cancel As Integer)
Cancel = 1
Form2.Visible = False
End Sub
