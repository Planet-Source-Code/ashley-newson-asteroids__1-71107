VERSION 5.00
Object = "{C1A8AF28-1257-101B-8FB0-0020AF039CA3}#1.1#0"; "MCI32.OCX"
Begin VB.Form Form6 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Sounds"
   ClientHeight    =   1455
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   3015
   LinkTopic       =   "Form6"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1455
   ScaleWidth      =   3015
   StartUpPosition =   3  'Windows Default
   Begin VB.CheckBox Check3 
      Caption         =   "Active"
      Height          =   255
      Left            =   2160
      TabIndex        =   8
      Top             =   840
      Width           =   855
   End
   Begin VB.CheckBox Check2 
      Caption         =   "Active"
      Height          =   255
      Left            =   2160
      TabIndex        =   5
      Top             =   480
      Width           =   855
   End
   Begin VB.CheckBox Check1 
      Caption         =   "Active"
      Height          =   255
      Left            =   2160
      TabIndex        =   4
      Top             =   120
      Width           =   855
   End
   Begin MCI.MMControl MMControl1 
      Height          =   330
      Index           =   0
      Left            =   1320
      TabIndex        =   0
      Top             =   120
      Width           =   810
      _ExtentX        =   1429
      _ExtentY        =   582
      _Version        =   393216
      PlayEnabled     =   -1  'True
      StopEnabled     =   -1  'True
      PrevVisible     =   0   'False
      NextVisible     =   0   'False
      PauseVisible    =   0   'False
      BackVisible     =   0   'False
      StepVisible     =   0   'False
      RecordVisible   =   0   'False
      EjectVisible    =   0   'False
      DeviceType      =   ""
      FileName        =   "explosion.wav"
   End
   Begin MCI.MMControl MMControl1 
      Height          =   330
      Index           =   1
      Left            =   1320
      TabIndex        =   1
      Top             =   480
      Width           =   810
      _ExtentX        =   1429
      _ExtentY        =   582
      _Version        =   393216
      PlayEnabled     =   -1  'True
      StopEnabled     =   -1  'True
      PrevVisible     =   0   'False
      NextVisible     =   0   'False
      PauseVisible    =   0   'False
      BackVisible     =   0   'False
      StepVisible     =   0   'False
      RecordVisible   =   0   'False
      EjectVisible    =   0   'False
      DeviceType      =   ""
      FileName        =   "hit.wav"
   End
   Begin MCI.MMControl MMControl1 
      Height          =   330
      Index           =   2
      Left            =   1320
      TabIndex        =   7
      Top             =   840
      Width           =   810
      _ExtentX        =   1429
      _ExtentY        =   582
      _Version        =   393216
      PlayEnabled     =   -1  'True
      StopEnabled     =   -1  'True
      PrevVisible     =   0   'False
      NextVisible     =   0   'False
      PauseVisible    =   0   'False
      BackVisible     =   0   'False
      StepVisible     =   0   'False
      RecordVisible   =   0   'False
      EjectVisible    =   0   'False
      DeviceType      =   ""
      FileName        =   "explosion2.wav"
   End
   Begin VB.Label Label4 
      Caption         =   "Sounds may slow down game by a lot."
      Height          =   255
      Left            =   120
      TabIndex        =   9
      Top             =   1200
      Width           =   2775
   End
   Begin VB.Label Label3 
      Caption         =   "Explosion:"
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   840
      Width           =   1095
   End
   Begin VB.Label Label2 
      Caption         =   "Hit:"
      Height          =   255
      Left            =   120
      TabIndex        =   3
      Top             =   480
      Width           =   1095
   End
   Begin VB.Label Label1 
      Caption         =   "Break:"
      Height          =   255
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   1095
   End
End
Attribute VB_Name = "Form6"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_Load()
On Error GoTo errorsound
If 0 = 1 Then
errorsound:
MsgBox ("WARNING: Sounds may not work properly, Error:" + Str$(Err.Number))
End If
MMControl1(0).Command = "open"
MMControl1(1).Command = "open"
MMControl1(2).Command = "open"
End Sub

Private Sub Form_Unload(Cancel As Integer)
Form6.Visible = False
Cancel = 1
End Sub
