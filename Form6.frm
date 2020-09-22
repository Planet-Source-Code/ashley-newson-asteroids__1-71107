VERSION 5.00
Object = "{C1A8AF28-1257-101B-8FB0-0020AF039CA3}#1.1#0"; "MCI32.OCX"
Begin VB.Form Form6 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Sounds"
   ClientHeight    =   2220
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   3015
   LinkTopic       =   "Form6"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2220
   ScaleWidth      =   3015
   StartUpPosition =   3  'Windows Default
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
   Begin VB.Label Label2 
      Caption         =   "Hit:"
      Height          =   255
      Left            =   120
      TabIndex        =   3
      Top             =   480
      Width           =   1095
   End
   Begin VB.Label Label1 
      Caption         =   "Explosion:"
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
MMControl1(0).Command = "open"
MMControl1(1).Command = "open"
End Sub

Private Sub Form_Unload(Cancel As Integer)
Form6.Visible = False
Cancel = 1
End Sub

Private Sub MMControl1_Done(Index As Integer, NotifyCode As Integer)

End Sub
