VERSION 5.00
Begin VB.Form Form4 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   1455
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   3015
   Icon            =   "GameOver.frx":0000
   LinkTopic       =   "Form4"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1455
   ScaleWidth      =   3015
   StartUpPosition =   3  'Windows Default
   Begin VB.Timer Timer1 
      Interval        =   500
      Left            =   0
      Top             =   0
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Quit Game"
      Height          =   375
      Left            =   1560
      TabIndex        =   3
      Top             =   960
      Width           =   1335
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Restart Game"
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   960
      Width           =   1335
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      Height          =   255
      Left            =   1440
      TabIndex        =   4
      Top             =   600
      Width           =   1455
   End
   Begin VB.Label Label2 
      Caption         =   "Your Final Score:"
      Height          =   255
      Left            =   120
      TabIndex        =   1
      Top             =   600
      Width           =   1335
   End
   Begin VB.Label Label1 
      BeginProperty Font 
         Name            =   "Lucida Console"
         Size            =   24
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   2775
   End
End
Attribute VB_Name = "Form4"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
Randomize Timer
score = 0
ship(1) = 0
ship(2) = 0
ship(3) = 255
For i& = 1 To 64
bullet(i&, 5) = 0
Next
For i& = 1 To 255
smoke(i&, 5) = 0
Next
For i& = 1 To 25
rocket(i&, 5) = 0
Next
For i& = 1 To 5
bomb(i&, 3) = 0
Next
warp = 0
nostars = Form2.Slider2.Value
distars = Form2.Text3.Text
For i& = 1 To nostars
stars(i&, 1) = Int(Rnd * 4001) - 2000
stars(i&, 2) = Int(Rnd * 4001) - 2000
stars(i&, 3) = (Int(Rnd * 1000) + 1) / distars
stars(i&, 4) = Int(Rnd * 192) + 64
Next
For i& = 1 To 6
item(i&, 1) = Int(Rnd * 4001) - 2000
item(i&, 2) = Int(Rnd * 4001) - 2000
item(i&, 3) = Int(Rnd * 4) + 1
Next
For i& = 1 To 16
asteroidh(i&, 6) = 41
Next
For i& = 17 To 32
asteroidb(i&, 6) = 21
Next
For i& = 33 To 64
asteroidm(i&, 6) = 11
Next
For i& = 65 To 128
asteroids(i&, 6) = 0
Next
For i& = 1 To 5
Do
asteroide(i&, 1) = Int(Rnd * 4001) - 2000
asteroide(i&, 2) = Int(Rnd * 4001) - 2000
Loop Until (asteroide(i&, 1) > 1000 Or asteroide(i&, 1) < -1000) And (asteroide(i&, 2) > 1000 Or asteroide(i&, 2) < -1000)
asteroide(i&, 3) = (Int(Rnd * 1001) - 500) / 45
asteroide(i&, 4) = (Int(Rnd * 1001) - 500) / 45
asteroide(i&, 5) = -1
Next
For i& = 1 To 16
Do
asteroidb(i&, 1) = Int(Rnd * 4001) - 2000
asteroidb(i&, 2) = Int(Rnd * 4001) - 2000
Loop Until (asteroidb(i&, 1) > 1000 Or asteroidb(i&, 1) < -1000) And (asteroidb(i&, 2) > 1000 Or asteroidb(i&, 2) < -1000)
asteroidb(i&, 3) = (Int(Rnd * 1001) - 500) / 75
asteroidb(i&, 4) = (Int(Rnd * 1001) - 500) / 75
asteroidb(i&, 5) = Int(Rnd * 192) + 64
asteroidb(i&, 6) = 20
Next
For i& = 1 To 32
Do
asteroidm(i&, 1) = Int(Rnd * 4001) - 2000
asteroidm(i&, 2) = Int(Rnd * 4001) - 2000
Loop Until (asteroidm(i&, 1) > 1000 Or asteroidm(i&, 1) < -1000) And (asteroidm(i&, 2) > 1000 Or asteroidm(i&, 2) < -1000)
asteroidm(i&, 3) = (Int(Rnd * 1001) - 500) / 40
asteroidm(i&, 4) = (Int(Rnd * 1001) - 500) / 40
asteroidm(i&, 5) = Int(Rnd * 192) + 64
asteroidm(i&, 6) = 10
Next
For i& = 1 To 64
Do
asteroids(i&, 1) = Int(Rnd * 4001) - 2000
asteroids(i&, 2) = Int(Rnd * 4001) - 2000
Loop Until (asteroids(i&, 1) > 1000 Or asteroids(i&, 1) < -1000) And (asteroids(i&, 2) > 1000 Or asteroids(i&, 2) < -1000)
asteroids(i&, 3) = (Int(Rnd * 1001) - 500) / 35
asteroids(i&, 4) = (Int(Rnd * 1001) - 500) / 35
asteroids(i&, 5) = Int(Rnd * 192) + 64
asteroids(i&, 6) = 5
Next
Form4.Caption = ""
Label1.Caption = ""
Form1.Image1.Visible = True
Form3.Image1.Visible = True
Form1.Enabled = True
Form3.Enabled = True
Unload Form4
End Sub

Private Sub Command2_Click()
End
End Sub

Private Sub Form_Load()
Label3.Caption = Str$(score)
Form1.Enabled = False
Form3.Enabled = False
Form1.Timer1.Enabled = False
Form3.Timer1.Enabled = False
End Sub

Private Sub Timer1_Timer()
Select Case Len(Form4.Caption)
Case 0
Form4.Caption = "G"
Label1.Caption = "G"
Case 1
Form4.Caption = "Ga"
Label1.Caption = "Ga"
Case 2
Form4.Caption = "Gam"
Label1.Caption = "Gam"
Case 3
Form4.Caption = "Game"
Label1.Caption = "Game"
Case 4
Form4.Caption = "Game O"
Label1.Caption = "Game O"
Case 6
Form4.Caption = "Game Ov"
Label1.Caption = "Game Ov"
Case 7
Form4.Caption = "Game Ove"
Label1.Caption = "Game Ove"
Case 8
Form4.Caption = "Game Over"
Label1.Caption = "Game Over"
Case 9
Form4.Caption = "Game Over."
Case 10
Form4.Caption = "Game Over.."
Case 11
Form4.Caption = "Game Over..."
Case 12
Timer1.Enabled = False
End Select

End Sub
