VERSION 5.00
Begin VB.Form Form3 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Full Screen"
   ClientHeight    =   5175
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   6975
   Icon            =   "FullScreen.frx":0000
   LinkTopic       =   "Form3"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5175
   ScaleWidth      =   6975
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox Screen 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      BorderStyle     =   0  'None
      Height          =   4935
      Left            =   0
      MousePointer    =   2  'Cross
      ScaleHeight     =   4935
      ScaleWidth      =   6975
      TabIndex        =   1
      Top             =   240
      Width           =   6975
      Begin VB.Timer Timer1 
         Enabled         =   0   'False
         Interval        =   1
         Left            =   0
         Top             =   0
      End
      Begin VB.Image Image1 
         Height          =   780
         Left            =   0
         Picture         =   "FullScreen.frx":08CA
         Stretch         =   -1  'True
         Top             =   1920
         Width           =   6960
      End
   End
   Begin VB.Label Label1 
      BackColor       =   &H00000000&
      Caption         =   "Asteroids"
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   6975
   End
End
Attribute VB_Name = "Form3"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_resize()
    Label1.Width = Form3.Width
    Screen.Width = Form3.Width
    Screen.Height = Form3.Height - Label1.Height
    Image1.Left = (Screen.Width - Image1.Width) / 2
    Image1.Top = (Screen.Height - Image1.Height) / 2
End Sub

Private Sub Screen_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    mouse(1) = X - Screen.Width / 2
    mouse(2) = Y - Screen.Height / 2
    If Button = 1 Then mouse(3) = 1
    If Button = 2 Then mouse(4) = 1
    If Button = 3 Then
        mouse(3) = 1
        mouse(4) = 1
    End If
End Sub

Private Sub Screen_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    mouse(1) = X - Screen.Width / 2
    mouse(2) = Y - Screen.Height / 2
    mouse(3) = 0
    mouse(4) = 0
    If Button = 1 Then mouse(3) = 1
    If Button = 2 Then mouse(4) = 1
    If Button = 3 Then
        mouse(3) = 1
        mouse(4) = 1
    End If
End Sub

Private Sub Screen_KeyDown(Keycode As Integer, Shift As Integer)
If Keycode = 27 Or Keycode = 122 Then
    Timer1.Enabled = False
    Unload Form3
End If
    If Keycode = 32 And ship(3) > 0 Then
        For i& = 1 To 5
            If bomb(i&, 3) = 0 Then
                If Form6.Check3.Value = 1 Then
                    Form6.MMControl1(2).Command = "stop"
                    Form6.MMControl1(2).Command = "prev"
                    Form6.MMControl1(2).Command = "play"
                End If
                bomb(i&, 3) = 1
                bomb(i&, 1) = 0
                bomb(i&, 2) = 0
                Exit For
            End If
        Next
    End If
    If Keycode = 19 Or Keycode = 80 Then
        If Timer1.Enabled = True Then
            Timer1.Enabled = False
            Form1.Caption = "Asteroids - Paused"
            Image1.Visible = True
        Else
            Timer1.Enabled = True
            Form1.Caption = "Asteroids"
            Image1.Visible = False
        End If
    End If
    If (Keycode = 17 Or Keycode = 18) And ship(3) > 0 Then
        si = 0
        id = 0
        dist = 10000
        For i& = 1 To 25
            If rocket(i&, 5) = 0 Then
                For v& = 1 To 16
                    If Sqr((asteroidh(v&, 1) ^ 2) + (asteroidh(v&, 2) ^ 2)) < dist And asteroidh(v&, 6) > 0 And asteroidh(v&, 6) < 21 Then
                        si = 1
                        id = v&
                        dist = Sqr((asteroidh(v&, 1) ^ 2) + (asteroidh(v&, 2) ^ 2))
                    End If
                Next
                For v& = 1 To 32
                    If Sqr((asteroidb(v&, 1) ^ 2) + (asteroidb(v&, 2) ^ 2)) < dist And asteroidb(v&, 6) > 0 And asteroidb(v&, 6) < 21 Then
                        si = 2
                        id = v&
                        dist = Sqr((asteroidb(v&, 1) ^ 2) + (asteroidb(v&, 2) ^ 2))
                    End If
                Next
                For v& = 1 To 64
                    If Sqr((asteroidm(v&, 1) ^ 2) + (asteroidm(v&, 2) ^ 2)) < dist And asteroidm(v&, 6) > 0 And asteroidm(v&, 6) < 11 Then
                        si = 3
                        id = v&
                        dist = Sqr((asteroidm(v&, 1) ^ 2) + (asteroidm(v&, 2) ^ 2))
                    End If
                Next
                For v& = 1 To 128
                    If Sqr((asteroids(v&, 1) ^ 2) + (asteroids(v&, 2) ^ 2)) < dist And asteroids(v&, 6) > 0 And asteroids(v&, 6) < 6 Then
                        si = 4
                        id = v&
                        dist = Sqr((asteroids(v&, 1) ^ 2) + (asteroids(v&, 2) ^ 2))
                    End If
                Next
                For v& = 1 To 5
                    If Sqr((asteroide(v&, 1) ^ 2) + (asteroide(v&, 2) ^ 2)) < dist And asteroide(v&, 5) = 0 Then
                        si = 5
                        id = v&
                        dist = Sqr((asteroide(v&, 1) ^ 2) + (asteroide(v&, 2) ^ 2))
                    End If
                Next
                rocket(i&, 1) = 0
                rocket(i&, 2) = 0
                rocket(i&, 3) = 0
                rocket(i&, 4) = 0
                rocket(i&, 5) = si
                rocket(i&, 6) = id
                Exit For
            End If
        Next
    End If
    If Keycode = 112 Then
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
    End If
End Sub

Private Sub Screen_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    mouse(1) = X - Screen.Width / 2
    mouse(2) = Y - Screen.Height / 2
    mouse(3) = 1
    mouse(4) = 1
    If Button = 1 Then mouse(3) = 0
    If Button = 2 Then mouse(4) = 0
    If Button = 3 Then
        mouse(3) = 1
        mouse(4) = 1
    End If
End Sub

Private Sub Timer1_Timer()
    On Error GoTo errorsolvefull
    If 0 = 1 Then
errorsolvefull:
        MsgBox ("Warning: An unexpected error has occoured and your gameplay may be affected! Asteroids will attempt to continue to play. Error:" + Str$(Err.Number) + ": " + Err.Description)
    End If
    mag = Val(Form2.Text2.Text)
    nostars = Form2.Slider2.Value
    If Form6.Check1.Value = 1 Then soundexp = 1 Else soundexp = 0
    If Form6.Check2.Value = 1 Then soundhit = 1 Else soundhit = 0
    If Form6.Check3.Value = 1 Then soundban = 1 Else soundban = 0
    bombcount = 0
    rocketcount = 0
    For i& = 1 To 5
        If bomb(i&, 3) = 0 Then bombcount = bombcount + 1
    Next
    For i& = 1 To 25
        If rocket(i&, 5) = 0 Then rocketcount = rocketcount + 1
    Next
Label1.Caption = "Asteroids - Score:" + Str$(score) + " Bombs:" + Str$(bombcount) + " Rockets:" + Str$(rocketcount)
    If mouse(1) <> 0 Or mouse(2) <> 0 Then mdx = mouse(1) / Sqr((mouse(1) ^ 2) + (mouse(2) ^ 2)) Else mdx = 0
    If mouse(1) <> 0 Or mouse(2) <> 0 Then mdy = mouse(2) / Sqr((mouse(1) ^ 2) + (mouse(2) ^ 2)) Else mdy = 0
    If ship(3) <= 0 And ship(3) >= -255 Then
        Screen.BackColor = -ship(3)
    Else
        Screen.BackColor = 0
    End If
    If ship(3) <= 254.9 And ship(3) > 0 Then
        ship(3) = ship(3) + 0.1
        score = score - 1
    End If
    If Form2.Check5.Value = 0 Then
        For i& = 1 To 6
            If item(i&, 3) = 4 Then
                item(i&, 1) = Int(Rnd * 4001) - 2000
                item(i&, 2) = Int(Rnd * 4001) - 2000
                item(i&, 3) = Int(Rnd * 3) + 1
            End If
        Next
    End If
    If Form2.Check3.Value = 1 Then
        For i& = 1 To 5
            If asteroide(i&, 5) = -1 Then
                asteroide(i&, 5) = 0
                Do
                    asteroide(i&, 1) = Int(Rnd * 4001) - 2000
                    asteroide(i&, 2) = Int(Rnd * 4001) - 2000
                Loop Until (asteroide(i&, 1) > 1000 Or asteroide(i&, 1) < -1000) And (asteroide(i&, 2) > 1000 Or asteroide(i&, 2) < -1000)
                asteroide(i&, 3) = (Int(Rnd * 1001) - 500) / 50
                asteroide(i&, 4) = (Int(Rnd * 1001) - 500) / 50
            End If
        Next
    End If
    h = 0
    b = 0
    m = 0
    s = 0
    For i& = 1 To 16
        If asteroidh(i&, 6) > 40 Then h = h + 1
    Next
    For i& = 1 To 32
        If asteroidb(i&, 6) > 20 Then b = b + 1
    Next
    For i& = 1 To 64
        If asteroidm(i&, 6) > 10 Then m = m + 1
    Next
    For i& = 1 To 128
        If asteroids(i&, 6) <= 0 Then s = s + 1
    Next
    If Form2.Check2.Value = 0 Then
        If b >= 1 And m >= 2 And s >= 4 Then
            For i& = 1 To 32
                If asteroidb(i&, 6) > 20 Then
                    Do
                        asteroidb(i&, 1) = Int(Rnd * 4001) - 2000
                        asteroidb(i&, 2) = Int(Rnd * 4001) - 2000
                    Loop Until (asteroidb(i&, 1) > 1000 Or asteroidb(i&, 1) < -1000) And (asteroidb(i&, 2) > 1000 Or asteroidb(i&, 2) < -1000)
                    asteroidb(i&, 3) = (Int(Rnd * 1001) - 500) / 75
                    asteroidb(i&, 4) = (Int(Rnd * 1001) - 500) / 75
                    asteroidb(i&, 5) = Int(Rnd * 192) + 64
                    asteroidb(i&, 6) = 20
                    b = b - 1
                    m = m - 2
                    s = s - 4
                End If
                If b = 0 Or m <= 1 Or s <= 3 Then Exit For
            Next
        End If
    Else
        If h >= 1 And b >= 2 And m >= 4 And s >= 8 Then
            For i& = 1 To 16
                If asteroidh(i&, 6) > 40 Then
                    Do
                        asteroidh(i&, 1) = Int(Rnd * 4001) - 2000
                        asteroidh(i&, 2) = Int(Rnd * 4001) - 2000
                    Loop Until (asteroidh(i&, 1) > 1000 Or asteroidh(i&, 1) < -1000) And (asteroidh(i&, 2) > 1000 Or asteroidh(i&, 2) < -1000)
                    asteroidh(i&, 3) = (Int(Rnd * 1001) - 500) / 150
                    asteroidh(i&, 4) = (Int(Rnd * 1001) - 500) / 150
                    asteroidh(i&, 5) = Int(Rnd * 192) + 64
                    asteroidh(i&, 6) = 40
                    h = h - 1
                    b = b - 2
                    m = m - 4
                    s = s - 8
                End If
                If h = 0 Or b = 1 Or m <= 3 Or s <= 7 Then Exit For
            Next
        End If
    End If
    For i& = 1 To 255
        v& = 256 - i&
        smoke(v&, 1) = smoke(v& - 1, 1)
        smoke(v&, 2) = smoke(v& - 1, 2)
        smoke(v&, 3) = smoke(v& - 1, 3)
        smoke(v&, 4) = smoke(v& - 1, 4)
        smoke(v&, 5) = smoke(v& - 1, 5)
        If Form2.Option3.Value = True Then smoke(v&, 5) = 0
        If Form2.Option2.Value = True And v& >= 48 Then smoke(v&, 5) = 0
    Next
    For i& = 1 To 63
        v& = 64 - i&
        bullet(v&, 1) = bullet(v& - 1, 1)
        bullet(v&, 2) = bullet(v& - 1, 2)
        bullet(v&, 3) = bullet(v& - 1, 3)
        bullet(v&, 4) = bullet(v& - 1, 4)
        bullet(v&, 5) = bullet(v& - 1, 5)
    Next
    If mouse(3) = 1 And ship(3) > 0 Then
        bullet(1, 5) = 1
        bullet(1, 3) = (mdx * 20) + (ship(1))
        bullet(1, 4) = (mdy * 20) + (ship(2))
    Else
        smoke(1, 5) = 0
    End If
    If mouse(4) = 1 And ship(3) > 0 Then
        If warp > 0 Then
            ship(1) = (ship(1) + (mdx * 5)) * 0.95
            ship(2) = (ship(2) + (mdy * 5)) * 0.95
            smoke(1, 3) = -mdx * 5
            smoke(1, 4) = -mdy * 5
            If Form2.Option1.Value = True Or Form2.Option2.Value = True Then smoke(1, 5) = 2
        Else
            ship(1) = (ship(1) + (mdx)) * 0.95
            ship(2) = (ship(2) + (mdy)) * 0.95
            smoke(1, 3) = -mdx
            smoke(1, 4) = -mdy
            If Form2.Option1.Value = True Or Form2.Option2.Value = True Then smoke(1, 5) = 1
        End If
    Else
        smoke(1, 5) = 0
        ship(1) = ship(1) * 0.95
        ship(2) = ship(2) * 0.95
    End If
    spd = Sqr((ship(1) ^ 2) + (ship(2) ^ 2))
    Screen.Cls
    If Form2.Check1.Value = 1 Then
        blurlevel = Form2.Slider1.Value / 100
        For i& = 1 To nostars
            stars(i&, 1) = stars(i&, 1) - ship(1)
            stars(i&, 2) = stars(i&, 2) - ship(2)
            If stars(i&, 1) < -2000 Then stars(i&, 1) = stars(i&, 1) + 4000
            If stars(i&, 1) > 2000 Then stars(i&, 1) = stars(i&, 1) - 4000
            If stars(i&, 2) < -2000 Then stars(i&, 2) = stars(i&, 2) + 4000
            If stars(i&, 2) > 2000 Then stars(i&, 2) = stars(i&, 2) - 4000
            If blurlevel > 0 Then
                Screen.Line ((stars(i&, 1) * 15 * mag * stars(i&, 3)) + (Screen.Width / 2), (stars(i&, 2) * 15 * mag * stars(i&, 3)) + (Screen.Height / 2))-(((stars(i&, 1) - (ship(1) * blurlevel)) * 15 * mag * stars(i&, 3)) + (Screen.Width / 2), ((stars(i&, 2) - (ship(2) * blurlevel)) * 15 * mag * stars(i&, 3)) + (Screen.Height / 2)), RGB(stars(i&, 4), stars(i&, 4), stars(i&, 4))
                Screen.PSet ((stars(i&, 1) * 15 * mag * stars(i&, 3)) + (Screen.Width / 2), (stars(i&, 2) * 15 * mag * stars(i&, 3)) + (Screen.Height / 2)), RGB(stars(i&, 4), stars(i&, 4), stars(i&, 4))
            Else
                Screen.PSet ((stars(i&, 1) * 15 * mag * stars(i&, 3)) + (Screen.Width / 2), (stars(i&, 2) * 15 * mag * stars(i&, 3)) + (Screen.Height / 2)), RGB(stars(i&, 4), stars(i&, 4), stars(i&, 4))
            End If
        Next
    End If
    If Form2.Check6.Value = 1 Then
        Screen.FillStyle = 0
    Else
        Screen.FillStyle = 1
    End If
    For i& = 1 To 5
        If asteroide(i&, 5) = 0 Then
            For v& = 1 To 64
                If bullet(v&, 5) = 1 Then
                    If Sqr(((asteroide(i&, 1) - bullet(v&, 1)) ^ 2) + ((asteroide(i&, 2) - bullet(v&, 2)) ^ 2)) < 12 Then
                        asteroide(i&, 5) = 1
                        bullet(v&, 5) = 0
                        score = score + 500
                        If soundban = 1 Then
                            Form6.MMControl1(2).Command = "stop"
                            Form6.MMControl1(2).Command = "prev"
                            Form6.MMControl1(2).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 25
                If rocket(v&, 5) > 0 Then
                    If Sqr(((asteroide(i&, 1) - rocket(v&, 1)) ^ 2) + ((asteroide(i&, 2) - rocket(v&, 2)) ^ 2)) < 20 Then
                        asteroide(i&, 5) = 1
                        rocket(v&, 5) = -1
                        score = score + 500
                        If soundban = 1 Then
                            Form6.MMControl1(2).Command = "stop"
                            Form6.MMControl1(2).Command = "prev"
                            Form6.MMControl1(2).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 5
                If bomb(v&, 3) >= 1 Then
                    If Sqr(((asteroide(i&, 1) - bomb(v&, 1)) ^ 2) + ((asteroide(i&, 2) - bomb(v&, 2)) ^ 2)) < bomb(v&, 3) - 10 Then
                        asteroide(i&, 5) = 1
                        score = score + 500
                        If soundban = 1 Then
                            Form6.MMControl1(2).Command = "stop"
                            Form6.MMControl1(2).Command = "prev"
                            Form6.MMControl1(2).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 5
                If asteroide(v&, 5) >= 1 Then
                    If Sqr(((asteroide(i&, 1) - asteroide(v&, 1)) ^ 2) + ((asteroide(i&, 2) - asteroide(v&, 2)) ^ 2)) < asteroide(v&, 5) - 10 Then
                        asteroide(i&, 5) = 1
                        score = score + 500
                        If soundban = 1 Then
                            Form6.MMControl1(2).Command = "stop"
                            Form6.MMControl1(2).Command = "prev"
                            Form6.MMControl1(2).Command = "play"
                        End If
                    End If
                End If
            Next
            asteroide(i&, 1) = (asteroide(i&, 1) - ship(1)) + asteroide(i&, 3)
            asteroide(i&, 2) = (asteroide(i&, 2) - ship(2)) + asteroide(i&, 4)
            If Sqr((asteroide(i&, 1) ^ 2) + (asteroide(i&, 2) ^ 2)) < 20 Then ship(3) = ship(3) - Sqr(((asteroide(i&, 3) - ship(1)) ^ 2) + ((asteroide(i&, 4) - ship(2)) ^ 2))
            If asteroide(i&, 1) < -2000 Then asteroide(i&, 1) = asteroide(i&, 1) + 4000
            If asteroide(i&, 1) > 2000 Then asteroide(i&, 1) = asteroide(i&, 1) - 4000
            If asteroide(i&, 2) < -2000 Then asteroide(i&, 2) = asteroide(i&, 2) + 4000
            If asteroide(i&, 2) > 2000 Then asteroide(i&, 2) = asteroide(i&, 2) - 4000
            If eac < 16 Then
                cr = 255 - ((eac) * 16)
                cg = (eac) * 16
                cb = 0
            End If
            If eac >= 16 And eac < 32 Then
                cr = 0
                cg = 255 - ((eac - 16) * 16)
                cb = (eac - 16) * 16
            End If
            If eac >= 32 And eac < 48 Then
                cr = (eac - 32) * 16
                cg = 0
                cb = 255 - ((eac - 32) * 16)
            End If
            Screen.FillColor = RGB(cr, cg, cb)
            Screen.Circle ((asteroide(i&, 1) * mag * 15) + (Screen.Width / 2), (asteroide(i&, 2) * mag * 15) + (Screen.Height / 2)), 150 * mag, RGB(cr, cg, cb)
        End If
    Next
    For i& = 1 To 128
        If asteroids(i&, 6) > 0 Then
            For v& = 1 To 64
                If bullet(v&, 5) = 1 Then
                    If Sqr(((asteroids(i&, 1) - bullet(v&, 1)) ^ 2) + ((asteroids(i&, 2) - bullet(v&, 2)) ^ 2)) < 12 Then
                        asteroids(i&, 6) = asteroids(i&, 6) - 1
                        bullet(v&, 5) = 0
                        score = score + 10
                        If soundhit = 1 Then
                            Form6.MMControl1(1).Command = "stop"
                            Form6.MMControl1(1).Command = "prev"
                            Form6.MMControl1(1).Command = "play"
                        End If
                        If soundexp = 1 And asteroids(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 25
                If rocket(v&, 5) > 0 Then
                    If Sqr(((asteroids(i&, 1) - rocket(v&, 1)) ^ 2) + ((asteroids(i&, 2) - rocket(v&, 2)) ^ 2)) < 20 Then
                        asteroids(i&, 6) = asteroids(i&, 6) - 40
                        rocket(v&, 5) = -1
                        score = score + 50
                        If soundhit = 1 Then
                            Form6.MMControl1(1).Command = "stop"
                            Form6.MMControl1(1).Command = "prev"
                            Form6.MMControl1(1).Command = "play"
                        End If
                        If soundexp = 1 And asteroids(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 5
                If bomb(v&, 3) >= 1 Then
                    If Sqr(((asteroids(i&, 1) - bomb(v&, 1)) ^ 2) + ((asteroids(i&, 2) - bomb(v&, 2)) ^ 2)) < bomb(v&, 3) - 10 Then
                        asteroids(i&, 6) = asteroids(i&, 6) - 10
                        score = score + 50
                        If soundexp = 1 And asteroids(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 5
                If asteroide(v&, 5) >= 1 Then
                    If Sqr(((asteroids(i&, 1) - asteroide(v&, 1)) ^ 2) + ((asteroids(i&, 2) - asteroide(v&, 2)) ^ 2)) < asteroide(v&, 5) - 10 Then
                        asteroids(i&, 6) = asteroids(i&, 6) - 100
                        score = score + 100
                        If soundexp = 1 And asteroids(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            asteroids(i&, 1) = (asteroids(i&, 1) - ship(1)) + asteroids(i&, 3)
            asteroids(i&, 2) = (asteroids(i&, 2) - ship(2)) + asteroids(i&, 4)
            If Sqr((asteroids(i&, 1) ^ 2) + (asteroids(i&, 2) ^ 2)) < 20 Then ship(3) = ship(3) - Sqr(((asteroids(i&, 3) - ship(1)) ^ 2) + ((asteroids(i&, 4) - ship(2)) ^ 2))
            If asteroids(i&, 1) < -2000 Then asteroids(i&, 1) = asteroids(i&, 1) + 4000
            If asteroids(i&, 1) > 2000 Then asteroids(i&, 1) = asteroids(i&, 1) - 4000
            If asteroids(i&, 2) < -2000 Then asteroids(i&, 2) = asteroids(i&, 2) + 4000
            If asteroids(i&, 2) > 2000 Then asteroids(i&, 2) = asteroids(i&, 2) - 4000
            Screen.FillColor = RGB(asteroids(i&, 5), asteroids(i&, 5), asteroids(i&, 5))
            Screen.Circle ((asteroids(i&, 1) * mag * 15) + (Screen.Width / 2), (asteroids(i&, 2) * mag * 15) + (Screen.Height / 2)), 150 * mag, RGB(asteroids(i&, 5), asteroids(i&, 5), asteroids(i&, 5))
        End If
    Next
    For i& = 1 To 64
        If asteroidm(i&, 6) > 0 And asteroidm(i&, 6) < 11 Then
            For v& = 1 To 64
                If bullet(v&, 5) = 1 Then
                    If Sqr(((asteroidm(i&, 1) - bullet(v&, 1)) ^ 2) + ((asteroidm(i&, 2) - bullet(v&, 2)) ^ 2)) < 22 Then
                        asteroidm(i&, 6) = asteroidm(i&, 6) - 1
                        bullet(v&, 5) = 0
                        score = score + 10
                        If soundhit = 1 Then
                            Form6.MMControl1(1).Command = "stop"
                            Form6.MMControl1(1).Command = "prev"
                            Form6.MMControl1(1).Command = "play"
                        End If
                        If soundexp = 1 And asteroidm(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 25
                If rocket(v&, 5) > 0 Then
                    If Sqr(((asteroidm(i&, 1) - rocket(v&, 1)) ^ 2) + ((asteroidm(i&, 2) - rocket(v&, 2)) ^ 2)) < 30 Then
                        asteroidm(i&, 6) = asteroidm(i&, 6) - 40
                        rocket(v&, 5) = -1
                        score = score + 100
                        If soundhit = 1 Then
                            Form6.MMControl1(1).Command = "stop"
                            Form6.MMControl1(1).Command = "prev"
                            Form6.MMControl1(1).Command = "play"
                        End If
                        If soundexp = 1 And asteroidm(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 5
                If bomb(v&, 3) >= 1 Then
                    If Sqr(((asteroidm(i&, 1) - bomb(v&, 1)) ^ 2) + ((asteroidm(i&, 2) - bomb(v&, 2)) ^ 2)) < bomb(v&, 3) - 20 Then
                        asteroidm(i&, 6) = asteroidm(i&, 6) - 10
                        score = score + 200
                        If soundexp = 1 And asteroidm(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 5
                If asteroide(v&, 5) >= 1 Then
                    If Sqr(((asteroidm(i&, 1) - asteroide(v&, 1)) ^ 2) + ((asteroidm(i&, 2) - asteroide(v&, 2)) ^ 2)) < asteroide(v&, 5) - 20 Then
                        asteroidm(i&, 6) = asteroidm(i&, 6) - 100
                        score = score + 200
                        If soundexp = 1 And asteroidm(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            asteroidm(i&, 1) = (asteroidm(i&, 1) - ship(1)) + asteroidm(i&, 3)
            asteroidm(i&, 2) = (asteroidm(i&, 2) - ship(2)) + asteroidm(i&, 4)
            If Sqr((asteroidm(i&, 1) ^ 2) + (asteroidm(i&, 2) ^ 2)) < 30 Then ship(3) = ship(3) - Sqr(((asteroidm(i&, 3) - ship(1)) ^ 2) + ((asteroidm(i&, 4) - ship(2)) ^ 2))
            If asteroidm(i&, 1) < -2000 Then asteroidm(i&, 1) = asteroidm(i&, 1) + 4000
            If asteroidm(i&, 1) > 2000 Then asteroidm(i&, 1) = asteroidm(i&, 1) - 4000
            If asteroidm(i&, 2) < -2000 Then asteroidm(i&, 2) = asteroidm(i&, 2) + 4000
            If asteroidm(i&, 2) > 2000 Then asteroidm(i&, 2) = asteroidm(i&, 2) - 4000
            Screen.FillColor = RGB(asteroidm(i&, 5), asteroidm(i&, 5), asteroidm(i&, 5))
            Screen.Circle ((asteroidm(i&, 1) * mag * 15) + (Screen.Width / 2), (asteroidm(i&, 2) * mag * 15) + (Screen.Height / 2)), 300 * mag, RGB(asteroidm(i&, 5), asteroidm(i&, 5), asteroidm(i&, 5))
        Else
            If asteroidm(i&, 6) <= 0 Then
                am = 0
                For v& = 1 To 128
                    If asteroids(v&, 6) <= 0 Then
                        asteroids(v&, 1) = asteroidm(i&, 1)
                        asteroids(v&, 2) = asteroidm(i&, 2)
                        asteroids(v&, 3) = (Int(Rnd * 1001) - 500) / 35
                        asteroids(v&, 4) = (Int(Rnd * 1001) - 500) / 35
                        asteroids(v&, 5) = Int(Rnd * 192) + 64
                        asteroids(v&, 6) = 5
                        am = am + 1
                    End If
                    If am = 2 Then Exit For
                Next
                asteroidm(i&, 6) = 11
            End If
        End If
    Next
    For i& = 1 To 32
        If asteroidb(i&, 6) > 0 And asteroidb(i&, 6) < 21 Then
            For v& = 1 To 64
                If bullet(v&, 5) = 1 Then
                    If Sqr(((asteroidb(i&, 1) - bullet(v&, 1)) ^ 2) + ((asteroidb(i&, 2) - bullet(v&, 2)) ^ 2)) < 42 Then
                        asteroidb(i&, 6) = asteroidb(i&, 6) - 1
                        bullet(v&, 5) = 0
                        score = score + 10
                        If soundhit = 1 Then
                            Form6.MMControl1(1).Command = "stop"
                            Form6.MMControl1(1).Command = "prev"
                            Form6.MMControl1(1).Command = "play"
                        End If
                        If soundexp = 1 And asteroidb(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 25
                If rocket(v&, 5) > 0 Then
                    If Sqr(((asteroidb(i&, 1) - rocket(v&, 1)) ^ 2) + ((asteroidb(i&, 2) - rocket(v&, 2)) ^ 2)) < 50 Then
                        asteroidb(i&, 6) = asteroidb(i&, 6) - 40
                        rocket(v&, 5) = -1
                        score = score + 200
                        If soundhit = 1 Then
                            Form6.MMControl1(1).Command = "stop"
                            Form6.MMControl1(1).Command = "prev"
                            Form6.MMControl1(1).Command = "play"
                        End If
                        If soundexp = 1 And asteroidb(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 5
                If bomb(v&, 3) >= 1 Then
                    If Sqr(((asteroidb(i&, 1) - bomb(v&, 1)) ^ 2) + ((asteroidb(i&, 2) - bomb(v&, 2)) ^ 2)) < bomb(v&, 3) - 40 Then
                        asteroidb(i&, 6) = asteroidb(i&, 6) - 10
                        score = score + 100
                        If soundexp = 1 And asteroidb(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 5
                If asteroide(v&, 5) >= 1 Then
                    If Sqr(((asteroidb(i&, 1) - asteroide(v&, 1)) ^ 2) + ((asteroidb(i&, 2) - asteroide(v&, 2)) ^ 2)) < asteroide(v&, 5) - 40 Then
                        asteroidb(i&, 6) = asteroidb(i&, 6) - 100
                        score = score + 400
                        If soundexp = 1 And asteroidb(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            asteroidb(i&, 1) = (asteroidb(i&, 1) - ship(1)) + asteroidb(i&, 3)
            asteroidb(i&, 2) = (asteroidb(i&, 2) - ship(2)) + asteroidb(i&, 4)
            If Sqr((asteroidb(i&, 1) ^ 2) + (asteroidb(i&, 2) ^ 2)) < 50 Then ship(3) = ship(3) - Sqr(((asteroidb(i&, 3) - ship(1)) ^ 2) + ((asteroidb(i&, 4) - ship(2)) ^ 2))
            If asteroidb(i&, 1) < -2000 Then asteroidb(i&, 1) = asteroidb(i&, 1) + 4000
            If asteroidb(i&, 1) > 2000 Then asteroidb(i&, 1) = asteroidb(i&, 1) - 4000
            If asteroidb(i&, 2) < -2000 Then asteroidb(i&, 2) = asteroidb(i&, 2) + 4000
            If asteroidb(i&, 2) > 2000 Then asteroidb(i&, 2) = asteroidb(i&, 2) - 4000
            Screen.FillColor = RGB(asteroidb(i&, 5), asteroidb(i&, 5), asteroidb(i&, 5))
            Screen.Circle ((asteroidb(i&, 1) * mag * 15) + (Screen.Width / 2), (asteroidb(i&, 2) * mag * 15) + (Screen.Height / 2)), 600 * mag, RGB(asteroidb(i&, 5), asteroidb(i&, 5), asteroidb(i&, 5))
        Else
            If asteroidb(i&, 6) <= 0 Then
                am = 0
                For v& = 1 To 64
                    If asteroidm(v&, 6) <= 0 Or asteroidm(v&, 6) >= 10 Then
                        asteroidm(v&, 1) = asteroidb(i&, 1)
                        asteroidm(v&, 2) = asteroidb(i&, 2)
                        asteroidm(v&, 3) = (Int(Rnd * 1001) - 500) / 45
                        asteroidm(v&, 4) = (Int(Rnd * 1001) - 500) / 45
                        asteroidm(v&, 5) = Int(Rnd * 192) + 64
                        asteroidm(v&, 6) = 10
                        am = am + 1
                    End If
                    If am = 2 Then Exit For
                Next
                asteroidb(i&, 6) = 21
            End If
        End If
    Next
    For i& = 1 To 16
        If asteroidh(i&, 6) > 0 And asteroidh(i&, 6) < 41 Then
            For v& = 1 To 64
                If bullet(v&, 5) = 1 Then
                    If Sqr(((asteroidh(i&, 1) - bullet(v&, 1)) ^ 2) + ((asteroidh(i&, 2) - bullet(v&, 2)) ^ 2)) < 82 Then
                        asteroidh(i&, 6) = asteroidh(i&, 6) - 1
                        bullet(v&, 5) = 0
                        score = score + 10
                        If soundhit = 1 Then
                            Form6.MMControl1(1).Command = "stop"
                            Form6.MMControl1(1).Command = "prev"
                            Form6.MMControl1(1).Command = "play"
                        End If
                        If soundexp = 1 And asteroidh(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 25
                If rocket(v&, 5) > 0 Then
                    If Sqr(((asteroidh(i&, 1) - rocket(v&, 1)) ^ 2) + ((asteroidh(i&, 2) - rocket(v&, 2)) ^ 2)) < 90 Then
                        asteroidh(i&, 6) = asteroidh(i&, 6) - 20
                        rocket(v&, 5) = -1
                        score = score + 400
                        If soundhit = 1 Then
                            Form6.MMControl1(1).Command = "stop"
                            Form6.MMControl1(1).Command = "prev"
                            Form6.MMControl1(1).Command = "play"
                        End If
                        If soundexp = 1 And asteroidh(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 5
                If bomb(v&, 3) >= 1 Then
                    If Sqr(((asteroidh(i&, 1) - bomb(v&, 1)) ^ 2) + ((asteroidh(i&, 2) - bomb(v&, 2)) ^ 2)) < bomb(v&, 3) - 80 Then
                        asteroidh(i&, 6) = asteroidh(i&, 6) - 10
                        score = score + 100
                        If soundexp = 1 And asteroidh(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            For v& = 1 To 5
                If asteroide(v&, 5) >= 1 Then
                    If Sqr(((asteroidh(i&, 1) - asteroide(v&, 1)) ^ 2) + ((asteroidh(i&, 2) - asteroide(v&, 2)) ^ 2)) < asteroide(v&, 5) - 80 Then
                        asteroidh(i&, 6) = asteroidh(i&, 6) - 100
                        score = score + 800
                        If soundexp = 1 And asteroidh(i&, 6) <= 0 Then
                            Form6.MMControl1(0).Command = "stop"
                            Form6.MMControl1(0).Command = "prev"
                            Form6.MMControl1(0).Command = "play"
                        End If
                    End If
                End If
            Next
            asteroidh(i&, 1) = (asteroidh(i&, 1) - ship(1)) + asteroidh(i&, 3)
            asteroidh(i&, 2) = (asteroidh(i&, 2) - ship(2)) + asteroidh(i&, 4)
            If Sqr((asteroidh(i&, 1) ^ 2) + (asteroidh(i&, 2) ^ 2)) < 90 Then ship(3) = ship(3) - Sqr(((asteroidh(i&, 3) - ship(1)) ^ 2) + ((asteroidh(i&, 4) - ship(2)) ^ 2))
            If asteroidh(i&, 1) < -2000 Then asteroidh(i&, 1) = asteroidh(i&, 1) + 4000
            If asteroidh(i&, 1) > 2000 Then asteroidh(i&, 1) = asteroidh(i&, 1) - 4000
            If asteroidh(i&, 2) < -2000 Then asteroidh(i&, 2) = asteroidh(i&, 2) + 4000
            If asteroidh(i&, 2) > 2000 Then asteroidh(i&, 2) = asteroidh(i&, 2) - 4000
            Screen.FillColor = RGB(asteroidh(i&, 5), asteroidh(i&, 5), asteroidh(i&, 5))
            Screen.Circle ((asteroidh(i&, 1) * mag * 15) + (Screen.Width / 2), (asteroidh(i&, 2) * mag * 15) + (Screen.Height / 2)), 1200 * mag, RGB(asteroidh(i&, 5), asteroidh(i&, 5), asteroidh(i&, 5))
        Else
            If asteroidh(i&, 6) <= 0 Then
                am = 0
                For v& = 1 To 64
                    If asteroidb(v&, 6) <= 0 Or asteroidb(v&, 6) >= 10 Then
                        asteroidb(v&, 1) = asteroidh(i&, 1)
                        asteroidb(v&, 2) = asteroidh(i&, 2)
                        asteroidb(v&, 3) = (Int(Rnd * 1001) - 500) / 75
                        asteroidb(v&, 4) = (Int(Rnd * 1001) - 500) / 75
                        asteroidb(v&, 5) = Int(Rnd * 192) + 64
                        asteroidb(v&, 6) = 20
                        am = am + 1
                    End If
                    If am = 2 Then Exit For
                Next
                asteroidh(i&, 6) = 41
            End If
        End If
    Next
    If Form2.Check8.Value = 1 Then
        Screen.FillStyle = 0
    Else
        Screen.FillStyle = 1
    End If
    For i& = 1 To 5
        If bomb(i&, 3) >= 1 Then
            bomb(i&, 1) = (bomb(i&, 1) - ship(1))
            bomb(i&, 2) = (bomb(i&, 2) - ship(2))
            If bomb(i&, 1) < -2000 Then bomb(i&, 1) = bomb(i&, 1) + 4000
            If bomb(i&, 1) > 2000 Then bomb(i&, 1) = bomb(i&, 1) - 4000
            If bomb(i&, 2) < -2000 Then bomb(i&, 2) = bomb(i&, 2) + 4000
            If bomb(i&, 2) > 2000 Then bomb(i&, 2) = bomb(i&, 2) - 4000
            Screen.FillColor = RGB(0, 255 - bomb(i&, 3), 0)
            Screen.Circle ((bomb(i&, 1) * mag * 15) + (Screen.Width / 2), (bomb(i&, 2) * mag * 15) + (Screen.Height / 2)), bomb(i&, 3) * mag * 15, RGB(0, 255 - bomb(i&, 3), 0)
            bomb(i&, 3) = bomb(i&, 3) + 10
            If bomb(i&, 3) >= 255 Then bomb(i&, 3) = -1
        End If
    Next
    For i& = 1 To 5
        If eac < 16 Then
            cr = 255 - ((eac) * 16)
            cg = (eac) * 16
            cb = 0
        End If
        If eac >= 16 And eac < 32 Then
            cr = 0
            cg = 255 - ((eac - 16) * 16)
            cb = (eac - 16) * 16
        End If
        If eac >= 32 And eac < 48 Then
            cr = (eac - 32) * 16
            cg = 0
            cb = 255 - ((eac - 32) * 16)
        End If
        Screen.FillColor = RGB(cr, cg, cb)
        If asteroide(i&, 5) >= 1 Then
            asteroide(i&, 1) = (asteroide(i&, 1) - ship(1))
            asteroide(i&, 2) = (asteroide(i&, 2) - ship(2))
            If Sqr((asteroide(i&, 1) ^ 2) + (asteroide(i&, 2) ^ 2)) < 10 + asteroide(i&, 5) Then ship(3) = ship(3) - 32
            If asteroide(i&, 1) < -2000 Then asteroide(i&, 1) = asteroide(i&, 1) + 4000
            If asteroide(i&, 1) > 2000 Then asteroide(i&, 1) = asteroide(i&, 1) - 4000
            If asteroide(i&, 2) < -2000 Then asteroide(i&, 2) = asteroide(i&, 2) + 4000
            If asteroide(i&, 2) > 2000 Then asteroide(i&, 2) = asteroide(i&, 2) - 4000
            Screen.Circle ((asteroide(i&, 1) * mag * 15) + (Screen.Width / 2), (asteroide(i&, 2) * mag * 15) + (Screen.Height / 2)), asteroide(i&, 5) * mag * 15, RGB(cr, cg, cb)
            asteroide(i&, 5) = asteroide(i&, 5) + 24
            If asteroide(i&, 5) >= 255 Then
                If Form2.Check3.Value = 0 Then
                    asteroide(i&, 5) = -1
                Else
                    asteroide(i&, 5) = 0
                    Do
                        asteroide(i&, 1) = Int(Rnd * 4001) - 2000
                        asteroide(i&, 2) = Int(Rnd * 4001) - 2000
                    Loop Until (asteroide(i&, 1) > 1000 Or asteroide(i&, 1) < -1000) And (asteroide(i&, 2) > 1000 Or asteroide(i&, 2) < -1000)
                    asteroide(i&, 3) = (Int(Rnd * 1001) - 500) / 50
                    asteroide(i&, 4) = (Int(Rnd * 1001) - 500) / 50
                End If
            End If
        End If
    Next
    If Form2.Check6.Value = 1 Then
        Screen.FillStyle = 0
    Else
        Screen.FillStyle = 1
    End If
    Screen.FillColor = RGB(128, 64, 0)
    For i& = 1 To 64
        If bullet(i&, 5) = 1 Then
            bullet(i&, 1) = (bullet(i&, 1) - ship(1)) + bullet(i&, 3)
            bullet(i&, 2) = (bullet(i&, 2) - ship(2)) + bullet(i&, 4)
            If bullet(i&, 1) < -2000 Then bullet(i&, 1) = bullet(i&, 1) + 4000
            If bullet(i&, 1) > 2000 Then bullet(i&, 1) = bullet(i&, 1) - 4000
            If bullet(i&, 2) < -2000 Then bullet(i&, 2) = bullet(i&, 2) + 4000
            If bullet(i&, 2) > 2000 Then bullet(i&, 2) = bullet(i&, 2) - 4000
            Screen.Circle ((bullet(i&, 1) * mag * 15) + (Screen.Width / 2), (bullet(i&, 2) * mag * 15) + (Screen.Height / 2)), 30 * mag, RGB(128, 64, 0)
        End If
    Next
    For i& = 1 To 25
        If rocket(i&, 5) > 0 Then
            If rocket(i&, 5) = 1 Then
                If asteroidh(rocket(i&, 6), 6) > 40 Then
                    si = 0
                    id = 0
                    dist = 10000
                    For v& = 1 To 16
                        If Sqr(((asteroidh(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidh(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidh(v&, 6) > 0 And asteroidh(v&, 6) < 41 Then
                            si = 1
                            id = v&
                            dist = Sqr(((asteroidh(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidh(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 32
                        If Sqr(((asteroidb(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidb(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidb(v&, 6) > 0 And asteroidb(v&, 6) < 21 Then
                            si = 2
                            id = v&
                            dist = Sqr(((asteroidb(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidb(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 64
                        If Sqr(((asteroidm(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidm(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidm(v&, 6) > 0 And asteroidm(v&, 6) < 11 Then
                            si = 3
                            id = v&
                            dist = Sqr(((asteroidm(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidm(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 128
                        If Sqr(((asteroids(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroids(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroids(v&, 6) > 0 And asteroids(v&, 6) < 6 Then
                            si = 4
                            id = v&
                            dist = Sqr(((asteroids(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroids(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 5
                        If Sqr(((asteroide(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroide(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroide(v&, 5) = 0 Then
                            si = 5
                            id = v&
                            dist = Sqr(((asteroide(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroide(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    rocket(i&, 5) = si
                    rocket(i&, 6) = id
                End If
            End If
            If rocket(i&, 5) = 2 Then
                If asteroidb(rocket(i&, 6), 6) > 20 Then
                    si = 0
                    id = 0
                    dist = 10000
                    For v& = 1 To 16
                        If Sqr(((asteroidh(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidh(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidh(v&, 6) > 0 And asteroidh(v&, 6) < 41 Then
                            si = 1
                            id = v&
                            dist = Sqr(((asteroidh(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidh(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 32
                        If Sqr(((asteroidb(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidb(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidb(v&, 6) > 0 And asteroidb(v&, 6) < 21 Then
                            si = 2
                            id = v&
                            dist = Sqr(((asteroidb(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidb(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 64
                        If Sqr(((asteroidm(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidm(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidm(v&, 6) > 0 And asteroidm(v&, 6) < 11 Then
                            si = 3
                            id = v&
                            dist = Sqr(((asteroidm(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidm(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 128
                        If Sqr(((asteroids(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroids(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroids(v&, 6) > 0 And asteroids(v&, 6) < 6 Then
                            si = 4
                            id = v&
                            dist = Sqr(((asteroids(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroids(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 5
                        If Sqr(((asteroide(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroide(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroide(v&, 5) = 0 Then
                            si = 5
                            id = v&
                            dist = Sqr(((asteroide(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroide(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    rocket(i&, 5) = si
                    rocket(i&, 6) = id
                End If
            End If
            If rocket(i&, 5) = 3 Then
                If asteroidm(rocket(i&, 6), 6) > 10 Then
                    si = 0
                    id = 0
                    dist = 10000
                    For v& = 1 To 16
                        If Sqr(((asteroidh(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidh(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidh(v&, 6) > 0 And asteroidh(v&, 6) < 41 Then
                            si = 1
                            id = v&
                            dist = Sqr(((asteroidh(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidh(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 32
                        If Sqr(((asteroidb(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidb(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidb(v&, 6) > 0 And asteroidb(v&, 6) < 21 Then
                            si = 2
                            id = v&
                            dist = Sqr(((asteroidb(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidb(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 64
                        If Sqr(((asteroidm(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidm(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidm(v&, 6) > 0 And asteroidm(v&, 6) < 11 Then
                            si = 3
                            id = v&
                            dist = Sqr(((asteroidm(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidm(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 128
                        If Sqr(((asteroids(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroids(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroids(v&, 6) > 0 And asteroids(v&, 6) < 6 Then
                            si = 4
                            id = v&
                            dist = Sqr(((asteroids(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroids(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 5
                        If Sqr(((asteroide(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroide(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroide(v&, 5) = 0 Then
                            si = 5
                            id = v&
                            dist = Sqr(((asteroide(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroide(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    rocket(i&, 5) = si
                    rocket(i&, 6) = id
                End If
            End If
            If rocket(i&, 5) = 4 Then
                If asteroids(rocket(i&, 6), 6) <= 0 Then
                    si = 0
                    id = 0
                    dist = 10000
                    For v& = 1 To 16
                        If Sqr(((asteroidh(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidh(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidh(v&, 6) > 0 And asteroidh(v&, 6) < 41 Then
                            si = 1
                            id = v&
                            dist = Sqr(((asteroidh(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidh(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 32
                        If Sqr(((asteroidb(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidb(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidb(v&, 6) > 0 And asteroidb(v&, 6) < 21 Then
                            si = 2
                            id = v&
                            dist = Sqr(((asteroidb(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidb(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 64
                        If Sqr(((asteroidm(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidm(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidm(v&, 6) > 0 And asteroidm(v&, 6) < 11 Then
                            si = 3
                            id = v&
                            dist = Sqr(((asteroidm(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidm(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 128
                        If Sqr(((asteroids(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroids(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroids(v&, 6) > 0 And asteroids(v&, 6) < 6 Then
                            si = 4
                            id = v&
                            dist = Sqr(((asteroids(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroids(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 5
                        If Sqr(((asteroide(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroide(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroide(v&, 5) = 0 Then
                            si = 5
                            id = v&
                            dist = Sqr(((asteroide(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroide(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    rocket(i&, 5) = si
                    rocket(i&, 6) = id
                End If
            End If
            If rocket(i&, 5) = 4 Then
                If asteroids(rocket(i&, 6), 6) > 0 Then
                    si = 0
                    id = 0
                    dist = 10000
                    For v& = 1 To 16
                        If Sqr(((asteroidh(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidh(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidh(v&, 6) > 0 And asteroidh(v&, 6) < 41 Then
                            si = 1
                            id = v&
                            dist = Sqr(((asteroidh(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidh(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 32
                        If Sqr(((asteroidb(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidb(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidb(v&, 6) > 0 And asteroidb(v&, 6) < 21 Then
                            si = 2
                            id = v&
                            dist = Sqr(((asteroidb(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidb(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 64
                        If Sqr(((asteroidm(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidm(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroidm(v&, 6) > 0 And asteroidm(v&, 6) < 11 Then
                            si = 3
                            id = v&
                            dist = Sqr(((asteroidm(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroidm(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 128
                        If Sqr(((asteroids(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroids(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroids(v&, 6) > 0 And asteroids(v&, 6) < 6 Then
                            si = 4
                            id = v&
                            dist = Sqr(((asteroids(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroids(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    For v& = 1 To 5
                        If Sqr(((asteroide(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroide(v&, 2) - rocket(i&, 2)) ^ 2)) < dist And asteroide(v&, 5) = 0 Then
                            si = 5
                            id = v&
                            dist = Sqr(((asteroide(v&, 1) - rocket(i&, 1)) ^ 2) + ((asteroide(v&, 2) - rocket(i&, 2)) ^ 2))
                        End If
                    Next
                    rocket(i&, 5) = si
                    rocket(i&, 6) = id
                End If
            End If
            If rocket(i&, 5) = 1 Then
                targetx = asteroidh(rocket(i&, 6), 1) - rocket(i&, 1)
                targety = asteroidh(rocket(i&, 6), 2) - rocket(i&, 2)
            End If
            If rocket(i&, 5) = 2 Then
                targetx = asteroidb(rocket(i&, 6), 1) - rocket(i&, 1)
                targety = asteroidb(rocket(i&, 6), 2) - rocket(i&, 2)
            End If
            If rocket(i&, 5) = 3 Then
                targetx = asteroidm(rocket(i&, 6), 1) - rocket(i&, 1)
                targety = asteroidm(rocket(i&, 6), 2) - rocket(i&, 2)
            End If
            If rocket(i&, 5) = 4 Then
                targetx = asteroids(rocket(i&, 6), 1) - rocket(i&, 1)
                targety = asteroids(rocket(i&, 6), 2) - rocket(i&, 2)
            End If
            If rocket(i&, 5) = 5 Then
                targetx = asteroide(rocket(i&, 6), 1) - rocket(i&, 1)
                targety = asteroide(rocket(i&, 6), 2) - rocket(i&, 2)
            End If
            dist = Sqr((targetx ^ 2) + (targety ^ 2))
            If dist <> 0 Then
                rocket(i&, 3) = targetx / dist
                rocket(i&, 4) = targety / dist
            End If
            rocket(i&, 1) = (rocket(i&, 1) - ship(1)) + (rocket(i&, 3) * 20)
            rocket(i&, 2) = (rocket(i&, 2) - ship(2)) + (rocket(i&, 4) * 20)
            If rocket(i&, 1) < -2000 Then rocket(i&, 1) = rocket(i&, 1) + 4000
            If rocket(i&, 1) > 2000 Then rocket(i&, 1) = rocket(i&, 1) - 4000
            If rocket(i&, 2) < -2000 Then rocket(i&, 2) = rocket(i&, 2) + 4000
            If rocket(i&, 2) > 2000 Then rocket(i&, 2) = rocket(i&, 2) - 4000
            Screen.Line ((rocket(i&, 1) * mag * 15) + (Screen.Width / 2), (rocket(i&, 2) * mag * 15) + (Screen.Height / 2))-((rocket(i&, 1) * mag * 15) + (Screen.Width / 2) + (rocket(i&, 3) * mag * 150), (rocket(i&, 2) * mag * 15) + (Screen.Height / 2) + (rocket(i&, 4) * mag * 150)), RGB(255, 128, 0)
        End If
    Next
    For i& = 1 To 4
        item(i&, 1) = (item(i&, 1) - ship(1))
        item(i&, 2) = (item(i&, 2) - ship(2))
        If item(i&, 1) < -2000 Then item(i&, 1) = item(i&, 1) + 4000
        If item(i&, 1) > 2000 Then item(i&, 1) = item(i&, 1) - 4000
        If item(i&, 2) < -2000 Then item(i&, 2) = item(i&, 2) + 4000
        If item(i&, 2) > 2000 Then item(i&, 2) = item(i&, 2) - 4000
        If item(i&, 3) = 1 Then
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2)), RGB(0, 255, 0)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + 60), RGB(0, 255, 0)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + (60 * mag)), RGB(0, 255, 0)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + 60), RGB(0, 255, 0)
            If Sqr((item(i&, 1) ^ 2) + (item(i&, 2) ^ 2)) < 25 Then
                item(i&, 1) = Int(Rnd * 4001) - 2000
                item(i&, 2) = Int(Rnd * 4001) - 2000
                item(i&, 3) = Int(Rnd * 4) + 1
                For v& = 1 To 5
                    If bomb(v&, 3) = -1 Then
                        bomb(v&, 3) = 0
                        Exit For
                    End If
                Next
            End If
        End If
        If item(i&, 3) = 2 Then
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2)), RGB(255, 0, 0)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + 60), RGB(255, 0, 0)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + (60 * mag)), RGB(255, 0, 0)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + 60), RGB(255, 0, 0)
            If Sqr((item(i&, 1) ^ 2) + (item(i&, 2) ^ 2)) < 25 Then
                item(i&, 1) = Int(Rnd * 4001) - 2000
                item(i&, 2) = Int(Rnd * 4001) - 2000
                item(i&, 3) = Int(Rnd * 4) + 1
                ship(3) = 255
            End If
        End If
        If item(i&, 3) = 3 Then
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2)), RGB(255, 255, 0)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + 60), RGB(255, 255, 0)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + (60 * mag)), RGB(255, 255, 0)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + 60), RGB(255, 255, 0)
            If Sqr((item(i&, 1) ^ 2) + (item(i&, 2) ^ 2)) < 25 Then
                item(i&, 1) = Int(Rnd * 4001) - 2000
                item(i&, 2) = Int(Rnd * 4001) - 2000
                item(i&, 3) = Int(Rnd * 4) + 1
                For rl = 1 To 5
                    For v& = 1 To 25
                        If rocket(v&, 5) = -1 Then
                            rocket(v&, 5) = 0
                            Exit For
                        End If
                    Next
                Next
            End If
        End If
        If item(i&, 3) = 4 Then
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2)), RGB(255, 255, 255)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + 60), RGB(255, 255, 255)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + (60 * mag)), RGB(255, 255, 255)
            Screen.Line ((item(i&, 1) * mag * 15) + (Screen.Width / 2) + (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) - (60 * mag))-((item(i&, 1) * mag * 15) + (Screen.Width / 2) - (60 * mag), (item(i&, 2) * mag * 15) + (Screen.Height / 2) + 60), RGB(255, 255, 255)
            If Sqr((item(i&, 1) ^ 2) + (item(i&, 2) ^ 2)) < 25 Then
                item(i&, 1) = Int(Rnd * 4001) - 2000
                item(i&, 2) = Int(Rnd * 4001) - 2000
                item(i&, 3) = Int(Rnd * 4) + 1
                warp = 255
            End If
        End If
    Next
    If ship(3) <= 0 Then
        ship(3) = ship(3) - 1
        If ship(3) <= -255 Then
            Load Form4
            Form4.Show
        End If
        Screen.Line ((mdx * mag * 300) + (Screen.Width / 2), (mdy * mag * 300) + (Screen.Height / 2))-((Screen.Width / 2), (Screen.Height / 2)), RGB(-ship(3), 64, 64)
        Screen.Line ((-mdy * mag * 150) + (Screen.Width / 2) + (-mdx * mag * 150), (mdx * mag * 150) + (Screen.Height / 2) + (-mdy * mag * 150))-((mdx * mag * 300) + (Screen.Width / 2), (mdy * mag * 300) + (Screen.Height / 2)), RGB(-ship(3), 64, 64)
        Screen.Line ((mdy * mag * 150) + (Screen.Width / 2) + (-mdx * mag * 150), (-mdx * mag * 150) + (Screen.Height / 2) + (-mdy * mag * 150))-((mdx * mag * 300) + (Screen.Width / 2), (mdy * mag * 300) + (Screen.Height / 2)), RGB(-ship(3), 64, 64)
        Screen.Line ((-mdy * mag * 150) + (Screen.Width / 2) + (-mdx * mag * 150), (mdx * mag * 150) + (Screen.Height / 2) + (-mdy * mag * 150))-((Screen.Width / 2), (Screen.Height / 2)), RGB(-ship(3), 64, 64)
        Screen.Line ((mdy * mag * 150) + (Screen.Width / 2) + (-mdx * mag * 150), (-mdx * mag * 150) + (Screen.Height / 2) + (-mdy * mag * 150))-((Screen.Width / 2), (Screen.Height / 2)), RGB(-ship(3), 64, 64)
        For i& = 1 To 255
            v& = 256 - i&
            smoke(v&, 1) = smoke(v& - 1, 1)
            smoke(v&, 2) = smoke(v& - 1, 2)
            smoke(v&, 3) = smoke(v& - 1, 3)
            smoke(v&, 4) = smoke(v& - 1, 4)
            smoke(v&, 5) = smoke(v& - 1, 5)
        Next
        For i& = 1 To 63
            v& = 64 - i&
            bullet(v&, 1) = bullet(v& - 1, 1)
            bullet(v&, 2) = bullet(v& - 1, 2)
            bullet(v&, 3) = bullet(v& - 1, 3)
            bullet(v&, 4) = bullet(v& - 1, 4)
            bullet(v&, 5) = bullet(v& - 1, 5)
        Next
        smoke(16, 5) = 1
        mouse(1) = Int(Rnd * 2001) - 1000
        mouse(2) = Int(Rnd * 2001) - 1000
        If mouse(1) <> 0 Or mouse(2) <> 0 Then mdx = mouse(1) / Sqr((mouse(1) ^ 2) + (mouse(2) ^ 2)) Else mdx = 0
        If mouse(1) <> 0 Or mouse(2) <> 0 Then mdy = mouse(2) / Sqr((mouse(1) ^ 2) + (mouse(2) ^ 2)) Else mdy = 0
        smoke(16, 3) = mdx
        smoke(16, 4) = mdy
        ship(1) = (ship(1) + (mdx)) * 0.95
        ship(2) = (ship(2) + (mdy)) * 0.95
    Else
        Screen.Line ((mdx * mag * 300) + (Screen.Width / 2), (mdy * mag * 300) + (Screen.Height / 2))-((Screen.Width / 2), (Screen.Height / 2)), RGB(255 - ship(3), ship(3), ship(3))
        Screen.Line ((-mdy * mag * 150) + (Screen.Width / 2) + (-mdx * mag * 150), (mdx * mag * 150) + (Screen.Height / 2) + (-mdy * mag * 150))-((mdx * mag * 300) + (Screen.Width / 2), (mdy * mag * 300) + (Screen.Height / 2)), RGB(255 - ship(3), ship(3), ship(3))
        Screen.Line ((mdy * mag * 150) + (Screen.Width / 2) + (-mdx * mag * 150), (-mdx * mag * 150) + (Screen.Height / 2) + (-mdy * mag * 150))-((mdx * mag * 300) + (Screen.Width / 2), (mdy * mag * 300) + (Screen.Height / 2)), RGB(255 - ship(3), ship(3), ship(3))
        Screen.Line ((-mdy * mag * 150) + (Screen.Width / 2) + (-mdx * mag * 150), (mdx * mag * 150) + (Screen.Height / 2) + (-mdy * mag * 150))-((Screen.Width / 2), (Screen.Height / 2)), RGB(255 - ship(3), ship(3), ship(3))
        Screen.Line ((mdy * mag * 150) + (Screen.Width / 2) + (-mdx * mag * 150), (-mdx * mag * 150) + (Screen.Height / 2) + (-mdy * mag * 150))-((Screen.Width / 2), (Screen.Height / 2)), RGB(255 - ship(3), ship(3), ship(3))
        If warp > 0 Then
            Screen.Line ((-mdy * mag * 180) + (Screen.Width / 2) + (-mdx * mag * 180), (mdx * mag * 180) + (Screen.Height / 2) + (-mdy * mag * 180))-((mdx * mag * 330) + (Screen.Width / 2), (mdy * mag * 330) + (Screen.Height / 2)), RGB(0, warp, 0)
            Screen.Line ((mdy * mag * 180) + (Screen.Width / 2) + (-mdx * mag * 180), (-mdx * mag * 180) + (Screen.Height / 2) + (-mdy * mag * 180))-((mdx * mag * 330) + (Screen.Width / 2), (mdy * mag * 330) + (Screen.Height / 2)), RGB(0, warp, 0)
            Screen.Line ((-mdy * mag * 180) + (Screen.Width / 2) + (-mdx * mag * 180), (mdx * mag * 180) + (Screen.Height / 2) + (-mdy * mag * 180))-((-mdx * mag * 30) + (Screen.Width / 2), (-mdy * mag * 30) + (Screen.Height / 2)), RGB(0, warp, 0)
            Screen.Line ((mdy * mag * 180) + (Screen.Width / 2) + (-mdx * mag * 180), (-mdx * mag * 180) + (Screen.Height / 2) + (-mdy * mag * 180))-((-mdx * mag * 30) + (Screen.Width / 2), (-mdy * mag * 30) + (Screen.Height / 2)), RGB(0, warp, 0)
        End If
        If spd >= 93 And Form2.Check4.Value = 1 Then
            Screen.Line ((mdy * mag * 450) + (-mdx * mag * 450 * (spd - 93)) + (Screen.Width / 2), (-mdx * mag * 450) + (-mdy * mag * 450 * (spd - 93)) + (Screen.Height / 2))-((mdx * mag * 300) + (Screen.Width / 2), (mdy * mag * 300) + (Screen.Height / 2)), RGB((spd - 93) * 125, (spd - 93) * 125, (spd - 93) * 125)
            Screen.Line ((-mdy * mag * 450) + (-mdx * mag * 450 * (spd - 93)) + (Screen.Width / 2), (mdx * mag * 450) + (-mdy * mag * 450 * (spd - 93)) + (Screen.Height / 2))-((mdx * mag * 300) + (Screen.Width / 2), (mdy * mag * 300) + (Screen.Height / 2)), RGB((spd - 93) * 125, (spd - 93) * 125, (spd - 93) * 125)
        End If
    End If
    For i& = 1 To 256
        If smoke(i&, 5) <> 0 Then
            smoke(i&, 1) = (smoke(i&, 1) - ship(1)) + smoke(i&, 3)
            smoke(i&, 2) = (smoke(i&, 2) - ship(2)) + smoke(i&, 4)
            smoke(i&, 3) = smoke(i&, 3) * 0.9
            smoke(i&, 4) = smoke(i&, 4) * 0.9
            If smoke(i&, 1) < -2000 Then smoke(i&, 1) = smoke(i&, 1) + 4000
            If smoke(i&, 1) > 2000 Then smoke(i&, 1) = smoke(i&, 1) - 4000
            If smoke(i&, 2) < -2000 Then smoke(i&, 2) = smoke(i&, 2) + 4000
            If smoke(i&, 2) > 2000 Then smoke(i&, 2) = smoke(i&, 2) - 4000
            If smoke(i&, 5) = 1 Then
                If i& >= 1 And i& < 8 Then
                    cr = 0
                    cg = 0
                    cb = i& * 32
                End If
                If i& >= 8 And i& < 16 Then
                    cr = 0
                    cg = 0
                    cb = 256 - (i& - 8) * 32
                End If
                If i& >= 16 And i& < 32 Then
                    cr = (i& - 16) * 16
                    cg = (i& - 16) * 8
                    cb = 0
                End If
                If i& >= 32 And i& < 48 Then
                    cr = 256 - (i& - 32) * 8
                    cg = 256 - (i& - 32) * 16
                    cb = 0
                End If
                If i& >= 48 And i& < 64 Then
                    cr = (i& - 48) * 12
                    cg = (i& - 48) * 12
                    cb = (i& - 48) * 12
                End If
                If i& >= 64 And i& < 256 Then
                    cr = 192 - (i& - 64)
                    cg = 192 - (i& - 64)
                    cb = 192 - (i& - 64)
                End If
            End If
            If smoke(i&, 5) = 2 Then
                If i& >= 0 And i& < 48 Then
                    cr = 0
                    cg = 256 - (i&) * 5.33333333333333
                    cb = 0
                End If
                If i& >= 48 And i& < 64 Then
                    cr = (i& - 48) * 12
                    cg = (i& - 48) * 12
                    cb = (i& - 48) * 12
                End If
                If i& >= 64 And i& < 256 Then
                    cr = 192 - (i& - 64)
                    cg = 192 - (i& - 64)
                    cb = 192 - (i& - 64)
                End If
            End If
            If Form2.Check7.Value = 1 Then
                Screen.FillStyle = 0
            Else
                Screen.FillStyle = 1
            End If
            Screen.FillColor = RGB(cr, cg, cb)
            Screen.Circle ((smoke(i&, 1) * mag * 15) + (Screen.Width / 2), (smoke(i&, 2) * mag * 15) + (Screen.Height / 2)), ((i&) + 30) * mag, RGB(cr, cg, cb)
        End If
    Next
    warp = warp - 1
    eac = eac + 1
    If eac = 48 Then eac = 0
End Sub

