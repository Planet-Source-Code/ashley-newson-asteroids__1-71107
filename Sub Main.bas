Attribute VB_Name = "Sub_Main"
Public asteroidh(1 To 16, 6) As Double
Public asteroidb(1 To 32, 6) As Double
Public asteroidm(1 To 64, 6) As Double
Public asteroids(1 To 128, 6) As Double
Public asteroide(1 To 5, 5) As Double
Public stars(1 To 8192, 4) As Double
Public mouse(1 To 4) As Long
Public ship(1 To 3) As Double
Public smoke(256, 5) As Double
Public bullet(64, 5) As Double
Public bomb(1 To 5, 3) As Double
Public rocket(1 To 25, 6) As Double
Public item(1 To 6, 3) As Double
Public warp As Double
Public eac As Long
Public score As Double

Sub Main()
On Error GoTo errorload
If 0 = 1 Then
errorload:
MsgBox ("Warning: An unexpected error has occoured while loading. Asteroids might not work correctly, and might be unstable. Error:" + Str$(Err.Number) + ": " + Err.Description)
End If
Load Form1
Form1.Show
Load Form2
Form2.Show
Form2.Visible = False
Load Form6
Form6.Show
Form6.Visible = False
If Dir("asteroids.cfg") = "asteroids.cfg" Then
Open "asteroids.cfg" For Random As #1
Get #1, 1, temps$
Form2.Text1.Text = temps$
Form1.Timer1.Interval = Val(temps$)
Get #1, 2, temps$
Form2.Text2.Text = temps$
Get #1, 3, temps$
Form2.Text3.Text = temps$
Get #1, 4, temp
Form2.Check1.Value = temp
Get #1, 5, temp
Form2.Check2.Value = temp
Get #1, 6, temp
Form2.Check3.Value = temp
Get #1, 7, temp
Form2.Check4.Value = temp
Get #1, 8, temp
Form2.Check5.Value = temp
Get #1, 9, temp
If temp = 0 Then
Form2.Option1.Value = True
End If
If temp = 1 Then
Form2.Option2.Value = True
End If
If temp = 2 Then
Form2.Option3.Value = True
End If
Get #1, 10, temp
Form2.Slider1.Value = temp
Get #1, 11, temp
Form2.Slider2.Value = temp
Get #1, 12, temp
Form6.Check1.Value = temp
Get #1, 13, temp
Form6.Check2.Value = temp
Get #1, 14, temp
Form6.Check3.Value = temp
Get #1, 15, temp
Form2.Check6.Value = temp
Get #1, 16, temp
Form2.Check7.Value = temp
Get #1, 17, temp
Form2.Check8.Value = temp
Close #1
End If
mouse(2) = -1
End Sub
