VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H00000000&
   Caption         =   "Form1"
   ClientHeight    =   6660
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   10215
   LinkTopic       =   "Form1"
   ScaleHeight     =   6660
   ScaleWidth      =   10215
   StartUpPosition =   3  'Windows Default
   Begin VB.Timer Timer1 
      Interval        =   1
      Left            =   3960
      Top             =   2640
   End
   Begin VB.Shape shpPlayer1 
      BorderColor     =   &H00FFFFFF&
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   975
      Left            =   480
      Top             =   2760
      Width           =   255
   End
   Begin VB.Shape shpPlayer2 
      BorderColor     =   &H00FFFFFF&
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   975
      Left            =   9480
      Top             =   2760
      Width           =   255
   End
   Begin VB.Shape shpBall 
      BorderColor     =   &H00FFFFFF&
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   4920
      Top             =   3120
      Width           =   255
   End
   Begin VB.Shape shpWallBottom 
      BorderColor     =   &H00FFFFFF&
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   375
      Left            =   0
      Top             =   6120
      Width           =   10215
   End
   Begin VB.Shape shpWallTop 
      BorderColor     =   &H00FFFFFF&
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   375
      Left            =   0
      Top             =   120
      Width           =   10215
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim vmom As Integer 'vertical momentum
Dim hmom As Integer 'horizontal momentum
Dim padSpeed As Integer 'the speed of the players paddle
Dim origPaddleLoc As Integer
Dim origBallLocY As Integer
Dim origBallLocX As Integer

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)

If KeyCode = 38 Then    'the up key
    padSpeed = -150
ElseIf KeyCode = 40 Then    'the down key
    padSpeed = 150
End If

End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
padSpeed = 0    'stop the paddle from moving
End Sub

Private Sub Form_Load()
hmom = -150
vmom = 0

'record the origional starting locations for everything
origPaddleLoc = shpPlayer1.Top
origBallLocX = shpBall.Left
origBallLocY = shpBall.Top

End Sub

Private Sub Timer1_Timer()
'move the ball
shpBall.Top = shpBall.Top + vmom
shpBall.Left = shpBall.Left + hmom

'check to see if the ball's hit a wall
If shpBall.Top + shpBall.Height >= shpWallBottom.Top Then
    shpBall.Top = shpWallBottom.Top - shpBall.Height
    vmom = -vmom
    Beep
ElseIf shpBall.Top <= shpWallTop.Top + shpWallTop.Height Then
    shpBall.Top = shpWallTop.Top + shpWallTop.Height
    vmom = -vmom
    Beep
End If

'move the paddle
If padSpeed <> 0 Then
    shpPlayer1.Top = shpPlayer1.Top + padSpeed
End If

'check to see if the paddle's hit a wall
If shpPlayer1.Top <= shpWallTop.Top + shpWallTop.Height Then
    shpPlayer1.Top = shpWallTop.Top + shpWallTop.Height
ElseIf shpPlayer1.Top + shpPlayer1.Height >= shpWallBottom.Top Then
    shpPlayer1.Top = shpWallBottom.Top - shpPlayer1.Height
End If

If shpPlayer2.Top <= shpWallTop.Top + shpWallTop.Height Then
    shpPlayer2.Top = shpWallTop.Top + shpWallTop.Height
ElseIf shpPlayer2.Top + shpPlayer2.Height >= shpWallBottom.Top Then
    shpPlayer2.Top = shpWallBottom.Top - shpPlayer2.Height
End If

'move the computer player's paddle
If shpBall.Top < shpPlayer2.Top Then
    shpPlayer2.Top = shpPlayer2.Top - 250
ElseIf shpBall.Top > shpPlayer2.Top + shpPlayer2.Height Then
    shpPlayer2.Top = shpPlayer2.Top + 250
End If

'if the ball has hit player 1's paddle
If shpBall.Left <= shpPlayer1.Left + shpPlayer1.Width And shpBall.Left >= shpPlayer1.Left - shpPlayer1.Width Then
    If shpBall.Top + shpBall.Height >= shpPlayer1.Top And shpBall.Top <= shpPlayer1.Top + shpPlayer1.Height Then
        'calculate the angle it's deflecting off at
        tmp = ((shpPlayer1.Top + (shpPlayer1.Height / 2)) - (shpBall.Top + (shpBall.Height / 2))) * 0.55
        vmom = vmom + -tmp
        Beep
        shpBall.Left = shpPlayer1.Left + shpPlayer1.Width
        'deflect the ball
        hmom = -hmom
    End If
End If


'if the ball has hit player 2's paddle
If shpBall.Left + shpBall.Width >= shpPlayer2.Left And shpBall.Left <= shpPlayer2.Left + shpPlayer2.Width Then
    If shpBall.Top + shpBall.Height >= shpPlayer2.Top And shpBall.Top <= shpPlayer2.Top + shpPlayer2.Height Then
        'calculate the angle it's deflecting off at
        tmp = ((shpPlayer2.Top + (shpPlayer2.Height / 2)) - (shpBall.Top + (shpBall.Height / 2))) * 0.55
        vmom = vmom + -tmp
        Beep
        shpBall.Left = shpPlayer2.Left - shpBall.Width
        'deflect the ball
        hmom = -hmom
    End If
End If

'see if someone's won
If shpBall.Left + shpBall.Width < 0 Then

    'reset the ball and paddles to their origional location
    shpBall.Left = origBallLocX
    shpBall.Top = origBallLocY
    shpPlayer1.Top = origPaddleLoc
    shpPlayer2.Top = origPaddleLoc
    hmom = -150
    vmom = 0

ElseIf shpBall.Left > Form1.Width Then

    'reset the ball and paddles to their origional location
    shpBall.Left = origBallLocX
    shpBall.Top = origBallLocY
    shpPlayer1.Top = origPaddleLoc
    shpPlayer2.Top = origPaddleLoc
    hmom = 150
    vmom = 0
End If
End Sub
