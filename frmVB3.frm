ÿÌ, Q  P    Form1ÿ Form1    Ü  
  ?*  2   B #ÿÿÿÿ$Form15Ü  6
  7?*  82  ÿ   Timer1ÿ   À  ¸  ÿ+   
shpPlayer2ÿÿÿÿ P(Ø	ÿ G ÿÿÿ  ÿ+   
shpPlayer1ÿÿÿÿ ð Ø	ÿ G ÿÿÿ  ÿ(   shpBallÿÿÿÿ  ¸ÿ ÿ  ÿÿÿ  ÿ.   shpWallBottomÿÿÿÿ   p?*w ÿÿÿ  ÿ+   
shpWallTopÿÿÿÿ   x ?*w ÿÿÿ  ÿ  ü  __	 
€Q€Qn           ” ¤   ´ o Gæ Ä   Z   Ï           w g û eØ       ƒ                     n    @	Form_Load(€vmom  €hmom  €padSpeed  €origPaddleLoc  €origBallLocY  €origBallLocX  @Form_KeyDownô €KeyCode€Shift@
Form_KeyUp  €
shpPlayer19 Top  €shpBall   Left  @Timer1_Timer   HeightU€shpWallBottom  €
shpWallTop^€
shpPlayer2   Width  €tmp  €Form1À      ü ,   	  g v   vertical momentum   ü V   	  o v   horizontal momentum   ü Œ     w v !  the speed of the players paddle   ü œ     ƒ   ü ¬     ”   ü ¼     ¤ 	  ÿÿÿÿÿÿÿÿ    Form_KeyDownpˆ      X  ´   Ä  €   Ï  €        Ä å & ¦ I T v   the up key å – ù  w    Ä å ( ¦ 6 ‚ v   the down key å –  w   8     9 	  ÿÿÿÿÿÿÿÿ	    
 Form_KeyUppR      X  Ø   Ä  €   Ï  €     ä  w v   stop the paddle from movinga  9 	  ÿÿÿÿÿÿÿÿ    	 Form_Loadp˜      X  Z      å – ù  o   ä  g     v 8   record the origional starting locations for everything   æ  ô  ƒ    û   ¤    û  ô  ”     9 	  ÿÿÿÿÿÿÿÿ
     Timer1_TimerpÄ     X       v    move the balli   û  ô  g £  û  ô    û   o £  û      v '   check to see if the ball's hit a walln   û  ô  û  £  ( ô Þ I Þ   ( ô  û  ÷  û  ô   g ù  g     û  ô  9 ô  9 £ â 6 .  9 ô  9 £  û  ô   g ù  g    8     v    move the paddle    w ä ñ I t  æ  ô  w £  æ  ô   8     v )   check to see if the paddle's hit a wall    æ  ô  9 ô  9 £ â I ê  9 ô  9 £  æ  ô    æ  ô  æ  £  ( ô Þ 6 *  ( ô  æ  ÷  æ  ô   8      G ô  9 ô  9 £ â I p  9 ô  9 £  G ô    G ô  G £  ( ô Þ 6 °  ( ô  G ÷  G ô   8     v #   move the computer player's paddlea   û  ô  G ô ã I   G ô å ú ÷  G ô    û  ô  G ô  G £ ß 6 R  G ô å ú £  G ô   8     v '   if the ball has hit player 1's paddlel   û   æ   æ  U£ â  û   æ   æ  U÷ Þ ¤ I æ  û  ô  û  £  æ  ô Þ  û  ô  æ  ô  æ  £ â ¤ I â  v ,  calculate the angle it's deflecting off at   æ  ô  æ  ä¥ î £ î  û  ô  û  ä¥ î £ î ÷ î ì š™™™™™á?ð  ^   g  ^ù £  g      æ   æ  U£  û    v   deflect the ball   o ù  o  8   8       v '   if the ball has hit player 2's paddle   û   û  U£  G Þ  û   G  G U£ â ¤ I |  û  ô  û  £  G ô Þ  û  ô  G ô  G £ â ¤ I x  v ,  calculate the angle it's deflecting off at   G ô  G ä¥ î £ î  û  ô  û  ä¥ î £ î ÷ î ì š™™™™™á?ð  ^   g  ^ù £  g      G  û  U÷  û    v   deflect the ball   o ù  o  8   8     v    see if someone's won   û   û  U£ ä ã I T   v 8  reset the ball and paddles to their origional location  ¤  û    ”  û  ô   ƒ  æ  ô   ƒ  G ô  å – ù  o  ä  g      û   e Uß 6     v 8  reset the ball and paddles to their origional location  ¤  û    ”  û  ô   ƒ  æ  ô   ƒ  G ô  å –  o  ä  g   8   9 	  ÿÿÿÿÿÿÿÿZ   ý