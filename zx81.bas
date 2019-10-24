`1
' RANDOMIZATION ROUTINE
PRINT "PRESS ENTER:"
INPUT A$
RAND 0
' VARIABLES
LET Z=0
LET Y=1
LET A=Z
LET T=Z
LET V=13
LET C=44
LET D=Z
LET E=Z
LET R=Z
LET X=2
LET I=Z
LET J=Z
DIM A(45)
LET G=100
LET H=4
LET K=512
LET L=32
LET U=16
LET N=255
LET O=99
LET F=65535
LET Q=6
LET S=3841
LET B=4096
LET M=39
LET P=Y
LET W=Z
GOTO `2520
`315
CLS
PRINT "WELCOME TO THE BATTLE SYSTEM TEST."
PRINT
PRINT
`350
PRINT "TWO SOLDIERS APPROACH."
PRINT
PRINT "PRESS A KEY TO CONTINUE."
SLOW
LET A$=INKEY$
IF A$="" THEN GOTO `870
FAST
`370
' MAIN LOOP
IF A((V*A)+Y)=Z THEN GOTO `420
PRINT "PRESS A KEY TO CONTINUE."
SLOW
`383
LET A$=INKEY$
IF A$="" THEN GOTO `383
FAST
CLS
IF A=Z THEN GOTO `780
LET T=Z
GOTO `1190
`420
' ITERATE ATTACKER
IF A(Y)=Z THEN GOTO `2410
LET A=A+Y
IF A<>Z THEN LET T=Z
IF A((V*A)+Y)=Z THEN LET A=A+Y
IF A>X THEN LET A=Z
LET D=Z
IF A((V*Y)+Y)=Z THEN LET D=D+Y
IF A((V*X)+Y)=Z THEN LET D=D+Y
IF D=X THEN GOTO `530
GOTO `370
`530
' NEW GAME
PRINT "YOU ARE VICTORIOUS."
PRINT
LET W=W+Y
LET A=Z
LET T=Z
IF W>=10 THEN GOSUB `690
FOR I=Y TO X
LET A((I*V)+Y)=A((I*V+Y)+Y)
NEXT I
GOSUB `2090
IF R<12.5 THEN PRINT "YOU OBTAINED A POTION."
IF R<12.5 THEN PRINT
IF R<12.5 THEN LET P=P+Y
IF P>O THEN LET P=O
GOTO `350
`690
' INCREASE DIFFICULTY
GOSUB `2090
IF R>=25 THEN RETURN
FOR I=V TO M-Y
LET A(I+Y)=A(I+Y)*1.5
NEXT I
PRINT "THE ENEMIES HAVE POWERED UP."
PRINT
RETURN
`780
' MENU LOOP
PRINT
CLS
PRINT A(Y);"/";A(X);" HP"
PRINT A(3);"/";A(H);" MP"
PRINT
PRINT "(A) ATTACK"
PRINT "(M) MAGIC"
PRINT "(I) ITEMS"
PRINT "(Q) QUIT"
SLOW
`870
LET A$=INKEY$
IF A$="" THEN GOTO `870
FAST
IF A$="A" OR A$="A" THEN GOTO `930
IF A$="M" OR A$="M" THEN GOTO `990
IF A$="I" OR A$="I" THEN GOTO `1100
IF A$="Q" OR A$="Q" THEN GOTO `2480
GOTO `780
`930
' ATTACK INPUT ROUTINE
GOSUB `2120
IF C=Z THEN GOTO `780
IF C>X THEN GOTO `930
IF C<Z THEN GOTO `930
GOTO `1190
`990
' MAGIC INPUT ROUTINE
PRINT
PRINT "(1) LIGHTNING - ";A(M+Y);" MP"
PRINT "(2) CURE - ";A(M+H);" MP"
PRINT "WHICH SPELL DO YOU WANT TO CAST?"
PRINT "TYPE (0) TO GO BACK."
SLOW
`1050
LET A$=INKEY$
IF A$="" THEN GOTO `1050
LET C=VAL A$
FAST
CLS
IF C=Y THEN GOTO `1630
IF C=X THEN GOTO `1850
IF C=Z THEN GOTO `780
GOTO `990
`1100
' ITEM ROUTINE
PRINT
PRINT "(1) POTION - ";P
PRINT "WHICH ITEM DO YOU WISH TO USE?"
PRINT "TYPE (0) TO GO BACK."
SLOW
`1150
LET A$=INKEY$
IF A$="" THEN GOTO `1150
LET C=VAL A$
FAST
CLS
IF C=Y THEN GOTO `1980
IF C=Z THEN GOTO `780
GOTO `1100
`1190
' ATTACK ROUTINE
LET C=((A((A*V+10)+Y)/H)+(A((A*V+5)+Y)))+A((A*V+7)+Y)-A((T*V+7)+Y)
GOSUB `2090
LET I=A((A*V+11)+Y)
GOSUB `2270
IF D>R THEN LET C=N
LET I=A((T*V+11)+Y)
GOSUB `2270
IF D>R THEN LET C=N
IF R<C THEN GOTO `1330
IF T>Z THEN PRINT "YOUR ATTACK MISSED."
IF T=Z THEN PRINT "SOLDIER ";A;" MISSED YOU."
PRINT
GOTO `420
`1330
' PHYSICAL DAMAGE ROUTINE
LET D=A((A*V+H)+Y)
LET E=A((A*V+12)+Y)
LET C=D+((D+E)/L)*((D*E)/L)
LET D=((U*(K-A((T*V+Q)+Y)))*C)/(U*K)
LET C=(A((A*V+11)+Y)+A((A*V+12)+Y)-A((T*V+12)+Y))/H
GOSUB `2300
IF R<=C THEN GOTO `1420
GOTO `1440
`1420
' CRITICAL HIT DAMAGE
LET TE=D*X
LET D=INT TE
`1440
' RANDOM DAMAGE VARIATION
LET TE=D*(S+(RND*N))/B
LET D=INT TE
IF D=Z THEN LET D=Y
LET A((T*V)+Y)=A((T*V)+Y)-D
IF A((T*V)+Y)<=Z THEN LET A((T*V)+Y)=Z
IF A<>Z THEN GOTO `1560
PRINT "YOU HIT SOLDIER ";T;" FOR ";D;" HP."
PRINT
IF A((T*V)+Y)<>Z THEN GOTO `1550
PRINT "YOU DEFEATED SOLDIER ";T
PRINT
`1550
GOTO `420
`1560
IF D>Z THEN GOSUB `2330
PRINT "SOLDIER ";A;" HIT YOU FOR ";D;" HP."
PRINT
IF A(Y)<>Z THEN GOTO `1620
PRINT "YOU DIED."
PRINT
`1620
GOTO `420
`1630
' LIGHTNING ROUTINE
LET E=Z
LET D=Z
IF A(M+Y)>A(3) THEN GOTO `1810
LET A(3)=A(3)-A(M+Y)
GOSUB `2120
IF C=Z THEN GOTO `990
IF C>X THEN GOTO `1630
IF C<Z THEN GOTO `1630
LET C=A(M+3)+A((A*V+12)+Y)-((A((T*V+12)+Y))/X)-Y
GOSUB `2090
IF R<C THEN GOSUB `2380
IF R>C THEN GOTO `1770
LET D=(A(M+X)*(K-A((T*V+9)+Y))*C)/(U*K)
`1770
IF D>Z THEN GOTO `1440
PRINT "YOUR SPELL MISSED."
PRINT
GOTO `420
`1810
' NOT ENOUGH MP ROUTINE
PRINT
PRINT "YOU DON,T HAVE ENOUGH MP TO CAST THAT SPELL."
GOTO `990
`1850
' CURE ROUTINE
LET E=3
IF A(M+H)>A(3) THEN GOTO `1810
LET A(3)=A(3)-A(M+H)
GOSUB `2380
LET D=C+22*A(M+Y+E+Y)
LET TE=D*(S+(RND*N))/B
LET D=INT TE
PRINT
PRINT "YOU HAVE BEEN HEALED FOR ";D;" HP."
PRINT
LET A(Y)=A(Y)+D
IF A(Y)>A(X) THEN LET A(Y)=A(X)
GOTO `420
`1980
' ITEM ROUTINE
IF P=Z THEN PRINT
IF P=Z THEN PRINT "YOU HAVE NO POTIONS."
IF P=Z THEN GOTO `780
LET A(Y)=A(Y)+G
IF A(Y)>A(X) THEN LET A(Y)=A(X)
LET P=P-Y
PRINT
PRINT "YOU HAVE BEEN HEALED FOR 100 HP."
PRINT
GOTO `420
`2090
' RANDOM 100 ROUTINE
LET TE=RND*G
LET R=INT TE
RETURN
`2120
' SHARED INPUT ROUTINE
PRINT
FOR I=Y TO X
IF A((V*I)+Y)>Z THEN PRINT "(";I;")";" SOLDIER ";I
NEXT I
PRINT "WHICH ENEMY DO YOU WANT TO TARGET?"
PRINT "(0) TO GO BACK."
SLOW
`2190
LET A$=INKEY$
IF A$="" THEN GOTO `2190
LET C=VAL A$
FAST
CLS
IF C=Z THEN RETURN
LET E=Z
IF A((V*C)+Y)<=Z THEN LET E=N
IF E=N THEN GOTO `2120
LET T=C
PRINT
RETURN
`2270
' LUCKY HIT ROUTINE
LET D=I/H
RETURN
`2300
' RANDOM 65535 ROUTINE
LET R=((RND*F)*O/F)+Y
RETURN
`2330
' DING ROUTINE
FOR J=Z TO G
NEXT J
' DING.
RETURN
`2380
' MAGIC DAMAGE ROUTINE
LET C=Q*(A(9)+A(V))
RETURN
`2410
' GAME OVER ROUTINE
PRINT
PRINT "GAME OVER"
PRINT
FOR I=Z TO Y
GOSUB `2330
NEXT I
`2480
PRINT
PRINT "YOU WON ";W;" TIMES."
PRINT
STOP
`2520
LET D$="314,314,54,54,38,96,24,1,21,17,6,14,6,30,30,0,0,6,1,4,1,1,1,50,4,2,30,30,0,0,6,1,4,1,1,1,50,4,2,4,8,100,5,5,255,"
LET C=Z
LET E=Y
LET I=45
`2550
LET T$=""
`2560
LET C=C+Y
IF D$(C)="," THEN GOTO `2600
LET T$=T$+D$(C)
GOTO `2560
`2600
LET A(E)=VAL T$
LET E=E+Y
IF E>I THEN GOTO `315
GOTO `2550
SAVE "BATLTEST"
GOTO `1
#data
#enddata
