' RANDOMIZATION ROUTINE
RANDOM
' CLEAR SCREEN ROUTINE
CLS
' VARIABLES
Z=0
Y=1
A=Z
T=Z
V=13
C=44
D=Z
E=Z
R=Z
X=2
I=Z
J=Z
DIM A(C)
G=100
H=4
K=512
L=32
U=16
N=255
O=99
F=65535
Q=6
S=3841
B=4096
M=39
P=Y
W=Z
FOR I=Z TO C
READ A(I)
NEXT I
PRINT "WELCOME TO THE BATTLE SYSTEM TEST!"
PRINT
PRINT
_350:
PRINT "TWO SOLDIERS APPROACH."
PRINT
_370:
' MAIN LOOP
IF A(V*A)=Z THEN _420
IF A=Z THEN _780
T=Z
GOTO _1190
_420:
' ITERATE ATTACKER
IF A(Z)=Z THEN _2410
A=A+Y
IF A<>Z THEN T=Z
IF A(V*A)=Z THEN A=A+Y
IF A>X THEN A=Z
D=Z
IF A(V*Y)=Z THEN D=D+Y
IF A(V*X)=Z THEN D=D+Y
IF D=X THEN _530
GOTO _370
_530:
' NEW GAME
PRINT "YOU ARE VICTORIOUS."
PRINT
W=W+Y
A=Z
T=Z
IF W>=10 THEN GOSUB _690
FOR I=Y TO X
A(I*V)=A(I*V+Y)
NEXT I
GOSUB _2090
IF R<12.5 THEN PRINT "YOU OBTAINED A POTION!"
IF R<12.5 THEN PRINT
IF R<12.5 THEN P=P+Y
IF P>O THEN P=O
GOTO _350
_690:
' INCREASE DIFFICULTY
GOSUB _2090
IF R>=25 THEN RETURN
FOR I=V TO M-Y
A(I)=A(I)*1.5
NEXT I
PRINT "THE ENEMIES HAVE POWERED UP!"
PRINT
RETURN
_780:
' MENU LOOP
PRINT
PRINT A(Z);"/";A(Y);"HP"
PRINT A(X);"/";A(3);"MP"
PRINT
PRINT "(A) ATTACK"
PRINT "(M) MAGIC"
PRINT "(I) ITEMS"
PRINT "(Q) QUIT"
_870:
A$=INKEY$
IF A$="" THEN _870
IF A$="A" OR A$="a" THEN _930
IF A$="M" OR A$="m" THEN _990
IF A$="I" OR A$="i" THEN _1100
IF A$="Q" OR A$="q" THEN _2480
GOTO _780
_930:
' ATTACK INPUT ROUTINE
GOSUB _2120
IF C=Z THEN _780
IF C>X THEN _930
IF C<Z THEN _930
GOTO _1190
_990:
' MAGIC INPUT ROUTINE
PRINT
PRINT "(1) LIGHTNING -";A(M);"MP"
PRINT "(2) CURE -";A(M+3);"MP"
PRINT "WHICH SPELL DO YOU WANT TO CAST?"
PRINT "TYPE (0) TO GO BACK."
_1050:
A$=INKEY$
IF A$="" THEN _1050
C=VAL(A$)
IF C=Y THEN _1630
IF C=X THEN _1850
IF C=Z THEN _780
GOTO _990
_1100:
' ITEM ROUTINE
PRINT
PRINT "(1) POTION -";P
PRINT "WHICH ITEM DO YOU WISH TO USE?"
PRINT "TYPE (0) TO GO BACK."
_1150:
A$=INKEY$
IF A$="" THEN _1150
C=VAL(A$)
IF C=Y THEN _1980
IF C=Z THEN _780
GOTO _1100
_1190:
' ATTACK ROUTINE
C=((A(A*V+10)/H)+(A(A*V+5)))+A(A*V+7)-A(T*V+7)
GOSUB _2090
I=A(A*V+11)
GOSUB _2270
IF D>R THEN C=N
I=A(T*V+11)
GOSUB _2270
IF D>R THEN C=N
IF R<C THEN _1330
IF T>Z THEN PRINT "YOUR ATTACK MISSED!"
IF T=Z THEN PRINT "SOLDIER";A;"MISSED YOU!"
PRINT
GOTO _420
_1330:
' PHYSICAL DAMAGE ROUTINE
D=A(A*V+H)
E=A(A*V+12)
C=D+((D+E)/L)*((D*E)/L)
D=((U*(K-A(T*V+Q)))*C)/(U*K)
C=(A(A*V+11)+A(A*V+12)-A(T*V+12))/H
GOSUB _2300
IF R<=C THEN _1420
GOTO _1440
_1420:
' CRITICAL HIT DAMAGE
D=INT(D*X)
_1440:
' RANDOM DAMAGE VARIATION
D=INT(D*(S+(RND(Z)*N))/B)
IF D=Z THEN D=Y
A(T*V)=A(T*V)-D
IF A(T*V)<=Z THEN A(T*V)=Z
IF A<>Z THEN _1560
PRINT "YOU HIT SOLDIER";T;"FOR";D;"HP."
PRINT
IF A(T*V)<>Z THEN _1550
PRINT "YOU DEFEATED SOLDIER";T
PRINT
_1550:
GOTO _420
_1560:
IF D>Z THEN GOSUB _2330
PRINT "SOLDIER";A;"HIT YOU FOR";D;"HP."
PRINT
IF A(Z)<>Z THEN _1620
PRINT "YOU DIED."
PRINT
_1620:
GOTO _420
_1630:
' LIGHTNING ROUTINE
E=Z
D=Z
IF A(M)>A(X) THEN _1810
A(X)=A(X)-A(M)
GOSUB _2120
IF C=Z THEN _990
IF C>X THEN _1630
IF C<Z THEN _1630
C=A(M+X)+A(A*V+12)-((A(T*V+12))/X)-Y
GOSUB _2090
IF R<C THEN GOSUB _2380
IF R>C THEN _1770
D=(A(M+Y)*(K-A(T*V+9))*C)/(U*K)
_1770:
IF D>Z THEN _1440
PRINT "YOUR SPELL MISSED."
PRINT
GOTO _420
_1810:
' NOT ENOUGH MP ROUTINE
PRINT
PRINT "YOU DON'T HAVE ENOUGH MP TO CAST THAT SPELL."
GOTO _990
_1850:
' CURE ROUTINE
E=3
IF A(M+3)>A(X) THEN _1810
A(X)=A(X)-A(M+3)
GOSUB _2380
D=C+22*A(M+Y+E)
D=INT(D*(S+(RND(Z)*N))/B)
PRINT
PRINT "YOU HAVE BEEN HEALED FOR";D;"HP."
PRINT
A(Z)=A(Z)+D
IF A(Z)>A(Y) THEN A(Z)=A(Y)
GOTO _420
_1980:
' ITEM ROUTINE
IF P=Z THEN PRINT
IF P=Z THEN PRINT "YOU HAVE NO POTIONS."
IF P=Z THEN _780
A(Z)=A(Z)+G
IF A(Z)>A(Y) THEN A(Z)=A(Y)
P=P-Y
PRINT
PRINT "YOU HAVE BEEN HEALED FOR 100 HP."
PRINT
GOTO _420
_2090:
' RANDOM 100 ROUTINE
R=INT(RND(Z)*G)
RETURN
_2120:
' SHARED INPUT ROUTINE
PRINT
FOR I=Y TO X
IF A(V*I)>Z THEN PRINT "(";I;")";" SOLDIER";I
NEXT I
PRINT "WHICH ENEMY DO YOU WANT TO TARGET?"
PRINT "(0) TO GO BACK."
_2190:
A$=INKEY$
IF A$="" THEN _2190
C=VAL(A$)
IF C=Z THEN RETURN
E=Z
IF A(V*C)<=Z THEN E=N
IF E=N THEN _2120
T=C
PRINT
RETURN
_2270:
' LUCKY HIT ROUTINE
D=I/H
RETURN
_2300:
' RANDOM 65535 ROUTINE
R=((RND(Z)*F)*O/F)+Y
RETURN
_2330:
' DING ROUTINE
FOR J=Z TO G
NEXT J
' DING!
RETURN
_2380:
' MAGIC DAMAGE ROUTINE
C=Q*(A(8)+A(12))
RETURN
_2410:
' GAME OVER ROUTINE
PRINT
PRINT "GAME OVER"
PRINT
FOR I=Z TO Y
GOSUB _2330
NEXT I
_2480:
PRINT
PRINT "YOU WON";W;"TIMES."
PRINT
END
#data
314
314
54
54
38
96
24
1
21
17
6
14
6
30
30
0
0
6
1
4
1
1
1
50
4
2
30
30
0
0
6
1
4
1
1
1
50
4
2
4
8
100
5
5
255
#enddata
