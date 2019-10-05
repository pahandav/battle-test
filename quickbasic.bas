' RANDOMIZATION ROUTINE
RANDOMIZE TIMER
' CLEAR SCREEN ROUTINE
CLS 0
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
@350
PRINT "TWO SOLDIERS APPROACH."
PRINT
@370
' MAIN LOOP
IF A(V*A)=Z THEN @420
IF A=Z THEN @780
T=Z
GOTO @1190
@420
' ITERATE ATTACKER
IF A(Z)=Z THEN @2410
A=A+Y
IF A<>Z THEN T=Z
IF A(V*A)=Z THEN A=A+Y
IF A>X THEN A=Z
D=Z
IF A(V*Y)=Z THEN D=D+Y
IF A(V*X)=Z THEN D=D+Y
IF D=X THEN @530
GOTO @370
@530
' NEW GAME
PRINT "YOU ARE VICTORIOUS."
PRINT
W=W+Y
A=Z
T=Z
IF W>=10 THEN GOSUB @690
FOR I=Y TO X
A(I*V)=A(I*V+Y)
NEXT I
GOSUB @2090
IF R<12.5 THEN PRINT "YOU OBTAINED A POTION!"
IF R<12.5 THEN PRINT
IF R<12.5 THEN P=P+Y
IF P>O THEN P=O
GOTO @350
@690
' INCREASE DIFFICULTY
GOSUB @2090
IF R>=25 THEN RETURN
FOR I=V TO M-Y
A(I)=A(I)*1.5
NEXT I
PRINT "THE ENEMIES HAVE POWERED UP!"
PRINT
RETURN
@780
' MENU LOOP
PRINT
PRINT A(Z);"/";A(Y);"HP"
PRINT A(X);"/";A(3);"MP"
PRINT
PRINT "(A) ATTACK"
PRINT "(M) MAGIC"
PRINT "(I) ITEMS"
PRINT "(Q) QUIT"
@870
A$=INKEY$
IF A$="" THEN @870
IF A$="A" OR A$="a" THEN @930
IF A$="M" OR A$="m" THEN @990
IF A$="I" OR A$="i" THEN @1100
IF A$="Q" OR A$="q" THEN @2480
GOTO @780
@930
' ATTACK INPUT ROUTINE
GOSUB @2120
IF C=Z THEN @780
IF C>X THEN @930
IF C<Z THEN @930
GOTO @1190
@990
' MAGIC INPUT ROUTINE
PRINT
PRINT "(1) LIGHTNING -";A(M);"MP"
PRINT "(2) CURE -";A(M+3);"MP"
PRINT "WHICH SPELL DO YOU WANT TO CAST?"
PRINT "TYPE (0) TO GO BACK."
@1050
A$=INKEY$
IF A$="" THEN @1050
C=VAL(A$)
IF C=Y THEN @1630
IF C=X THEN @1850
IF C=Z THEN @780
GOTO @990
@1100
' ITEM ROUTINE
PRINT
PRINT "(1) POTION -";P
PRINT "WHICH ITEM DO YOU WISH TO USE?"
PRINT "TYPE (0) TO GO BACK."
@1150
A$=INKEY$
IF A$="" THEN @1150
C=VAL(A$)
IF C=Y THEN @1980
IF C=Z THEN @780
GOTO @1100
@1190
' ATTACK ROUTINE
C=((A(A*V+10)/H)+(A(A*V+5)))+A(A*V+7)-A(T*V+7)
GOSUB @2090
I=A(A*V+11)
GOSUB @2270
IF D>R THEN C=N
I=A(T*V+11)
GOSUB @2270
IF D>R THEN C=N
IF R<C THEN @1330
IF T>Z THEN PRINT "YOUR ATTACK MISSED!"
IF T=Z THEN PRINT "SOLDIER";A;"MISSED YOU!"
PRINT
GOTO @420
@1330
' PHYSICAL DAMAGE ROUTINE
D=A(A*V+H)
E=A(A*V+12)
C=D+((D+E)/L)*((D*E)/L)
D=((U*(K-A(T*V+Q)))*C)/(U*K)
C=(A(A*V+11)+A(A*V+12)-A(T*V+12))/H
GOSUB @2300
IF R<=C THEN @1420
GOTO @1440
@1420
' CRITICAL HIT DAMAGE
D=INT(D*X)
@1440
' RANDOM DAMAGE VARIATION
D=INT(D*(S+(RND(Y)*N))/B)
IF D=Z THEN D=Y
A(T*V)=A(T*V)-D
IF A(T*V)<=Z THEN A(T*V)=Z
IF A<>Z THEN @1560
PRINT "YOU HIT SOLDIER";T;"FOR";D;"HP."
PRINT
IF A(T*V)<>Z THEN @1550
PRINT "YOU DEFEATED SOLDIER";T
PRINT
@1550
GOTO @420
@1560
IF D>Z THEN GOSUB @2330
PRINT "SOLDIER";A;"HIT YOU FOR";D;"HP."
PRINT
IF A(Z)<>Z THEN @1620
PRINT "YOU DIED."
PRINT
@1620
GOTO @420
@1630
' LIGHTNING ROUTINE
E=Z
D=Z
IF A(M)>A(X) THEN @1810
A(X)=A(X)-A(M)
GOSUB @2120
IF C=Z THEN @990
IF C>X THEN @1630
IF C<Z THEN @1630
C=A(M+X)+A(A*V+12)-((A(T*V+12))/X)-Y
GOSUB @2090
IF R<C THEN GOSUB @2380
IF R>C THEN @1770
D=(A(M+Y)*(K-A(T*V+9))*C)/(U*K)
@1770
IF D>Z THEN @1440
PRINT "YOUR SPELL MISSED."
PRINT
GOTO @420
@1810
' NOT ENOUGH MP ROUTINE
PRINT
PRINT "YOU DON'T HAVE ENOUGH MP TO CAST THAT SPELL."
GOTO @990
@1850
' CURE ROUTINE
E=3
IF A(M+3)>A(X) THEN @1810
A(X)=A(X)-A(M+3)
GOSUB @2380
D=C+22*A(M+Y+E)
D=INT(D*(S+(RND(Y)*N))/B)
PRINT
PRINT "YOU HAVE BEEN HEALED FOR";D;"HP."
PRINT
A(Z)=A(Z)+D
IF A(Z)>A(Y) THEN A(Z)=A(Y)
GOTO @420
@1980
' ITEM ROUTINE
IF P=Z THEN PRINT
IF P=Z THEN PRINT "YOU HAVE NO POTIONS."
IF P=Z THEN @780
A(Z)=A(Z)+G
IF A(Z)>A(Y) THEN A(Z)=A(Y)
P=P-Y
PRINT
PRINT "YOU HAVE BEEN HEALED FOR 100 HP."
PRINT
GOTO @420
@2090
' RANDOM 100 ROUTINE
R=INT(RND(Y)*G)
RETURN
@2120
' SHARED INPUT ROUTINE
PRINT
FOR I=Y TO X
IF A(V*I)>Z THEN PRINT "(";I;")";" SOLDIER";I
NEXT I
PRINT "WHICH ENEMY DO YOU WANT TO TARGET?"
PRINT "(0) TO GO BACK."
@2190
A$=INKEY$
IF A$="" THEN @2190
C=VAL(A$)
IF C=Z THEN RETURN
E=Z
IF A(V*C)<=Z THEN E=N
IF E=N THEN @2120
T=C
PRINT
RETURN
@2270
' LUCKY HIT ROUTINE
D=I/H
RETURN
@2300
' RANDOM 65535 ROUTINE
R=((RND(Y)*F)*O/F)+Y
RETURN
@2330
' DING ROUTINE
FOR J=Z TO G
NEXT J
SOUND 1400,6
RETURN
@2380
' MAGIC DAMAGE ROUTINE
C=Q*(A(8)+A(12))
RETURN
@2410
' GAME OVER ROUTINE
PRINT
PRINT "GAME OVER"
PRINT
FOR I=Z TO Y
GOSUB @2330
NEXT I
@2480
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
