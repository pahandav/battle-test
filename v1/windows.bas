1 REM RANDOMIZATION ROUTINE
2 RANDOMIZE TIMER
3 REM CLEAR SCREEN ROUTINE
4 CLS 0
10 REM VARIABLES
20 Z=0
30 Y=1
40 A=Z
50 T=Z
60 V=13
70 C=44
80 D=Z
90 E=Z
100 R=Z
110 X=2
120 I=Z
130 J=Z
140 DIM AR(C)
150 G=100
160 H=4
170 K=512
180 L=32
190 U=16
200 N=255
210 O=99
220 F=65535
230 Q=6
240 S=3841
250 B=4096
260 M=39
270 P=Y
280 W=Z
290 FOR I=Z TO C
300 READ AR(I)
310 NEXT I
320 PRINT "WELCOME TO THE BATTLE SYSTEM TEST!"
330 PRINT
340 PRINT
350 PRINT "TWO SOLDIERS APPROACH."
360 PRINT
370 REM MAIN LOOP
380 IF AR(V*A)=Z THEN 420
390 IF A=Z THEN 780
400 T=Z
410 GOTO 1190
420 REM ITERATE ATTACKER
430 IF AR(Z)=Z THEN 2410
440 A=A+Y
450 IF A<>Z THEN T=Z
460 IF AR(V*A)=Z THEN A=A+Y
470 IF A>X THEN A=Z
480 D=Z
490 IF AR(V*Y)=Z THEN D=D+Y
500 IF AR(V*X)=Z THEN D=D+Y
510 IF D=X THEN 530
520 GOTO 370
530 REM NEW GAME
540 PRINT "YOU ARE VICTORIOUS."
550 PRINT
560 W=W+Y
570 A=Z
580 T=Z
590 IF W>=10 THEN GOSUB 690
600 FOR I=Y TO X
610 AR(I*V)=AR(I*V+Y)
620 NEXT I
630 GOSUB 2090
640 IF R<12.5 THEN PRINT "YOU OBTAINED A POTION!"
650 IF R<12.5 THEN PRINT
660 IF R<12.5 THEN P=P+Y
670 IF P>O THEN P=O
680 GOTO 350
690 REM INCREASE DIFFICULTY
700 GOSUB 2090
710 IF R>=25 THEN RETURN
720 FOR I=V TO M-Y
730 AR(I)=AR(I)*1.5
740 NEXT I
750 PRINT "THE ENEMIES HAVE POWERED UP!"
760 PRINT
770 RETURN
780 REM MENU LOOP
790 PRINT
800 PRINT AR(Z);"/";AR(Y);" HP"
810 PRINT AR(X);"/";AR(3);" MP"
820 PRINT
830 PRINT "(A) ATTACK"
840 PRINT "(M) MAGIC"
850 PRINT "(I) ITEMS"
860 PRINT "(Q) QUIT"
870 A$=INKEY$
871 IF A$="" THEN 870
880 IF A$="A" OR A$="a" THEN 930
890 IF A$="M" OR A$="m" THEN 990
900 IF A$="I" OR A$="i" THEN 1100
910 IF A$="Q" OR A$="q" THEN 2480
920 GOTO 780
930 REM ATTACK INPUT ROUTINE
940 GOSUB 2120
950 IF C=Z THEN 780
960 IF C>X THEN 930
970 IF C<Z THEN 930
980 GOTO 1190
990 REM MAGIC INPUT ROUTINE
1000 PRINT
1010 PRINT "(1) LIGHTNING - ";AR(M);" MP"
1020 PRINT "(2) CURE - ";AR(M+3);" MP"
1030 PRINT "WHICH SPELL DO YOU WANT TO CAST?"
1040 PRINT "TYPE (0) TO GO BACK."
1050 A$=INKEY$
1051 IF A$="" THEN 1050
1052 C=VAL(A$)
1060 IF C=Y THEN 1630
1070 IF C=X THEN 1850
1080 IF C=Z THEN 780
1090 GOTO 990
1100 REM ITEM ROUTINE
1110 PRINT
1120 PRINT "(1) POTION - ";P
1130 PRINT "WHICH ITEM DO YOU WISH TO USE?"
1140 PRINT "TYPE (0) TO GO BACK."
1150 A$=INKEY$
1151 IF A$="" THEN 1150
1152 C=VAL(A$)
1160 IF C=Y THEN 1980
1170 IF C=Z THEN 780
1180 GOTO 1100
1190 REM ATTACK ROUTINE
1200 C=((AR(A*V+10)/H)+(AR(A*V+5)))+AR(A*V+7)-AR(T*V+7)
1210 GOSUB 2090
1220 I=AR(A*V+11)
1230 GOSUB 2270
1240 IF D>R THEN C=N
1250 I=AR(T*V+11)
1260 GOSUB 2270
1270 IF D>R THEN C=N
1280 IF R<C THEN 1330
1290 IF T>Z THEN PRINT "YOUR ATTACK MISSED!"
1300 IF T=Z THEN PRINT "SOLDIER ";A;" MISSED YOU!"
1310 PRINT
1320 GOTO 420
1330 REM PHYSICAL DAMAGE ROUTINE
1340 D=AR(A*V+H)
1350 E=AR(A*V+12)
1360 C=D+((D+E)/L)*((D*E)/L)
1370 D=((U*(K-AR(T*V+Q)))*C)/(U*K)
1380 C=(AR(A*V+11)+AR(A*V+12)-AR(T*V+12))/H
1390 GOSUB 2300
1400 IF R<=C THEN 1420
1410 GOTO 1440
1420 REM CRITICAL HIT DAMAGE
1430 D=INT(D*X)
1440 REM RANDOM DAMAGE VARIATION
1450 D=INT(D*(S+(RND(Y)*N))/B)
1460 IF D=Z THEN D=Y
1470 AR(T*V)=AR(T*V)-D
1480 IF AR(T*V)<=Z THEN AR(T*V)=Z
1490 IF A<>Z THEN 1560
1500 PRINT "YOU HIT SOLDIER ";T;" FOR ";D;" HP."
1510 PRINT
1520 IF AR(T*V)<>Z THEN 1550
1530 PRINT "YOU DEFEATED SOLDIER ";T
1540 PRINT
1550 GOTO 420
1560 IF D>Z THEN GOSUB 2330
1570 PRINT "SOLDIER ";A;" HIT YOU FOR ";D;" HP."
1580 PRINT
1590 IF AR(Z)<>Z THEN 1620
1600 PRINT "YOU DIED."
1610 PRINT
1620 GOTO 420
1630 REM LIGHTNING ROUTINE
1640 E=Z
1650 D=Z
1660 IF AR(M)>AR(X) THEN 1810
1670 AR(X)=AR(X)-AR(M)
1680 GOSUB 2120
1690 IF C=Z THEN 990
1700 IF C>X THEN 1630
1710 IF C<Z THEN 1630
1720 C=AR(M+X)+AR(A*V+12)-((AR(T*V+12))/X)-Y
1730 GOSUB 2090
1740 IF R<C THEN GOSUB 2380
1750 IF R>C THEN 1770
1760 D=(AR(M+Y)*(K-AR(T*V+9))*C)/(U*K)
1770 IF D>Z THEN 1440
1780 PRINT "YOUR SPELL MISSED."
1790 PRINT
1800 GOTO 420
1810 REM NOT ENOUGH MP ROUTINE
1820 PRINT
1830 PRINT "YOU DON'T HAVE ENOUGH MP TO CAST THAT SPELL."
1840 GOTO 990
1850 REM CURE ROUTINE
1860 E=3
1870 IF AR(M+3)>AR(X) THEN 1810
1880 AR(X)=AR(X)-AR(M+3)
1890 GOSUB 2380
1900 D=C+22*AR(M+Y+E)
1910 D=INT(D*(S+(RND(Y)*N))/B)
1920 PRINT
1930 PRINT "YOU HAVE BEEN HEALED FOR ";D;" HP."
1940 PRINT
1950 AR(Z)=AR(Z)+D
1960 IF AR(Z)>AR(Y) THEN AR(Z)=AR(Y)
1970 GOTO 420
1980 REM ITEM ROUTINE
1990 IF P=Z THEN PRINT
2000 IF P=Z THEN PRINT "YOU HAVE NO POTIONS."
2010 IF P=Z THEN 780
2020 AR(Z)=AR(Z)+G
2030 IF AR(Z)>AR(Y) THEN AR(Z)=AR(Y)
2040 P=P-Y
2050 PRINT
2060 PRINT "YOU HAVE BEEN HEALED FOR 100 HP."
2070 PRINT
2080 GOTO 420
2090 REM RANDOM 100 ROUTINE
2100 R=INT(RND(Y)*G)
2110 RETURN
2120 REM SHARED INPUT ROUTINE
2130 PRINT
2140 FOR I=Y TO X
2150 IF AR(V*I)>Z THEN PRINT "(";I;") ";" SOLDIER ";I
2160 NEXT I
2170 PRINT "WHICH ENEMY DO YOU WANT TO TARGET?"
2180 PRINT "(0) TO GO BACK."
2190 A$=INKEY$
2191 IF A$="" THEN 2190
2192 C=VAL(A$)
2200 IF C=Z THEN RETURN
2210 E=Z
2220 IF AR(V*C)<=Z THEN E=N
2230 IF E=N THEN 2120
2240 T=C
2250 PRINT
2260 RETURN
2270 REM LUCKY HIT ROUTINE
2280 D=I/H
2290 RETURN
2300 REM RANDOM 65535 ROUTINE
2310 R=((RND(Y)*F)*O/F)+Y
2320 RETURN
2330 REM DING ROUTINE
2340 FOR J=Z TO G
2350 NEXT J
2360 PRINT CHR$(7)
2370 RETURN
2380 REM MAGIC DAMAGE ROUTINE
2390 C=Q*(AR(8)+AR(12))
2400 RETURN
2410 REM GAME OVER ROUTINE
2420 PRINT
2430 PRINT "GAME OVER"
2440 PRINT
2450 FOR I=Z TO Y
2460 GOSUB 2330
2470 NEXT I
2480 PRINT
2490 PRINT "YOU WON ";W;" TIMES."
2500 PRINT
2505 INPUT "PRESS ANY KEY TO QUIT"; A$
2510 END
2520 DATA 314,314,54,54,38,96,24,1,21,17,6,14,6
2530 DATA 30,30,0,0,6,1,4,1,1,1,50,4,2
2540 DATA 30,30,0,0,6,1,4,1,1,1,50,4,2
2550 DATA 4,8,100,5,5,255
