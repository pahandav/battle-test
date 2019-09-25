1 rem randomization routine
2 print "press a key:"
3 get a$
4 if a$="" then 3
5 z=rnd(-ti)
6 rem clear screen routine
7 print chr$(147)
8 poke 59468,12
10 rem variables
20 z=0
30 y=1
40 a=z
50 t=z
60 v=13
70 c=44
80 d=z
90 e=z
100 r=z
110 x=2
120 i=z
130 j=z
140 dim a(c)
150 g=100
160 h=4
170 k=512
180 l=32
190 u=16
200 n=255
210 o=99
220 f=65535
230 q=6
240 s=3841
250 b=4096
260 m=39
270 p=y
280 w=z
290 for i=z to c
300 read a(i)
310 next i
320 print "welcome to the battle system test!"
330 print
340 print
350 print "two soldiers approach."
360 print
370 rem main loop
380 if a(v*a)=z then 420
390 if a=z then 780
400 t=z
410 goto 1190
420 rem iterate attacker
430 if a(z)=z then 2410
440 a=a+y
450 if a<>z then t=z
460 if a(v*a)=z then a=a+y
470 if a>x then a=z
480 d=z
490 if a(v*y)=z then d=d+y
500 if a(v*x)=z then d=d+y
510 if d=x then 530
520 goto 370
530 rem new game
540 print "you are victorious."
550 print
560 w=w+y
570 a=z
580 t=z
590 if w>=10 then gosub 690
600 for i=y to x
610 a(i*v)=a(i*v+y)
620 next i
630 gosub 2090
640 if r<12.5 then print "you obtained a potion!"
650 if r<12.5 then print
660 if r<12.5 then p=p+y
670 if p>o then p=o
680 goto 350
690 rem increase difficulty
700 gosub 2090
710 if r>=25 then return
720 for i=v to m-y
730 a(i)=a(i)*1.5
740 next i
750 print "the enemies have powered up!"
760 print
770 return
780 rem menu loop
790 print
800 print a(z);"/";a(y);"hp"
810 print a(x);"/";a(3);"mp"
820 print
830 print "(a) attack"
840 print "(m) magic"
850 print "(i) items"
860 print "(q) quit"
870 get a$
871 if a$="" then 870
880 if a$="a" or a$="a" then 930
890 if a$="m" or a$="m" then 990
900 if a$="i" or a$="i" then 1100
910 if a$="q" or a$="q" then 2480
920 goto 780
930 rem attack input routine
940 gosub 2120
950 if c=z then 780
960 if c>x then 930
970 if c<z then 930
980 goto 1190
990 rem magic input routine
1000 print
1010 print "(1) lightning -";a(m);"mp"
1020 print "(2) cure -";a(m+3);"mp"
1030 print "which spell do you want to cast?"
1040 print "type (0) to go back."
1050 get a$
1051 if a$="" then 1050
1052 c=val(a$)
1060 if c=y then 1630
1070 if c=x then 1850
1080 if c=z then 780
1090 goto 990
1100 rem item routine
1110 print
1120 print "(1) potion -";p
1130 print "which item do you wish to use?"
1140 print "type (0) to go back."
1150 get a$
1151 if a$="" then 1150
1152 c=val(a$)
1160 if c=y then 1980
1170 if c=z then 780
1180 goto 1100
1190 rem attack routine
1200 c=((a(a*v+10)/h)+(a(a*v+5)))+a(a*v+7)-a(t*v+7)
1210 gosub 2090
1220 i=a(a*v+11)
1230 gosub 2270
1240 if d>r then c=n
1250 i=a(t*v+11)
1260 gosub 2270
1270 if d>r then c=n
1280 if r<c then 1330
1290 if t>z then print "your attack missed!"
1300 if t=z then print "soldier";a;"missed you!"
1310 print
1320 goto 420
1330 rem physical damage routine
1340 d=a(a*v+h)
1350 e=a(a*v+12)
1360 c=d+((d+e)/l)*((d*e)/l)
1370 d=((u*(k-a(t*v+q)))*c)/(u*k)
1380 c=(a(a*v+11)+a(a*v+12)-a(t*v+12))/h
1390 gosub 2300
1400 if r<=c then 1420
1410 goto 1440
1420 rem critical hit damage
1430 d=int(d*x)
1440 rem random damage variation
1450 d=int(d*(s+(rnd(y)*n))/b)
1460 if d=z then d=y
1470 a(t*v)=a(t*v)-d
1480 if a(t*v)<=z then a(t*v)=z
1490 if a<>z then 1560
1500 print "you hit soldier";t;"for";d;"hp."
1510 print
1520 if a(t*v)<>z then 1550
1530 print "you defeated soldier";t
1540 print
1550 goto 420
1560 if d>z then gosub 2330
1570 print "soldier";a;"hit you for";d;"hp."
1580 print
1590 if a(z)<>z then 1620
1600 print "you died."
1610 print
1620 goto 420
1630 rem lightning routine
1640 e=z
1650 d=z
1660 if a(m)>a(x) then 1810
1670 a(x)=a(x)-a(m)
1680 gosub 2120
1690 if c=z then 990
1700 if c>x then 1630
1710 if c<z then 1630
1720 c=a(m+x)+a(a*v+12)-((a(t*v+12))/x)-y
1730 gosub 2090
1740 if r<c then gosub 2380
1750 if r>c then 1770
1760 d=(a(m+y)*(k-a(t*v+9))*c)/(u*k)
1770 if d>z then 1440
1780 print "your spell missed."
1790 print
1800 goto 420
1810 rem not enough mp routine
1820 print
1830 print "you don't have enough mp to cast that spell."
1840 goto 990
1850 rem cure routine
1860 e=3
1870 if a(m+3)>a(x) then 1810
1880 a(x)=a(x)-a(m+3)
1890 gosub 2380
1900 d=c+22*a(m+y+e)
1910 d=int(d*(s+(rnd(y)*n))/b)
1920 print
1930 print "you have been healed for";d;"hp."
1940 print
1950 a(z)=a(z)+d
1960 if a(z)>a(y) then a(z)=a(y)
1970 goto 420
1980 rem item routine
1990 if p=z then print
2000 if p=z then print "you have no potions."
2010 if p=z then 780
2020 a(z)=a(z)+g
2030 if a(z)>a(y) then a(z)=a(y)
2040 p=p-y
2050 print
2060 print "you have been healed for 100 hp."
2070 print
2080 goto 420
2090 rem random 100 routine
2100 r=int(rnd(y)*g)
2110 return
2120 rem shared input routine
2130 print
2140 for i=y to x
2150 if a(v*i)>z then print "(";i;")";" soldier";i
2160 next i
2170 print "which enemy do you want to target?"
2180 print "(0) to go back."
2190 get a$
2191 if a$="" then 2190
2192 c=val(a$)
2200 if c=z then return
2210 e=z
2220 if a(v*c)<=z then e=n
2230 if e=n then 2120
2240 t=c
2250 print
2260 return
2270 rem lucky hit routine
2280 d=i/h
2290 return
2300 rem random 65535 routine
2310 r=((rnd(y)*f)*o/f)+y
2320 return
2330 rem ding routine
2340 for j=z to g
2350 next j
2360 print chr$(7)
2370 return
2380 rem magic damage routine
2390 c=q*(a(8)+a(12))
2400 return
2410 rem game over routine
2420 print
2430 print "game over"
2440 print
2450 for i=z to y
2460 gosub 2330
2470 next i
2480 print
2490 print "you won";w;"times."
2500 print
2510 end
2520 data 314,314,54,54,38,96,24,1,21,17,6,14,6
2530 data 30,30,0,0,6,1,4,1,1,1,50,4,2
2540 data 30,30,0,0,6,1,4,1,1,1,50,4,2
2550 data 4,8,100,5,5,255
