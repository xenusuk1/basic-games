#lang "qb"
' Original Code from: Ahl - More Basic Computer Games
CLS: COLOR 12
PRINT TAB(37); "Seabat"
PRINT TAB(31); "Creative Computing"
PRINT TAB(29); "Morristown, New Jersey"
PRINT: PRINT: PRINT: COLOR 15
RANDOMIZE TIMER
REM
REM  PROGRAM BY VINCENT ERIKSON
REM   ORIGINALLY IN H.P. BASIC
REM   CONVERTED TO MICROSOFT BASIC BY S.N.
REM
REM  NOTE THE FOLLOWING ABOUT CONVERSIONS:
REM  1)  RESTORE <LINE NUMBER> MEANS TO SET THE DATA
REM      POINTER TO THE SPECIFIED LINE.  THIS IS ONLY
REM      PRESENT IN TRS-80 LEVEL II AND CP/M BASIC.
REM      FOR OTHERS, IMPROVISE BY USING A RESTORE, AND
REM      FOR...NEXT WITH READ STATEMENTS TO SKIP OVER
REM      THE DATA THAT SHOULD BE IGNORED.
REM
REM  2)  LOGICAL EXPRESSIONS ARE USED OFTEN.  A TRUE
REM      EXPRESSION EVALUATES AS A (-1) AND A FALSE EXPRESSION
REM      EVALUATES AS A (0).  THUS IF THE PROGRAM SAYS:
REM          X = (D<50)
REM      IT MEANS, LET X=0 IF D>=50, AND LET X=-1 IF D<50.
REM      AGAIN, IMPROVISE IF YOUR BASIC DOESN'T HAVE THIS
REM      (BUT ALL MICROSOFT BASICS DO.)
REM
REM   The real name of this program is, "Underwater Pie Lob"
REM *** PROGRAM FOLLOWS ***
REM ***

DIM SHARED A(20, 20), DM(9), B(5)
DIM SHARED I,J,X,Y,S,S1,S2,S3,S4,X1,X2 AS INTEGER



COLOR 10: PRINT "Do you want instructions (Y/N)";
INPUT A$: COLOR 15
IF UCASE$(LEFT$(A$, 1)) = "Y" THEN CALL SubInst

COLOR 10: PRINT "What is your name";
INPUT N$: COLOR 15
PRINT

REM *** SET UP AREA ***
FOR I = 1 TO 20
FOR J = 1 TO 20
	A(I, J) = 0
NEXT J
NEXT I

REM *** ISLAND 1***
RESTORE 6300
FOR X = 7 TO 13
FOR Y = 7 TO 12
	READ A(X, Y)
NEXT Y
NEXT X

REM *** SUB 2***
S1 = 10: S2 = 10
A(S1, S2) = 2

REM *** ENEMY SHIPS 3***
S = INT(RND(1) * 16) + 15
RESTORE 6090
FOR X = 1 TO (INT(RND(1) * 4) + 1) * 2 - 1
	READ D8, D9
NEXT X

FOR X = 1 TO S
	DO
		X1 = INT(RND(1) * 20) + 1
		X2 = INT(RND(1) * 20) + 1
	LOOP UNTIL A(X1, X2) = 0
	A(X1, X2) = 3
NEXT X

PRINT "You must destroy"; S; " enemy ships to win, "; N$; "."

REM *** HEADQUARTERS 4***
DO
	S3 = INT(RND(1) * 20) + 1
	S4 = INT(RND(1) * 20) + 1
LOOP UNTIL A(S3, S4) = 0
A(S3, S4) = 4

REM *** UNDERWATER MINES 5***
FOR X = 1 TO INT(RND(1) * 8) + 8
	DO
		X1 = INT(RND(1) * 20) + 1
		X2 = INT(RND(1) * 20) + 1
	LOOP UNTIL A(X1, X2) = 0
	A(X1, X2) = 5
NEXT X

REM *** SEA MONSTERS 6***
FOR X = 1 TO 4
	DO
		X1 = INT(RND(1) * 18) + 2
		X2 = INT(RND(1) * 18) + 2
	LOOP UNTIL A(X1, X2) = 0

	A(X1, X2) = 6
	RESTORE 6090
	FOR Y = 1 TO INT(RND(1) * 8) + 1
		READ M1, M2
	NEXT Y
NEXT X

REM *** SET STARTING VALUES ***
FOR I = 1 TO 9
	DM(I) = 0
NEXT I
DIM SHARED C,P,F,T,M,D,D2,W,V AS INTEGER
C = 30
P = 6000
F = 2500
T = 10
M = 3
D = 1000
D2 = 2

'Main Loop
880 REM *** COMMAND SECTION ***
PRINT: COLOR 10: PRINT "What are your orders (1-9), "; N$;
INPUT O: COLOR 15

SELECT CASE O
	CASE 0
	 GOTO 1040
	CASE 1
	 GOTO 1680
	CASE 2
	 GOTO 2220
	CASE 3
	 GOTO 2680
	CASE 4
	 GOTO 3250
	CAsE 5
	 GOTO 3410
	CASE 6
	 GOTO 3700
	CASE 7
	 GOTO 3880
	CASE 8
	 GOTO 4400
	CASE 9
	 GOTO 4660
CASE ELSE
'ON INT(O + 1) GOTO 1040, 1680, 2220, 2680, 3250, 3410, 3700, 3880, 4400, 4660
	PRINT "The commands are:"
	COLOR 11: PRINT "     #0: Navigation", , "#5: Status/damage report"
	PRINT "     #1: Sonar", , "#6: Headquarters"
	PRINT "     #2: Torpedo control", , "#7: Sabotage"
	PRINT "     #3: Polaris missile control", "#8: Power conversion"
	PRINT "     #4: Maneuvering", , "#9: Surrender": COLOR 15
END SELECT

1030 GOTO 880

FUNCTION SubStatusCheck (SC%, CR%) AS INTEGER
subStatusCheck = 1

IF DM(SC%) < 0 THEN
	subStatusCheck = 0
	SELECT CASE SC%
	CASE 1
		PRINT "Engines are under repair, "; N$; "."
	CASE 2
		PRINT "The sonar is under repair "; N$;"."
	CASE 3
		PRINT "Torpedo tubes are under repair, "; N$; "."
	END SELECT
END IF

IF C <= CR% THEN
	subStatusCheck = 0
	SELECT CASE SC%
	CASE 1
		PRINT "Not enough crew to man the engines, "; N$; "."
	CASE 2
		PRINT "Not enough crew to work sonar "; N$; "."
	CASE 3
		PRINT "Not enough crew to fire a torpedo, "; N$; "."
	END SELECT
END IF

END FUNCTION  'subStatusCheck

1040 REM *** #0: NAVIGATION ***
IF SubStatusCheck(1,8)=0 THEN 880
'1050 IF DM(1) >= 0 THEN 1080
'1060 PRINT "Engines are under repair, "; N$; "."
'1070 GOTO 880
'1080 IF C > 8 THEN 1110
'1090 PRINT "Not enough crew to man the engines, "; N$; "."
'1100 GOTO 880
1110 D1 = 1 - ((.23 + RND(1) / 10) * (-(D <= 50)))
1120 GOSUB 6080
1130 PRINT "Power available ="; P;: COLOR 10: PRINT " Power to use";
1140 INPUT P1: COLOR 15
1150 IF P1 < 0 OR P1 > P THEN 1130
1160 IF P1 <= 1000 THEN 1210
1170 IF RND(1) < .43 THEN 1210
1180 PRINT "Your atomic pile went supercriticial, "; N$; "!  Headquarters will warn all"
1190 PRINT "subs to stay from the radioactive area!"
1200 CALL SubLose
1210 X = S1
1220 Y = S2
1230 Q1 = 1
1240 FOR X2 = 1 TO INT(INT(P1 / 100 + .5) * D1 + .5)
1250 IF X + X1 > 0 AND X + X1 < 21 AND Y + Y1 > 0 AND Y + Y1 < 21 THEN 1280
1260 PRINT "You can't leave the area, "; N$; "!"
1270 GOTO 1340
1280 ON A(X + X1, Y + Y1) + 1 GOTO 1290, 1330, 1630, 1390, 1440, 1470, 1490
1290 X = X + X1
1300 Y = Y + Y1
1310 P = P - 100
1320 GOTO 1520
1330 PRINT "You almost ran aground, "; N$; "!"
1340 A(X, Y) = 2
1350 A(S1, S2) = 0
1360 S1 = X
1370 S2 = Y
1380 GOTO 4690
1390 IF D > 50 THEN 1290
1400 PRINT "You rammed a ship!  You're both sunk, "; N$; "!"
1410 S = S - 1
1420 IF S = 0 THEN CALL SubWin
1430 CALL SubLose
1440 IF D > 50 THEN 1290
1450 PRINT "You rammed your headquarters!  You're sunk!"
1460 CALL SubLose
1470 PRINT "You've been blown up by a mine, "; N$; "!"
1480 CALL SubLose
1490 IF RND(1) < .21 THEN 1630
1500 PRINT "You were eaten by a sea monster, "; N$; "!"
1510 CALL SubLose

1520 REM *** CHECK FOR NEARBY SEA MONSTERS ***
1530 FOR X3 = X - 2 TO X + 2
1540 FOR Y3 = Y - 2 TO Y + 2
1550 IF X3 < 1 OR X3 > 20 OR Y3 < 1 OR Y3 > 20 THEN 1610
1560 IF A(X, Y) <> 6 THEN 1610
1570 IF RND(1) < .25 THEN 1500
1580 IF Q1 = 0 THEN 1610
1590 PRINT "You just had a narrow escape with a sea monster, "; N$; "!"
1600 Q1 = 0
1610 NEXT Y3
1620 NEXT X3
1630 NEXT X2
1640 PRINT "Navigation complete.  Power left:"; P
CALL SubFuel
1650 GOTO 1340


1680 REM *** #1: SONAR ***
IF SubStatusCheck(2,5) = 0 THEN 880
'1690 IF DM(2) >= 0 THEN 1720
'1700 PRINT "The sonar is under repair, "; N$; "."
'1710 GOTO 880
'1720 IF C > 5 THEN 1750
'1730 PRINT "Not enough crew to work sonar, "; N$; "."
'1740 GOTO 880
1750 PRINT "Option number (0,1)";
1760 INPUT O
1770 ON INT(O + 1) GOTO 1790, 2010
1780 GOTO 1750



SUB SubFuel
IF P <= 0 THEN
	PRINT "The atomic pile has gone dead!  Sub sinks, crew suffocates."
	CALL SubLose
END IF
END SUB 'Fuel

SUB SubMap
PRINT
FOR X = 1 TO 20
FOR Y = 1 TO 20
CO = A(X,Y)
'land & sub always visible
IF D < 50 AND RND(1) < .23 AND A(X,Y) > 2 THEN CO = 8
IF D>50 AND RND(1) < .15 AND A(X,Y) > 2 THEN CO = 8

IF (X=1 OR X=20 OR Y=1 OR Y=20) AND CO=0 THEN CO=8
SELECT CASE CO
CASE 0
	PRINT "   ";
CASE 1
	COLOR 14: PRINT "***";
CASE 2
	COLOR 11: PRINT "(X)";
CASE 3
	COLOR 12 : PRINT "\S/";
CASE 4
	COLOR 12 : PRINT "!H!";
CASE 5
	COLOR 13 : PRINT " $ ";
CASE 6
	COLOR 12 : PRINT "-#-";

CASE 8
	COLOR 9: PRINT " . ";: COLOR 15
END SELECT
NEXT Y
PRINT
NEXT X
COLOR 15
END SUB 'Map

1790 REM *** PRINT OUT MAP ***

CALL SubMap
1980 P = P - 50
CALL SubFuel
1990 GOTO 880


2010 REM *** DIRECTIONAL INFORMATION ***
2020 FOR I = 1 TO 5
2022 B(I) = 0
2024 NEXT I
2030 PRINT "Direction   # of Ships     Distances": COLOR 11
2040 RESTORE 6090
2050 FOR X = 1 TO 8
2060 READ X1, Y1
2070 X3 = 0
2080 FOR X4 = 1 TO 20
2090 IF S1 + X1 * X4 < 1 OR S1 + X1 * X4 > 20 OR S2 + Y1 * X4 < 1 OR S2 + Y1 * X4 > 20 THEN 2140
2100 IF A(S1 + X1 * X4, S2 + Y1 * X4) <> 3 THEN 2130
2110 X3 = X3 + 1
2120 B(X3) = X4
2130 NEXT X4
2140 IF X3 = 0 THEN 2200
2150 PRINT "   "; X, X3,
2160 FOR X4 = 1 TO X3
2170 PRINT B(X4);
2180 NEXT X4
2190 PRINT
2200 NEXT X: COLOR 15
2210 GOTO 1980

2220 REM *** #2: TORPEDO CONTROL ***
IF SubStatusCheck(3,10) = 0 THEN 880
'2230 IF DM(3) >= 0 THEN 2260
'2240 PRINT "Torpedo tubes are under repair, "; N$; "."
'2250 GOTO 880
'2260 IF C >= 10 THEN 2290
'2270 PRINT "Not enough crew to fire a torpedo, "; N$; "."
'2280 GOTO 880
2290 IF T THEN 2320
2300 PRINT "No torpedos left, "; N$; "."
2310 GOTO 880
2320 IF D < 2000 THEN 2360
2330 IF RND(1) > .5 THEN 2360
2340 PRINT "Pressure implodes the sub upon firing... you're crushed!"
2350 CALL SubLose
2360 GOSUB 6080
2370 X = S1
2380 Y = S2
2390 FOR X2 = 1 TO INT(7 + 5 * (-(D > 50)) - RND(1) * 4 + .5)
2400 IF X + X1 > 0 AND X + X1 < 21 AND Y + Y1 > 0 AND Y + Y1 < 21 THEN 2460
2410 PRINT "Torpedo out of sonar range... ineffectual, "; N$; "."
2420 T = T - 1
2430 P = P - 150
'2440 IF P > 0 THEN 4690
'2450 GOTO 1660
CALL SubFuel
GOTO 4690

2460 ON A(X + X1, Y + Y1) + 1 GOTO 2470, 2510, 2650, 2540, 2580, 2610, 2630
2470 X = X + X1
2480 Y = Y + Y1
2490 PRINT "..!..";
CALL SubWait(1)
BEEP
2500 GOTO 2650
2510 PRINT "You took out some island, "; N$; "!"
2520 A(X + X1, Y + Y1) = 0
2530 GOTO 2420
2540 PRINT "Ouch!  You got one, "; N$; "!"
2550 S = S - 1
2560 IF S <> 0 THEN 2520
2570 CALL SubWin
2580 PRINT "You blew up your headquarters, "; N$; "!"
2590 S3 = 0: S4 = 0: D2 = 0
2600 GOTO 2520
2610 PRINT "Blam!  Shot wasted on a mine, "; N$; "!"
2620 GOTO 2520
2630 PRINT "A sea monster had a torpedo for lunch, "; N$; "!"
2640 GOTO 2420
2650 NEXT X2
2660 PRINT "Dud."
2670 GOTO 2420

2680 REM *** #3: POLARIS MISSILE CONTROL ***
2690 IF DM(4) >= 0 THEN 2720
2700 PRINT "Missile silos are under repair, "; N$; "."
2710 GOTO 880
2720 IF C > 23 THEN 2750
2730 PRINT "Not enough crew to launch a missile, "; N$; "."
2740 GOTO 880
2750 IF M <> 0 THEN 2780
2760 PRINT "No missiles left, "; N$; "."
2770 GOTO 880
2780 IF D > 50 AND D < 2000 THEN 2850
2790 PRINT "I recommend that you do not fire at this depth!  ";: COLOR 10: PRINT "Proceed (Y/N)";
2800 INPUT A$: COLOR 15
2810 IF LEFT$(A$, 1) = "N" OR LEFT$(A$, 1) = "n" THEN 880
2820 IF RND(1) < .5 THEN 2850
2830 PRINT "The missile explodes upon firing, "; N$; "!  You're dead!"
2840 CALL SubLose
2850 GOSUB 6080
2860 COLOR 10: PRINT "How much fuel (lbs.)";
2870 INPUT F1: COLOR 15
2880 IF F1 > 0 AND F1 <= F THEN 2910
2890 PRINT "You have"; F; "lbs. of fuel left, "; N$; "."
2900 GOTO 2860
2910 F2 = INT(F1 / 75 + .5)
2920 IF S1 + X1 * F2 > 0 AND S1 + X1 * F2 < 21 AND S2 + Y1 * F2 > 0 AND S2 + Y1 * F2 < 21 THEN 2980
2930 PRINT "Missile out of sonar tracking, "; N$; ".  Missile lost."
2940 M = M - 1
2950 F = F - F1
2960 P = P - 300
CALL SubFuel
2970 GOTO 4690

2980 D3 = 0: D4 = 0: D5 = 0: D6 = 0
2990 FOR X = S1 + X1 * F2 - 1 TO S1 + X1 * F2 + 1
3000 FOR Y = S2 + Y1 * F2 - 1 TO S2 + Y1 * F2 + 1
3010 IF X < 1 OR X > 20 OR Y < 1 OR Y > 20 THEN 3140
3020 D3 = D3 - (A(X, Y) = 3)
3030 D4 = D4 - (A(X, Y) = 6)
3040 D5 = D5 - (A(X, Y) = 5)
3050 D6 = D6 - (A(X, Y) = 1)
3060 IF A(X, Y) <> 4 THEN 3100
3070 PRINT "You've destroyed your headquarters, "; N$; "!"
3080 D3 = 0: S4 = 0: D2 = 0
3090 GOTO 3130
3100 IF A(X, Y) <> 2 THEN 3130
3110 PRINT "You just destroyed yourself, "; N$; "!  Dummy!"
3120 CALL SubLose
3130 A(X, Y) = 0
3140 NEXT Y
3150 NEXT X
3160 IF D6 = 0 THEN 3180
3170 PRINT "You blew up some island, "; N$; "."
3180 IF D5 = 0 THEN 3200
3190 PRINT "You destroyed"; D5; "mines, "; N$; "."
3200 IF D4 = 0 THEN 3220
3210 PRINT "You got"; D4; "sea monsters, "; N$; "!  Good work!"
3220 PRINT "You destroyed"; D3; "enemy ships, "; N$; "!"
3230 S = S - D3
3240 GOTO 2940

3250 REM *** MANUEVERING ***
3260 IF DM(5) >= 0 THEN 3290
3270 PRINT "Ballast controls are being repaired, "; N$; "."
3280 GOTO 880
3290 IF C > 12 THEN 3320
3300 PRINT "There are not enough crew to work the controls, "; N$; "."
3310 GOTO 880
3320 COLOR 10: PRINT "New depth";
3330 INPUT D1: COLOR 15
3340 IF D1 >= 0 AND D1 < 3000 THEN 3370
3350 PRINT "Hull crushed by pressure, "; N$; "!"
3360 CALL SubLose
3370 P = P - INT(ABS((D - D1) / 2 + .5))
3380 PRINT "Maneuver complete.  Power loss: "; INT(ABS((D - D1) / 2 + .5))
3390 D = D1
3400 GOTO 4690

3410 REM *** #5: STATUS / DAMAGE REPORT ***
3420 IF DM(6) >= 0 THEN 3450
3430 PRINT "No reports are able to get through, "; N$; "."
3440 GOTO 880
3450 IF C > 3 THEN 3480
3460 PRINT "No one left to give the report, "; N$; "."
3470 GOTO 880
3480 PRINT: COLOR 11
3485 PRINT "# of enemy ships left ......."; S
3490 PRINT "# of power units left ......."; P
3500 PRINT "# of torpedos left .........."; T
3510 PRINT "# of missiles left .........."; M
3520 PRINT "# of crewmen left ..........."; C
3530 PRINT "Lbs. of fuel left ..........."; F
3540 PRINT
3550 COLOR 10: PRINT "Want damage report (Y/N)";
3560 INPUT A$: COLOR 15
3570 IF LEFT$(A$, 1) = "N" OR LEFT$(A$, 1) = "n" THEN 3670
3580 PRINT
3585 PRINT "   Item         Damage  (+ Good, 0 Neutral, - Bad)"
3590 PRINT "   ----         ------"
3600 DATA "Engines","Sonar","Torpedos","Missiles","Maneuvering"
3610 DATA "Status","Headquarters","Sabotage","Converter"
3620 RESTORE 3600
3630 COLOR 11: FOR X = 1 TO 9
3640 READ A$
3650 PRINT A$, DM(X)
3660 NEXT X: COLOR 15
3670 S1$ = STR$(S1): S1$ = RIGHT$(S1$, LEN(S1$) - 1)
3671 S2$ = STR$(S2): S2$ = RIGHT$(S2$, LEN(S2$) - 1)
3675 PRINT "You are at location ("; S1$; ", "; S2$; ")."
3680 PRINT
3690 GOTO 880

3700 REM *** #6: HEADQUARTERS ***
3710 IF DM(7) >= 0 THEN 3740
3720 PRINT "Headquarters is damaged and unable to help, "; N$; "."
3730 GOTO 880
3740 IF D2 <> 0 THEN 3770
3750 PRINT "Headquarters is deserted, "; N$; "."
3760 GOTO 880
3770 IF SQR((S1 - S3) ^ 2 + (S2 - S4) ^ 2) <= 2 AND D < 51 THEN 3800
3780 PRINT "Unable to comply with docking orders, "; N$; "."
3790 GOTO 880
3800 PRINT "Divers from headquarters bring out supplies and men."
3810 P = 4000
3820 T = 8
3830 M = 2
3840 F = 1500
3850 C = 25
3860 D2 = D2 - 1
3870 GOTO 4690

3880 REM *** #7: SABOTAGE ***
3890 IF DM(8) >= 0 THEN 3920
3900 PRINT "Hatches are inaccessible, "; N$; ".  No sabotage possible."
3910 GOTO 880
3920 IF C > 10 THEN 3950
3930 PRINT "Not enough crew to go on a mission, "; N$; "."
3940 GOTO 880
3950 D3 = 0: D4 = 0
3960 FOR X = S1 - 2 TO S1 + 2
3970 FOR Y = S2 - 2 TO S2 + 2
3980 IF X < 1 OR X > 20 OR Y < 1 OR Y > 20 THEN 4010
3990 D3 = D3 - (A(X, Y) = 3)
4000 D4 = D4 - (A(X, Y) = 6)
4010 NEXT Y
4020 NEXT X
4030 IF D3 <> 0 THEN 4060
4040 PRINT "No ships in range, "; N$; "."
4050 GOTO 880
4060 PRINT "There are"; D3; "ships in range, "; N$; "."
4070 COLOR 10: PRINT "How many men are going, "; N$;
4080 INPUT Q1: COLOR 15
4090 IF C - Q1 >= 10 THEN 4120
4100 PRINT "You must leave at least 10 men on board, "; N$; "."
4110 GOTO 4070
4120 D5 = INT(D3 / Q1 + .5)
4130 D6 = 0
4140 FOR X = S1 - 2 TO S1 + 2
4150 FOR Y = S2 - 2 TO S2 + 2
4160 IF D3 / Q1 > 1 - RND(1) AND RND(1) + D3 / Q1 < .9 THEN 4220
4170 IF A(X, Y) <> 3 THEN 4220
4180 D6 = D6 + 1
4190 A(X, Y) = 0
4200 S = S - 1
4210 IF S = 0 THEN CALL SubWin
4220 NEXT Y
4230 NEXT X
4240 PRINT D6; "ships were destroyed, "; N$; "."
4250 D6 = 0: D7 = 0
4260 FOR X = 1 TO Q1
4270 D7 = D7 - (RND(1) > .6)
4280 NEXT X
4290 FOR X = 1 TO Q1 - D7
4300 D6 = D6 - (RND(1) < .15)
4310 NEXT X
4320 IF D4 = 0 THEN 4360
4330 PRINT "A sea monster smells the men on the way back!"
4340 PRINT D7; "men were eaten, "; N$; "!"
4350 C = C - D7
4360 PRINT D6; "men were lost through accidents, "; N$; "."
4370 C = C - D6
4380 P = P - INT(10 * Q1 + RND(1) * 10)
4390 GOTO 3690

4400 REM *** #8: POWER CONVERTER ***
4410 IF DM(9) >= 0 THEN 4440
4420 PRINT "The power converter is damaged, "; N$; "."
4430 GOTO 880
4440 IF C > 5 THEN 4470
4450 PRINT "Not enough men to work the converter, "; N$; "."
4460 GOTO 880
4470 COLOR 10: PRINT "Option (1=fuel to power, 2=power to fuel)";
4480 INPUT O: COLOR 15
4490 ON O GOTO 4510, 4580
4500 GOTO 4470

4510 REM *** FUEL TO POWER CONVERSION ***
4520 PRINT "Fuel available:"; F;: COLOR 10: PRINT " Convert how much";
4530 INPUT C1: COLOR 15
4540 IF C1 > F OR C1 < 0 THEN 4520
4550 F = F - C1
4560 P = P + INT(C1 / 3)
4570 GOTO 4640

4580 REM *** POWER TO FUEL CONVERSION ***
4590 PRINT "Power avaiable:"; P - 1;: COLOR 10: PRINT " Convert how much";
4600 INPUT C1: COLOR 15
4610 IF C1 > P - 1 OR C1 < 0 THEN 4590
4620 P = P - C1
4630 F = F + INT(C1 * 3)
4640 PRINT "Conversion complete.  Power ="; P; " Fuel ="; F
4650 GOTO 4690

4660 REM *** #9: SURRENDER ***
4670 PRINT "Coward!  You're not very patriotic, "; N$; "!"
4680 CALL SubLose

4690 REM *** RETALIATION SECTION ***
4700 Q = 0
4710 FOR X = S1 - 4 TO S1 + 4
4720 FOR Y = S2 - 4 TO S2 + 4
4730 IF X < 1 OR X > 20 OR Y < 1 OR Y > 20 THEN 4760
4740 IF A(X, Y) <> 3 THEN 4760
4750 Q = Q + (RND(1) / SQR((S1 - X) ^ 2 + (S2 - Y) ^ 2))
4760 NEXT Y
4770 NEXT X
4780 IF Q THEN 4810
4790 PRINT "No ships in range to depth charge you, "; N$; "!"
4800 GOTO 5210
4810 PRINT "Depth charges off the ";
4820 IF RND(1) > .5 THEN 4850
4830 PRINT "port side, "; N$; "!"
4840 GOTO 4860
4850 PRINT "starboard side, "; N$; "!"
4860 IF Q > .13 OR RND(1) > .92 THEN 4890
4870 PRINT "No real damage sustained, "; N$; "."
4880 GOTO 5210
4890 IF Q > .36 OR RND(1) > .96 THEN 4940
4900 PRINT "Light, superficial damage, "; N$; "."
4910 P = P - 50
4920 DM(INT(RND(1) * 9) + 1) = -RND(1) * 2
4930 GOTO 5210
4940 IF Q > .6 OR RND(1) > .975 THEN 5020
4950 PRINT "Moderate damage.  Repairs needed."
4960 P = P - 75 + INT(RND(1) * 30)
4970 FOR Y = 1 TO 2
4980 X = INT(RND(1) * 9) + 1
4990 DM(X) = DM(X) - RND(1) * 8
5000 NEXT Y
5010 GOTO 5210
5020 IF Q > .9 OR RND(1) > .983 THEN 5100
5030 PRINT "Heavy damage!  Repairs immediate, "; N$; "!"
5040 P = P - (200 + INT(RND(1) * 76))
5050 FOR X = 1 TO 4 + INT(RND(1) * 2)
5060 Y = INT(RND(1) * 9) + 1
5070 DM(Y) = DM(Y) - RND(1) * 11
5080 NEXT X
5090 GOTO 5210

5100 PRINT "Damage critical!  We need help!"
5110 A$ = "VRAVUKXCNVPCRHFDRSAXQURLQTRHXYACVFZYITLCBSSYYKDQIPCAEGQGPCNOTSIO"
5120 X = INT(RND(1) * 16) + 1
5130 PRINT "Send 'HELP' in code.  Here is the code: ";: COLOR 8, 7: PRINT MID$(A$, X, 4);: COLOR 15, 0: PRINT
5132 REM  TIME DELAY AND THEN ERASE THE CODE
5140 COLOR 10: INPUT "Enter code"; B$: COLOR 15
5150 PRINT
5160 IF B$ <> MID$(A$, X, 4) THEN 5190
5170 PRINT "Fast work, "; N$; "!  Help arrives in time to save you!"
5180 GOTO 5040
5190 PRINT "Message garbled, "; N$; "... no help arrives!"
5200 CALL SubLose

5210 REM *** MOVE SHIPS / SEA MONSTERS ***
5220 IF DM(1) >= 0 OR DM(3) >= 0 OR DM(4) >= 0 OR DM(5) >= 0 OR DM(7) >= 0 THEN 5260
5230 IF DM(8) >= 0 OR DM(9) >= 0 THEN 5260
5240 PRINT "Damage too much, "; N$; "!  You're sunk!"
5250 CALL SubLose

5260 REM *** MOVE SHIPS / SEA MONSTERS ***
5270 PRINT: PRINT "---*** Result of Last Enemy Maneuver **---"
5280 FOR X = 1 TO 20
5290 FOR Y = 1 TO 20
5300 IF A(X, Y) <> 3 THEN 5690

5310 REM *** MOVE A SHIP ***
5320 W = D8
5330 V = D9
5340 IF X + W > 0 AND X + W < 21 AND Y + V > 0 AND Y + V < 21 THEN 5420
5350 FOR X0 = 19 TO 1 STEP -1
5360 IF A(X - W * X0, Y - V * X0) <> 0 THEN 5400
5370 A(X - W * X0, Y - V * X0) = 3
5380 A(X, Y) = 0
5390 GOTO 6000
5400 NEXT X0
5410 STOP

5420 ON A(X + W, Y + V) + 1 GOTO 5430, 5460, 5530, 5460, 5560, 5600, 5650
5430 A(X + W, Y + V) = 3
5440 A(X, Y) = 0
5450 GOTO 6000

5460 REM *** CHANGE DIRECTION ***
5470 RESTORE 6090
5480 FOR X0 = 1 TO INT(RND(1) * 8) + 1
5490 READ W, V
5500 NEXT X0
5510 IF X + W < 1 OR X + W > 20 OR Y + V < 1 OR Y + V > 20 THEN 5470
5520 GOTO 5420

5530 IF D > 50 THEN 5460
5540 COLOR 12: PRINT "*** You've been rammed by a ship, "; N$; "!": COLOR 15
5550 CALL SubLose

5560 IF RND(1) < .15 THEN 5460
5570 COLOR 12: PRINT "*** Your headquarters was rammed, "; N$; "!": COLOR 15
5580 S3 = 0: S4 = 0: D2 = 0: A(X + W, Y + V) = 0
5590 GOTO 5620

5600 IF RND(1) < .7 THEN 5460
5610 COLOR 12: PRINT "*** Ship destroyed by a mine, "; N$; "!": COLOR 15
5620 S = S - 1
5630 IF S <> 0 THEN 5440
5640 CALL SubWin

5650 IF RND(1) < .8 THEN 5460
5660 COLOR 12: PRINT "*** Ship eaten by a sea monster, "; N$; "!": COLOR 15
5670 S = S - 1
5680 GOTO 5630

5690 REM *** MOVE A SEA MONSTER ***
5700 IF A(X, Y) <> 6 THEN 6000
5710 IF X + M1 < 1 OR X + M1 > 20 OR Y + M2 < 1 OR Y + M2 > 20 THEN 5760
5720 ON A(X + M1, Y + M2) + 1 GOTO 5730, 5760, 5830, 5850, 5900, 5730, 5930

5730 A(X + M1, Y + M2) = 6
5740 A(X, Y) = 0
5750 GOTO 6000

5760 REM *** CHANGE DIRECTION ***
5770 RESTORE 6090
5780 FOR X0 = 1 TO INT(RND(1) * 8) + 1
5790 READ M1, M2
5800 NEXT X0
5810 IF X + M1 < 1 OR X + M1 > 20 OR Y + M2 < 1 OR Y + M2 > 20 THEN 5760
5820 GOTO 5720

5830 COLOR 12: PRINT "*** You've been eaten by a sea monster, "; N$; "!": COLOR 15
5840 CALL SubLose

5850 IF RND(1) > .2 THEN 5760
5860 COLOR 12: PRINT "*** Ship eaten by a sea monster, "; N$; "!": COLOR 15
5870 S = S - 1
5880 IF S <> 0 THEN 5730
5890 CALL SubWin

5900 COLOR 12: PRINT "*** A sea monster ate your headquarters, "; N$; "!": COLOR 15
5910 S3 = 0: S4 = 0: D2 = 0
5920 GOTO 5730

5930 IF RND(1) < .75 THEN 5760
5940 COLOR 12: PRINT "*** A sea monster fight, "; N$; "!  ";
5950 IF RND(1) < .8 THEN 5980
5960 PRINT "And one dies!": COLOR 15
5970 GOTO 5730
5980 PRINT "It's a tie!": COLOR 15
5990 GOTO 5760

6000 NEXT Y
6010 NEXT X

6020 REM *** MAKE REPAIRS ***
6030 FOR Y = 1 TO 9
6040 X = INT(RND(1) * 9) + 1
6050 DM(X) = DM(X) + (RND(1) * (2 + RND(1) * 2)) * (1 + (-(D < 51) OR -(D > 2000))) * (-(DM(X) < 3))
6060 NEXT Y
6070 GOTO 880


6080 REM *** GOSUB FOR COURSE / DIRECTION ***
6090 DATA -1,0,-1,1,0,1,1,1,1,0,1,-1,0,-1,-1,-1
6100 COLOR 10: PRINT "What course (1-8)";
6110 INPUT C1: COLOR 15
6120 IF C1 < 1 OR C1 > 8 THEN 6100
6130 RESTORE 6090
6140 FOR X9 = 1 TO INT(C1 + .5)
6150 READ X1, Y1
6160 NEXT X9
6170 RETURN

'Lose
SUB SubLose
REM *** DESTROYED ? ***
PRINT "There are still"; S; " enemy ships left, "; N$; "."
PRINT "You will be demoted to the rank of deck scrubber!"
WL=1
END
END SUB 'Lose
'6210 COLOR 10: PRINT "Want another game (Y/N)";
'6220 INPUT A$: COLOR 15
'6230 IF LEFT$(A$, 1) <> "Y" AND LEFT$(A$, 1) <> "y" THEN 6250
'6240 GOTO 310
'6250 STOP
'Win
SUB SubWin
PRINT "Good work, "; N$; "!  You got them all!!!"
PRINT "Promotions and commendations will be given immediately!"
WL=2
END
END SUB 'Win
'6280 GOTO 6210

6290 REM *** ISLAND DATA ***
6300 DATA 0,1,1,1,0,0,0,1,1,1,1,0,1,1,1,0,1,1,1,1,0,0,0,1
6310 DATA 1,1,0,0,1,1,0,1,1,0,1,0,0,0,1,0,0,0
6320 END

SUB SubInst
COLOR 14: PRINT
PRINT "This is the game of Sea Battle!  The object of the game is to destroy all of"
PRINT "the enemy ships in your 20-by-20 area with the various weapons in your sub-"
PRINT "marine's arsenal.  You must do this, however, without letting the enemy destroy"
PRINT "you first!"
PRINT
PRINT "There are several interesting hazards in the game  They include depth charges"
PRINT "from nearby enemy ships, very hungry sea monsters, and hidden underwater mines."
PRINT
PRINT "The depth charges are effective to any depth, but they lose their effectiveness"
PRINT "over distance, so the farther you are from any ships, the better!"
PRINT
PRINT "The sea monsters take a meandering course through your area that may bring them"
PRINT "close enough to attack you.  You rarely survive.  They also like to eat your"
PRINT "torpedos, but missiles will kill them."
PRINT
PRINT "The enemy ships move on every turn, in a fixed course, unless they encounter"
PRINT "ostacles.  They will get blown up by mines and eaten by sea monsters too."
PRINT
COLOR 13: PRINT "(Press any key.)";
DO
LOOP WHILE INKEY$ = ""
COLOR 14: PRINT
PRINT "You have ten orders that you may give.  They are:"
PRINT
COLOR 11: PRINT "#0: NAVIGATION - ";: COLOR 14: PRINT "This command allows you to move in a particular direction and"
PRINT "distance across your area.  The direction is determined by the graph at left."
COLOR 12: PRINT "  8 1 2";: COLOR 14: PRINT "  There are 8 directions to move in, and they are the same anytime you"
COLOR 12: PRINT "   \|/ ";: COLOR 14: PRINT "  are asked for a course.  For example, to move north, you would use"
COLOR 12: PRINT "  7-*-3";: COLOR 14: PRINT "  course #1.  The computer will also ask for an amount of power.  It"
COLOR 12: PRINT "   /|\ ";: COLOR 14: PRINT "  takes 100 units of power to move your sub 1 space.  Beware of"
COLOR 12: PRINT "  6 5 4";: COLOR 14: PRINT "  obstacles!  If you use more than 1000 units in a turn, there is an"
PRINT "overload danger, so be very careful!"
PRINT
COLOR 11: PRINT "#1: SONAR - ";: COLOR 14: PRINT "This command has two options.  Option #1 gives directional infor-";
PRINT "ation, showing the directions and distances in which there are enemy ships."
PRINT "This is useful for shooting at long ranges, where it is difficult to tell if a"
PRINT "ship is in direct line."
PRINT
PRINT "Option #0 prints out a map of your area in a square.  It uses symbols for the"
PRINT "map.  ";: COLOR 12: PRINT "*";: COLOR 14: PRINT " indicates dry land, ";: COLOR 12: PRINT "$";: COLOR 14
PRINT " is an underwater mine, ";: COLOR 12: PRINT "\S/";: COLOR 14: PRINT " is an enemy ship."
COLOR 12: PRINT "-#-";: COLOR 14: PRINT " is a sea monster.  ";: COLOR 12: PRINT "!H!";
COLOR 14: PRINT " is your headquarters, and finally, ";: COLOR 12: PRINT "(X)";: COLOR 14: PRINT " is you!"
PRINT: COLOR 13: PRINT "(Press any key.)";
DO
LOOP WHILE INKEY$ = ""
PRINT: COLOR 14
PRINT "Every so often, a '.' will appear inside the screen.  This is a sonar malfunc-"
PRINT "tion, and so the object there isn't identified.  If you are above 50 feet, waves"
PRINT "will show up as '.'."
PRINT
COLOR 11: PRINT "#2: TORPEDO CONTROL - ";: COLOR 14: PRINT "This command allows you to shoot 1 of your 10 torpedos at"
PRINT "enemy ships.  The computer will only require the direction to shoot, using the"
PRINT "indicator above.  They have a range of 7-13 spaces.  One torpedo gets one ship."
PRINT
COLOR 11: PRINT "#3: POLARIS MISSILE CONTROL - ";: COLOR 14: PRINT "This command allows you to launch one of your"
PRINT "Polaris missiles against the enemy.  The computer will ask for a course and"
PRINT "fuel.  It takes 75 lbs. of fuel to boost a missile 1 space.  Since they are so"
PRINT "much more powerful, they will completely destroy the space they land on, plus"
PRINT "all of the immediately adjacent ones.  Missiles destroy everything!"
PRINT
COLOR 11: PRINT "#4: MANUEVERING - ";: COLOR 14: PRINT "This command allows you to change the depth you're at.  You"
PRINT "may want to do this if you are badly damaged, because repairs go on twice as"
PRINT "quickly below 2500 ft. and above 50 ft. than in-between.  You start the game at"
PRINT "100 ft.  You use up about 1 power unit for every 2 ft. you change."
PRINT
COLOR 11: PRINT "#5: STATUS/DAMAGE REPORT - ";: COLOR 14: PRINT "This command gives you the status of your sub.  It"
PRINT "tells you how much is left in your arsenal, which items are damaged, and how"
PRINT "much."
COLOR 13: PRINT: PRINT "(Press any key.)";
DO
LOOP WHILE INKEY$ = ""
PRINT: COLOR 14
COLOR 11: PRINT "#6: HEADQUARTERS - ";: COLOR 14: PRINT "This command allows scuba divers from your headquarters to"
PRINT "replenish your supply of weapons and men.  You must be at 50 ft. or less, and"
PRINT "2 or less spaces away to do this, however, and you can only do it twice."
PRINT
COLOR 11: PRINT "#7: SABOTAGE (SCUBA) - ";: COLOR 14: PRINT "This command allows you to send men out ona sabotage"
PRINT "mission against enemy ships.  You may only go against ships within 3 spaces of"
PRINT "you, and you must leave at least 10 men on board the sub to run it."
PRINT
COLOR 11: PRINT "#8: POWER CONVERSION - ";: COLOR 14: PRINT "This command allows you to change fuel to power, or"
PRINT "vica-versa."
PRINT
COLOR 11: PRINT "#9: SURRENDER - ";: COLOR 14: PRINT "This command is only for cowards and traitors!"
PRINT
PRINT "You start the game with 6000 units of power, 2500 lbs. of rocket fuel, 10"
PRINT "torpedos, 3 missiles, 1 headquarters, and a random number of ships."
PRINT
PRINT "I left some interesting details out of the instructions, to make playing the"
PRINT "game the first few times more interesting."
PRINT
PRINT "You start the game in the island's lagoon, and it is your duty to seek out and"
PRINT "destroy the enemy at all costs!"
COLOR 13: PRINT: PRINT "(Press any key.)";
DO
LOOP WHILE INKEY$ = ""
PRINT: COLOR 15: RETURN
END SUB


SUB SubWait (Wait!)
Start! = TIMER
DO
	Now! = TIMER
LOOP UNTIL Now! - Start! > Wait!
END SUB
