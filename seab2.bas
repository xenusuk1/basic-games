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

DIM SHARED A(20, 20), DM(9), B(5), DM$(9)
DIM SHARED C1,I,J,X,Y,S,S1,S2,S3,S4,X1,X2,Y1,Y2,P1 AS INTEGER



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

REM *** SUB 2 position ***
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

REM Device Names
RESTORE 3600
FOR X = 1 TO 9
	READ DM$(X)
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
D = 100
D2 = 2

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
	CASE 4
		PRINT "Missile silos are under repair, "; N$; "."
	CASE 5
		PRINT "Ballast controls are being repaired, "; N$; "."
	CASE 6
		PRINT "No reports are able to get through, "; N$; "."
	CASE 7
		PRINT "Headquarters is damaged and unable to help, "; N$; "."
	CASE 8
		PRINT "Hatches are inaccessible, "; N$; ".  No sabotage possible."
	CASE 9
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
	CASE 4
		PRINT "Not enough crew to launch a missile, "; N$; "."
	CASE 5
		PRINT "There are not enough crew to work the controls, "; N$; "."
	CASE 6
		PRINT "No one left to give the report, "; N$; "."
	CASE 7
	CASE 8
		PRINT "Not enough crew to go on a mission, "; N$; "."
	CASE 9
	END SELECT
END IF

END FUNCTION  'subStatusCheck

'Main Loop
880 REM *** COMMAND SECTION ***
PRINT: COLOR 10: PRINT "What are your orders (0-9), "; N$;
INPUT O: COLOR 15

SELECT CASE O
	CASE 0
		CALL SubNavigation
		IF SubStatusCheck(1,8)<>0 THEN 4690
	CASE 1
		CALL SubSonar
	CASE 2
		CALL SubTorpedo
		IF SubStatusCheck(3,10) <> 0 THEN 4690
'	 GOTO 2220
	CASE 3
	 GOTO 2680
	CASE 4
		CALL SubManoevre
		IF SubStatusCheck(5,12) > 0 THEN 4690
'	 GOTO 3250
	CAsE 5
		CALL SubStatus
	 'GOTO 3410
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



SUB SubNavigation
REM *** #0: NAVIGATION ***
IF SubStatusCheck(1,8)=0 THEN EXIT SUB

D1 = 1 - ((.23 + RND(1) / 10) * (-(D <= 50)))

CALL SubCourse

DO
	PRINT "Power available ="; P;: COLOR 10: PRINT " Power to use (100/square) ";
	INPUT P1: COLOR 15
LOOP UNTIL P1 >=0 OR P1<=P

IF P1 > 1000 AND RND(1) > .43 THEN
	PRINT "Your atomic pile went supercriticial, "; N$; "!  Headquarters will warn all"
	PRINT "subs to stay from the radioactive area!"
	CALL SubLose
END IF
X = S1
Y = S2
Q1 = 1

' Move loop
FOR X2 = 1 TO INT(INT(P1 / 100 + .5) * D1 + .5)

IF X + X1 <= 0 OR X + X1 >= 21 OR Y + Y1 <= 0 OR Y + Y1 >= 21 THEN
	PRINT "You can't leave the area, "; N$; "!"
	EXIT FOR
END IF

'debug
PRINT X,X1,Y,Y1,A(X+X1, Y+Y1);

'check content of new location
SELECT CASE A(X+X1, Y+Y1)
	CASE 0  'empty square
		X = X + X1
		Y = Y + Y1
		P = P - 100
		BEEP
	CASE 1  'island
		PRINT "You almost ran aground, "; N$; "!"

	CASE 2  'you

	CASE 3  'ship
		IF D <= 50 THEN
			PRINT "You rammed a ship!  You're both sunk, "; N$; "!"
			S = S - 1
			IF S = 0 THEN CALL SubWin
			CALL SubLose
		END IF

	CASE 4   'HQ
		IF D <= 50 THEN
			PRINT "You rammed your headquarters!  You're sunk!"
			CALL SubLose
		END IF

	CASE 5  'mine
		PRINT "You've been blown up by a mine, "; N$; "!"
		CALL SubLose
	CASE 6  'sea monster
		IF RND(1) >= .21 THEN
			PRINT "You were eaten by a sea monster, "; N$; "!"
			CALL SubLose
		END IF

END SELECT

REM *** CHECK FOR NEARBY SEA MONSTERS 6 ***
FOR X3 = X - 2 TO X + 2
FOR Y3 = Y - 2 TO Y + 2

IF X3>=0 AND X3<=19 AND Y3>=0 AND Y3 <=19 AND A(X,Y)= 6 THEN
	IF RND(1) < .25 THEN PRINT "You were eaten by a sea monster, "; N$; "!" : CALL SubLose
	IF Q1 = 1 THEN PRINT "You just had a narrow escape with a sea monster, "; N$; "!" : Q1 = 0
END IF

NEXT Y3
NEXT X3

NEXT X2  'move loop

1640 PRINT "Navigation complete.  Power left: "; P
CALL SubFuel
'complete the move
A(X, Y) = 2
IF S1<>X OR S2<>Y THEN
	A(S1, S2) = 0
	S1 = X
	S2 = Y
END IF
END SUB 'Navigation

1680 REM *** #1: SONAR ***
SUB SubSonar
IF SubStatusCheck(2,5) = 0 THEN EXIT SUB

CALL SubMap
CALL SubShipDir
P = P - 100
CALL SubFuel

END SUB 'Sonar


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


SUB SubShipDir
FOR I = 1 TO 5
	B(I) = 0
NEXT I
PRINT "Direction   # of Ships     Distances": COLOR 11

FOR X = 1 TO 9

CALL SubDirection(X)
	X3 = 0
	FOR X4 = 1 TO 20
		IF S1 + X1 * X4 < 1 OR S1 + X1 * X4 > 20 OR S2 + Y1 * X4 < 1 OR S2 + Y1 * X4 > 20 THEN 2140
		IF A(S1 + X1 * X4, S2 + Y1 * X4) <> 3 THEN 2130
		X3 = X3 + 1
		B(X3) = X4
	2130 NEXT X4
2140 IF X3 = 0 THEN 2200
PRINT "   "; X, X3,
FOR X4 = 1 TO X3
	PRINT B(X4);
NEXT X4
PRINT
2200 NEXT X: COLOR 15
END SUB  'SubShipDir

SUB SubTorpedo
REM *** #2: TORPEDO CONTROL ***
IF SubStatusCheck(3,10) = 0 THEN EXIT SUB

IF T<=0 THEN
	PRINT "No torpedos left, "; N$; "."
	EXIT SUB
END IF

IF D > 2000 AND RND(1) > .5 THEN
	PRINT "Pressure implodes the sub upon firing... you're crushed!"
	CALL SubLose
END IF

CALL SubCourse
X = S1
Y = S2

X2MAX = INT(7 + 5 * (-(D > 50)) - RND(1) * 4 + .5)
X2=1
DO

'debug
'PRINT X,Y,X1,Y1,A(X + X1, Y + Y1),X2MAX

IF X + X1 <= 0 OR X + X1 >= 20 OR Y + Y1 <= 0 OR Y + Y1 >= 20 THEN
	PRINT "Torpedo out of sonar range... ineffectual, "; N$; "."
	X2MAX=99
	EXIT DO
END IF

SELECT CASE A(X + X1, Y + Y1)
CASE 0
	X = X + X1
	Y = Y + Y1
	PRINT "..!..";
	CALL SubWait(1)
	BEEP
CASE 1
	PRINT "You took out some island, "; N$; "!"
	A(X + X1, Y + Y1) = 0
	X2MAX=99
CASE 2
' your sub
CASE 3
	PRINT "Ouch!  You got one, "; N$; "!"
	S = S - 1
	IF S <= 0 THEN CALL SubWin
	A(X + X1, Y + Y1) = 0
	X2MAX=99
CASE 4
	PRINT "You blew up your headquarters, "; N$; "!"
	S3 = 0: S4 = 0: D2 = 0
	A(X + X1, Y + Y1) = 0
	X2MAX=99
CASE 5
	PRINT "Blam!  Shot wasted on a mine, "; N$; "!"
	A(X + X1, Y + Y1) = 0
	X2MAX=99
CASE 6
	PRINT "A sea monster had a torpedo for lunch, "; N$; "!"
	X2MAX=99

END SELECT

X2=X2+1
LOOP UNTIL X2>X2MAX OR X2MAX = 99
'2650 NEXT X2

IF X2MAX<99 THEN PRINT "Dud."

T = T - 1
P = P - 150

CALL SubFuel
END SUB 'Torpedo



2680 REM *** #3: POLARIS MISSILE CONTROL ***
IF SubStatusCheck(4,23) = 0 THEN 880

IF M <=0 THEN
	PRINT "No missiles left, "; N$; "."
	GOTO 880
END IF

IF D <= 50 OR D >= 2000 THEN
	PRINT "I recommend that you do not fire at this depth!  ";: COLOR 10: PRINT "Proceed (Y/N)";
	INPUT A$: COLOR 15
	IF UCASE$(LEFT$(A$, 1)) = "Y" THEN
		IF RND(1) > .5 THEN
			PRINT "The missile explodes upon firing, "; N$; "!  You're dead!"
			CALL SubLose
		END IF
	ELSE GOTO 880
	END IF
END IF

CALL SubCourse

DO
	COLOR 10: PRINT "How much fuel (lbs.)";
	INPUT F1: COLOR 15
LOOP UNTIL F1 > 0 AND F1 <= F

PRINT "You have"; F; "lbs. of fuel left, "; N$; "."

F2 = INT(F1 / 75 + .5)
IF S1 + X1 * F2 <= 0 OR S1 + X1 * F2 >= 21 OR S2 + Y1 * F2 <= 0 OR S2 + Y1 * F2 >= 21 THEN
	PRINT "Missile out of sonar tracking, "; N$; ".  Missile lost."
ELSE

	D3 = 0: D4 = 0: D5 = 0: D6 = 0
	FOR X = S1 + X1 * F2 - 1 TO S1 + X1 * F2 + 1
	FOR Y = S2 + Y1 * F2 - 1 TO S2 + Y1 * F2 + 1
		IF X < 1 OR X > 20 OR Y < 1 OR Y > 20 THEN 3140
	D3 = D3 - (A(X, Y) = 3)
	D4 = D4 - (A(X, Y) = 6)
	D5 = D5 - (A(X, Y) = 5)
	D6 = D6 - (A(X, Y) = 1)

IF A(X, Y) = 4 THEN
	PRINT "You've destroyed your headquarters, "; N$; "!"
	D3 = 0: S4 = 0: D2 = 0
END IF
IF A(X, Y) = 2 THEN
	PRINT "You just destroyed yourself, "; N$; "!  Dummy!"
	CALL SubLose
END IF
A(X, Y) = 0
3140 NEXT Y
3150 NEXT X

IF D6 > 0 THEN PRINT "You blew up some island, "; N$; "."
IF D5 > 0 THEN PRINT "You destroyed "; D5; " mines, "; N$; "."
IF D4 > 0 THEN PRINT "You got "; D4; " sea monsters, "; N$; "!  Good work!"
PRINT "You destroyed "; D3; " enemy ships, "; N$; "!"
S = S - D3

END IF 'tracking
M = M - 1
F = F - F1
P = P - 300
CALL SubFuel

GOTO 4690


SUB SubManoevre
REM *** MANUEVERING ***
IF SubStatusCheck(5,12) = 0 THEN EXIT SUB

COLOR 10: PRINT "New depth";
INPUT D1: COLOR 15
IF D1 > 3000 THEN
	PRINT "Hull crushed by pressure, "; N$; "!"
	CALL SubLose
END IF

P = P - INT(ABS((D - D1) / 2 + .5))
PRINT "Maneuver complete.  Power loss: "; INT(ABS((D - D1) / 2 + .5))
CALL SubFuel
D = D1
END SUB

SUB SubStatus
3410 REM *** #5: STATUS / DAMAGE REPORT ***
IF SubStatusCheck(6,3) = 0 THEN EXIT SUB

PRINT: COLOR 11
PRINT "# of enemy ships left ......."; S
PRINT "# of power units left ......."; P
PRINT "# of torpedos left .........."; T
PRINT "# of missiles left .........."; M
PRINT "# of crewmen left ..........."; C
PRINT "Lbs. of fuel left ..........."; F
PRINT "Depth ......................."; D
PRINT

PRINT "   Item         Damage  (+ Good, 0 Neutral, - Bad)"
PRINT "   ----         ------"

COLOR 11
FOR X = 1 TO 9
	PRINT DM$(X), DM(X)
NEXT X
COLOR 15
S1$ = STR$(S1): S1$ = RIGHT$(S1$, LEN(S1$) - 1)
S2$ = STR$(S2): S2$ = RIGHT$(S2$, LEN(S2$) - 1)
PRINT "You are at location ("; S1$; ", "; S2$; ")."
PRINT
END SUB

'GOTO 880

3700 REM *** #6: HEADQUARTERS ***
IF SubStatusCheck(7,-99)=0 THEN 880

IF D2 = 0 THEN PRINT "Headquarters is destroyed, "; N$; "." : GOTO 880

'are we there yet
IF SQR((S1 - S3) ^ 2 + (S2 - S4) ^ 2) > 2 OR D >= 51 THEN
	PRINT "Unable to comply with docking orders, too deep or far away, "; N$; "."
	GOTO 880
END IF

PRINT "Divers from headquarters bring out supplies and men."
P = 4000
T = 8
M = 2
F = 1500
C = 25
D2 = D2 - 1
GOTO 4690

3880 REM *** #7: SABOTAGE ***
IF SubStatusCheck(8,10)=0 THEN 880

D3 = 0: D4 = 0
FOR X = S1 - 2 TO S1 + 2
FOR Y = S2 - 2 TO S2 + 2
3980 IF X < 1 OR X > 20 OR Y < 1 OR Y > 20 THEN 4010
D3 = D3 - (A(X, Y) = 3)
D4 = D4 - (A(X, Y) = 6)
4010 NEXT Y
4020 NEXT X

IF D3 = 0 THEN
	PRINT "No ships in range, "; N$; "."
	GOTO 880
END IF

PRINT "There are "; D3; " ships in range, "; N$; "."

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

4240 PRINT D6; " ships were destroyed, "; N$; "."
4250 D6 = 0: D7 = 0
4260 FOR X = 1 TO Q1
4270 D7 = D7 - (RND(1) > .6)
4280 NEXT X

4290 FOR X = 1 TO Q1 - D7
4300 D6 = D6 - (RND(1) < .15)
4310 NEXT X

4320 IF D4 = 0 THEN 4360
4330 PRINT "A sea monster smells the men on the way back!"
4340 PRINT D7; " men were eaten, "; N$; "!"
4350 C = C - D7
4360 PRINT D6; " men were lost through accidents, "; N$; "."
4370 C = C - D6
4380 P = P - INT(10 * Q1 + RND(1) * 10)
4390 GOTO 4690

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
'check whole map for ships
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
'empty space 0
5430 A(X + W, Y + V) = 3
5440 A(X, Y) = 0
5450 GOTO 6000

'island 1
5460 REM *** CHANGE DIRECTION ***
5470 RESTORE 6090
5480 FOR X0 = 1 TO INT(RND(1) * 8) + 1
5490 READ W, V
5500 NEXT X0
5510 IF X + W < 1 OR X + W > 20 OR Y + V < 1 OR Y + V > 20 THEN 5470
5520 GOTO 5420

'sub 2
5530 IF D > 50 THEN 5460
5540 COLOR 12: PRINT "*** You've been rammed by a ship, "; N$; "!": COLOR 15
5550 CALL SubLose

' HQ 3
5560 IF RND(1) < .15 THEN 5460
5570 COLOR 12: PRINT "*** Your headquarters was rammed, "; N$; "!": COLOR 15
5580 S3 = 0: S4 = 0: D2 = 0: A(X + W, Y + V) = 0
5590 GOTO 5620

' mine 4
5600 IF RND(1) < .7 THEN 5460
5610 COLOR 12: PRINT "*** Ship destroyed by a mine, "; N$; "!": COLOR 15
5620 S = S - 1
5630 IF S <> 0 THEN 5440
5640 CALL SubWin

' seas monster 5
5650 IF RND(1) < .8 THEN 5460
5660 COLOR 12: PRINT "*** Ship eaten by a sea monster, "; N$; "!": COLOR 15
5670 S = S - 1
5680 GOTO 5630

'check map for sea monsters
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
PRINT "Repairing ";
6030 FOR Y = 1 TO 9
6040 X = INT(RND(1) * 9) + 1
6050 DM(X) = DM(X) + (RND(1) * (2 + RND(1) * 2)) * (1 + (-(D < 51) OR -(D > 2000))) * (-(DM(X) < 3))
PRINT ".";
6060 NEXT Y
PRINT
6070 GOTO 880


SUB SubCourse
'6080 REM *** GOSUB FOR COURSE / DIRECTION ***
6100 COLOR 10: PRINT "What course (1-9) ";
INPUT C1: COLOR 15
IF C1 < 1 OR C1 > 9 THEN 6100
IF C1=5 THEN 6100

CALL SubDirection(C1)

'RESTORE 6090
'FOR X9 = 1 TO INT(C1 + .5)
'	READ X1, Y1
'NEXT X9
'6170 RETURN
END SUB ' Course

SUB SubDirection (DIR)
SELECT CASE DIR
CASE 1
	X1=1:Y1=-1
CASE 2
	X1=1:Y1=0
CASE 3
	X1=1:Y1=1
CASE 4
	X1=0:Y1=-1
CASE 5
	X1=0:Y1=0
CASE 6
	X1=0:Y1=1
CASE 7
	X1=-1:Y1=-1
CASE 8
	X1=-1:Y1=0
CASE 9
	X1=-1:Y1=1
END SELECT

END SUB


'Lose
SUB SubLose
REM *** DESTROYED ? ***
PRINT "There are still "; S; " enemy ships left, "; N$; "."
PRINT "You will be demoted to the rank of deck scrubber!"
WL=1
END
END SUB 'Lose

'Win
SUB SubWin
PRINT "Good work, "; N$; "!  You got them all!!!"
PRINT "Promotions and commendations will be given immediately!"
WL=2
END
END SUB 'Win

'Device name data
3600 DATA "Engines","Sonar","Torpedos","Missiles","Maneuvering"
3610 DATA "Status","Headquarters","Sabotage","Converter"

'Direction data
6090 DATA -1,0,-1,1,0,1,1,1,1,0,1,-1,0,-1,-1,-1

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
COLOR 12: PRINT "  7 8 9";: COLOR 14: PRINT "  There are 8 directions to move in, and they are the same anytime you"
COLOR 12: PRINT "   \|/ ";: COLOR 14: PRINT "  are asked for a course.  For example, to move north, you would use"
COLOR 12: PRINT "  4-*-6";: COLOR 14: PRINT "  course #1.  The computer will also ask for an amount of power.  It"
COLOR 12: PRINT "   /|\ ";: COLOR 14: PRINT "  takes 100 units of power to move your sub 1 space.  Beware of"
COLOR 12: PRINT "  1 2 3";: COLOR 14: PRINT "  obstacles!  If you use more than 1000 units in a turn, there is an"
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
