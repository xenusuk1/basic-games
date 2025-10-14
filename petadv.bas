#lang "qb"

' Personal Computer World DEc 1981
' https://archive.org/details/PersonalComputerWorld1981-12/page/182/mode/2up
' loader & game program as one


FUNCTION FNR(X AS INTEGER) AS INTEGER
	FNR=INT(X*RND(1)+1)
END FUNCTION

FUNCTION Dice (N AS INTEGER, A AS INTEGER) AS INTEGER
	Dice = INT((RND(1)*N) +1) + A
END FUNCTION



DIM SHARED TM%(30)

' T% temp for room names on data load 
' then used for various stuff in room
' byte space converted to DIMs
'B1=826   'start of byte space
'B2=8070
'B3=8130

'Const for Items, Monsters, Rooms, Doors

CONST NI=28 , NM=30 , NR=26, ND=16, NA=12

DIM SHARED AS INTEGER I,J,L,X,K,P,N,T,Q,E,U,Y,V

DIM SHARED B1(NR,6), B2(NM,2), B3(NI,2), B4(NR,5)  AS INTEGER

'descs held in string arrays
DIM SHARED M$(NM), I$(NI), D$(ND), R$(NR), C$(12)

' 2 dimensional arrays used to hold original bytes, simplifies logic
' string arrays to hold descriptions, C$ array from lines 10-65

SUB Array
' test proc to check array content

' exits & room
PRINT "Exits"
FOR I = 1 TO NR
PRINT I,
FOR J = 1 TO 6
	PRINT B1(I,J),
NEXT J
	PRINT " "; R$(B1(I,6))
NEXT I

PRINT "Monsters"
FOR I = 1 TO NM
PRINT B2(I,1), B2(I,2), M$(I)
NEXT I

PRINT "Items"
'Item loc & value, name
FOR I=1 TO NI
	PRINT B3(I,1), B3(I,2), I$(I)
NEXT

PRINT "Loc", N, B2(1,2), R$(B1(N,6))

PRINT "Temp", T
FOR I = 1 to 9
PRINT TM%(I),
NEXT I

END SUB

'B1() 5 doors + room id, B2() Monster(Str/Loc), B3() locations of items

SUB ArrayDoors
PRINT "Doors"
FOR I = 1 TO NR
PRINT I,
FOR J = 1 TO 5
	PRINT B4(I,J); " "; D$(B4(I,J)),
NEXT J
	PRINT " "; R$(B1(I,6))
	
NEXT I

END SUB


REM Exits in B1
' Hold bytes in B1()
FOR I = 1 TO NR 
FOR J= 1 TO 5
	READ X
	B1(I,J)=X
NEXT J
NEXT I


' original door type was 3 bits randomised into 16 doors using
' multiples of 32 to door numbers in rooms >0, next room bottom 5 bits, door type = top 3 bits
' to fix the doors and remove random names, setup new dim B4() for doors to match exits & refactor code

'FOR I = 1 TO NR
'FOR J= 1 TO 5
'	X=B1(I,J)
'	IF X=0 OR X>NR THEN 220
'	Y=32*FNR(7)
'	B1(I,J)=X+Y
	' set the same door on the other side
'	FOR K= 5 TO 1
'		IF B1(I,K)=I THEN B1(I,K)=I+Y
'	NEXT K
'220 NEXT J
'NEXT I



'Tag 1 golden door by removing 32 multiples
'P=16+FNR(10)
'B1(P,1)=B1(P,1) AND 31

'set door # for each exit <> 0, 1  = golden door
FOR I = 1 TO NR
FOR J= 1 TO 5
	X=B1(I,J)
	IF X>0 AND X<=NR  AND B4(I,J)=0 THEN
	B4(I,J)=FNR(15)+1
	' set the same door on the other side
	' X is room through door, I is current room	
	FOR L = 1 TO 5
		IF B1(X,L)=I THEN B4(X,L)=B4(I,J)
	NEXT L
	
	END IF
NEXT J
NEXT I

'Exit Door
B4((16+FNR(10)),1)=1



REM Room Names
FOR I =1 TO NR
	TM%(I)=I
NEXT

'random room name swaps
FOR I= NR TO 2 STEP-1
	X=FNR(I)
	K=TM%(X)
	TM%(X)=TM%(I)
	TM%(I)=K
NEXT

' add room id to B1
FOR I=1 TO NR
	B1(I,6)=TM%(I)
NEXT


REM STRENGTHS of monsters in B2,1
' 1 is person
B2(1,1)=255

FOR I=2 TO NM
	B2(I,1)=170+FNR(70)
NEXT


REM LOCATIONS of monsters in B2,2

FOR I=1 TO NM
	B2(I,2)=FNR(NR)
NEXT


REM start room/monster of items in B3
' B3(x,1) = value  B3(x,2) = location
' in room if X < 64, on monster if X > 64, =64 you have it
' change logic to increase items on monsters

FOR I=1 TO NI
	X=64
	IF FNR(9)>3 THEN 
		X=64+FNR(NM)
	ELSEIF FNR(9)>3 THEN
		X=FNR(NR)
	ELSE
		X=64
	END IF
	B3(I,2)=X
NEXT


REM VALUES of items

FOR I=1 TO NI
	READ X
	B3(I,1)=X
NEXT



PRINT "LOADING FANTASY 2" 
' read desc strings

FOR I = 1 TO NM
	READ M$(I)
NEXT

FOR I = 1 TO NI
	READ I$(I)
NEXT I

FOR I = 1 TO ND
	READ D$(I)
NEXT

FOR I = 1 TO NR
	READ R$(I)
NEXT I

FOR I = 1 TO NA
	READ C$(I)
NEXT I

'N is current room
'B2(1,n) is You

N=B2(1,2)

L$=" "

PRINT
CALL Desc
'CALL SubWait
CALL SubItems
CALL Exits
CALL Health(1)
CALL SubYourMove

DO
PRINT

SELECT CASE FNR(4)
CASE 1
	CALL SubWait
	PRINT
	CALL Desc
	CALL SubItems
	CALL Exits
	CALL Health(1)
	CALL SubYourMove
CASE 2
'4000
	CALL MonsterSelect
CASE 3
'4000
	CALL MonsterSelect
CASE 4
'4800
	CALL RandomAction
CASE 5
'4700
	CALL MonsterArrive
END SELECT

LOOP UNTIL B2(1,1)<1

CALL SubEnd

END

' Main Loop
'1500 ON FNR(5) GOSUB 1700,4000,4000,4800,4700




'Procs

SUB Instructions
PRINT "You are trapped in a giant maze from which the only escape is a golden door."
PRINT "If you’re going to escape, however, you must first have treasure to the value of 1000 gold pieces."
PRINT "If you can do this, you win the game. Treasure is to be found by searching the"
PRINT "rooms that you visit in the maze or by attacking others in the maze and forcing them to drop what they have."
PRINT "When you are attacked, your strength diminishes and can only be restored by the First aid room."
PRINT "The computer asks you which action you want to take." 
PRINT "You respond with a number corresponding to one of the commands used in the game."
PRINT "Any other response will give you a list of the options available to you."
END SUB

SUB SubEnd
' gosub 5000, 6000

IF B2(1,1)<1 THEN
	PRINT "YOU DIED OF YOUR WOUNDS. NEXT TIME LOOK FOR FIRST AID OR KEEP OUT OF FIGHTS"
	CALL SubItems
	END
ELSE
T=0
FOR I = 1 TO N
	IF B3(I,2)=64 THEN T=T+B3(I,1)
NEXT

IF T<1000 THEN
	PRINT"YOU CAN'T-YOUR TREASURE IS ONLY WORTH ";T;" GOLD PIECES"
ELSE
	PRINT"THE GOLDEN DOOR OPENS!!!"
	PRINT"YOU HAVE TREASURE TO THE VALUE OF "; T;"GOLD PIECES-YOU ARE A HERO!!"
	END
ENDIF 'T<1000

END IF 'B2<1

END SUB


SUB SubYourMove
' GOSUB 1700
DO
INPUT "YOUR ACTION M/S/T/D/E/I/H/O/A (1-9)";Q
SELECT CASE Q
CASE -1
	STOP
CASE 0
	CALL Array
	CALL ArrayDoors
CASE 1
	CALL Move
CASE 2
	CALL Search
CASE 3
	CALL Take
CASE 4
	CALL Drop
CASE 5
	CALL Exits
CASE 6
	CALL SubItems
CASE 7
	CALL Health(1)
CASE 8
	CALL Desc
	
CASE 9
	CALL YourAttack
	
CASE ELSE
	PRINT"VOUR OPTIONS"
	PRINT"1.MOVE "
	PRINT"2.SEARCH ROOM "
	PRINT"3.TAKE ITEM "
	PRINT"4.DROP ITEM " 
	PRINT"5.CHECK EXITS "
	PRINT"6.CHECK ITEMS"
	PRINT"7.CHECK HEALTH"
	PRINT "8.CHECK OCCUPANTS "
	PRINT"9.ATTACK " 
END SELECT
LOOP UNTIL Q>0 AND Q<10

'CALL SubWait

END SUB

SUB Move
'GOSUB 2000
' random player drop
'P=N :N=64 : GOSUB 300: N=P
CALL SubItemList(64) 
IF T>FNR(14) THEN
	V=0
	CALL MonsterDrop(1)
END IF 

'pick exit
CALL Exits

DO
INPUT "WHICH EXIT # ";Q
LOOP UNTIL Q>0 AND Q<=T

M=TM%(Q)
R=Q

' check for golden door
IF B4(N,TM%(Q))=1 THEN
	CALL SubEnd
ELSE
D=TM%(Q)
' check too weak to open door
IF B2(1,1)<FNR(55+5*D) THEN 
	PRINT"VOU'RE TOO WEAK"
ELSE

' move ok
	N=TM%(Q)
	B2(1,2) = N
'	G=0
	PRINT"VOU HAVE ENTERED A ";R$(B1(N,6))
	PRINT

	IF B1(N,6)=1 THEN
		PRINT "YOU HAVE BEEN HEALED ' !"
		PRINT "YOU ARE BACK TO FULL STRENGTH"
		B2(1,1) = 255
	END IF  'B1
	CALL Desc
	CALL Exits
	CALL SubWait

END IF 'Not weak
END IF 'golden
END SUB

SUB Search
'GOSUB 2300
G=1 
PRINT " YOU FIND-"
CALL SubItemList(N)
IF T=0 THEN
  PRINT "NOTHING!!'"
ELSE
FOR J=1 TO T
'	E=TM%(J)
'	Z$=I$(E)
	PRINT J;"A ";I$(TM%(J))
NEXT
END IF
END SUB

SUB Take
'GOSUB 2400
CALL Search

IF T=0 THEN
	PRINT "NOTHING FOUND TO TAKE!"
ELSE
	DO
		INPUT " TAKE ITEM# ", Q
	LOOP UNTIL Q>0 AND Q<=T
	E=TM%(Q)
	B3(E,2)=64
	PRINT"VOU HAVE PICKED UP A ";I$(E)
END IF

END SUB

SUB Drop
'GOSUB 2500

CALL SubItems

IF T<1 THEN
	PRINT " VOU HAVE NOTHING."
ELSE
	DO
		INPUT;"DROP ITEM# ", Q
	LOOP UNTIL Q>0 AND Q<=T
	E=TM%(Q)
	B3(E,2)=N
	PRINT" A "; I$(E); " HAS BEEN DROPPED"
END IF

END SUB

SUB Exits
' GOSUB 2600

PRINT"THE EXITS ARE"

'CALL ExitList
' List doors
'put adj rooms in TM%
T=0
FOR I=1 TO 5
	IF B1(N,I) > 0 THEN
		T=T+1
		TM%(T)=B1(N,I)
		PRINT I; "A "; D$(B4(N,I))
	END IF
NEXT I

END SUB

SUB EvalItem
'GOSUB 2800

END SUB

SUB YourAttack
'GOSUB 2200
'Select MOnster
CALL RoomMonsters

IF T=1 THEN
	PRINT"THERE'S NOBODY THERE!!»"
ELSE
	FOR J=2 TO T
		V=TM%(J)
		IF V>99 THEN V=V-99
		Z$=M$(V)
		IF TM%(J)>99 THEN Z$=Z$+" <DEAD> "
		PRINT J;Z$
NEXT

	DO
	INPUT "ATTACK WHO # ", Q
	LOOP UNTIL Q>=1 AND Q<=T
'	P=0
'	X=0
'	Y=TM%(Q)
	IF B2(TM%(Q),1) > 0 THEN
	CALL Strike(1, TM%(Q))
	'Strke back
	IF  FNR(9)>3 AND B2(TM%(Q),1) >= 40 THEN
		CALL Strike(TM%(Q),1)
	END IF
	ELSE
		PRINT "ALREADY DEAD!"
	END IF
END IF

END SUB

SUB MonsterSelect

CALL RoomMonsters
IF T>1 THEN
'pick monster & action

SELECT CASE FNR(1)
CASE 1,2
	' Attack
	CALL MonsterAttack(Dice(T-1,1))
CASE 3
' 4300 Leave
	CALL MonsterLeave(Dice(T-1,1))
CASE 4
'4400 Pickup
	CALL MonsterPick(Dice(T-1,1))
CASE 5
'4500 Drop
CALL MonsterDrop(Dice(T-1,1))

END SELECT

END IF  'T>1

END SUB


SUB MonsterAttack(A AS INTEGER)
' GOSUB 4000
' calc D <=T not A
DEFINT D

DO
	D = FNR(T)
LOOP UNTIL A<>D

'PRINT A,TM%(A),D,TM%(D)

CALL Strike(TM%(A),TM%( D))

IF  FNR(9)>3 AND B2(TM%(D),1) >= 40 THEN
	CALL Strike(TM%(D),TM%(A))
END IF
	
END SUB


SUB MonsterLeave(A AS INTEGER)
' GOSUB 4300

END SUB


SUB MonsterPick(A AS INTEGER)
' GOSUB 4400

END SUB

SUB MonsterDrop(A AS INTEGER)
' GOSUB 4500
' pass monster #

END SUB


SUB MonsterArrive

END SUB

SUB RandomAction

IF FNR(9) <=2 THEN
	C=FNR(12)
	CALL RoomMonsters

'	IF T=1 THEN V=1

	IF V=1 THEN
		PRINT "YOU HAVE "+C$(C) 
	ELSE
		PRINT M$(TM%(V))+ " HAS " +C$(C)
	END IF

	B2(V,1)=B2(V,1)-FNR(C+8)
'	P=1
'	Y=V
	CALL Health(TM%(V))
'PRINT T,V
END IF 'FNR

END SUB


SUB Strike(A AS INTEGER,D AS INTEGER)

' A attacks B
' GOSUB 4040
' damage = 9+hp/9 * 1.6+(D=1) extra dam from YOU - miss if < 9
U=FNR(INT( (9+B2(A,1)/9) *(2.6-(60*RND(1)>.95))))
' U=FNR((9+(PEEK(B2+2*X+1)) /9 ) * (1.6+ (Y=0) -60*RND(1)>.95 ))

'PRINT "DAM: ";U
BEEP

SELECT CASE U
CASE IS <9
	B$=" MISSED "
	U=0
CASE IS <33
	B$=" INFLICTED A LIGHT WOUND ON "
CASE IS <50
	B$=" MADE A VICIOUS ASSAULT ON "
CASE IS >=50
	B$=" DEALT A MIGHTY BL0W TO "
END SELECT

PRINT M$(A);B$;M$(D)

IF U>0 THEN 
' apply Damage
	B2(D,1)=B2(D,1)-U

	CALL Health(D)

'Drop all items if dead
	IF B2(D,1)<0 THEN
		B2(D,1)=0
		B2(D,2)=N+99

		FOR I=1 TO NI
			IF B3(I,2)=D+64 THEN
				B3(I,2)=N
				PRINT M$(D); " DROPPED SOMETHING"
			END IF
		NEXT
	END IF
END IF  'U>0


END SUB


SUB Health ( H AS INTEGER)
' GOSUB 4100
SELECT CASE H
CASE 1
	PRINT "YOU ARE ";
CASE IS <17
	PRINT "HE IS ";
CASE IS <24
	PRINT "SHE IS ";
CASE IS >23
	PRINT "IT IS ";
END SELECT

SELECT CASE B2(H,1)
CASE 255
	PRINT "FULLY FIT"
CASE IS  >=170
	PRINT "NOT BADLY HURT"
CASE IS  >=130
	PRINT "STILL O.K."
CASE IS >=75
	PRINT "SLIGHTLY WOUNDED"
CASE IS >=40
	PRINT "SERIOUSLY INJURED"
CASE IS >0
	PRINT "CRAWLING ON THE GROUND!!"
CASE IS <=0
	PRINT "DEAD"
END SELECT


'PRINT "HEALTH ", B2(H,1)

END SUB

SUB ExitList
'GOSUB 200

END SUB


SUB Desc
' GOSUB 2900
PRINT "YOU ARE IN A "; R$(B1(N,6))

PRINT"IN THE ROOM IS "
' look for monsters in room
' T is count TM%() holds number max 9

CALL RoomMonsters

' Look for dead monsters in room - loc+99

N=N+99
FOR I=2 TO NM
	IF B2(I,2)=N THEN
		T=T+1
		TM%(T)=I+99
	END IF
	IF T=9 THEN I=NM
NEXT I

N=N-99

IF T=1 THEN
	PRINT "NO-ONE.VOU'RE QUITE ALONE"
ELSE
' Print list of monsters
FOR J=1 TO T
	V=TM%(J)
	IF V>99 THEN V=V-99
' GOSUB 720
	Z$=M$(V)
	IF TM%(J)>99 THEN Z$=Z$+" <DEAD> "
	PRINT J;Z$
NEXT
END IF

END SUB

SUB RoomMonsters
'GOSUB 100

T=0
FOR I=1 TO NM
	IF B2(I,2)=N THEN
		T=T+1
		TM%(T)=I
	END IF
'	IF T=9 THEN I=NM
NEXT
'V returns random monster in room
V=FNR(T)

END SUB

SUB SubWait
'GOSUB 400
PRINT "PRESS SPACE TO CONTINUE"
DO
LOOP WHILE INKEY$ = ""

END SUB

SUB MonsterName
'GOSUB 720
	Z$=M$(V)
END SUB

SUB ItemName
'GOSUB 700
Z$=I$(E)
END SUB

SUB SubItems
'GOSUB 2700

PRINT"VOU POSSESS : "
'P=N
'N=64
CALL SubItemLIst(64)
'N=P
IF T=0 THEN
  PRINT "NOTHING!!'"
ELSE
FOR J=1 TO T
	E=TM%(J)
	Z$=I$(E)
	PRINT J;" A ";I$(TM%(J));" WORTH";B3(TM%(J),1);" GOLD PIECES"
NEXT
END IF

END SUB

SUB SubItemList ( IL AS INTEGER)
'GOSUB 300
'sets TM% as current itemlist of T items
T=0
FOR I = 1 TO NI
	IF B3(I,2)=IL THEN
		T=T+1
		TM%(T) = I
	END IF
'	IF T=9 THEN I=NI
NEXT

U=TM%(FNR(T))
END SUB

SUB PlayNote (N AS INTEGER)
BEEP
END SUB


'room doors
DATA 2,8,22,0,0
DATA 1,3,0,0,0
DATA 2,11,20,24,0
DATA 5,12,18,22,0
DATA 4,10,22,0,0 
DATA 7,16,0,0,0
DATA 6,8,21,22,0
DATA 1,7,19,0,0
DATA 13,14,0,0,0
DATA 5,23,24,0,0 
DATA 3,12,18,21,26
DATA 4,11,0,0,0
DATA 9,14,16,0,0
DATA 9,13,15,0,0
DATA 14,19,24,25,0
DATA 6,13,0,0,0
DATA 21,18,18,23,0
DATA 4,11,17,17,19
DATA 15,8,18,23,25
DATA 26,3,0,0,0 
DATA 7,11,17,0,0
DATA 4,1,5,7,26
DATA 17,10,19,0,0
DATA 10,3,15,25,25
DATA 15,19,24,24,0 
DATA 20,11,22,0,0

'items
DATA 250,220, 180, 160, 100, 50, 120, 2,90,40,0,160,10,4,0
DATA 15, 100,125,0,0,45,5,3,90,140,0,30,0

'some PET byte code not used
'DATA 169,32,162,240,157,119, 128, 157, 103,129,202,208,247,96
'DATA 169,32,162,200,157,87, 130, 157,31,131,202,208,247,96,-1

'Monsters
DATA YOU,SARGON,ATTILA THE HUN,DARTH VADER,COLIN THE CAMEL,SUPERMAN,IGOR
DATA STEVE ZODIAC,HISSING SID,BIGGLES,GOLIATH,KERMIT THE FROG,MR.WOO
DATA THE LONE RANGER,RICHARD III,COUNT DRACULA,JOHN OF GAUNT
DATA SIR JASPER,THE WITCH OF AGNESI,JOAN OF ARC,THE MERRY WIDOW,SUE ELLEN
DATA ESKIMO NELL,JULIE ANDREWS,IOLANTHE,THE MAGIC COW,PILTDOWN MAN
DATA THE THING FROM THE DEEP,THE INVISIBLE WOMBLE,IT CAME FROM SPACE

'Items
DATA PLATINUM BAR,BOX OF GEMS,PICASSO PAINTING,MING VASE,SILVER SALVER
DATA LIFE OF SHAKESPEARE,GOLDEN HARE,MAP OF WIGAN,FUR COAT
DATA MONA LISA(FORGERY),STUFFED WEASEL,SACK OF MONEY,CLARINET,RUSTY SABRE
DATA CAN OF COLA,FLOPPY DISC,PENNY BLACK,DIAMOND TIARA,LUMP OF CHEESE
DATA USED TEABAG,CUP FINAL TICKET,FLORAL TIE,SINCLAIR ZX81,G0LD WATCH
DATA PEARL NECKLACE,DAILY MIRROR,PAIR OF EARRINGS,BOX OF PAPERCLIPS

'Doors
DATA GOLDEN DOOR,VELVET CURTAIN,TALL ARCHWAY,SMALL TIMBER DOOR,TRAPDOOR
DATA WOODEN DOOR,LOOSE GRILLE,HOLE IN THE WALL,RED DOOR
DATA PANELLED OAK DOOR,NARROW SHAFT,FLIGHT OF STEPS,RUSTED METAL DOOR
DATA STEEL DOOR,HEAVY IRON GATE,SOLID STONE PORTAL

'Rooms
DATA FIRST AID ROOM,LONG DARK TUNNEL,SECRET PASSAGE,LIBRARY,COLD PANTRY
DATA LARGE SQUARE ROOM,DRAUGHTY CORRIDOR,DIMLY LIT PASSAGE,DUSTY CHAPEL
DATA BIG BOXROOM,LONG GALLERY,HALLWAY,DINING ROOM,CONSERVATORY,CELLAR
DATA ROOM WITH GREEN WALLS,DIRTY KITCHEN,WINDOWLESS CELL,DISUSED ATTIC
DATA BANQUETING HALL,PANELLED STUDY,LOFTY TURRET,SUMPTUOUS BEDCHAMBER
DATA TILED BATHROOM,ROOM WITH NO CARPET,HUGE LOUNGE

'Random Event
DATA "SPRAINED AN ANKLE"
DATA "TRIPPED OVER A BRICK"
DATA "BEEN STUNG BY A BEE"
DATA "BEEN ATTACKED BY VRMPIRE BATS"
DATA "BEEN BITTEN BY A MAD DOG"
DATA "BEEN PLAGUED BY KILLER MOTHS"
DATA "STEPPED ON A RAKE"
DATA "CONTRACTED A BAD COUGH"
DATA "STUMBLED TO THE GROUND"
DATA "HAD A BAD FALL"
DATA "SLIPPED ON A BANANA SKIN"
DATA "BUMPED INTO A WALL"

