$ERROR 	ALL-
$LIB 		ALL-
$OPTION	CNTLBREAK-, GOSUB-

$FLOAT PROCEDURE 'NPX (CoPro present),EMULATE, PROCEDURE (larger but faster)
$CPU 80286			'Alt: 8086, 80286, 80386
$OPTIMIZE SPEED 'Alt SIZE

CDProg$="TECOD"
DEFINT A-Z
%TE=-1					:%TK=0				:%TC=0				:%TK5=0
%LenBite=10000	:%CfgItems=12	:%LenData=36

$INCLUDE "BIOS.INC"
$INCLUDE "CHKERR.INC"
$INCLUDE "CODSUBS.INC"
$INCLUDE "DOS.INC"
$INCLUDE "ERRAS.INC"
$INCLUDE "KONST.INC"
$INCLUDE "SCREEN.INC"

$STACK 3072

DIM KeyFun$(1:20),DR$(1:12)
DIM FileEntry$(140),FileLen&(140)
DIM Tag$(140),TagList$(140)

PW=%FALSE:Jump=0:StartDir$=CURDIR$

ON ERROR GOTO ABORT
OPEN "A:\"+CDProg$+".CNF" FOR INPUT AS #1
FOR L=1 TO %CfgItems
	LINE INPUT #1,DR$(L)
	IF L=%CfgItems THEN DR$(L)=CODEKEY$(DR$(L),0)
NEXT L
CLOSE

SETPARAMS
READKEYS KeyFun$()

IF PASSWORD=%FALSE THEN
	CLS:END
ELSE
	PW=%TRUE
END IF

PRINT BORDER$;

MENU:
	DO
		BF=0:M=0
		KeyOn=%TRUE:NoDraw=%FALSE:Tagable=%TRUE
		Title$="":WhichFiles$="*.*":EncState$="off)"
		ON ERROR GOTO ERMESG
		IF Jump THEN
			IF Jump<>%F10 THEN Drive$=RTRIM$(CURDIR$,"\")
			FromMenu=%FALSE
		ELSE
			Drive$=RAM$
			FromMenu=%TRUE
			Tagged=0
			IF FF>70 THEN LOCATE 16,80:PRINT CHR$(182);
		END IF
		IF FromMenu THEN
			HL=0:LH=0:XF=%FALSE:XFH=%FALSE:XHL=0
			PRINT TOPLINE$;
			CLEARWINDOW %TopLeft1,%BottomRight1
			CLEARWINDOW %TopLeft3,%BottomRight3
			COLOR BWYellow,%Cyan:PRINT CENTRE$(2," C O D E R ");
			COLOR %Yellow,Paper :PRINT CENTRE$(4,"Level "+Level$);
			COLOR BWYellow,%Cyan:PRINT CENTRE$(6," MAIN MENU ");
			COLOR ,Paper
			Y=8:X=29
			RESTORE MENU
			FOR ROW=1 TO 13
				LOCATE Y,X
				COLOR %LCyan
				READ MenuItem$:PRINT MenuItem$;
				COLOR Ink
				LOCATE Y,X+4
				READ MenuItem$:PRINT MenuItem$;
				INCR Y
			NEXT ROW
			DATA F1,": Copy files"
			DATA F2,": Rename/Move files"
			DATA F3,": Delete files"
			DATA " "," "
			DATA F4,": Encipher files"
			DATA F5,": Decipher files"
			DATA " "," "
			DATA F6,": Type a new document"
			DATA F7,": Edit/Read documents"
			DATA F8,": Configure"
			DATA F9,": Terminal"
			DATA " "," "
			DATA F10,": Quit"
			COLOR %Yellow
			LOCATE 23,3 :PRINT "ALT Menu: Alt F1 - F10";TAB(63);"Level: Alt 1 - 4";
			PRINT CENTRE$(25,"Version  8.02");
			COLOR Ink
			DO
				DO
					CH$=INKEY$
					IF CH$=CHR$(%Ent) THEN QUITTA
				LOOP WHILE LEN(CH$)<2
				M=ASC(RIGHT$(CH$,1))
				IF M>119 AND M<124 THEN
					Level=M-120
					Level$=LTRIM$(STR$(Level+1))
					COLOR %Yellow
					LOCATE 4,43:PRINT Level$;
				END IF
			LOOP UNTIL M=>%F1 AND M<=%F10 OR M=>%AltF1 AND M<=%AltF10
		END IF
		IF Jump THEN M=Jump
		SELECT CASE M
			CASE %F1,%F2				:COPYA M
			CASE %F3,%AltF3			:DELLA M
			CASE %F4						:ENCA
			CASE %F5,%F9				:DECA M
			CASE %F6,%F7				:EDITOR M
			CASE %F8,%AltF8			:CONFIG
			CASE %F10						:QUITTA
			CASE %AltF1					:MERJA
			CASE %AltF2					:WDISK
			CASE %AltF4,%AltF5	:COMPA M
			CASE %AltF6					:SAFESAV
			CASE %AltF7					:SAFELOD
			CASE %AltF9					:RUNNA
			CASE %AltF10				:DOS
		END SELECT
	LOOP
:
SUB Copya(M) SHARED
	IF M=%F1 THEN
		Heading$="COPY FILES"
		Action$="Copy":Doing$="Copy"
	ELSE
		Heading$="RENAME/MOVE FILES"
		Action$="Move":Doing$="Mov"
	END IF
	Title$=SPACE$(43)+"To: ["+SPACE$(27)+"]"
	DOSCREEN Heading$,Title$,Drive$,WhichFiles$
	DO
		DO
			BF=0:YN$=""
			PRINT TITEL$(Title$);
			NM$=GETNM$(Drive$)
			IF NM$="" THEN EXIT SUB
			IF DoTags THEN
				LOCATE %SelLine,3
				PRINT Action$+NM$+SPACE$(12);
			END IF
			OutFile$=OUTNM$(NM$,OutName$,OutPath$,M)
			IF OutFile$=CHR$(%Esc) THEN EXIT SUB
			IF OutPath$=RTRIM$(CURDIR$,"\") THEN
				IF NM$<>OutName$ THEN
					IF M=%F2 THEN
						NAME NM$ AS OutName$
						GOTO SKIP
					END IF
				ELSE
					BF=%NoCopy
				END IF
			END IF
			IF ERRA(BF) THEN EXIT SUB
		LOOP WHILE OutFile$="" OR BF
		FOR N=0 TO FF
			IF INSTAT THEN EXIT SUB
			IF DoTags THEN
				NM$=TagList$(N)
				OutName$=NM$
				OutFile$=OutPath$+"\"+OutName$
			END IF
			DO WHILE NM$<>""
				ExistFile=%FALSE:BF=0
				IF EXIST(OutPath$,OutName$,0) THEN
					ExistFile=%TRUE
					IF YN$<>"A" THEN
						YN$=YESNO$("Overwrite "+OutName$+" ? (Y/N/A) Y")
						IF YN$=CHR$(%Esc) THEN
							LOCATE ,,0:EXIT SUB
						ELSEIF YN$="N" THEN
							CLEARLINE %SelLine,77,1
							EXIT LOOP
						END IF
					END IF
					OPEN OutFile$ FOR BINARY AS #1
					LenFile&=LOF(1):CLOSE
				END IF
				IF ExistFile THEN
					IF (LenFile&-FileLen&(XHL))>FREESPACE???(OutPath$) THEN BF=%Full
				ELSE
					IF FileLen&(XHL)>FREESPACE???(OutPath$) THEN BF=%Full
				END IF
				IF BF THEN
					IF ERRA(BF) THEN EXIT SUB ELSE EXIT LOOP
				END IF
				CPRINT Doing$+"ing "+NM$+" to "+OutFile$+"...", 1
				SHELL "COPY "+NM$+" "+OutFile$+" > NUL"
				IF M=%F2 THEN
					WIPEFILE 1,NM$
				END IF
				EXIT LOOP
			LOOP
			IF DoTags=%FALSE THEN EXIT FOR
		NEXT N
		SKIP:
		IF M=%F2 THEN
			PRINT REFILES$(WhichFiles$);
		ELSEIF RTRIM$(CURDIR$,"\")=OutPath$ THEN
			PRINT REFILES$(WhichFiles$);
		END IF
	LOOP
END SUB
:
SUB Della(M) SHARED
	IF M=%F3 THEN
		Heading$="DELETE FILES"
		Action$="Delete ":Doing$="Deleting "
		WipeCount=1
	ELSE
		Heading$="WIPE FILES"
		Action$="Wipe ":Doing$="Wiping "
		WipeCount=Wipes
	END IF
	DOSCREEN Heading$,Title$,Drive$,WhichFiles$
	DO
		DO
			NM$=GETNM$(Drive$)
			IF NM$="" THEN EXIT SUB
			KillDone=%FALSE
			YN$=YESNO$(Action$+LTRIM$(NM$," ")+" ? (Y/N): Y")
			IF YN$=CHR$(%Esc) THEN EXIT SUB
		LOOP WHILE YN$="N" OR YN$="A"
		FOR N=0 TO FF
			IF INSTAT THEN EXIT SUB
			IF DoTags THEN
				NM$=TagList$(N)
			END IF
			IF NM$<>"" THEN
				CPRINT Doing$+NM$+"...", 1
				WIPEFILE WipeCount, NM$
				KillDone=%TRUE
			END IF
			IF DoTags=%FALSE THEN EXIT FOR
		NEXT N
		IF KillDone THEN PRINT REFILES$(WhichFiles$);
	LOOP
END SUB
:
SUB Enca SHARED
	Heading$="ENCIPHER FILES (Level: "+Level$+")"
	DOSCREEN Heading$,Title$,Drive$,WhichFiles$
	DO
		DO
			NM$=GETNM$(Drive$)
			IF NM$="" THEN EXIT SUB
			CLEARLINE %ActLine,50,0
			CLEARWINDOW %TopLeft3,%BottomRight3
			IF DoTags THEN
				CPRINT "Encipher"+NM$+SPACE$(15)+"Name for Output file: [        ]", 0
				OutFile$=GETSHRINK$
				IF OutFile$=CHR$(%Esc) THEN EXIT SUB
				IF OutFile$="" THEN
					TIDYUP
				END IF
			ELSE
				IF INSTR(NM$,".") THEN
					OutFile$=LEFT$(NM$,(INSTR(NM$,".")-1))
				ELSE
					OutFile$=NM$
				END IF
			END IF
		LOOP WHILE OutFile$=""
		DO
			BF=0
			IF Level=0 THEN
				LL=0:Page=0
				SendData$=GETENCKEY$(Page,LL)
				IF SendData$=CHR$(%Esc) THEN
					EXIT SUB
				ELSEIF SendData$="" THEN
					EXIT LOOP
				END IF
			ELSE
				CPRINT "Ready to encipher "+LTRIM$(NM$)+"", 0
				IF WARNDISK(DDPath$,"DATAPTR","DATA") THEN
					EXIT SUB
				END IF
				SM&=0:LenData=%LenData
				SendData$=GETDATA$(SM&,LenData,"SNUM")
				IF SendData$="" THEN
					IF ERRA(%NoData) THEN EXIT SUB ELSE EXIT LOOP
				END IF
			END IF
			StripKey$=REMOVE$(SendData$, ANY CHR$(0)+CHR$(13)+CHR$(32)+CHR$(34))
			IF LEN(StripKey$)> 20 THEN
				StripKey$=LEFT$(StripKey$,20)
			END IF
			ZipKey$=" -s"+CHR$(34)+StripKey$+CHR$(34)
			DOZIP M,DoTags,NM$,RAM$,OutFile$,OutName$,ZipKey$
			IF INSTAT THEN
				KILL OutFile$+".ZIP"
				EXIT SUB
			END IF
			OPEN OutFile$+".ZIP" FOR BINARY AS #1
			LenMsg&=LOF(1):CLOSE
			IF LenMsg&=0 THEN
				KILL OutFile$+".ZIP"
				IF ERRA(%BadZip) THEN EXIT SUB ELSE EXIT LOOP
			END IF
			IF Level>1 THEN
				IF Level=2 THEN
					MaxLength&=163750
					LenData=LenMsg&\5
				ELSE
					MaxLength&=32750
					LenData=LenMsg&
				END IF
				IF LenMsg&>MaxLength& THEN
					KILL OutFile$+".ZIP"
					IF ERRA(%TooLong) THEN EXIT SUB ELSE EXIT LOOP
				END IF
				CPRINT "Reading data...", 1
				IF NOT EXIST(DDPath$,"SNUM",0) THEN
					IF WARNDISK(DDPath$,"SNUM","DATA") THEN
						KILL OutFile$+".ZIP"
						EXIT SUB
					END IF
				END IF
				SendData$=GETDATA$(SM&,LenData,"SNUM")
				IF SendData$="" THEN
					IF ERRA(%NoData) THEN EXIT SUB ELSE EXIT LOOP
				END IF
			END IF
			IF DoTags THEN
				EncFile$=OutFile$
			ELSE
				EncFile$=NM$
			END IF
			OPEN OutFile$+".ZIP" FOR BINARY AS #1
			OPEN OutFile$+Ext$ FOR BINARY AS #2
			IF Level=0 THEN
				PageLN$=CHR$(Page)+CHR$(LL)
				SM&=CVI(PageLN$)
			ELSEIF Level=2 THEN
				LenData=0
			END IF
			SM$=STARTM$(Level,SM&)
			PUT$ #2,SM$
			CLEARLINE %ActLine,77,0
			LOCATE %ActLine,3:PRINT "File length:";LenMsg&+LEN(SM$);"characters";
			CPRINT "Enciphering "+EncFile$+"...", 1
			LongFile=CHKLEN(Repeat,Extra,LenMsg&)
			Seed=0
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						GET$ #1,Extra,MSG$
					ELSE
						GET$ #1,%LenBite,MSG$
					END IF
				ELSE
					GET$ #1,LenMsg&,MSG$
				END IF
				SELECT CASE Level
					CASE 0,1
						Encrypt$=ENCLEV2$(LN,MSG$,SendData$)
					CASE 2
						WipeCount=0
						Encrypt$=ENCLEV3$(LN,MSG$,SendData$,WipeCount)
						INCR LenData,WipeCount
					CASE 3
						Encrypt$=ENCLEV4$(MSG$,SendData$)
				END SELECT
				PUT$ #2,Encrypt$
				MSG$="":Encrypt$=""
			NEXT LN
			CLOSE:SendData$=""
			IF Level THEN
				IF NOT EXIST(DDPath$,"SNUM",0) THEN
					IF WARNDISK(DDPath$,"SNUM","DATA") THEN
						KILL OutFile$+".ZIP"
						EXIT SUB
					END IF
				END IF
				DISKFLUSH SM&,LenData,"SNUM"
			END IF
			KILL OutFile$+".ZIP"
			PRINT REFILES$(WhichFiles$);
		LOOP WHILE BF
		TIDYUP
	LOOP
END SUB
:
FUNCTION ENCLEV2$(LoopNo,Message$,EncData$)
	LOCAL KeyPos?
	LenMsg=LEN(Message$)
	Prev?=INITRAND(LoopNo,EncData$)
	DEF SEG=STRSEG(Message$)
	SP&=STRPTR(Message$)
	$EVENT OFF
	FOR EncLoop=1 TO LenMsg
		RL?=INT(RND*256)
		CD?=((PEEK(SP&)+RL?) AND &HFF) XOR Prev?
		POKE SP&,CD?
		Prev?=RL?
		IF RL? MOD 6=0 THEN
			KeyPos?=RL? MOD %LenData
			Prev?=ASC(MID$(EncData$,KeyPos?+1,1)) XOR RL?
			RANDOMIZE LoopNo+EncLoop+Prev?
		END IF
		SP&=SP&+1
	NEXT
	$EVENT ON
	ENCLEV2$=Message$
	DEF SEG
END FUNCTION
:
FUNCTION ENCLEV3$(LoopNo,Message$,EncData$,WC)
	LenMsg=LEN(Message$)
	Prev?=INITRAND(LoopNo,EncData$)
	DEF SEG=STRSEG(Message$)
	SP&=STRPTR(Message$)
	$EVENT OFF
	FOR EncLoop=1 TO LenMsg
		RL?=INT(RND*256)
		CD?=((PEEK(SP&)+RL?) AND &HFF) XOR Prev?
		POKE SP&,CD?
		Prev?=RL?
		IF RL? MOD 6=0 THEN
			Prev?=ASC(MID$(EncData$,WC+1,1))
			RANDOMIZE Prev?+EncLoop+LoopNo
			INCR WC
		END IF
		SP&=SP&+1
	NEXT EncLoop
	$EVENT ON
	ENCLEV3$=Message$
	DEF SEG
END FUNCTION
:
FUNCTION ENCLEV4$(Message$,EncData$)
	LenMsg=LEN(Message$)
	Prev?=ASC(RIGHT$(EncData$,1))
	DEF SEG=STRSEG(Message$)
	SP&=STRPTR(Message$)
	$EVENT OFF
	FOR EncLoop=1 TO LenMsg
		RL?=ASC(MID$(EncData$,EncLoop,1))
		CD?=((PEEK(SP&)+RL?) AND &HFF) XOR Prev?
		POKE SP&,CD?
		Prev?=RL?
		INCR SP&
	NEXT EncLoop
	$EVENT ON
	ENCLEV4$=Message$
	DEF SEG
END FUNCTION
:
FUNCTION INITRAND(Bite,KeyData$)
	SHARED Seed
	IF Bite=1 THEN
		FOR N=1 TO 20
			INCR Seed,ASC(MID$(KeyData$,N,1))
		NEXT N
	END IF
	Posn=20-((Bite-1) MOD 20)
	Seed=(Seed+ASC(MID$(KeyData$,Posn,1))+Bite MOD 32750)
	RANDOMIZE Seed
	INITRAND=INT(RND*256)
END FUNCTION
:
FUNCTION GETENCKEY$(Page,LL)
	LOCATE %SelLine,3,0
	PRINT "Page No: [   ]   Line No: [  ]   ";
	PRINT "Key: [";SPACE$(%LenData);"]";
	FOR N=1 TO 2
		DO
			BF=0
			IF N=1 THEN
				X=13:CRS=3
			ELSE
				X=30:CRS=2
			END IF
			LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(CRS-1);
			PgLn$=KEYA$(%SelLine,X,48,57,CRS)
			IF PgLn$="" OR PgLn$=CHR$(%Esc) THEN
				GETENCKEY$=PgLn$:EXIT FUNCTION
			END IF
			IF N=1 THEN
				Page=VAL(PgLn$)
				IF Page<1 OR Page>255 THEN
					BF=%BadPage
					IF ERRA(BF) THEN GETENCKEY$="":EXIT FUNCTION
				END IF
			ELSE
				LL=VAL(PgLn$)
				IF LL<1 OR LL>99 THEN
					BF=%BadLine
					IF ERRA(BF) THEN GETENCKEY$="":EXIT FUNCTION
				END IF
			END IF
		LOOP WHILE BF
	NEXT N
	DO
		X=42:CRS=%LenData
		LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(CRS-1);
		KeyWord$=KEYA$(%SelLine,X,32,126,CRS)
		IF KeyWord$="" OR KeyWord$=CHR$(%Esc) THEN
			GETENCKEY$=KeyWord$:EXIT FUNCTION
		END IF
		IF LEN(KeyWord$)<>CRS THEN
			SOUND 88,8
		ELSE
			EXIT LOOP
		END IF
	LOOP
	GETENCKEY$=KeyWord$
END FUNCTION
:
FUNCTION GETDATA$(SM&,LenKey,DataFile$)
	SHARED DDPath$,Ink
	NULL$=STRING$(3,CHR$(0))
	IF DataFile$="SNUM" AND SM&=0 THEN
		OPEN DDPath$+"\DATAPTR" FOR INPUT AS #1
		INPUT #1,SM&:CLOSE
	END IF
	IF SM&+LenKey>360000 THEN
		GETDATA$="":EXIT FUNCTION
	END IF
	OPEN DDPath$+"\"+DataFile$ FOR BINARY AS #1
	DO
		SEEK #1,SM&:GET$ #1,LenKey,KeyData$
		PosBad=INSTR(KeyData$,NULL$)
		IF DataFile$="SNUM" THEN
			IF PosBad THEN
				SM&=SM&+PosBad+10
			ELSE
				EXIT LOOP
			END IF
		ELSE
			IF PosBad THEN
				KeyData$=""
			END IF
			EXIT LOOP
		END IF
	LOOP
	CLOSE
	GETDATA$=KeyData$
END FUNCTION
:
FUNCTION STARTM$(Level,SM&)
	RANDOMIZE TIMER
	StartRand=INT(RND*256)
	RANDOMIZE StartRand
	SM$=MKL$(SM&)
	FOR N=1 TO 4
		RC=INT(RND*256)
		SELECT CASE N
			CASE 1:Char1$=CHR$(ASC(LEFT$(SM$,1)) XOR RC)
			CASE 2:Char2$=CHR$(ASC(MID$(SM$,2,1)) XOR RC)
			CASE 3:Char3$=CHR$(ASC(MID$(SM$,3,1)) XOR RC)
			CASE 4:Lev=Level XOR RC:Char4$=CHR$(Lev)
		END SELECT
	NEXT N
	STARTM$=CHR$(StartRand)+Char1$+Char2$+Char3$+Char4$
END FUNCTION
:
SUB DiskFlush(SM&,BytesToWipe,DataFile$)
	SHARED DDPath$
	IF BytesToWipe=0 THEN EXIT SUB
	DataLeft$=LTRIM$(STR$(CEIL(((SM&+BytesToWipe)/360000)*100)))
	LOCATE %ActLine,3,0:PRINT "Data disk ";DataLeft$;" per cent used                  ";
	CPRINT "Wiping used data...", 1
	WipeData$=STRING$(BytesToWipe,0)
	OPEN DDPath$+"\"+DataFile$ FOR BINARY AS #1
	SEEK #1,SM&
	PUT$ #1,WipeData$
	CLOSE
	IF DataFile$="SNUM" THEN
		SM&=SM&+BytesToWipe
		OPEN DDPath$+"\DATAPTR" FOR OUTPUT AS #1
		PRINT #1,SM&
		CLOSE
	END IF
END SUB
:
SUB Deca(M) SHARED
	IF M>%F5 THEN
		TERMINAL
		Drive$=RAM$
		CHPATH Drive$
		Jump=0
	END IF
	Heading$="DECIPHER FILES"
	DOSCREEN Heading$,Title$,Drive$,WhichFiles$
	DO
		KeyOn=%TRUE:Tagable=%FALSE
		BF=0:Header=5:Ver=0
		NM$=GETNM$(Drive$)
		IF NM$="" THEN EXIT SUB
		OPEN NM$ FOR BINARY AS #1
		LenCgm&=LOF(1):GET$ #1,Header,Ver$:CLOSE
		DECR LenCgm&,Header
		SM&=READSM&(Ver$,Ver)
		IF Ver>3 THEN
			BF=%BadFile
			IF ERRA(BF) THEN EXIT SUB
		END IF
		KeyOn=%FALSE
		CLEARWINDOW %TopLeft3,%BottomRight3
		CLEARLINE %ActLine,50,0
		SELECT CASE Ver
			CASE 0
				RecData$=GETDECKEY$(SM&)
				IF RecData$=CHR$(%Esc) THEN EXIT SUB
				IF RecData$="" THEN BF=%TRUE
			CASE 1,2,3
				IF WARNDISK(DDPath$,"RNUM","DATA") THEN EXIT SUB
				SELECT CASE Ver
					CASE 1:LenData=%LenData
					CASE 2:LenData=LenCgm&\5
					CASE 3:LenData=LenCgm&
				END SELECT
				RecData$=GETDATA$(SM&,LenData,"RNUM")
				IF RecData$="" THEN
					BF=%DecDone
					IF ERRA(BF) THEN EXIT SUB
				END IF
		END SELECT
		DO WHILE BF=0
			CLEARLINE %ActLine,77,0
			LOCATE %ActLine,3:PRINT "File length:";LenCgm&;"characters";
			CPRINT "Deciphering "+LTRIM$(NM$," ")+" (Level "+CHR$(49+Ver)+")...", 1
			IF INSTR(NM$,".")THEN
				DNM$=LEFT$(NM$,(INSTR(NM$,".")-1))+".ZIP"
			ELSE
				DNM$=NM$+".ZIP"
			END IF
			OPEN NM$ FOR BINARY AS #1
			OPEN DNM$ FOR BINARY AS #2
			SEEK #1,Header
			LongFile=CHKLEN(Repeat,Extra,LenCgm&)
			Seed=0
			IF Ver=2 THEN LenData=0
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						GET$ #1,Extra,CGM$
					ELSE
						GET$ #1,%LenBite,CGM$
					END IF
				ELSE
					GET$ #1,LenCgm&,CGM$
				END IF
				SELECT CASE Ver
					CASE=0,1
						Decrypt$=DECLEV2$(LN,CGM$,RecData$)
					CASE=2
						WipeCount=0
						Decrypt$=DECLEV3$(LN,CGM$,RecData$,WipeCount)
						INCR LenData,WipeCount
					CASE=3
						Decrypt$=DECLEV4$(CGM$,RecData$)
				END SELECT
				PUT$ #2,Decrypt$
				CGM$="":Decrypt$=""
			NEXT LN
			CLOSE
			IF NOT CHKCOMP(DNM$) THEN
				KILL DNM$
				BF=%Corrupt
				IF ERRA(BF) THEN EXIT SUB ELSE EXIT LOOP
			END IF
			IF Ver THEN
				IF NOT EXIST(DDPath$,"RNUM",0) THEN
					IF WARNDISK(DDPath$,"RNUM","DATA") THEN
						KILL DNM$
						EXIT SUB
					END IF
				END IF
				DISKFLUSH SM&,LenData,"RNUM"
			END IF
			IF FD1 THEN
				IF WARNDISK(UTPath$,ProgName$,"PROGRAM") THEN EXIT SUB
			END IF
			CPRINT "Decompressing "+NM$+"...", 1
			StripKey$=REMOVE$(RecData$, ANY CHR$(0)+CHR$(13)+CHR$(32)+CHR$(34))
			IF LEN(StripKey$)> 20 THEN
				StripKey$=LEFT$(StripKey$,20)
			END IF
			ZipKey$=CHR$(34)+StripKey$+CHR$(34)
			SHELL UTPath$+"\PKUNZIP -o -s"+ZipKey$+" "+DNM$+" > NUL"
			KILL DNM$
			PRINT REFILES$(WhichFiles$);
			EXIT LOOP
		LOOP
		TIDYUP
	LOOP
END SUB
:
FUNCTION DECLEV2$(LoopNo,CG$,DecData$)
	LOCAL KeyPos
	LenCG=LEN(CG$)
	Prev?=INITRAND(LoopNo,DecData$)
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	$EVENT OFF
	FOR DecLoop=1 TO LenCG
		RL?=INT(RND*256)
		PK?=PEEK(SP&)
		PL?=((PK? XOR Prev?)-RL?) AND &HFF
		POKE SP&,PL?
		Prev?=RL?
		IF RL? MOD 6=0 THEN
			KeyPos=RL? MOD %LenData
			Prev?=ASC(MID$(DecData$,KeyPos+1,1)) XOR RL?
			RANDOMIZE LoopNo+DecLoop+Prev?
		END IF
		SP&=SP&+1
	NEXT DecLoop
	$EVENT ON
	DECLEV2$=CG$
	DEF SEG
END FUNCTION
:
FUNCTION DECLEV3$(LoopNo,CG$,DecData$,WC)
	LenCG=LEN(CG$)
	Prev?=INITRAND(LoopNo,DecData$)
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	$EVENT OFF
	FOR DecLoop=1 TO LenCG
		RL?=INT(RND*256)
		PK?=PEEK(SP&)
		PL?=((PK? XOR Prev?)-RL?) AND &HFF
		POKE SP&,PL?
		Prev?=RL?
		IF RL? MOD 6=0 THEN
			Prev?=ASC(MID$(DecData$,WC+1,1))
			RANDOMIZE Prev?+DecLoop+LoopNo
			INCR WC
		END IF
		INCR SP&
	NEXT DecLoop
	$EVENT ON
	DECLEV3$=CG$
	DEF SEG
END FUNCTION
:
FUNCTION DECLEV4$(CG$,DecData$)
	LenCG=LEN(CG$)
	Prev?=ASC(RIGHT$(DecData$,1))
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	$EVENT OFF
	FOR DecLoop=1 TO LenCG
		RL?=ASC(MID$(DecData$,DecLoop,1))
		PK?=PEEK(SP&)
		PL?=((PK? XOR Prev?)-RL?) AND &HFF
		POKE SP&,PL?
		Prev?=RL?
		INCR SP&
	NEXT DecLoop
	$EVENT ON
	DECLEV4$=CG$
	DEF SEG
END FUNCTION
:
FUNCTION READSM&(SM$,Level)
	StartRand=ASC(LEFT$(SM$,1))
	RANDOMIZE StartRand
	FOR N=1 TO 4
		RC=INT(RND*256)
		SELECT CASE N
			CASE 1:Char1$=CHR$(ASC(MID$(SM$,2,1)) XOR RC)
			CASE 2:Char2$=CHR$(ASC(MID$(SM$,3,1)) XOR RC)
			CASE 3:Char3$=CHR$(ASC(MID$(SM$,4,1)) XOR RC)
			CASE 4:Level$=RIGHT$(SM$,1)
		END SELECT
	NEXT N
	Char4$=CHR$(0)
	Level=ASC(Level$) XOR RC
	READSM&=CVL(Char1$+Char2$+Char3$+Char4$)
END FUNCTION
:
FUNCTION GETDECKEY$(SM&)
	SHARED Ink
	PageLN$=MKL$(SM&)
	Page=ASC(LEFT$(PageLN$,1))
	LNE=ASC(MID$(PageLN$,2,1))
	IF Page<1 OR Page>255 OR LNE<1 OR LNE>99 THEN
		IF ERRA(%Corrupt) THEN GETDECKEY$=CHR$(%Esc) ELSE GETDECKEY$=""
		EXIT FUNCTION
	END IF
	CPRINT "Page No:", 0
	COLOR %Yellow:PRINT Page;
	COLOR Ink:PRINT TAB(17);" Line No:";
	COLOR %Yellow:PRINT LNE;TAB(35);
	COLOR Ink
	PRINT "Key: [";SPACE$(%LenData);"]";
	DO
		X=41
		LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(35);
		KeyWord$=KEYA$(%SelLine,X,32,126,%LenData)
		IF KeyWord$="" OR KeyWord$=CHR$(%Esc) THEN
			GETDECKEY$=KeyWord$:EXIT FUNCTION
		END IF
		IF LEN(KeyWord$)<%LenData THEN
			SOUND 88,8
		ELSE
			EXIT LOOP
		END IF
	LOOP
	GETDECKEY$=KeyWord$
END FUNCTION
:
SUB Editor(M) SHARED
	IF M=%F6 THEN
		Heading$="TYPE A NEW DOCUMENT"
		NoDraw=%TRUE:KeyOn=%FALSE:Jump=0
		CLEARWINDOW %TopLeft3,%BottomRight3
		DOSCREEN Heading$,Title$,Drive$,WhichFiles$
		LOCATE %SelLine,3,0:COLOR Ink,Paper
		PRINT "Document name: [";SPACE$(51);"]";
		OutFile$=OUTNM$(NM$,OutName$,OutPath$,M)
		IF OutFile$="" OR OutFile$=CHR$(%Esc) THEN EXIT SUB
		EDIT OutPath$,OutName$
		Drive$=(OutPath$)
		NoDraw=%FALSE:KeyOn=%TRUE:FromMenu=%FALSE
	END IF
	Tagable=%FALSE
	DO
		Heading$="EDIT/READ FILES"
		DOSCREEN Heading$,Title$,Drive$,WhichFiles$
		NM$=GETNM$(Drive$)
		IF NM$="" THEN EXIT SUB
		EDIT Drive$,NM$
		PRINT FUNKEY$;
	LOOP
END SUB
:
SUB EDIT(Path$,NM$)
	SHARED EdPath$,EdProg$
	ProgName$=PROG$(EdProg$)
	IF WARNDISK(EdPath$,ProgName$,"PROGRAM") THEN EXIT SUB
	LOCATE %SelLine,3,0
	CHPATH Path$
	SHELL EdPath$+"\"+EdProg$+" "+Path$+"\"+NM$
	PRINT BORDER$;
END SUB
:
SUB Terminal SHARED
	IF FD1 THEN
		IF WARNDISK(TXPath$,PROG$(TXProg$),"PROGRAM") THEN EXIT SUB
	END IF
	IF FromMenu THEN
		PRINT TOPLINE$;
		CLEARWINDOW %TopLeft1,%BottomRight1
		CLEARWINDOW %TopLeft3,%BottomRight3
	END IF
	LOCATE %SelLine,3,0
	PRINT "Loading communications program...";
	CHPATH TXPath$
	SHELL TXProg$
	M=%F5
	PRINT BORDER$;
	PRINT HORLINE$;
	PRINT FUNKEY$;
END SUB
:
SUB Quitta SHARED
	Jump=0:KeyOn=%FALSE
	CHPATH StartDir$
	CLS:END
END SUB
:
SUB Merja SHARED
	Heading$="MERGE FILES"
	Title$=SPACE$(38)+"Join to: ["+SPACE$(27)+"]"
	DOSCREEN Heading$,Title$,Drive$,WhichFiles$
	DO
		DO
			PRINT TITEL$(Title$);
			NM$=GETNM$(Drive$)
			IF NM$="" THEN EXIT SUB
			IF DoTags THEN
				LOCATE %SelLine,3
				PRINT "Merge tagged files             ";
			END IF
			OutFile$=OUTNM$(NM$,OutName$,OutPath$,M)
			IF OutFile$=CHR$(%Esc) THEN RETURN
		LOOP WHILE OutFile$=""
		FOR N=0 TO FF
			IF INSTAT THEN EXIT SUB
			IF DoTags THEN
				NM$=TagList$(N)
			END IF
			IF NM$<>"" THEN
				IF OutName$="tagged files" THEN OutFile$=OutPath$+"\MERGED"
				CPRINT "Merging "+NM$+" to "+OutFile$+"...", 1
				OPEN NM$ FOR BINARY AS #1
				OPEN OutFile$ FOR BINARY AS #2
				Length&=LOF(1)
				LongFile=CHKLEN(Repeat,Extra,Length&)
				FOR LN=1 TO Repeat+1
					IF LongFile THEN
						IF LN>Repeat THEN
							GET$ #1,Extra,PutStr$
						ELSE
							GET$ #1,%LenBite,PutStr$
						END IF
					ELSE
						GET$ #1,Length&,PutStr$
					END IF
					LenJoin&=LOF(2)
					SEEK #2,LenJoin&
					PUT$ #2,PutStr$
					PutStr$=""
				NEXT LN
				CLOSE
			END IF
			IF DoTags=%FALSE THEN EXIT FOR
		NEXT N
		IF RTRIM$(CURDIR$,"\")=OutPath$ THEN PRINT REFILES$(WhichFiles$);
	LOOP
END SUB
:
SUB Wdisk SHARED
	Heading$="WIPE DISK FREE SPACE"
	Drive$=DDPath$
	NoDraw=%TRUE:Tagable%=FALSE
	DOSCREEN Heading$,Title$,Drive$,WhichFiles$
	DO
		DO
			NM$=GETNM$(Drive$)
			IF NM$="" THEN EXIT SUB
			FD=ASCII(CURDIR$)
			IF FD<65 OR FD>66 THEN
				IF ERRA(%NoWipe) THEN EXIT SUB ELSE EXIT LOOP
			END IF
			CPRINT "Wiping free space...", 1
			WIPEDISK
			EXIT LOOP
		LOOP
	LOOP
END SUB
:
SUB Compa(M) SHARED
	IF M=%AltF4 THEN
		Heading$="COMPRESS FILES (Alt E: Encryption "+EncState$
		Title$=SPACE$(34)+"Output name: ["+SPACE$(27)+"]"
		Action$="Compress"
		ZipProg$="PKZIP.EXE"
	ELSE
		Heading$="DECOMPRESS FILES (Alt E: Encryption "+EncState$
		Title$=SPACE$(34)+"Output path: ["+SPACE$(27)+"]"
		Action$="Decompress"
		ZipProg$="PKUNZIP.EXE"
	END IF
	DOSCREEN Heading$,Title$,Drive$,WhichFiles$
	DO
		DO
			PRINT TITEL$(Title$);
			NM$=GETNM$(Drive$)
			IF NM$="" THEN EXIT SUB
			IF DoTags THEN
				LOCATE %Selline,3
				PRINT Action$+NM$+SPACE$(11);
			END IF
			OutFile$=OUTNM$(NM$,OutName$,OutPath$,M)
			IF OutFile$=CHR$(%Esc) THEN EXIT SUB
		LOOP WHILE OutFile$=""
		ZipKey$=GETKEY$(EncState$)
		IF EncKey$=CHR$(%Esc) THEN EXIT SUB
		IF FD1 THEN
			IF WARNDISK(UtilPath$,ZipProg$,"PROGRAM") THEN EXIT SUB
		END IF
		DOZIP M,DoTags,NM$,OutPath$,OutFile$,OutName$,ZipKey$
		IF RTRIM$(CURDIR$,"\")=OutPath$ THEN PRINT REFILES$(WhichFiles$);
	LOOP
END SUB
:
SUB SafeSav SHARED
	Heading$="STORE FILES"
	Title$=SPACE$(39)+"Add to: ["+SPACE$(27)+"]"
	DOSCREEN Heading$,Title$,Drive$,WhichFiles$
	DO
		DO
			PRINT TITEL$(Title$);
			NM$=GETNM$(Drive$)
			IF NM$="" THEN EXIT SUB
			IF DoTags THEN
				LOCATE %SelLine,3
				PRINT "Store"+NM$+SPACE$(12);
			END IF
			LOCATE %SelLine,51:PRINT CHR$(219);SPACE$(26);
			OutFile$=OUTNM$(NM$,OutName$,OutPath$,M)
			IF OutFile$=CHR$(%Esc) THEN EXIT SUB
			IF OutFile$<>"" THEN
				IF OutName$="" THEN
					IF DoTags THEN OutName$="TAGGED" ELSE OutName$="STORE"
				END IF
				IF INSTR(OutFile$,".") THEN
					OutName$=LEFT$(OutName$,(INSTR(OutName$,".")-1))+".SAV"
				ELSE
					OutName$=OutName$+".SAV"
				END IF
				OutFile$=OutPath$+"\"+OutName$
				IF NOT EXIST(OutPath$,OutName$,0) THEN
					YN$=YESNO$("Create "+OutFile$+" ? (Y/N) Y")
					IF YN$=CHR$(%Esc) THEN EXIT SUB
				END IF
			END IF
		LOOP WHILE OutFile$="" OR YN$="N"
		IF DoTags THEN
			OPEN "LIST" FOR OUTPUT AS #1
			FOR N=0 TO FF
				IF TagList$(N)<>"" THEN
					PRINT #1,TagList$(N)
				END IF
			NEXT N
			CLOSE
		END IF
		CPRINT "Adding "+LTRIM$(NM$," ")+" to "+OutFile$+"...", 1
		IF DoTags THEN NM$="@LIST"
		SHELL UTPath$+"\PKZIP -A"+StoreKey$+" "+OutFile$+" "+NM$+" > NUL"
		IF DoTags THEN KILL "LIST"
		PRINT REFILES$(WhichFiles$);
	LOOP
END SUB
:
SUB SafeLod SHARED
	Heading$="EXTRACT STORED FILES (Ctrl F for contents)"
	IF Jump=0 THEN Drive$=DDPath$
	WhichFiles$="*.SAV"
	Title$=SPACE$(40)+"Get file: ["+SPACE$(12)+"]"
	DO
		NM$=""
		DOSCREEN Heading$,Title$,Drive$,WhichFiles$
		DO
			BF=0
			PRINT TITEL$(Title$);
			NM$=GETNM$(Drive$)
			IF NM$="" THEN
				IF Jump<>%AltF5 THEN
					WhichFiles$="*.*"
					PRINT REFILES$(WhichFiles$);
				END IF
				EXIT SUB
			END IF
			IF NM$=CHR$(6) THEN
				IF FF=0 THEN
					BF=%NoFile
					IF ERRA(BF) THEN EXIT SUB ELSE EXIT LOOP
				END IF
				SHOWZIP FileEntry$(XHL)
				EXIT LOOP
			END IF
			DO
				BF=0
				LOCATE %SelLine,54:PRINT CHR$(219);SPACE$(11);
				XNM$=UCASE$(KEYA$(%SelLine,54,33,126,12))
				IF XNM$=CHR$(%Esc) THEN EXIT SUB
				BF=CHECKNAME(XNM$)
				IF ERRA(BF) THEN EXIT SUB
			LOOP WHILE BF
			IF XNM$<>"" THEN
				CPRINT "Extracting "+XNM$+"...", 1
				IF LEN(RAM$)=2 THEN OutFile$=RAM$+"\" ELSE OutFile$=RAM$
				SHELL UTPath$+"\PKUNZIP.EXE "+NM$+" "+XNM$+" -O"+StoreKey$+" "+OutFile$+" > NUL"
				CLEARLINE %SelLine,77,0
			END IF
		LOOP
	LOOP WHILE NM$=CHR$(6)
END SUB
:
SUB Runna SHARED
	Heading$="RUN A PROGRAM"
	SwitchBox$="Switches: ["+CHR$(219)+SPACE$(26)+"]"
	Tagable=%FALSE
	DO
		DOSCREEN Heading$,"",Drive$,WhichFiles$
		DO
			NM$=GETNM$(Drive$)
			IF NM$="" THEN EXIT SUB
			Exec$=RIGHT$(NM$,4)
			IF Exec$<>".COM" AND Exec$<>".EXE" AND Exec$<>".BAT" THEN BF=%NoEx
			IF ERRA(BF) THEN EXIT SUB
		LOOP WHILE BF
		LOCATE %SelLine,40
		PRINT SwitchBox$;
		Switch$=UCASE$(KEYA$(%SelLine,51,32,126,27))
		IF Switch$=CHR$(%Esc) THEN EXIT SUB
		CLS
		COLOR Ink,%Black
		SHELL NM$+" "+Switch$
		LOCATE 25,1
		PRINT "Press any key to return to Coder...";
		GoOn$=INPUT$(1)
		PRINT BORDER$;
	LOOP
END SUB
:
SUB Config SHARED
	SETUP:
	PRINT TOPLINE$;
	CLEARWINDOW %TopLeft1,%BottomRight1
	CLEARWINDOW %TopLeft3,%BottomRight3
	KeyOn=%FALSE
	RESTORE SETUP
	FOR N=1 TO %CfgItems
		READ ConfigList$
		COLOR %LCyan
		LOCATE N+1,3:PRINT CHR$(N+64);
		COLOR Ink
		PRINT TAB(7);ConfigList$;TAB(36);"-   ";DR$(N);
	NEXT N
	DATA "Colour, B&W or LCD",Default level (1 - 4)
	DATA Path for data/message disk,Path for work area,Path for editor program
	DATA Name of editor program,Path for comms program,Name of comms program
	DATA Path for compression progs,Wipe count,Encrypted file extension,Storage key
	PRINT HORLINE$;
	LOCATE %SelLine,3:PRINT "Press letter of option to change";TAB(52);
	COLOR %LCyan
	PRINT "S";
	COLOR Ink
	PRINT " - Save changes";
	LOCATE %HeadLine,3:COLOR BWYellow,%Cyan:PRINT "CHANGE CONFIGURATION";:COLOR Ink,Paper
	PRINT ESCMAIN$;
	DO
		BF=0
		DO
			LOCATE %SelLine,36
			COLOR %Flash+Ink:PRINT CHR$(219);:COLOR Ink
			Choice$=UCASE$(INPUT$(1))
			IF Choice$=CHR$(%Lfd) THEN Choice$=CHR$(%Ent)
			IF ASC(Choice$)=>65 AND ASC(Choice$)<=64+%CfgItems THEN
				LOCATE %SelLine,36:PRINT " ";
				TempName$=""
				Opt=ASC(Choice$)-64
				Y=Opt+1:X=40
				LOCATE Y,X,0,0,7
				PRINT CHR$(219);SPACE$(39);
				Opt$=UCASE$(KEYA$(Y,X,32,126,39))
				IF Opt$="" OR Opt$=CHR$(%Esc) THEN
					LOCATE Y,40:PRINT SPACE$(39);
					LOCATE Y,40:PRINT DR$(Opt);
				ELSE
					DR$(Opt)=RTRIM$(Opt$, ANY " \/.,")
					SETPARAMS
				END IF
				LOCATE ,,0
			END IF
			IF Choice$="S" THEN
				IF WARNDISK("A:",CDProg$+".CNF","CONFIG") THEN EXIT SUB
				OPEN "A:\"+CDProg$+".CNF" FOR OUTPUT AS #1
				FOR N=1 TO %CfgItems
					IF N<12 THEN
						PRINT #1,UCASE$(DR$(N))
					ELSE
						PRINT #1,CODEKEY$(DR$(%CfgItems),BF)
					END IF
				NEXT N
				CLOSE
			END IF
		LOOP UNTIL Choice$=CHR$(%Esc) OR Choice$=CHR$(%Ent)
		IF ERRA(BF) THEN EXIT SUB
	LOOP WHILE BF
	JUMP=0
END SUB
:
FUNCTION CODEKEY$(ShrinKey$,KeyStat)
	LOCAL TempKey$
	LK=LEN(ShrinKey$)
	IF LK=1 THEN CODEKEY$="":EXIT FUNCTION
	RANDOMIZE LK
	FOR N=1 TO LK
		RK=INT(RND*256)
		CK=ASC(MID$(ShrinKey$,N,1))
		XK=RK XOR CK
		TempKey$=TempKey$+CHR$(XK)
	NEXT N
	IF INSTR(TempKey$, ANY CHR$(10)+CHR$(13)+CHR$(26)) THEN KeyStat=%BadKey
	CODEKEY$=TempKey$
END FUNCTION
:
SUB DOS SHARED
	Jump=0
	COLOR Ink,%Black
	CLS
	PRINT "Type EXIT to return to program..."
	SHELL
	PRINT BORDER$;
END SUB
:
FUNCTION ERRA(BF)
	ERRA=%FALSE
	IF BF<1 THEN EXIT FUNCTION
	SHARED KeyOn,LenMsg&,MaxLength&
	CLOSE:SOUND 88,8
	SELECT CASE BF
		CASE %BadName:ERRMSG$="Unacceptable characters in name"
		CASE %NoCopy :ERRMSG$="File cannot be copied onto itself"
		CASE %BadPage:ERRMSG$="Unacceptable page number. Acceptable pages: 1 - 255"
		CASE %BadLine:ERRMSG$="Unacceptable line number. Acceptable lines: 1 - 99"
		CASE %BadFile:ERRMSG$="Un-enciphered or corrupted file"
		CASE %Corrupt:ERRMSG$="Bad key or corrupted file"
		CASE %TooLong:ERRMSG$="Message too long by"+STR$(LenMsg&-MaxLength&)+" bytes"
		CASE %NoData :ERRMSG$="Insufficient data left. Start a new DATA disk"
		CASE %DecDone:ERRMSG$="This file has already been deciphered"
		CASE %Full	 :ERRMSG$="Disk full"
		CASE %NoDir	 :ERRMSG$="Invalid path"
		CASE %Failed :ERRMSG$="Copy failed"
		CASE %NoFile :ERRMSG$="File not found"
		CASE %Exists :ERRMSG$="File already exists. Choose another name"
		CASE %NoWipe :ERRMSG$="Only floppy disks may be wiped"
		CASE %BadSav :ERRMSG$="You are attempting to copy/move to your Data Disk!"
		CASE %NoEx	 :ERRMSG$="This is not an executable file"
		CASE %BadKey :ERRMSG$="Storage key unsuitable. Try another"
		CASE %BadZip :ERRMSG$="File could not compress. Possibly out of memory"
	END SELECT
	IF WARNERR(ERRMSG$) THEN
		ERRA=%TRUE:EXIT FUNCTION
	ELSE
		IF KeyOn THEN PRINT FUNKEY$;
	END IF
END FUNCTION
:
ERMESG:
	CLOSE:SOUND 88,8
	SELECT CASE ERR
		CASE 53:ERRMSG$="File not found"
		CASE 58:ERRMSG$="File already exists. Choose another name"
		CASE 61:ERRMSG$="Out of memory/disk full. Delete files to make space"
		CASE 64:ERRMSG$="Invalid name"
		CASE 70:ERRMSG$="Disk write-protected. Switch tab to continue"
		CASE 71:ERRMSG$="Disk not in drive or door open"
		CASE 72:ERRMSG$="Disk corrupted"
		CASE 75:ERRMSG$="Path/File access error"
		CASE 76:ERRMSG$="Path not found"
		CASE ELSE:ERRMSG$="Error number"+STR$(ERR)+" at address"+STR$(ERADR)
	END SELECT
	IF WARNERR(ERRMSG$) THEN Jump=0
RESUME MENU
:
SUB SetParams SHARED
	StartInk=ASCII(DR$(1))
	SELECT CASE StartInk
		CASE 66		'B&W
			StartInk=&H0000
			Ink=%HWhite:Paper=%Black:BWYellow=Paper
		CASE 67		'COLOR
			StartInk=&H1000
			Ink=%HWhite:Paper=%Blue:BWYellow=%Yellow
		CASE 76		'LCD
			StartInk=&H0000
			Ink=%White:Paper=%Black:BWYellow=Paper
	END SELECT
	Level$=DR$(2)		:DDPath$=DR$(3)	:RAM$=DR$(4)
	EDPath$=DR$(5)	:EDProg$=DR$(6)	:TXPath$=DR$(7)	:TXProg$=DR$(8)
	UTPath$=DR$(9)	:Wipes=VAL(DR$(10)):Ext$=DR$(11):StoreKey$=" -s"+DR$(12)
	Level=VAL(Level$)-1
	IF EDPath$=DDPath$ THEN FD1=%TRUE ELSE FD1=%FALSE
END SUB
:
FUNCTION PASSWORD
	SHARED Ink,Paper
	Pass$="":PL=10
	DIM PW$(PL)
	PW$(9)=CHR$(82):PW$(4)=CHR$(65):PW$(1)=CHR$(86):PW$(5)=CHR$(66):PW$(2)=CHR$(79)
	PW$(3)=CHR$(67):PW$(6)=CHR$(85):PW$(8)=CHR$(65):PW$(10)=CHR$(89):PW$(7)=CHR$(76)
	FOR N=1 TO PL:Pass$=Pass$+PW$(N):NEXT N
	ERASE PW$
	COLOR Ink,Paper:CLS
	LOCATE 12,20,0:PRINT "ENTER PASSWORD: [               ]";
	DO
		Y=12:X=37
		LOCATE Y,X:PRINT CHR$(219);SPACE$(14);
		PSW$=KEYA$(Y,X,32,126,15)
		IF PSW$="" THEN PASSWORD=%FALSE:EXIT FUNCTION
		PSW$=UCASE$(PSW$)
		IF PSW$=Pass$ THEN EXIT LOOP
		SOUND 88,8
	LOOP
	PASSWORD=%TRUE
END FUNCTION
:
ABORT:
	CLS
	PRINT "DATA NOT FOUND - ABORTING!"
	END
