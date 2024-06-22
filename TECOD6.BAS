CDProg$="TECOD6"
DEFINT A-Z
%TC=0:%TE=-1:%TK=0

$INCLUDE "BIOS.INC"
$INCLUDE "CHKERR.INC"
$INCLUDE "DOS.INC"
$INCLUDE "KONST.INC"
$INCLUDE "CODSUBS.INC"

$STACK 2048

DIM Salphabet(256),Ralphabet(256)
DIM FileEntry$(140),FileLen&(140),KeyFun$(1:20),DR$(1:11)

OPEN "A:\"+CDProg$+".CNF" FOR INPUT AS #1
FOR L=1 TO 11
	LINE INPUT #1,DR$(L)
NEXT L
CLOSE

CALL SetParams

PW=%TRUE		:Jump=0
%BadName=1	:%NoCopy=2	:%BadPage=3	:%BadLine=4	:%BadFile=5	:%Corrupt=6
%TooLong=7 	:%NoData=8 	:%DecDone=9 :%Full=10		:%NoDir=11	:%Failed=12
%NoFile=13	:%Exists=14

FOR K=1 TO 20
	READ KeyFun$(K)
NEXT K
DATA "Load ","Save ","Del  ","Enc  ","Dec  ","Type ","Read ",Renam,Comms,Quit
DATA Compr,Decom,"Wipe ",Wdisk,"Hide ",Unhid,"Date ",Merge,Setup,"DOS "

CALL PASSWORD
PRINT BORDER$;

MENU:
	DO
		BF=0:M=0:LenBite=10000
		Wild=%TRUE:NoDraw=%FALSE:KeyOn=%TRUE
		Title$="":WhichFiles$="*.*"
		ON ERROR GOTO ERMESG
		IF Jump THEN
			Drive$=RTRIM$(CURDIR$,"\")
			GOTO SkipMenu
		ELSE
			Drive$=RAM$
		END IF
		HL=0:LH=0:XF=%FALSE:XFH=%FALSE:XHL=0
		PRINT TOPLINE$;
		CALL ClearWindow(%TopLeft1,%BottomRight1)
		CALL ClearWindow(%TopLeft3,%BottomRight3)
		COLOR BWYellow,%Cyan:PRINT CENTRE$(2," C O D E R ");
		COLOR %Yellow,Paper :PRINT CENTRE$(4,"Level "+Level$);
		COLOR BWYellow,%Cyan:PRINT CENTRE$(6," MAIN   MENU ");
		COLOR ,Paper
		Y=8:X=27
		RESTORE MENU
		FOR ROW=1 TO 13
			LOCATE Y,X
			COLOR %LCyan
			READ MenuItem$:PRINT MenuItem$
			COLOR Ink
			LOCATE Y,X+4
			READ MenuItem$:PRINT MenuItem$
			INCR Y
		NEXT ROW
		DATA F1,": Load files from disk"
		DATA F2,": Save files onto disk"
		DATA F3,": Delete files on disk"
		DATA " "," "
		DATA F4,": Encipher files"
		DATA F5,": Decipher files"
		DATA " "," "
		DATA F6,": Type a new document"
		DATA F7,": Edit/read documents"
		DATA F8,": Rename files"
		DATA F9,": Terminal"
		DATA " "," "
		DATA F10,": Quit"
		COLOR %Yellow
		PRINT CENTRE$(23,"  ALT Menu: Alt F1 - F10"+SPACE$(38)+"Level: Alt 1 - 4")
		PRINT CENTRE$(25,"Version 6.42");
		COLOR Ink
		DO
		  DO
				CH$=INKEY$
				IF CH$=CHR$(%Esc) OR CH$=CHR$(%Ent) THEN QUITTA
			LOOP WHILE LEN(CH$)<2
			M=ASC(RIGHT$(CH$,1))
			IF M>119 AND M<124 THEN
				Level=M-120
				Level$=LTRIM$(STR$(Level+1))
				PRINT SHOWLEVEL$(4,43,Level$);
			END IF
			SkipMenu:
			IF Jump THEN M=Jump
			SELECT CASE M
				CASE %F1:GOSUB LOADA
				CASE %F2:GOSUB SAVA
				CASE %F3:GOSUB DELLA
				CASE %F4:GOSUB ENCA
				CASE %F5:GOSUB DECA
				CASE %F6:GOSUB NEWDOC
				CASE %F7:GOSUB OLDOC
				CASE %F8:GOSUB RENNA
				CASE %F9:GOSUB TERMINAL
				CASE %F10:GOSUB QUITTA
				CASE %AltF1:GOSUB COMPA
				CASE %AltF2:GOSUB DECOMPA
				CASE %AltF3:GOSUB WIPA
				CASE %AltF4:GOSUB WDISK
				CASE %AltF5:GOSUB HIDA
				CASE %AltF6:GOSUB UNHIDA
				CASE %AltF7:GOSUB ALTA
				CASE %AltF8:GOSUB MERJA
				CASE %AltF9:GOSUB CONFIG
				CASE %AltF10:GOSUB DOS
			END SELECT
		LOOP UNTIL M=>%F1 AND M<=%F10 OR M=>%AltF1 AND M<=%AltF10
	LOOP
:
LOADA:
	Heading$="LOAD FILES INTO MEMORY"
	IF Jump=0 THEN Drive$=DDPath$
	Wild=%FALSE
	ReLoad:
	CALL DoScreen
	DO
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		PRINT CLEARLINE$(%SelLine,77,1);
		PRINT "Copying '";NM$;"'...";
		IF LEN(RAM$)=2 THEN OutFile$=RAM$+"\" ELSE OutFile$=RAM$
		SHELL "COPY "+NM$+" "+OutFile$+" > NUL"
		PRINT CLEARLINE$(%SelLine,77,0);
	LOOP
RETURN
:
SAVA:
	Heading$=""
	Drive$=RAM$:OutPath$=DDPath$:Wild=%FALSE
	ReSave:
	IF Jump AND RTRIM$(CURDIR$,"\")=Drive$ THEN
		CALL ReScreen(Heading$,Title$,WhichFiles$)
	ELSE
		Jump=0
		CALL DoScreen
	END IF
	Heading$="SAVE FILES IN MEMORY ONTO "
	PRINT FITHEAD$(Heading$,DDPath$);
	DO
		DO
			BF=0
			DO
				CD=%FALSE
				CALL SELECTA(NM$)
				IF NM$="" OR Jump THEN RETURN
				IF MID$(NM$,2,1)=":" THEN
					IF NOT OKDIR(RTRIM$(NM$,"\")) THEN
						BF=%NoDir:EXIT LOOP
					END IF
					CD=%TRUE
					OutPath$=NM$
					PRINT FITHEAD$(Heading$,OutPath$);
				END IF
			LOOP WHILE CD
			IF BF<>%NoDir THEN
				IF OutPath$=RTRIM$(CURDIR$,"\") THEN BF=%NoCopy
				BF=CHECKFILE(NM$)
			END IF
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		DO
			IF INSTR(NM$,"*")=0 AND EXIST(OutPath$,NM$,0) THEN
				Msg$="Overwrite "+OutPath$+"\"+NM$+" ? (Y/N) Yes"
				LOCATE %SelLine,3:PRINT Msg$;SPACE$(77-LEN(Msg$))
				LOCATE %SelLine,LEN(Msg$),1
				SOUND 800,5
				DO
					YN$=UCASE$(INPUT$(1))
				LOOP UNTIL INSTR(YN$, ANY "YN"+CHR$(%Ent)+CHR$(%Esc))
				IF YN$=CHR$(%Esc) THEN LOCATE ,,0:RETURN
				IF YN$="N" THEN
					PRINT CLEARLINE$(%SelLine,77,1);
					EXIT LOOP
				END IF
			ELSE
				IF FileLen&(XHL)>FREESPACE&(OutPath$) THEN
					IF ERRA(%Full) THEN RETURN
					EXIT LOOP
				END IF
			END IF
			PRINT CLEARLINE$(%SelLine,77,0);
			PRINT "Copying '";NM$;"'...";
			IF LEN(OutPath$)=2 THEN OutPath$=OutPath$+"\"
			SHELL "COPY "+NM$+" "+OutPath$+" > NUL"
			PRINT CLEARLINE$(%SelLine,77,1);
			IF INSTR(NM$,"*")=0 THEN
				IF NOT EXIST(OutPath$,NM$,0) THEN
					IF ERRA(%Failed) THEN RETURN
					EXIT LOOP
				END IF
			END IF
		LOOP WHILE BF
	LOOP
RETURN
:
DELLA:
	Heading$="DELETE FILES"
	IF Jump=0 THEN Drive$=DDPath$
	ReDel:
	CALL DoScreen
	DO
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		DelNM$=STRING$(8,255)
		PRINT CLEARLINE$(%SelLine,77,1);
		PRINT "Are you sure? (Y/N) Yes"
		LOCATE %SelLine,23
		SOUND 800,5
		DO
			DO
				YN$=UCASE$(INPUT$(1))
			LOOP UNTIL INSTR(YN$, ANY "YN"+CHR$(%Ent)+CHR$(%Esc))
			IF YN$=CHR$(%Esc) THEN CLOSE:RETURN
			PRINT CLEARLINE$(%SelLine,77,1);
			IF YN$="N" THEN CLOSE:EXIT LOOP
			PRINT "Deleting '";NM$;"'...";
			OPEN NM$ FOR BINARY AS #1
			LongFile=CHKLEN(Repeat,Extra,LenBite,FileLen&(XHL))
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						PUT$ #1,STRING$(Extra,0)
					ELSE
						PUT$ #1,STRING$(LenBite,0)
					END IF
				ELSE
					PUT$ #1,STRING$(FileLen&(XHL),0)
				END IF
			NEXT LN
			CLOSE
			NAME NM$ AS DelNM$
			KILL DelNM$
			PRINT REFILES$(WhichFiles$);
		LOOP UNTIL INSTR(YN$, ANY "Y"+CHR$(%Ent))
	LOOP
RETURN
:
ENCA:
	Heading$="ENCIPHER FILES (L."+Level$+")"
	Wild=%FALSE
	ReEnc:
	CALL DoScreen
	DO
		DO
			KeyOn=%TRUE
			NM$=GETNM$(Drive$)
			IF NM$="" THEN RETURN
			KeyOn=%FALSE
			IF INSTR(NM$,"*") THEN
				LOCATE %SelLine,39:PRINT "Name for Output file: [        ]"
				OutFile$=GETSHRINK$
				IF OutFile$=CHR$(%Esc) THEN RETURN
				IF OutFile$="" THEN LOCATE %SelLine,62:PRINT " ";
			ELSE
				IF INSTR(NM$,".") THEN
					OutFile$=LEFT$(NM$,(INSTR(NM$,".")-1))
				ELSE
					OutFile$=NM$
				END IF
			END IF
		LOOP WHILE OutFile$=""
		PRINT CLEARLINE$(%ActLine,40,0);
		CALL ClearWindow(%TopLeft3,%BottomRight3)
		DO
			IF Level=0 THEN
				LL=0:PAGE=0:AC=77
				EncKey$=GETENCKEY$(PAGE,LL)
				IF EncKey$="" THEN
					EXIT LOOP
				ELSEIF EncKey$=CHR$(%Esc) THEN
					RETURN
				END IF
			ELSE
				AC=40
			END IF
			PRINT CLEARLINE$(%ActLine,AC,0)
			IF NOT DOSHRINK(NM$,OutFile$,"") THEN RETURN
			OPEN OutFile$+".ZIP" FOR BINARY AS #1
			LenMsg&=LOF(1):CLOSE
			IF Level THEN
				PRINT CLEARLINE$(%SelLine,77,0);
				PRINT "Ready to encipher '";NM$;"'";
				IF WARNDISK(DDPath$,"DATAPTR","DATA") THEN
					KILL OutFile$+".ZIP":RETURN
				END IF
				PRINT CLEARLINE$(%ActLine,77,0);
				SELECT CASE Level
					CASE 1:LenData=26
					CASE 2:LenData=LenMsg&/5:MaxLength&=163835
					CASE 3:LenData=LenMsg&  :MaxLength&=32750
				END SELECT
				IF Level>1 THEN
					IF LenMsg&>MaxLength& THEN
						KILL OutFile$+".ZIP"
						IF ERRA(%TooLong) THEN RETURN
						EXIT LOOP
					END IF
				END IF
				SM&=0:SendData$=GETDATA$(SM&,LenData,"SNUM")
				IF SendData$="" THEN
					KILL OutFile$+".ZIP"
					IF ERRA(%NoData) THEN RETURN
					EXIT LOOP
				END IF
				EncKey$=LEFT$(SendData$,26)
			END IF
			PRINT CLEARLINE$(%SelLine,77,0);
			PRINT "Enciphering '";NM$;"'...";
			COLOR Ink+%Flash:PRINT CHR$(219):COLOR Ink
			CALL MakeAlpha(EncKey$,Salphabet(),Ralphabet())
			ENM$=OutFile$+Ext$
			OPEN OutFile$+".ZIP" FOR BINARY AS #1
			OPEN ENM$ FOR BINARY AS #2
			LOCATE %ActLine,3:PRINT "File length:";LenMsg&;"characters";
			MSG$="":SM$=STARTM$(Level,SM&)
			IF Level=0 THEN
				SM$=SM$+CHR$(PAGE)+CHR$(LL)
			END IF
			PUT$ #2,SM$
			LongFile=CHKLEN(Repeat,Extra,LenBite,LenMsg&)
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						GET$ #1,Extra,MSG$
					ELSE
						GET$ #1,LenBite,MSG$
					END IF
				ELSE
					GET$ #1,LenMsg&,MSG$
				END IF
				RANDOMIZE CKSUM
				SELECT CASE Level
					CASE 0,1
						CALL EncLevelOneTwo(LN,MSG$,Salphabet(),Ralphabet())
					CASE 2
						CALL EncLevelThree(LN,MSG$,SendData$,Salphabet())
					CASE 3
						CALL EncLevelFour(MSG$,SendData$,Salphabet())
				END SELECT
				MSG$=""
			NEXT LN
			CLOSE:SendData$=""
			KILL OutFile$+".ZIP"
			IF Level THEN
				IF NOT EXIST(DDPath$,"SNUM",0) THEN
					IF WARNDISK(DDPath$,"SNUM","DATA") THEN RETURN
				END IF
				CALL DiskFlush(SM&,LenData,"SNUM")
			END IF
		LOOP WHILE BF
		PRINT REFILES$(WhichFiles$);
		CALL TidyUp
	LOOP
RETURN
:
SUB EncLevelOneTwo(LN,MSG$,Salphabet(),Ralphabet())
	LENMSG=LEN(MSG$)
	DEF SEG=STRSEG(MSG$)
	SP&=STRPTR(MSG$)
	PREV=INT(RND*256)
	$EVENT OFF
	FOR ENC=1 TO LENMSG
		RL=INT(RND*256)
		CD=(PEEK(SP&) XOR RL) XOR PREV
		POKE SP&,Salphabet(CD)
		IF RL MOD 5=0 THEN
			RANDOMIZE ENC+Ralphabet(INT(RND*256))+LN
		END IF
		PREV=RL
		INCR SP&
	NEXT ENC
	$EVENT ON
	PUT$ #2,MSG$
	DEF SEG
END SUB
:
SUB EncLevelThree(LN,MSG$,SNUM$,Salphabet())
	LOCAL SnumChar
	LENMSG=LEN(MSG$)
	DEF SEG=STRSEG(MSG$)
	SP&=STRPTR(MSG$)
	LENSNUM=LEN(SNUM$)
	PREV=INT(RND*256)
	$EVENT OFF
	FOR ENC=1 TO LENMSG
		RL=INT(RND*256)
		CD=(PEEK(SP&) XOR RL) XOR PREV
		POKE SP&,Salphabet(CD)
		IF RL MOD 5=0 THEN
			SnumChar=(SnumChar MOD LENSNUM)+1
			RANDOMIZE ENC+ASC(MID$(SNUM$,SnumChar,1))+LN
		END IF
		PREV=RL
		INCR SP&
	NEXT ENC
	$EVENT ON
	PUT$ #2,MSG$
	DEF SEG
END SUB
:
SUB EncLevelFour(MSG$,SNUM$,Salphabet())
	LENMSG=LEN(MSG$)
	DEF SEG=STRSEG(MSG$)
	SP&=STRPTR(MSG$)
	$EVENT OFF
	FOR ENC=1 TO LENMSG
		CH=PEEK(SP&)
		RL=ASC(MID$(SNUM$,ENC,1))
		CD=CH XOR RL
		POKE SP&,Salphabet(CD)
		INCR SP&
	NEXT ENC
	$EVENT ON
	PUT$ #2,MSG$
	DEF SEG
END SUB
:
FUNCTION GETENCKEY$(PAGE,LL)
	LOCATE %SelLine,3,0
	PRINT "Page No: [   ]    Line No: [  ]    ";
	PRINT "26 Char Key: [                          ]"
	FOR N=1 TO 2
		DO
			BF=0
			IF N=1 THEN
				X=13:CRS=3
			ELSE
				X=31:CRS=2
			END IF
			LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(CRS-1)
			PgLn$=KEYA$(%SelLine,X,48,57,CRS)
			IF PgLn$="" OR PgLn$=CHR$(%Esc) THEN
				GETENCKEY$=PgLn$:EXIT FUNCTION
			END IF
			IF N=1 THEN
				PAGE=VAL(PgLn$)
				IF PAGE<1 OR PAGE>255 THEN
					BF=%BadPage
					IF ERRA(BF) THEN GETENCKEY$="":EXIT FUNCTION
				END IF
			ELSE
				LNE=VAL(PgLn$)
				IF LNE<1 OR LNE>99 THEN
					BF=%BadLine
					IF ERRA(BF) THEN GETENCKEY$="":EXIT FUNCTION
				END IF
			END IF
		LOOP WHILE BF
	NEXT N
	DO
		X=52
		LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(25)
		KeyWord$=KEYA$(%SelLine,X,32,126,26)
		IF KeyWord$="" OR KeyWord$=CHR$(%Esc) THEN
			GETENCKEY$=KeyWord$:EXIT FUNCTION
		END IF
		IF LEN(KeyWord$)<>26 THEN
			SOUND 88,8
		ELSE
			EXIT LOOP
		END IF
	LOOP
	RANDOMIZE PAGE
	LL=(LNE XOR INT(RND*256))
	RANDOMIZE PAGE*LNE
	GETENCKEY$=KeyWord$
END FUNCTION
:
SUB MakeAlpha(KeyWord$,Salphabet(),Ralphabet())
	SHARED CKSUM
	DIM DYNAMIC ALF(256),Check(26)
	CKSUM=0
	FOR N=1 TO 26
		CKSUM=(CKSUM MOD 26392)+(ASC(MID$(KeyWord$,N,1))*N)
		Check(N-1)=CKSUM
	NEXT N
	FOR N=0 TO 255
		IF N MOD 10=0 THEN RANDOMIZE Check(N/10)
		DO
			CH=INT(RND*256)
		LOOP WHILE ALF(CH)=%TRUE
		ALF(CH)=%TRUE
		Salphabet(N)=(CH)
		Ralphabet(CH)=N
	NEXT N
	ERASE ALF,Check
END SUB
:
FUNCTION STARTM$(Level,SM&)
	RANDOMIZE TIMER
	StartRand=INT(RND*256) AND &HFC
	IF Level=0 THEN STARTM$=CHR$(StartRand):EXIT FUNCTION
	RANDOMIZE StartRand
	SecondRand=INT(RND*256) AND &HF8
	GetPos$=MKL$(SM&)
	Char1$=CHR$(StartRand XOR Level)
	Char2$=CHR$(SecondRand XOR ASC(MID$(GetPos$,3,1)))
	Char3$=MID$(GetPos$,2,1)
	Char4$=LEFT$(GetPos$,1)
	STARTM$=CHAR1$+CHAR2$+CHAR3$+CHAR4$
END FUNCTION
:
FUNCTION GETDATA$(SM&,LenKey,DataFile$)
	SHARED DDPath$,Ink
	NULL$=STRING$(5,0)
	IF DataFile$="SNUM" THEN
		OPEN DDPath$+"\DATAPTR" FOR INPUT AS #1
		INPUT #1,SM&:CLOSE
		IF SM&+LenKey>360000& THEN
			GETDATA$="":EXIT FUNCTION
		END IF
	END IF
	OPEN DDPath$+"\"+DataFile$ FOR BINARY AS #1
	PRINT CLEARLINE$(%SelLine,77,1);
	PRINT "Reading data...";
	DO
		BadKey=%FALSE
		SEEK #1,SM&:GET$ #1,LenKey,KeyData$
		PosBad=INSTR(KeyData$,NULL$)
		IF PosBad THEN
			IF DataFile$="SNUM" THEN
				BadKey=%TRUE
				INCR SM&,PosBad+5
			ELSE
				KeyData$=""
				EXIT LOOP
			END IF
		END IF
	LOOP WHILE BadKey
	CLOSE
	GETDATA$=KeyData$
END FUNCTION
:
SUB DiskFlush(SM&,BytesToWipe,DataFile$)
	SHARED DDPath$
	DataLeft$=LTRIM$(STR$(CEIL(((SM&+BytesToWipe)/360000)*100)))
	LOCATE %ActLine,3,0:PRINT "Data disk ";DataLeft$;" per cent used                  "
	PRINT CLEARLINE$(%SelLine,77,1);
	PRINT "Wiping used data...";
	OPEN DDPath$+"\"+DataFile$ FOR BINARY AS #1
	WipeData$=STRING$(BytesToWipe,0)
	SEEK #1,SM&
	PUT$ #1,WipeData$
	CLOSE
	IF DataFile$="SNUM" THEN
		INCR SM&,BytesToWipe
		OPEN DDPath$+"\DATAPTR" FOR OUTPUT AS #1
		PRINT #1,SM&
		CLOSE
	END IF
END SUB
:
DECA:
	Heading$="DECIPHER FILES"
	ReDec:
	CALL DoScreen
	DO
		KeyOn=%TRUE
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		DO
			CALL ClearWindow(%TopLeft3,%BottomRight3)
			PRINT CLEARLINE$(%ActLine,40,0);
			KeyOn=%FALSE
			OPEN NM$ FOR BINARY AS #1
			LenCgm&=LOF(1):GET$ #1,4,DecVer$:CLOSE
			DecVer=ASC(LEFT$(DecVer$,1)) AND 3
			IF DecVer THEN Header=4 ELSE Header=3
			DECR LenCgm&,Header:CGM$=""
			IF DecVer=0 THEN
				DecKey$=GETKEYWORD$(DecVer$)
				IF DecKey$=CHR$(%Esc) THEN
					RETURN
				ELSEIF DecKey$="" THEN
					EXIT LOOP
				END IF
			ELSE
				IF WARNDISK(DDPath$,"RNUM","DATA") THEN RETURN
				SM&=READSM&(DecVer$)
				IF SM&=%TRUE THEN
					IF ERRA(%BadFile) THEN RETURN
					EXIT LOOP
				END IF
				SELECT CASE DecVer
					CASE 1:LenData=26
					CASE 2:LenData=LenCgm&/5
					CASE 3:LenData=LenCgm&
				END SELECT
				RecData$=GETDATA$(SM&,LenData,"RNUM")
				IF RecData$="" THEN
					IF ERRA(%DecDone) THEN RETURN
					EXIT LOOP
				END IF
				DecKey$=LEFT$(RecData$,26)
			END IF
			PRINT CLEARLINE$(%SelLine,77,0);
			PRINT "Deciphering '";NM$;"' - Level ";CHR$(49+DecVer);"...";
			COLOR Ink+%Flash:PRINT CHR$(219):COLOR Ink
			CALL MakeAlpha(DecKey$,Salphabet(),Ralphabet())
			IF INSTR(NM$,".")THEN
				DNM$=LEFT$(NM$,(INSTR(NM$,".")-1))+".BAD"
			ELSE
				DNM$=NM$+".BAD"
			END IF
			OPEN NM$ FOR BINARY AS #1
			OPEN DNM$ FOR BINARY AS #2
			PRINT CLEARLINE$(%ActLine,77,0);
			PRINT "File length:";LenCgm&;"characters";
			SEEK #1,Header
			LongFile=CHKLEN(Repeat,Extra,LenBite,LenCgm&)
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						GET$ #1,Extra,CGM$
					ELSE
						GET$ #1,LenBite,CGM$
					END IF
				ELSE
					GET$ #1,LenCgm&,CGM$
				END IF
				RANDOMIZE CKSUM
				SELECT CASE DecVer
					CASE=0,1
						CALL DecLevelOneTwo(LN,CGM$,Ralphabet())
					CASE=2
						CALL DecLevelThree(LN,CGM$,RecData$,Ralphabet())
					CASE=3
						CALL DecLevelFour(CGM$,RecData$,Ralphabet())
				END SELECT
				CGM$=""
			NEXT LN
			CLOSE:RecData$=""
			IF NOT CHKCOMP(DNM$) THEN
				KILL DNM$
				IF ERRA(%Corrupt) THEN RETURN
				EXIT LOOP
			END IF
			IF DecVer THEN
				IF NOT EXIST(DDPath$,"RNUM",0) THEN
					IF WARNDISK(DDPath$,"RNUM","DATA") THEN RETURN
				END IF
				CALL DiskFlush(SM&,LenData,"RNUM")
			END IF
			IF NOT EXPAND(DNM$,"") THEN RETURN
		LOOP WHILE BF
		PRINT REFILES$(WhichFiles$);
		CALL TidyUp
	LOOP
RETURN
:
SUB DecLevelOneTwo(LN,CG$,Ralphabet())
	LENCG=LEN(CG$)
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	PREV=INT(RND*256)
	$EVENT OFF
	FOR DEC=1 TO LENCG
		RL=INT(RND*256)
		PL=(Ralphabet(PEEK (SP&)) XOR PREV) XOR RL
		POKE SP&,PL
		IF RL MOD 5=0 THEN
			RANDOMIZE DEC+Ralphabet(INT(RND*256))+LN
		END IF
		PREV=RL
		INCR SP&
	NEXT DEC
	$EVENT ON
	PUT$ #2,CG$
	DEF SEG
END SUB
:
SUB DecLevelThree(LN,CG$,RNUM$,Ralphabet())
	LOCAL RnumChar
	LENCG=LEN(CG$)
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	LENRNUM=LEN(RNUM$)
	PREV=INT(RND*256)
	$EVENT OFF
	FOR DEC=1 TO LENCG
		RL=INT(RND*256)
		PL=(Ralphabet(PEEK (SP&)) XOR PREV) XOR RL
		POKE SP&,PL
		IF RL MOD 5=0 THEN
			RnumChar=(RnumChar MOD LENRNUM)+1
			RANDOMIZE DEC+ASC(MID$(RNUM$,RnumChar,1))+LN
		END IF
		PREV=RL
		INCR SP&
	NEXT DEC
	$EVENT ON
	PUT$ #2,CG$
	DEF SEG
END SUB
:
SUB DecLevelFour(CG$,RNUM$,Ralphabet())
	LENCG=LEN(CG$)
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	$EVENT OFF
	FOR DEC=1 TO LENCG
		RL=ASC(MID$(RNUM$,DEC,1))
		PL=Ralphabet(PEEK (SP&)) XOR RL
		POKE SP&,PL
		INCR SP&
	NEXT DEC
	$EVENT ON
	PUT$ #2,CG$
	DEF SEG
END SUB
:
FUNCTION GETKEYWORD$(Ver$)
	PAGE=ASC(MID$(Ver$,2,1))
	LNE =ASC(MID$(Ver$,3,1))
	RANDOMIZE PAGE
	LNE=LNE XOR INT(RND*256)
	RANDOMIZE PAGE*LNE
	IF PAGE<1 OR PAGE>255 OR LNE<1 OR LNE>99 THEN
		IF ERRA(%BadFile) THEN GETKEYWORD$=""
		EXIT FUNCTION
	END IF
	LOCATE %SelLine,3
	PRINT "Page No:";PAGE;TAB(21);" Line No:";LNE;TAB(38);
	PRINT "26 Char Key: [                          ]"
	DO
		X=52:CRS=26
		LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(CRS-1)
		KeyWord$=KEYA$(%SelLine,X,32,126,26)
		IF KeyWord$="" OR KeyWord$=CHR$(%Esc) THEN
			GETKEYWORD$=KeyWord$:EXIT FUNCTION
		END IF
		IF LEN(KeyWord$)<>CRS THEN
			SOUND 88,8
		ELSE
			EXIT LOOP
		END IF
	LOOP
	GETKEYWORD$=KeyWord$
END FUNCTION
:
FUNCTION READSM&(Ver$)
	Rndz=ASC(LEFT$(Ver$,1)) AND &HFC
	RANDOMIZE Rndz
	RandChar=INT(RND*256) AND &HF8
	Segmt=ASC(MID$(Ver$,2,1)) AND &H07
	Check=ASC(MID$(Ver$,2,1)) XOR Segmt
	CHAR1$=RIGHT$(Ver$,1)
	CHAR2$=MID$(Ver$,3,1)
	CHAR3$=CHR$(Segmt)
	CHAR4$=CHR$(&H00)
	IF RandChar <> Check THEN
		READSM&=%TRUE:EXIT FUNCTION
	END IF
	READSM&=CVL(CHAR1$+CHAR2$+CHAR3$+CHAR4$)
END FUNCTION
:
NEWDOC:
	PRINT TOPLINE$;
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	PRINT ESCMAIN$;
	LOCATE 2,3:COLOR BWYellow,%LCyan:PRINT "TYPE A NEW DOCUMENT":COLOR Ink,Paper
	LOCATE 4,3:PRINT "Give a name to the document you are about to type"
	LOCATE 6,3:PRINT "Document name: [            ]"
	DO
		BF=0:Jump=0:KeyOn=%FALSE:X=19:Y=6
		LOCATE Y,X:PRINT CHR$(219);SPACE$(11)
		NM$=KEYA$(Y,X,33,126,12)
		IF NM$=CHR$(%Esc) OR NM$="" THEN RETURN
		BF=CHECKNAME(NM$)
		IF ERRA(BF) THEN RETURN
	LOOP WHILE BF
	IF NOT EDIT(RAM$,NM$) THEN RETURN
RETURN
:
OLDOC:
	Heading$="EDIT/READ FILES"
	ReOld:
	DO
		CALL DoScreen
		NM$=GETNM$(Drive$)
		IF NM$="" THEN EXIT LOOP
		Path$=RTRIM$(CURDIR$,"\")
		IF NOT EDIT(Path$,NM$) THEN EXIT LOOP
	LOOP
RETURN
:
RENNA:
	Heading$="RENAME FILES"
	Title$=SPACE$(40)+"New name: [            ]"
	ReRen:
	CALL DoScreen
	DO
		PRINT TITEL$(Title$);
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		DO
			BF=0:X=54
			LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(11)
			NNM$=KEYA$(%SelLine,X,33,126,12)
			IF NNM$=CHR$(%Esc) THEN RETURN
			IF NNM$="" THEN EXIT LOOP
			NNM$=UCASE$(NNM$)
			IF INSTR(NNM$,"*") THEN BF=%BadName
			BF=CHECKNAME(NNM$)
			IF NNM$=NM$ THEN BF=%Exists
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		IF NNM$<>"" THEN
			NAME NM$ AS NNM$
			PRINT REFILES$(WhichFiles$);
		END IF
	LOOP
RETURN
:
TERMINAL:
	Jump=0
	LOCATE 23,2
	CALL ChPath(RAM$)
	SHELL "REN * *.Q$} > NUL"
	IF ASCII(TXPath$)=ASCII(CDPath$) THEN
		IF WARNDISK(CDPath$,CDProg$+".EXE","PROGRAM") THEN RETURN
	END IF
	IF INSTR(TXProg$, ANY " /\-") THEN
		ComProg$=LEFT$(TXProg$,INSTR(TXProg$, ANY " /\-")-1)
	ELSE
		ComProg$=TXProg$
	END IF
	IF WARNDISK(TXPath$,ComProg$,"PROGRAM") THEN RETURN
	CALL ChPath(TXPath$)
	PRINT TOPLINE$;
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	LOCATE 6,3,0:PRINT "Loading communications program...";
	SHELL TXProg$
	LOCATE 23,2,0
	ReCom:
	CALL ChPath(RAM$)
	SHELL	"REN \*.Q$} * > NUL"
	PRINT BORDER$;
RETURN
:
QUITTA:
	Jump=0:KeyOn=%FALSE
	CALL ChPath(CDPath$)
	IF WARNDISK(CDPath$,CDProg$+".EXE","PROGRAM") THEN MENU
	CLS:END
RETURN
:
COMPA:
	Heading$="COMPRESS FILES"
	Title$=SPACE$(32)+"Name for Compressed file: [        ]"
	Wild=%FALSE
	ReComp:
	CALL DoScreen
	DO
		DO
			OutFile$=""
			PRINT TITEL$(Title$);
			NM$=GETNM$(Drive$)
			IF NM$="" THEN RETURN
			OutFile$=GETSHRINK$
		LOOP WHILE OutFile$=""
		IF OutFile$=CHR$(%Esc) THEN RETURN
		IF NOT DOSHRINK(NM$,OutFile$,"") THEN RETURN
		PRINT REFILES$(WhichFiles$);
	LOOP
RETURN
:
DECOMPA:
	Heading$="DECOMPRESS FILES"
	WhichFiles$="*.ZIP":Wild=%FALSE
	ReDecomp:
	CALL DoScreen
	DO
		NM$=GETNM$(Drive$)
		IF NM$="" THEN
			IF Jump THEN
				WhichFiles$="*.*"
				PRINT REFILES$(WhichFiles$);
			END IF
			RETURN
		END IF
		IF NOT CHKCOMP(NM$)THEN RETURN
		IF NOT EXPAND(NM$,"") THEN RETURN
		PRINT CLEARLINE$(%SelLine,77,0);
		PRINT SPACEFREE$(0);
	LOOP
RETURN
:
WIPA:
	Heading$="WIPE FILES"
	IF Jump=0 THEN Drive$=DDPath$
	Wild=%FALSE
	ReWipe:
	DO
		CALL DoScreen
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		IF ASCII(UTPath$)=ASCII(CDPath$) THEN
			IF WARNDISK(CDPath$,CDProg$+".EXE","PROGRAM") THEN RETURN
		END IF
		IF WARNDISK(UTPath$,"WIPEFILE.EXE","PROGRAM") THEN RETURN
		CLS:SHELL UTPath$+"\WIPEFILE "+NM$+" /G"
		PRINT BORDER$;
		PRINT REFILES$(WhichFiles$);
	LOOP
RETURN
:
WDISK:
	Heading$="WIPE ERASED & FREE SPACE"
	IF Jump=0 THEN Drive$=DDPath$
	ReWdisk:
	DO
		NoDraw=%TRUE
		CALL DoScreen
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		IF ASCII(UTPath$)=ASCII(CDPath$) THEN
			IF WARNDISK(CDPath$,CDProg$+".EXE","PROGRAM") THEN RETURN
		END IF
		IF WARNDISK(UTPath$,"WIPEDISK.EXE","PROGRAM") THEN RETURN
		CLS:SHELL UTPath$+"\WIPEDISK "+Drive$+" /E"
		PRINT BORDER$;
	LOOP
RETURN
:
HIDA:
	Heading$="HIDE FILES"
	IF Jump=0 THEN Drive$=DDPath$
	ReHide:
	CALL DoScreen
	DO
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		ATTRIB NM$,2
		PRINT REFILES$(WhichFiles$);
	LOOP
RETURN
:
UNHIDA:
	Heading$="UNHIDE FILES"
	IF Jump=0 THEN Drive$=DDPath$
	NoDraw=%TRUE
	ReUnhide:
	CALL DoScreen
	DO
		DO
			NM$=GETNM$(Drive$)
			IF NM$="" THEN RETURN
			IF NOT EXIST(Drive$,NM$,2) THEN BF=%NoFile ELSE BF=0
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		ATTRIB NM$,0
		PRINT REFILES$(WhichFiles$);
	LOOP
RETURN
:
ALTA:
	Heading$="ALTER TIME/DATE OF FILES"
	DIM DT$(1:6)
	ReAlter:
	CALL DoScreen
	DO
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		LOCATE %SelLine,3
		PRINT "New date (dd|mm|yy): [  |  |  ]  New time (hh|mm|ss): [  |  |  ]"
		Start=25
		FOR N=1 TO 6
			DO
				X=Start
				BF=%FALSE
				LOCATE %SelLine,X:PRINT CHR$(219);" "
				DT$(N)=KEYA$(%SelLine,X,47,57,2)
				IF DT$(N)=CHR$(%Esc) THEN RETURN
				IF DT$(N)="" THEN EXIT FOR
				IF LEN(DT$(N))=1 THEN DT$(N)="0"+DT$(N)
				IF N=1 THEN IF VAL(DT$(N))>31 THEN BF=%TRUE
				IF N=2 THEN IF VAL(DT$(N))>12 THEN BF=%TRUE
				IF N=4 THEN IF VAL(DT$(N))>24 THEN BF=%TRUE
				IF N=5 THEN IF VAL(DT$(N))>59 THEN BF=%TRUE
				IF N=6 THEN IF VAL(DT$(N))>59 THEN BF=%TRUE
				IF BF THEN SOUND 88,8
			LOOP WHILE BF
			IF N=3 THEN INCR Start,24
			INCR Start,3
		NEXT N
		IF N=7 THEN
			NewDate$=DT$(2)+"/"+DT$(1)+"/"+DT$(3)
			NewTime$=DT$(4)+":"+DT$(5)+":"+DT$(4)
			IF ASCII(UTPath$)=ASCII(CDPath$) THEN
				IF WARNDISK(CDPath$,CDProg$+".EXE","PROGRAM") THEN RETURN
			END IF
			IF WARNDISK(UTPath$,"FDATE.COM","PROGRAM") THEN RETURN
			LOCATE %SelLine,3
			SHELL UTPath$+"\FDATE "+NM$+" "+NewDate$+" "+NewTime$
		END IF
		PRINT CLEARLINE$(%SelLine,77,0);
	LOOP
RETURN
:
MERJA:
	Heading$="MERGE FILES"
	Title$=SPACE$(38)+"Join to: ["+SPACE$(27)+"]"
	CALL DoScreen
	ReMrg:
	DO
		PRINT TITEL$(Title$);
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		DO
			BF=0:X=51
			LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(26)
			MNM$=KEYA$(%SelLine,X,33,126,27)
			IF MNM$=CHR$(%Esc) THEN RETURN
			IF MNM$="" THEN EXIT LOOP
			MNM$=UCASE$(MNM$)
			IF LEFT$(MNM$,1)="\" THEN
				MNM$=LEFT$(CURDIR$,2)+MNM$
			END IF
			IF INSTR(MNM$,":") THEN
				IF OKDIR(MNM$) THEN
					IF TALLY(MNM$,"\") THEN
						Path$=FINDPATH$(MNM$)
						Nom$=LTRIM$(REMOVE$(MNM$,Path$),"\")
						IF NOT EXIST(Path$,Nom$,0) THEN BF=%NoFile
					ELSE
						Nom$=RIGHT$(MNM$,LEN(MNM$)-2)
						IF NOT EXIST(LEFT$(MNM$,2),Nom$,0) THEN BF=%NoFile
					END IF
				ELSE
					BF=%NoDir
				END IF
			ELSE
				BF=CHECKFILE(MNM$)
			END IF
			IF INSTR(MNM$,"*") THEN BF=%BadName
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		IF MNM$<>"" THEN
			PRINT CLEARLINE$(%SelLine,77,1);
			PRINT "Merging '";NM$;"' to '";MNM$;"'...";
			OPEN NM$ FOR BINARY AS #1
			OPEN MNM$ FOR BINARY AS #2
			Repeat=0:Extra=0:Length&=LOF(1)
			LongFile=CHKLEN(Repeat,Extra,LenBite,Length&)
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						GET$ #1,Extra,PutStr$
					ELSE
						GET$ #1,LenBite,PutStr$
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
			PRINT REFILES$(WhichFiles$);
		END IF
	LOOP
RETURN
:
CONFIG:
	PRINT TOPLINE$;
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	KeyOn=%FALSE
	RESTORE CONFIG
	FOR N=1 TO 11
		READ ConfigList$
		COLOR %LCyan
		LOCATE N+1,3:PRINT CHR$(N+64);
		COLOR Ink
		PRINT TAB(7);ConfigList$;TAB(36);"-   ";DR$(N);
	NEXT N
	DATA "Colour, B&W or LCD",Default level (1 - 4),Path for Coder program
	DATA Path for data/message disk,RAM disk,Path for editor program
	DATA Name of editor program,Path for comms program,Name of comms program
	DATA Path for utilities,Encrypted file extension
	PRINT HORLINE$;
	LOCATE %SelLine,3:PRINT "Press letter of option to change";TAB(52);
	COLOR %LCyan
	PRINT "S";
	COLOR Ink
	PRINT " - Save changes"
	LOCATE %HeadLine,3:COLOR BWYellow,%Cyan:PRINT "CHANGE CONFIGURATION":COLOR Ink,Paper
	PRINT ESCMAIN$;
	ReConfig:
	DO
		DO
			LOCATE %SelLine,36
			COLOR %Flash+Ink:PRINT CHR$(219):COLOR Ink
			Choice$=UCASE$(INPUT$(1))
			IF ASC(Choice$)=>65 AND ASC(Choice$)<=75 THEN
				LOCATE%SelLine,36:PRINT " "
				TempName$=""
				Opt=ASC(Choice$)-64
				Y=Opt+1:X=40
				LOCATE Y,X,1,0,7
				DO
					DO
						Char$=INKEY$
					LOOP WHILE Char$=""
					IF LEN(Char$)=2 THEN
						IF ASC(RIGHT$(Char$,1))=%LfAr THEN
							Char$=CHR$(%Bsp)
						ELSE
							Char$=" "
						END IF
					END IF
					SELECT CASE Char$
						CASE CHR$(%Ent)
							IF TempName$<>"" THEN DR$(Opt)=RTRIM$(TempName$,"\")
							CALL SetParams
							Level=VAL(Level$)-1
							EXIT LOOP
						CASE CHR$(%Esc)
							LOCATE Y,40:PRINT SPACE$(40)
							LOCATE Y,40:PRINT DR$(Opt)
							EXIT LOOP
						CASE CHR$(%Bsp)
							DECR X
							IF X<40 THEN X=40:EXIT SELECT
							LOCATE Y,X:PRINT CHR$(32):LOCATE Y,X
							TempName$=LEFT$(TempName$,LEN(TempName$)-1)
						CASE ELSE
							IF X<78 THEN
								PRINT UCASE$(Char$);SPACE$(39-LEN(TempName$));
								INCR X:LOCATE Y,X
								TempName$=TempName$+UCASE$(Char$)
							ELSE
								SOUND 88,8
							END IF
					END SELECT
				LOOP
				LOCATE ,,0
			END IF
			IF Choice$="S" THEN
				IF WARNDISK(CDPath$,"TECOD.CNF","PROGRAM") THEN RETURN
				OPEN CDPath$+"\TECOD.CNF" FOR OUTPUT AS #1
				FOR N=1 TO 11
					PRINT #1,DR$(N)
				NEXT N
				CLOSE
			END IF
		LOOP UNTIL Choice$=CHR$(%Esc) OR Choice$=CHR$(%Ent)
		IF Choice$=CHR$(%Esc) OR Choice$=CHR$(%Ent) THEN
			JUMP=0:EXIT LOOP
		END IF
	LOOP
RETURN
:
DOS:
	Jump=0
	COLOR Ink,%Black
	CLS
	PRINT "Type EXIT to return to program..."
	SHELL
	PRINT BORDER$;
RETURN
:
FUNCTION ERRA(BF)
	ERRA=%FALSE
	IF BF=0 THEN EXIT FUNCTION
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
	IF WARNERR(ERRMSG$) THEN RESUME MENU
RESUME MENU
:
SUB SetParams SHARED
	StartInk=ASCII(DR$(1))
	SELECT CASE StartInk
		CASE 66
			StartInk=&H0000
			Ink=%HWhite:Paper=%Black:BWYellow=Paper
		CASE 67
			StartInk=&H1000
			Ink=%HWhite:Paper=%Blue:BWYellow=%Yellow
		CASE 76
			StartInk=&H0000
			Ink=%White:Paper=%Black:BWYellow=Paper
	END SELECT
	Level$=DR$(2):CDPath$=DR$(3):DDPath$=DR$(4):RAM$=DR$(5):EDPath$=DR$(6)
	EDProg$=DR$(7):TXPath$=DR$(8):TXProg$=DR$(9):UTPath$=DR$(10):Ext$=DR$(11)
	Level=VAL(Level$)-1
END SUB
:
SUB PASSWORD SHARED
	Pass$="":PL=10
	DIM PW$(PL)
	PW$(9)=CHR$(66):PW$(4)=CHR$(66):PW$(1)=CHR$(84):PW$(5)=CHR$(79):PW$(2)=CHR$(73)
  PW$(3)=CHR$(77):PW$(6)=CHR$(66):PW$(8)=CHR$(77):PW$(10)=CHR$(79):PW$(7)=CHR$(73)
	FOR N=1 TO PL:Pass$=Pass$+PW$(N):NEXT N
	ERASE PW$
	COLOR Ink,Paper:CLS
	LOCATE 12,20,0:PRINT "ENTER PASSWORD: [               ]"
	DO
		Y=12:X=37
		LOCATE Y,X:PRINT CHR$(219);SPACE$(14)
		PSW$=KEYA$(Y,X,32,126,15)
		IF PSW$="" THEN CLS:END
		PSW$=UCASE$(PSW$)
		IF PSW$=Pass$ THEN EXIT LOOP
		SOUND 88,8
	LOOP
	PW=%FALSE
END SUB
