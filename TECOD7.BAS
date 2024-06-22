$ERROR 	PARAM+,		BOUNDS-, 	NUMERIC-,		OVERFLOW-, 		STACK-
$LIB 		LPT-, 		COM-, 		GRAPH-, 		FULLFLOAT-,  IPRINT-
$OPTION	CNTLBREAK-,					AUTODIM-

$FLOAT NPX			'Alt: NPX (CoPro present),EMULATE, PROCEDURE (larger but faster)
$CPU 80386			'Alt: 8086, 80286, 80386
$OPTIMIZE SPEED 'Alt SIZE

CDProg$="TECOD"
DEFINT A-Z
%TE=-1:%TK=0:%TC=0:%TK5=0

$INCLUDE "BIOS.INC"
$INCLUDE "CHKERR.INC"
$INCLUDE "CODSUBS.INC"
$INCLUDE "DOS.INC"
$INCLUDE "KONST.INC"
$INCLUDE "SCREEN.INC"

$STACK 2048

DIM KeyFun$(1:20),DR$(1:12)
DIM FileEntry$(140),FileLen&(140)
DIM Tag$(140),TagList$(140)

%BadName=1	:%NoCopy=2	:%Corrupt=3		:%TooLong=4		:%NoData=5
%DecDone=6	:%Full=7		:%NoDir=8			:%Failed=9		:%NoFile=10
%Exists=11	:%NoWipe=12	:%BadFile=13	:%BadSav=14		:%NoEx=15
%LenBite=10000

Jump=0:StartDir$=CURDIR$

ON ERROR GOTO ABORT
OPEN "A:\"+CDProg$+".CNF" FOR INPUT AS #1
FOR L=1 TO 12
	LINE INPUT #1,DR$(L)
	IF L=12 THEN DR$(L)=CODEKEY$(DR$(L))
NEXT L
CLOSE

CALL SetParams
CALL ReadKeys(KeyFun$())
IF PASSWORD THEN CLS:END
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
			CALL ClearWindow(%TopLeft1,%BottomRight1)
			CALL ClearWindow(%TopLeft3,%BottomRight3)
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
			PRINT CENTRE$(25,"Version  7.20");
			COLOR Ink
			DO
				DO
					CH$=INKEY$
					IF CH$=CHR$(%Ent) THEN CALL QUITTA
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
			CASE %F1,%F2				:CALL Copya(M)
			CASE %F3,%AltF3			:CALL Della(M)
			CASE %F4						:CALL Enca
			CASE %F5,%F9				:CALL Deca(M)
			CASE %F6,%F7				:CALL Editor(M)
			CASE %F8,%AltF8			:CALL Config
			CASE %F10						:CALL Quitta
			CASE %AltF1					:CALL Merja
			CASE %AltF2					:CALL Wdisk
			CASE %AltF4,%AltF5	:CALL Compa(M)
			CASE %AltF6					:CALL SafeSav
			CASE %AltF7					:CALL SafeLod
			CASE %AltF9					:CALL Runna
			CASE %AltF10				:CALL DOS
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
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
	DO
		DO
			NM$=GETNM$(Drive$)
			IF NM$="" THEN EXIT SUB
			KillDone=%FALSE
			YN$=YESNO$(Action$+LTRIM$(NM$," ")+" ? (Y/N): Y")
			IF YN$=CHR$(%Esc) THEN EXIT SUB
		LOOP WHILE YN$="N"
		FOR N=0 TO FF
			IF INSTAT THEN EXIT SUB
			IF DoTags THEN
				NM$=TagList$(N)
			END IF
			IF NM$<>"" THEN
				CLEARLINE$ %SelLine,77,1
				PRINT Doing$;NM$;"...";
				CALL WipeFile(WipeCount,NM$)
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
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
	DO
		DO
			NM$=GETNM$(Drive$)
			IF NM$="" THEN EXIT SUB
			CLEARLINE$ %ActLine,40,0
			CALL ClearWindow(%TopLeft3,%BottomRight3)
			IF DoTags THEN
				LOCATE %SelLine,3
				PRINT "Encipher";NM$;SPACE$(15);"Name for Output file: [        ]";
				OutFile$=GETSHRINK$
				IF OutFile$=CHR$(%Esc) THEN EXIT SUB
				IF OutFile$="" THEN
					CALL TidyUp
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
				LL=0:PAGE=0
				SendData$=GETENCKEY$(PAGE,LL)
				IF SendData$=CHR$(%Esc) THEN
					EXIT SUB
				ELSEIF SendData$="" THEN
					EXIT LOOP
				END IF
			END IF
			CALL DOZIP(M,DoTags,NM$,RAM$,OutFile$,OutName$,ZipKey$)
			IF INSTAT THEN
				KILL OutFile$+".ZIP"
				EXIT SUB
			END IF
			OPEN OutFile$+".ZIP" FOR BINARY AS #1
			LenMsg&=LOF(1):CLOSE
			IF Level THEN
				CRINT "Ready to encipher "+LTRIM$(NM$)+"", 0
				IF WARNDISK(DDPath$,"DATAPTR","DATA") THEN
					KILL OutFile$+".ZIP"
					EXIT SUB
				END IF
				SELECT CASE LEVEL
					CASE 1:LenData=36
					CASE 2:LenData=LenMsg&\5:MaxLength&=163835
					CASE 3:LenData=LenMsg&  :MaxLength&=32750
				END SELECT
				IF Level>1 THEN
					IF LenMsg&>MaxLength& THEN
						BF=%TooLong
						IF BF THEN
							KILL OutFile$+".ZIP"
							IF ERRA(BF) THEN EXIT SUB ELSE EXIT LOOP
						END IF
					END IF
				END IF
				SM&=0:SendData$=GETDATA$(SM&,LenData,"SNUM")
				IF SendData$="" THEN
					KILL OutFile$+".ZIP"
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
			CLEARLINE$ %ActLine,77,0
			LOCATE %ActLine,3:PRINT "File length:";LenMsg&;"characters";
			CPRINT "Enciphering "+EncFile$+"...", 1
			SM$=STARTM$(Level,SM&)
			IF Level=0 THEN
				SM$=SM$+CHR$(PAGE)+CHR$(LL)
			END IF
			PUT$ #2,SM$
			LongFile=CHKLEN(Repeat,Extra,LenMsg&)
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
						LenData=WipeCount
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
				CALL DiskFlush(SM&,LenData,"SNUM")
			END IF
			KILL OutFile$+".ZIP"
			PRINT REFILES$(WhichFiles$);
		LOOP WHILE BF
		CALL TidyUp
	LOOP
END SUB
:
FUNCTION ENCLEV2$(LoopNo,Message$,EncData$)
	LOCAL KeyPos
	LENMSG=LEN(Message$)
	PREV=INITRAND(2,EncData$)
	DEF SEG=STRSEG(Message$)
	SP&=STRPTR(Message$)
	$EVENT OFF
	FOR ENC=1 TO LENMSG
		RL=INT(RND*256)
		CD=((PEEK(SP&)+RL) AND &HFF) XOR PREV
		POKE SP&,CD
		PREV=RL
		IF RL MOD 6=0 THEN
			R=ASC(MID$(EncData$,(KeyPos MOD 36)+1,1))
			RANDOMIZE LoopNo+ENC+R
			INCR KeyPos
		END IF
		INCR SP&
	NEXT ENC
	$EVENT ON
	ENCLEV2$=Message$
	DEF SEG
END FUNCTION
:
FUNCTION ENCLEV3$(LoopNo,Message$,EncData$,WC)
	LENMSG=LEN(Message$)
	PREV=INITRAND(3,EncData$)
	DEF SEG=STRSEG(Message$)
	SP&=STRPTR(Message$)
	$EVENT OFF
	FOR ENC=1 TO LENMSG
		RL=INT(RND*256)
		CD=((PEEK(SP&)+RL) AND &HFF) XOR PREV
		POKE SP&,CD
		IF RL MOD 6=0 THEN
			RL=ASC(MID$(EncData$,WC+1,1))
			RANDOMIZE RL+ENC+LoopNo
			INCR WC
		END IF
		PREV=RL
		INCR SP&
	NEXT ENC
	$EVENT ON
	ENCLEV3$=Message$
	DEF SEG
END FUNCTION
:
FUNCTION ENCLEV4$(Message$,EncData$)
	LENMSG=LEN(Message$)
	PREV=ASC(RIGHT$(EncData$,1))
	DEF SEG=STRSEG(Message$)
	SP&=STRPTR(Message$)
	$EVENT OFF
	FOR ENC=1 TO LENMSG
		RL=ASC(MID$(EncData$,ENC,1))
		CD=((PEEK(SP&)+RL) AND &HFF) XOR PREV
		POKE SP&,CD
		PREV=RL
		INCR SP&
	NEXT ENC
	$EVENT ON
	PUT$ #2,Message$
	DEF SEG
END FUNCTION
:
FUNCTION GETENCKEY$(PAGE,LL)
	LOCATE %SelLine,3,0
	PRINT "Page No: [   ]   Line No: [  ]   ";
	PRINT "Key: [";SPACE$(36);"]";
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
		X=42:CRS=36
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
	RANDOMIZE PAGE
	LL=(LNE XOR INT(RND*256))
	RANDOMIZE PAGE*LNE
	GETENCKEY$=KeyWord$
END FUNCTION
:
FUNCTION GETDATA$(SM&,LenKey???,DataFile$)
	SHARED DDPath$,Ink
	NULL$=STRING$(5,0)
	IF DataFile$="SNUM" THEN
		OPEN DDPath$+"\DATAPTR" FOR INPUT AS #1
		INPUT #1,SM&:CLOSE
		IF SM&+LenKey???>360000??? THEN
			GETDATA$="":EXIT FUNCTION
		END IF
	END IF
	OPEN DDPath$+"\"+DataFile$ FOR BINARY AS #1
	CPRINT "Reading data...", 1
	DO
		BadData=%FALSE
		SEEK #1,SM&:GET$ #1,LenKey???,KeyData$
		IF DataFile$="SNUM" THEN
			PosBad???=INSTR(KeyData$,NULL$)
			IF PosBad??? THEN
				BadData=%TRUE
				SM&=SM&+PosBad???+10???
			END IF
		ELSE
			IF LEFT$(KeyData$,5)=NULL$ THEN
				KeyData$=""
				EXIT LOOP
			END IF
		END IF
	LOOP WHILE BadData
	CLOSE
	GETDATA$=KeyData$
END FUNCTION
:
FUNCTION STARTM$(Level,SM&)
	RANDOMIZE TIMER
	StartRand=INT(RND*256) AND &HFC
	IF Level=0 THEN STARTM$=CHR$(StartRand):EXIT FUNCTION
	RANDOMIZE StartRand
	SecondRand=INT(RND*256) AND &HF8
	GetPos$=MKDWD$(SM&)
	Char1$=CHR$(StartRand XOR Level)
	Char2$=CHR$(SecondRand XOR ASC(MID$(GetPos$,3,1)))
	Char3$=MID$(GetPos$,2,1)
	Char4$=LEFT$(GetPos$,1)
	STARTM$=CHAR1$+CHAR2$+CHAR3$+CHAR4$
END FUNCTION
:
FUNCTION INITRAND(Lev,KeyData$)
	LOCAL StartRand
	LenKey=LEN(KeyData$)
	SELECT CASE Lev
		CASE 2
			FOR N=1 TO LenKey
				INCR StartRand,ASC(MID$(KeyData$,N,1))
			NEXT N
		CASE 3
		StartRand=LenKey XOR ASCII(KeyData$)
	END SELECT
	RANDOMIZE StartRand
	INITRAND=INT(RND*256)
END FUNCTION
:
SUB DiskFlush(SM&,BytesToWipe,DataFile$)
	SHARED DDPath$
	DataLeft$=LTRIM$(STR$(CEIL(((SM&+BytesToWipe)/360000)*100)))
	LOCATE %ActLine,3,0:PRINT "Data disk ";DataLeft$;" per cent used                  ";
	CPRINT "Wiping used data...", 1
	OPEN DDPath$+"\"+DataFile$ FOR BINARY AS #1
	WipeData$=STRING$(BytesToWipe,0)
	SEEK #1,SM&
	PUT$ #1,WipeData$
	CLOSE
	IF DataFile$="SNUM" THEN
		SM&=SM&+BytesToWipe
		OPEN DDPath$+"\DATAPTR" FOR OUTPUT AS #1
		PRINT #1,SM&;
		CLOSE
	END IF
END SUB
:
SUB Deca(M) SHARED
	IF M>%F5 THEN
		CALL TERMINAL
		Drive$=RAM$
		CALL ChPath(Drive$)
		Jump=0
	END IF
	Heading$="DECIPHER FILES"
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
	DO
		KeyOn=%TRUE:Tagable=%FALSE:BF=0
		NM$=GETNM$(Drive$)
		IF NM$="" THEN EXIT SUB
		OPEN NM$ FOR BINARY AS #1
		LenCgm???=LOF(1):GET$ #1,4,Ver$:CLOSE
		Ver=ASC(LEFT$(Ver$,1)) AND 3
		IF Ver THEN Header=4 ELSE Header=3
		DECR LenCgm???,Header
		KeyOn=%FALSE
		CALL ClearWindow(%TopLeft3,%BottomRight3)
		CLEARLINE$ %ActLine,40,0
		SELECT CASE Ver
			CASE 0
				RecData$=GETDECKEY$(Ver$)
				IF RecData$=CHR$(%Esc) THEN EXIT SUB
				IF RecData$="" THEN BF=%TRUE
			CASE 1,2,3
				IF WARNDISK(DDPath$,"RNUM","DATA") THEN EXIT SUB
				SM&=READSM???(Ver$)
				IF SM&=%TRUE THEN
					BF=%BadFile
					IF ERRA(BF) THEN EXIT SUB ELSE EXIT SELECT
				END IF
				SELECT CASE Ver
					CASE 1:LenData=36
					CASE 2:LenData=LenCgm???\4
					CASE 3:LenData=LenCgm???
				END SELECT
				RecData$=GETDATA$(SM&,LenData,"RNUM")
				IF RecData$="" THEN
					BF=%DecDone
					IF ERRA(BF) THEN EXIT SUB
				END IF
		END SELECT
		DO WHILE BF=0
			CLEARLINE$ %ActLine,77,0
			LOCATE %ActLine,3:PRINT "File length:";LenCgm???;"characters";
			CPRINT "Deciphering ";LTRIM$(NM$," ");" (Level "+CHR$(49+Ver)+")...", 1
			IF INSTR(NM$,".")THEN
				DNM$=LEFT$(NM$,(INSTR(NM$,".")-1))+".ZIP"
			ELSE
				DNM$=NM$+".ZIP"
			END IF
			OPEN NM$ FOR BINARY AS #1
			OPEN DNM$ FOR BINARY AS #2
			SEEK #1,Header
			LongFile=CHKLEN(Repeat,Extra,LenCgm???)
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						GET$ #1,Extra,CGM$
					ELSE
						GET$ #1,%LenBite,CGM$
					END IF
				ELSE
					GET$ #1,LenCgm???,CGM$
				END IF
				SELECT CASE Ver
					CASE=0,1
						Decrypt$=DECLEV2$(LN,CGM$,RecData$)
					CASE=2
						WipeCount=0
						Decrypt$=DECLEV3$(LN,CGM$,RecData$,WipeCount)
						LenData=WipeCount
					CASE=3
						Decrypt$=DECLEV4$(CGM$,RecData$)
				END SELECT
				PUT$ #2,Decrypt$
				CGM$="":Decrypt$=""
			NEXT LN
			CLOSE:RecData$=""
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
				CALL DiskFlush(SM&,LenData,"RNUM")
			END IF
			IF FD1 THEN
				IF WARNDISK(UTPath$,ProgName$,"PROGRAM") THEN EXIT SUB
			END IF
			CPRINT "Decompressing "+DNM$+"...", 1
			SHELL UTPath$+"\PKUNZIP -O"+ZipKey$+" "+DNM$+" > NUL"
			KILL DNM$
			PRINT REFILES$(WhichFiles$);
			EXIT LOOP
		LOOP
		CALL TidyUp
	LOOP
END SUB
:
FUNCTION DECLEV2$(LoopNo,CG$,DecData$)
	LOCAL KeyPos
	LENCG=LEN(CG$)
	PREV=INITRAND(2,DecData$)
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	$EVENT OFF
	FOR DEC=1 TO LENCG
		RL=INT(RND*256)
		PK=PEEK(SP&)
		PL=((PK XOR PREV)-RL) AND &HFF
		POKE SP&,PL
		PREV=RL
		IF RL MOD 6=0 THEN
			R=ASC(MID$(DecData$,(KeyPos MOD 36)+1,1))
			RANDOMIZE LoopNo+DEC+R
			INCR KeyPos
		END IF
		INCR SP&
	NEXT DEC
	$EVENT ON
	DECLEV2$=CG$
	DEF SEG
END FUNCTION
:
FUNCTION DECLEV3$(LoopNo,CG$,DecData$,WC)
	LENCG=LEN(CG$)
	PREV=INITRAND(3,DecData$)
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	$EVENT OFF
	FOR DEC=1 TO LENCG
		RL=INT(RND*256)
		PK=PEEK(SP&)
		PL=((PK XOR PREV)-RL) AND &HFF
		POKE SP&,PL
		IF RL MOD 6=0 THEN
			RL=ASC(MID$(DecData$,WC+1,1))
			RANDOMIZE RL+DEC+LoopNo
			INCR WC
		END IF
		PREV=RL
		INCR SP&
	NEXT DEC
	$EVENT ON
	DECLEV3$=CG$
	DEF SEG
END FUNCTION
:
FUNCTION DECLEV4$(CG$,DecData$)
	LENCG=LEN(CG$)
	PREV=ASC(RIGHT$(DecData$,1))
	DEF SEG=STRSEG(CG$)
	SP&=STRPTR(CG$)
	$EVENT OFF
	FOR DEC=1 TO LENCG
		RL=ASC(MID$(DecData$,DEC,1))
		PK=PEEK(SP&)
		PL=((PK XOR PREV)-RL) AND &HFF
		POKE SP&,PL
		PREV=RL
		INCR SP&
	NEXT DEC
	$EVENT ON
	DECLEV4$=CG$
	DEF SEG
END FUNCTION
:
FUNCTION GETDECKEY$(Ver$)
	SHARED Ink
	PAGE=ASC(MID$(Ver$,2,1))
	RANDOMIZE PAGE
	LNE=ASC(MID$(Ver$,3,1)) XOR INT(RND*256)
	RANDOMIZE PAGE*LNE
	IF PAGE<1 OR PAGE>255 OR LNE<1 OR LNE>99 THEN
		IF ERRA(%Corrupt) THEN GETDECKEY$=CHR$(%Esc) ELSE GETDECKEY$=""
		EXIT FUNCTION
	END IF
	CPRINT "Page No:", 0
	COLOR %Yellow:PRINT PAGE;
	COLOR Ink:PRINT TAB(17);" Line No:";
	COLOR %Yellow:PRINT LNE;TAB(35);
	COLOR Ink
	PRINT "Key: [";SPACE$(36);"]";
	DO
		X=41
		LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(35);
		KeyWord$=KEYA$(%SelLine,X,32,126,36)
		IF KeyWord$="" OR KeyWord$=CHR$(%Esc) THEN
			GETDECKEY$=KeyWord$:EXIT FUNCTION
		END IF
		IF LEN(KeyWord$)<36 THEN
			SOUND 88,8
		ELSE
			EXIT LOOP
		END IF
	LOOP
	GETDECKEY$=KeyWord$
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
SUB Editor(M) SHARED
	IF M=%F6 THEN
		Heading$="TYPE A NEW DOCUMENT"
		NoDraw=%TRUE:KeyOn=%FALSE:Jump=0
		CALL ClearWindow(%TopLeft3,%BottomRight3)
		CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
		LOCATE %SelLine,3,0:COLOR Ink,Paper
		PRINT "Document name: [";SPACE$(51);"]";
		OutFile$=OUTNM$(NM$,OutName$,OutPath$,M)
		IF OutFile$="" OR OutFile$=CHR$(%Esc) THEN EXIT SUB
		CALL EDIT(OutPath$,OutName$)
		Drive$=(OutPath$)
		NoDraw=%FALSE:KeyOn=%TRUE:FromMenu=%FALSE
	END IF
	Tagable=%FALSE
	DO
		Heading$="EDIT/READ FILES"
		CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
		NM$=GETNM$(Drive$)
		IF NM$="" THEN EXIT SUB
		CALL EDIT(Drive$,NM$)
		PRINT FUNKEY$;
	LOOP
END SUB
:
SUB EDIT(Path$,NM$)
	SHARED EdPath$,EdProg$
	ProgName$=PROG$(EdProg$)
	IF WARNDISK(EdPath$,ProgName$,"PROGRAM") THEN EXIT SUB
	LOCATE %SelLine,3,0
	CALL ChPath(Path$)
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
		CALL ClearWindow(%TopLeft1,%BottomRight1)
		CALL ClearWindow(%TopLeft3,%BottomRight3)
	END IF
	LOCATE %SelLine,3,0
	PRINT "Loading communications program...";
	CALL ChPath(TXPath$)
	SHELL TXProg$
	M=%F5
	PRINT BORDER$;
	PRINT HORLINE$;
	PRINT FUNKEY$;
END SUB
:
SUB Quitta SHARED
	Jump=0:KeyOn=%FALSE
	CALL ChPath(StartDir$)
	CLS:END
END SUB
:
SUB Merja SHARED
	Heading$="MERGE FILES"
	Title$=SPACE$(38)+"Join to: ["+SPACE$(27)+"]"
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
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
				Length???=LOF(1)
				LongFile=CHKLEN(Repeat,Extra,Length???)
				FOR LN=1 TO Repeat+1
					IF LongFile THEN
						IF LN>Repeat THEN
							GET$ #1,Extra,PutStr$
						ELSE
							GET$ #1,%LenBite,PutStr$
						END IF
					ELSE
						GET$ #1,Length???,PutStr$
					END IF
					LenJoin???=LOF(2)
					SEEK #2,LenJoin???
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
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
	DO
		DO
			NM$=GETNM$(Drive$)
			IF NM$="" THEN EXIT SUB
			FD=ASCII(CURDIR$)
			IF FD<65 OR FD>66 THEN
				IF ERRA(%NoWipe) THEN EXIT SUB ELSE EXIT LOOP
			END IF
			CPRINT "Wiping free space...", 1
			CALL WipeDisk
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
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
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
		CALL DOZIP(M,DoTags,NM$,OutPath$,OutFile$,OutName$,ZipKey$)
		IF RTRIM$(CURDIR$,"\")=OutPath$ THEN PRINT REFILES$(WhichFiles$);
	LOOP
END SUB
:
SUB SafeSav SHARED
	Heading$="STORE FILES"
	Title$=SPACE$(39)+"Add to: ["+SPACE$(27)+"]"
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
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
		CPRINT "Adding "+RTRIM$(NM$," ")+" to "+OutFile$+"...", 1
		IF DoTags THEN NM$="@LIST"
		SHELL UTPath$+"\PKZIP -A"+ZipKey$+" "+OutFile$+" "+NM$+" > NUL"
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
		CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
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
				CALL SHOWZIP(FileEntry$(XHL))
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
				SHELL UTPath$+"\PKUNZIP.EXE "+NM$+" "+XNM$+" -O"+ZipKey$+" "+OutFile$+" > NUL"
				CLEARLINE$ %SelLine,77,0
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
		CALL DoScreen(Heading$,"",Drive$,WhichFiles$)
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
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	KeyOn=%FALSE
	RESTORE SETUP
	FOR N=1 TO 12
		READ ConfigList$
		COLOR %LCyan
		LOCATE N+1,3:PRINT CHR$(N+64);
		COLOR Ink
		PRINT TAB(7);ConfigList$;TAB(36);"-   ";DR$(N);
	NEXT N
	DATA "Colour, B&W or LCD",Default level (1 - 4)
	DATA Path for data/message disk,RAM disk,Path for editor program
	DATA Name of editor program,Path for comms program,Name of comms program
	DATA Path for utilities,Wipe count,Encrypted file extension,Secondary key
	PRINT HORLINE$;
	LOCATE %SelLine,3:PRINT "Press letter of option to change";TAB(52);
	COLOR %LCyan
	PRINT "S";
	COLOR Ink
	PRINT " - Save changes";
	LOCATE %HeadLine,3:COLOR BWYellow,%Cyan:PRINT "CHANGE CONFIGURATION";:COLOR Ink,Paper
	PRINT ESCMAIN$;
	ReConfig:
	DO
		DO
			LOCATE %SelLine,36
			COLOR %Flash+Ink:PRINT CHR$(219);:COLOR Ink
			Choice$=UCASE$(INPUT$(1))
			IF Choice$=CHR$(%Lfd) THEN Choice$=CHR$(%Ent)
			IF ASC(Choice$)=>65 AND ASC(Choice$)<=76 THEN
				LOCATE%SelLine,36:PRINT " ";
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
							LOCATE Y,40:PRINT SPACE$(40);
							LOCATE Y,40:PRINT DR$(Opt);
							EXIT LOOP
						CASE CHR$(%Bsp)
							DECR X
							IF X<40 THEN X=40:EXIT SELECT
							LOCATE Y,X:PRINT CHR$(32);:LOCATE Y,X
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
				IF WARNDISK("A:",CDProg$+".CNF","CONFIG") THEN EXIT SUB
				OPEN "A:\"+CDProg$+".CNF" FOR OUTPUT AS #1
				FOR N=1 TO 12
					IF N<12 THEN
						PRINT #1,DR$(N)
					ELSE
						PRINT #1,CODEKEY$(DR$(12))
					END IF
				NEXT N
				CLOSE
			END IF
		LOOP UNTIL Choice$=CHR$(%Esc) OR Choice$=CHR$(%Ent)
		IF Choice$=CHR$(%Esc) OR Choice$=CHR$(%Ent) THEN
			JUMP=0:EXIT LOOP
		END IF
	LOOP
END SUB
:
FUNCTION CODEKEY$(ShrinKey$)
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
	UTPath$=DR$(9)	:Wipes=VAL(DR$(10)):Ext$=DR$(11):ZipKey$=" -s"+DR$(12)
	Level=VAL(Level$)-1
	IF EDPath$=DDPath$ THEN FD1=%TRUE ELSE FD1=%FALSE
END SUB
:
FUNCTION PASSWORD
	SHARED Ink,Paper,PW
	Pass$="":PL=10:PW=%TRUE
	DIM PW$(PL)
	PW$(9)=CHR$(66):PW$(4)=CHR$(66):PW$(1)=CHR$(84):PW$(5)=CHR$(79):PW$(2)=CHR$(73)
 	PW$(3)=CHR$(77):PW$(6)=CHR$(66):PW$(8)=CHR$(77):PW$(10)=CHR$(79):PW$(7)=CHR$(73)
	FOR N=1 TO PL:Pass$=Pass$+PW$(N):NEXT N
	ERASE PW$
	COLOR Ink,Paper:CLS
	LOCATE 12,20,0:PRINT "ENTER PASSWORD: [               ]";
	DO
		Y=12:X=37
		LOCATE Y,X:PRINT CHR$(219);SPACE$(14);
		PSW$=KEYA$(Y,X,32,126,15)
		IF PSW$="" THEN PASSWORD=%TRUE:EXIT FUNCTION
		PSW$=UCASE$(PSW$)
		IF PSW$=Pass$ THEN EXIT LOOP
		SOUND 88,8
	LOOP
	PW=%FALSE
	PASSWORD=%FALSE
END FUNCTION
:
ABORT:
	CLS
	PRINT "DATA NOT FOUND - ABORTING!"
	END
