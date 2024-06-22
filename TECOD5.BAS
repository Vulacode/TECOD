$INCLUDE "BIOS.INC"
$INCLUDE "CHKERR.INC"
$INCLUDE "DOS.INC"
$INCLUDE "KONST.INC"
:
$STACK 2048
:
CLEAR:DEFINT A-Z
DIM CKS(26),CKR(26)
DIM ALF(256),ALPH0(256),RALF(256),RALPH0(256),RALPH1(256)
DIM FileEntry$(140),FileLen&(140),KeyFun$(1:20),SK$(256),DR$(1:14)
:
OPEN "A:\TECOD5.CNF" FOR INPUT AS #1
FOR N=1 TO 14
	LINE INPUT #1,DR$(N)
NEXT N
CLOSE
:
ForeG=VAL(DR$(1))
VER=VAL(DR$(2)):SER=VAL(DR$(3)):FD1=VAL(DR$(4))
PW=%TRUE:Jump=0:%BadName=3:%NoFile=18
NULL$=STRING$(10,0)
Arrows$=CHR$(27)+CHR$(24)+CHR$(25)+CHR$(26)+" to highlight file, ENTER selects"
ClearLine$=SPACE$(77):HorLine$=CHR$(199)+STRING$(78,196)+CHR$(182)
LenBite=12000
:
FOR N=1 TO 20
	READ KeyFun$(N)
NEXT N
DATA "Load ","Save ","Del  ","Enc  ","Dec  ","ForeG ","Read ",Renam,Comms,Quit
DATA Compr,Decom,"Wipe ",Wdisk,"Hide ",Unhid,"Date ",Files,Setup,Quit
:
GOSUB PASSWORD
GOSUB BORDER
:
MENU:
	DO
		BF=0:M=0
		CD=%FALSE:RR=%FALSE:KeyOn=%TRUE
		NoDraw=%FALSE:WhichFiles$="*.*"
		ON ERROR GOTO ERMESG
		IF Jump THEN SkipMenu
		HL=0:LH=0:XF=%FALSE:XFH=%FALSE:XHL=0
		FLP2$=DR$(6):RAM$=DR$(7)
		CALL ClearWindow(%TopLeft1,%BottomRight1)
		CALL ClearWindow(%TopLeft3,%BottomRight3)
		SELECT CASE VER
			CASE 0:VER$=" Disk  Version "
			CASE 1:VER$=" ASCII Version "
			CASE 2:VER$="Keyword Version"
		END SELECT
		LOCATE 2,35:COLOR %PAPER,ForeG:PRINT " C O D E R ":COLOR ForeG,%PAPER
		LOCATE 4,33:PRINT VER$
		LOCATE 6,34:COLOR %PAPER,ForeG:PRINT " MAIN   MENU ":COLOR ForeG,%PAPER
		LOCATE 8,27:PRINT "F1  : Load files from disk"
		LOCATE 9,27:PRINT "F2  : Save files onto disk"
		LOCATE 10,27:PRINT "F3  : Delete files on disk"
		LOCATE 12,27:PRINT "F4  : Encipher files"
		LOCATE 13,27:PRINT "F5  : Decipher files"
		LOCATE 15,27:PRINT "F6  : ForeG a new document"
		LOCATE 16,27:PRINT "F7  : Edit/read documents"
		LOCATE 17,27:PRINT "F8  : Rename files"
		LOCATE 18,27:PRINT "F9  : Terminal"
		LOCATE 20,27:PRINT "F10 : Quit"
		LOCATE 23,3:PRINT "ALT Menu: Alt F1 - F10";TAB(59);"Version: Alt A, D, K"
		LOCATE 25,34:PRINT "Version  5.26";
		DO
		  DO
				CH$=INKEY$
				IF CH$=CHR$(%Esc) OR CH$=CHR$(%Ent) THEN QUITTA
			LOOP WHILE LEN(CH$)<2
		  CH$=RIGHT$(CH$,1)
		  V=ASC(CH$)-30
		  SELECT CASE V
		  	CASE 0
					VER=1:VER$=" ASCII Version ":GOSUB CHARS:LOCATE 4,33:PRINT VER$
				CASE 2
					VER=0:VER$=" Disk  Version ":LOCATE 4,33:PRINT VER$
				CASE 7
					VER=2:VER$="Keyword Version":LOCATE 4,33:PRINT VER$
			END SELECT
			M=ASC(CH$)
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
				CASE %AltF8:GOSUB LISTA
				CASE %AltF9:GOSUB CONFIG
				CASE %AltF10:RETURN MENU
			END SELECT
		LOOP UNTIL M=>%F1 AND M<=%F10 OR M=>%AltF1 AND M<=%AltF10
	LOOP
:
LOADA:
	Heading$="LOAD INTO MEMORY FILES FROM DRIVE "
	HeadX=37
	IF Jump THEN
		CALL ReScreen(HeadX,Heading$,WhichFiles$)
	ELSE
		CALL NewScreen(DR$(6),WhichFiles$,Heading$+DR$(6))
	END IF
	ReLoad:
	DO
		DO
			BF=0
			DO
				CD=%FALSE
				GOSUB SELECTA
				IF NM$="" OR Jump THEN RETURN
				IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					CD=%TRUE:FLP2$=NEWDIR$(NM$)
					CALL ReScreen(HeadX,Heading$,WhichFiles$)
				END IF
			LOOP WHILE CD
			BF=CheckFile(NM$,BF)
			IF BF THEN GOSUB ERRA
		LOOP WHILE BF
		LOCATE %SelLine,2:PRINT ClearLine$
		LOCATE %SelLine,3,1:PRINT "Copying '";NM$;"'...";
		IF LEN(DR$(7))=2 THEN DR$(7)=DR$(7)+"\"
		SHELL "COPY "+NM$+" "+DR$(7)+" > NUL"
		LOCATE %SelLine,2:PRINT ClearLine$
	LOOP
RETURN
:
SAVA:
	Heading$="SAVE FILES IN MEMORY ONTO DRIVE "
	HeadX=35:FLP2$=DR$(6)
	IF Jump AND RTRIM$(CURDIR$,"\")=DR$(7) THEN
		CALL ReScreen(HeadX,Heading$,WhichFiles$)
		COLOR %PAPER,ForeG
		LOCATE %HeadLine,HeadX:PRINT FLP2$;
		COLOR ForeG,%PAPER
		PRINT SPACE$(24-LEN(FLP2$))
	ELSE
		CALL NewScreen(DR$(7),WhichFiles$,Heading$+FLP2$)
	END IF
	ReSave:
	DO
		DO
			BF=0
			DO
				CD=%FALSE
				GOSUB SELECTA
				IF NM$="" OR Jump THEN RETURN
				IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					IF NOT VALIDRIVE(NM$) THEN
						BF=17:EXIT LOOP
					END IF
					CD=%TRUE
					FLP2$=RTRIM$(NM$,"\")
					COLOR %PAPER,ForeG
					LOCATE %HeadLine,HeadX:PRINT FLP2$;
					COLOR ForeG,%PAPER
					PRINT SPACE$(24-LEN(FLP2$))
				END IF
			LOOP WHILE CD
			IF BF<>17 THEN
				IF FLP2$=RTRIM$(CURDIR$,"\") THEN BF=4
				BF=CHECKFILE(NM$,BF)
			END IF
			IF BF THEN GOSUB ERRA
		LOOP WHILE BF
		DO
			IF EXIST(FLP2$,NM$,0) THEN
				LOCATE %SelLine,2:PRINT ClearLine$
				LOCATE %SelLine,3
				PRINT "Overwrite ";RTRIM$(FLP2$,"\")+"\"+NM$;" ? (Y/N) Yes"
				LOCATE %SelLine,23+LEN(FLP2$+NM$),1
				SOUND 800,5
				DO
					YN$=UCASE$(INPUT$(1))
				LOOP UNTIL YN$="N" OR YN$="Y" OR YN$=CHR$(%Ent) OR YN$=CHR$(%Esc)
				IF YN$=CHR$(%Esc) THEN LOCATE ,,0:RETURN
				IF YN$="N" THEN
					LOCATE %SelLine,2:PRINT ClearLine$
					EXIT LOOP
				END IF
			ELSE
				IF FileLen&(XHL)>FREESPACE&&(FLP2$) THEN
					BF=15:GOSUB ERRA
					EXIT LOOP
				END IF
			END IF
			LOCATE %SelLine,2,0:PRINT ClearLine$
			LOCATE %SelLine,3:PRINT "Copying '";NM$;"'...";
			SHELL "COPY "+NM$+" "+FLP2$+" > NUL"
			LOCATE %SelLine,2:PRINT ClearLine$
			IF INSTR(NM$,"*")=0 THEN
				IF NOT EXIST(FLP2$,NM$,0) THEN
					BF=17:GOSUB ERRA
					EXIT LOOP
				END IF
			END IF
		LOOP WHILE BF
	LOOP
RETURN
:
DELLA:
	Heading$="DELETE FILES ON DRIVE "
	HeadX=25
	IF Jump THEN
		CALL ReScreen(HeadX,Heading$,WhichFiles$)
	ELSE
		CALL NewScreen(FLP2$,WhichFiles$,Heading$+FLP2$)
	END IF
	ReDel:
	DO
		DO
			BF=0
			DO
				CD=%FALSE
				GOSUB SELECTA
				IF NM$="" OR Jump THEN RETURN
				IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					CD=%TRUE:FLP2$=NEWDIR$(NM$)
					CALL ReScreen(HeadX,Heading$,WhichFiles$)
				END IF
			LOOP WHILE CD
			BF=CHECKFILE(NM$,BF)
			IF INSTR(NM$,"*") THEN BF=%BadName
			IF BF THEN GOSUB ERRA
		LOOP WHILE BF
		DelNM$=STRING$(8,255)
		OPEN NM$ FOR BINARY AS #1
		LenMsg&=LOF(1)
		LOCATE %SelLine,2:PRINT ClearLine$
		LOCATE %SelLine,3,1:PRINT "Are you sure? (Y/N) Yes"
		LOCATE %SelLine,23
		SOUND 800,5
		DO
			DO
				YN$=UCASE$(INPUT$(1))
			LOOP UNTIL YN$="N" OR YN$="Y" OR YN$=CHR$(%Ent) OR YN$=CHR$(%Esc)
			LOCATE %SelLine,2,0:PRINT ClearLine$
			IF YN$=CHR$(%Esc) THEN CLOSE:RETURN
			IF YN$="N" THEN CLOSE:EXIT LOOP
			LOCATE %SelLine,3,1:PRINT "Deleting '";NM$;"'...";
			LongFile=%FALSE:Repeat=0
			IF LenMsg&>LenBite THEN
				LongFile=%TRUE
				Repeat=LenMsg&\LenBite
				Extra=LenMsg& MOD LenBite
			END IF
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						PUT$ #1,STRING$(Extra,0)
					ELSE
						PUT$ #1,STRING$(LenBite,0)
					END IF
				ELSE
					PUT$ #1,STRING$(LenMsg&,0)
				END IF
			NEXT LN
			CLOSE
			NAME NM$ AS DelNM$
			KILL DelNM$
			LOCATE %SelLine,2,0:PRINT ClearLine$
			CALL RePrint(WhichFiles$)
		LOOP UNTIL YN$="Y" OR YN$=CHR$(%Ent)
	LOOP
RETURN
:
ENCA:
	Heading$="ENCIPHER FILES ("+LEFT$(LTRIM$(Ver$),1)+") ON DRIVE "
	HeadX=31
	IF Jump THEN
		CALL ReScreen(HeadX,Heading$,WhichFiles$)
	ELSE
		CALL NewScreen(RAM$,WhichFiles$,Heading$+RAM$)
	END IF
	ReEnc:
	DO
		DO
			BF=0
			DO
				CD=%FALSE:KeyOn=%TRUE
				GOSUB SELECTA
				IF NM$="" OR Jump THEN RETURN
				IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					CD=%TRUE:RAM$=NEWDIR$(NM$)
					CALL ReScreen(HeadX,Heading$,WhichFiles$)
				END IF
			LOOP WHILE CD
			BF=CheckFile(NM$,BF)
			IF BF THEN GOSUB ERRA
		LOOP WHILE BF
		CALL ClearWindow(%TopLeft3,%BottomRight3)
		LOCATE %ActLine,3:PRINT SPACE$(40);
		IF INSTR(NM$,".ARC")=0 THEN
			IF FD1 THEN
				CALL WarnDisk("PROGRAM")
				DO
					BF=0
					IF NOT EXIST(DR$(12),"PKPAK.EXE",0) THEN BF=21:GOSUB ERRA
				LOOP WHILE BF
			END IF
			IF INSTR(NM$,"*") THEN
				LOCATE %SelLine,39:PRINT "Name for Output file: [        ]"
				DO
					BF=0
					X=62:LOLIM=33:UPLIM=126:CRS=8
					LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(7)
					CALL KEYA(%SelLine,X,LOLIM,UPLIM,CRS,STNG$)
					OP$=UCASE$(STNG$)
					IF OP$="" THEN RETURN
					IF INSTR(OP$,".") THEN BF=%BadName
					BF=CHECKNAME(OP$,BF)
					IF BF THEN GOSUB ERRA
				LOOP WHILE BF
			ELSE
				IF INSTR(NM$,".") THEN
					OP$=LEFT$(NM$,(INSTR(NM$,".")-1))
				ELSE
					OP$=NM$
				END IF
			END IF
			LOCATE %SelLine,3:PRINT "Compressing '";NM$;"'...";
			SHELL DR$(12)+"\PKPAK -a "+OP$+" "+NM$+" > NUL"
			NM$=OP$+".ARC"
		END IF
		KeyOn=%FALSE
		LOCATE %SelLine,3,0:PRINT ClearLine$
		IF VER THEN
			GOSUB GetEncKey
		ELSE
			LOCATE %SelLine,3:PRINT "Ready to encipher '";NM$;"'"
			CALL WarnDisk("DATA")
			GOSUB GetEncDisk
		END IF
		IF INSTR(NM$,".") THEN
			OPEN LEFT$(NM$,(INSTR(NM$,".")-1))+DR$(14) FOR BINARY AS #1
		ELSE
			OPEN NM$+DR$(14) FOR BINARY AS #1
		END IF
		OPEN NM$ FOR BINARY AS #2
		LenMsg&=LOF(2):Repeat=0:LongFile=%FALSE:MSG$=""
		IF VER THEN
			LOCATE %ActLine,3,0:PRINT "File length:";LenMsg&;"characters             ";
			CT=2
		ELSE
			CT=0
		END IF
		IF LenMsg&>LenBite THEN
			LongFile=%TRUE
			Repeat=LenMsg&\LenBite
			Extra=LenMsg& MOD LenBite
		END IF
		SELECT CASE VER
			CASE 0:SM$=STRING$(5,32)+CHR$(ASC(DNO$)+33)
			CASE 1:SM$=STRING$(5,33)+CHR$(126-PAGE)+CHR$(128-(LNE*2))
			CASE 2:SM$=STRING$(5,0)+CHR$(PAGE)+CHR$(LL)
		END SELECT
		PUT$ #1,SM$
		FOR LN=1 TO Repeat+1
			IF LongFile THEN
				IF LN>Repeat THEN
					GET$ #2,Extra,MSG$
				ELSE
					GET$ #2,LenBite,MSG$
				END IF
			ELSE
				GET$ #2,LenMsg&,MSG$
			END IF
			SELECT CASE VER
				CASE 0
					IF LenMsg&>32750 THEN
						BF=11:GOSUB ERRA:EXIT LOOP
					ELSE
						CALL EncDisk(MSG$,SALF$,SNUM$)
					END IF
				CASE 1
					CALL AscEnc(MSG$,ALPH0(),SK$())
				CASE 2
					CALL KwEnc(LN,MSG$,ALPH0())
			END SELECT
			MSG$=""
		NEXT LN
		IF VER=0 THEN
			CLOSE:SALF$="":SNUM$=""
			GOSUB EncFlush
		END IF
		IF VER=1 THEN
			PUT$ #1,STRING$(5,34)+CHR$(%Ent)+CHR$(10)
		END IF
		CLOSE
		ERASE ALF
		KILL NM$
		CALL Reprint(WhichFiles$)
		CALL TidyUp
	LOOP
RETURN
:
GetEncKey:
	LOCATE %SelLine,3:PRINT "Page No: [";
	IF VER=1 THEN PRINT "  ]" ELSE PRINT "   ]"
	LOCATE %SelLine,21:PRINT "Line No: [  ]    26 Char Key: [                          ]"
	CRS=VER+1
	DO
		BF=0
		LOCATE %SelLine,13:PRINT CHR$(219);SPACE$(CRS-1)
		X=13:LOLIM=48:UPLIM=57:CALL KEYA(%SelLine,X,LOLIM,UPLIM,CRS,STNG$)
		IF STNG$="" THEN MENU
		PAGE=VAL(STNG$)
		SELECT CASE VER
			CASE=1
				IF PAGE <1 OR PAGE=62 OR PAGE>92 THEN BF=4:GOSUB ERRA
			CASE=2
				IF PAGE <1 OR PAGE>255 THEN BF=5:GOSUB ERRA
		END SELECT
	LOOP WHILE BF
	DO
		BF=0
		LOCATE %SelLine,31:PRINT CHR$(219);" "
		X=31:LOLIM=48:UPLIM=57:CRS=2:CALL KEYA(%SelLine,X,LOLIM,UPLIM,CRS,STNG$)
		IF STNG$="" THEN MENU
		LNE=VAL(STNG$)
		SELECT CASE VER
			CASE=1
				IF LNE<1 OR LNE=32 OR LNE>47 THEN BF=6:GOSUB ERRA
			CASE=2
				IF LNE<1 THEN BF=7:GOSUB ERRA
		END SELECT
	LOOP WHILE BF
	DO
		LOCATE %SelLine,52:PRINT CHR$(219);SPACE$(25)
		X=52:LOLIM=32:UPLIM=126:CRS=26:CALL KEYA(%SelLine,X,LOLIM,UPLIM,CRS,STNG$)
		IF STNG$="" THEN MENU
		IF LEN(STNG$)<>CRS THEN SOUND 88,8
	LOOP UNTIL LEN(STNG$)=CRS
	LOCATE %SelLine,2:PRINT ClearLine$
	LOCATE %SelLine,3:PRINT "Enciphering '";NM$;"'...";
	COLOR %FLASH:PRINT CHR$(219):COLOR ForeG
	IF VER=2 THEN
		RANDOMIZE PAGE
		LL=(LNE XOR (RND*256))
	END IF
	RANDOMIZE PAGE*LNE:CKSUM=(RND*32767)
	FOR N=26 TO 1 STEP -1
		CKSUM=(CKSUM MOD 29492)+(ASC(MID$(STNG$,N,1))*N)
		CKS(N)=CKSUM
	NEXT N
	FOR N=0 TO 255
		IF N MOD 10=0 THEN RANDOMIZE CKS(N/10)
		DO
			CH=INT(RND*256)
		LOOP WHILE ALF(CH)=-1
		ALF(CH)=-1
		ALPH0(N)=CH
	NEXT N
RETURN
:
GetEncDisk:
	LOCATE %SelLine,3,0:PRINT ClearLine$
	LOCATE %SelLine,3:PRINT "Enciphering '";NM$;"'..."
	DO
		BF=0
		IF NOT EXIST(FLP2$,"SETNO",0) THEN BF=20:GOSUB ERRA
	LOOP WHILE BF
	OPEN "I",1,FLP2$+"SETNO":INPUT#1,GetNum:CLOSE
	IF GetNum>99 THEN BF=15:GOSUB ERRA:GOTO MENU
	LOCATE %ActLine,3:PRINT "Using data set no:";GetNum+1
	OPEN FLP2$+"SALF" FOR BINARY AS #1
	SEEK #1,GetNum*256:GET$ #1,256,SALF$
	OPEN FLP2$+"SNUM" FOR BINARY AS #2
	G&=GetNum
	SEEK #2,G&*3000:GET$ #2,3000,SNUM$
	IF INSTR(SNUM$,NULL$) THEN
		INCR GetNum:OPEN "O",3,FLP2$+"SETNO":PRINT#3,GetNum:CLOSE
		BF=12:GOSUB ERRA
		CALL TidyUp
		GOTO ReEnc
	END IF
	OPEN FLP2$+"SDNO" FOR BINARY AS #3
	SEEK #3,GetNum:GET$ #3,1,DNO$:CLOSE
RETURN
:
SUB AscEnc(MS$,AL0(1),SKA$(1))
	SHARED CT
	SP$=" "
	LenMsg=LEN(MS$)
	$EVENT OFF
	FOR AE=1 TO LenMsg
		MS=ASC(MID$(MS$,AE,1))
		RL=INT(RND*256)
		CD=MS XOR RL:INCR CT
		SLET$=SKA$(AL0(CD))
		IF CT MOD 5=0 THEN SLET$=SLET$+SP$
		PUT$ #1,SLET$
		IF RL MOD 4=0 THEN RANDOMIZE AL0(RND*256)+(CT-248)
		CT=CT MOD 32767
	NEXT AE
	$EVENT ON
END SUB
:
SUB KwEnc(LNG,MS$,AL0(1))
	LenMsg=LEN(MS$)
	$EVENT OFF
	FOR KE=1 TO LenMsg
		RL=INT(RND*256)
		CD=ASC(MID$(MS$,KE,1)) XOR RL
		PUT$ #1,CHR$(AL0(CD))
		IF RL MOD 4=0 THEN RANDOMIZE (KE-AL0(RND*256))+LNG
	NEXT KE
	$EVENT ON
END SUB
:
SUB EncDisk(MS$,SLF$,SNM$)
	SHARED CT
	LOCAL CP
	LOCATE %ActLine,3:PRINT "Character  number: ";SPACE$(20)
	LenMsg=LEN(MS$)
	$EVENT OFF
	FOR DE=1 TO LenMsg
		CH=ASC(MID$(MS$,DE,1))
		INCR CT:INCR CP
		RL=ASC(MID$(SNM$,CP,1))
		CD=CH XOR RL
		SLET$=MID$(SLF$,CD+1,1)
		PUT$ #1,SLET$
		CP=CP MOD 3000
		LOCATE %ActLine,22:PRINT CT
	NEXT DE
	$EVENT ON
END SUB
:
EncFlush:
	LOCATE %SelLine,3
	PRINT "Enciphering finished. Wiping data set";STR$(GetNum+1);"..."
	LOCATE %ActLine,3:PRINT "Total file length: ";CT;" characters";
	DO
		BF=0
		IF NOT EXIST(FLP2$,"SETNO",0) THEN BF=20:GOSUB ERRA
	LOOP WHILE BF
	OPEN FLP2$+"SALF" FOR BINARY AS #1
	DSA$=STRING$(256,0):SEEK #1,GetNum*256:PUT$ #1,DSA$
	OPEN FLP2$+"SNUM" FOR BINARY AS #2
	G&=GetNum
	DSN$=STRING$(3000,0):SEEK #2,G&*3000:PUT$ #2,DSN$
	INCR GetNum:OPEN "O",3,FLP2$+"SETNO":PRINT#3,GetNum
RETURN
:
DECA:
	Heading$="DECIPHER FILES ON DRIVE "
	HeadX=27
	IF Jump THEN
		CALL ReScreen(HeadX,Heading$,WhichFiles$)
	ELSE
		CALL NewScreen(RAM$,WhichFiles$,Heading$+RAM$)
	END IF
	ReDec:
	DO
		DO
			BF=0
			DO
				CD=%FALSE:KeyOn=%TRUE
				GOSUB SELECTA
				IF NM$="" OR Jump THEN MENU
				IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					CD=%TRUE:RAM$=NEWDIR$(NM$)
					CALL ReScreen(HeadX,Heading$,WhichFiles$)
				END IF
			LOOP WHILE CD
			BF=CheckFile(NM$,BF)
			IF BF THEN GOSUB ERRA
		LOOP WHILE BF
		DO
			CALL ClearWindow(%TopLeft3,%BottomRight3)
			OPEN NM$ FOR BINARY AS #1
			GET$ #1,10,VR$:CLOSE
			IF INSTR(VR$,STRING$(3,32)) THEN
				VER=0:EXIT IF
			ELSEIF INSTR(VR$,STRING$(3,33)) THEN
				VER=1:EXIT IF
			ELSEIF INSTR(VR$,STRING$(3,0)) THEN
				VER=2
			ELSE
				BF=8:GOSUB ERRA:EXIT LOOP
			END IF
			KeyOn=%FALSE
			IF VER THEN
				GOSUB GetDecKey
			ELSE
				CALL WarnDisk("DATA")
				GOSUB GetDecDisk
			END IF
			IF INSTR(NM$,".")THEN
				DNM$=LEFT$(NM$,(INSTR(NM$,".")-1))+".DEC"
			ELSE
				DNM$=NM$+".DEC"
			END IF
			OPEN DNM$ FOR BINARY AS #1
			OPEN NM$ FOR BINARY AS #2
			LenCgm&=LOF(2)-GoIn:Repeat=0:LongFile=%FALSE:CGM$=""
			LOCATE %ActLine,3,0:PRINT "File length:";LenCgm&;"characters             ";
			IF LenCgm&>LenBite THEN
				LongFile=%TRUE
				Repeat=LenCgm&\LenBite
				Extra=LenCgm& MOD LenBite
			END IF
			CT=2:SEEK #2,GoIn
			FOR LN=1 TO Repeat+1
				IF LongFile THEN
					IF LN>Repeat THEN
						GET$ #2,Extra,CGM$
					ELSE
						GET$ #2,LenBite,CGM$
					END IF
				ELSE
					GET$ #2,LenCgm&,CGM$
				END IF
				SELECT CASE VER
					CASE=0
						CALL DECDISK(CGM$,RALF$,RNUM$)
					CASE=1
						CALL ASCDEC(CGM$,RALPH0(),RALPH1())
					CASE=2
						CALL KWDEC(LN,CGM$,RALPH0(),RALPH1())
				END SELECT
				CGM$=""
			NEXT LN
			CLOSE
			IF VER=0 THEN
				RALF$="":RNUM$=""
				GOSUB DECFLUSH
			END IF
			OPEN DNM$ FOR BINARY AS #1
			SEEK #1,0:GET$ #1,1,CCMP$:CLOSE
			IF ASC(CCMP$)=26 THEN
				ANM$=LEFT$(DNM$,(INSTR(DNM$,".")-1))+".ARC"
				NAME DNM$ AS ANM$
				IF FD1 THEN
					CALL WarnDisk("PROGRAM")
					DO
						BF=0
						IF NOT EXIST(DR$(12),"PKUNPAK.EXE",0) THEN BF=21:GOSUB ERRA
					LOOP WHILE BF
				END IF
				LOCATE %SelLine,2:PRINT ClearLine$
				LOCATE %SelLine,3:PRINT "Decompressing ";ANM$;"...";
				SHELL DR$(12)+"\PKUNPAK -r "+ANM$+" > NUL"
				KILL ANM$
			END IF
			ERASE RALF
			CALL Reprint(WhichFiles$)
			LOOP WHILE BF
		CALL TidyUp
	LOOP
RETURN
:
GetDecKey:
	OPEN NM$ FOR BINARY AS #1
	GoIn=0:SC=0
	IF VER=1 THEN
		DO
			GET$ #1,1,P$:PAGE=ASC(P$):INCR GoIn
			IF PAGE=33 THEN INCR SC
		LOOP UNTIL SC>3 AND PAGE<>33
	ELSE
		DO
			GET$ #1,1,P$:PAGE=ASC(P$):INCR GoIn
			IF PAGE=0 THEN INCR SC
		LOOP UNTIL SC>3 AND PAGE<>0
	END IF
	GET$ #1,1,LL$:INCR GoIn:CLOSE
	IF VER=1 THEN
		PAGE=126-PAGE:LNE=(128-ASC(LL$))/2
		IF PAGE>92 OR LNE>47 THEN BF=9:GOSUB ERRA:GOTO MENU
	ELSE
		RANDOMIZE PAGE
		LNE=(ASC(LL$) XOR (RND*256))
	END IF
	LOCATE %ActLine,3:PRINT SPACE$(40)
	LOCATE %SelLine,3 :PRINT "Page No:";PAGE;TAB(21);"Line No:";LNE
	LOCATE %SelLine,38:PRINT "26 Char Key: [                          ]"
	DO
		LOCATE %SelLine,52:PRINT CHR$(219);SPACE$(25)
		X=52:LOLIM=32:UPLIM=126:CRS=26
		CALL KEYA(%SelLine,X,LOLIM,UPLIM,CRS,STNG$)
		IF STNG$="" THEN MENU
		IF LEN(STNG$)<>CRS THEN SOUND 88,8
	LOOP UNTIL LEN(STNG$)=CRS
	LOCATE %SelLine,2:PRINT ClearLine$
	LOCATE %ActLine,2:PRINT ClearLine$
	LOCATE %SelLine,3:PRINT "Deciphering '";NM$;"'...";
	COLOR %FLASH:PRINT CHR$(219):COLOR ForeG
	RANDOMIZE PAGE*LNE:CKSUM=RND*32767
	FOR N=26 TO 1 STEP -1
		CKSUM=(CKSUM MOD 29492)+(ASC(MID$(STNG$,N,1))*N)
		CKR(N)=CKSUM
	NEXT N
	FOR N=0 TO 255
		IF N MOD 10=0 THEN RANDOMIZE CKR(N/10)
		DO
			CH=INT(RND*256)
		LOOP WHILE RALF(CH)=-1
		RALF(CH)=-1
		RALPH0(N)=CH
		RALPH1(CH)=N
	NEXT N
RETURN
:
GetDecDisk:
	LOCATE %ActLine,2:PRINT ClearLine$
	LOCATE %SelLine,3:PRINT "Deciphering '";NM$;"'...";
	COLOR %FLASH:PRINT CHR$(219):COLOR ForeG
	OPEN NM$ FOR BINARY AS #1
	GoIn=0:SC=0
	DO
		GET$ #1,1,L$:L=ASC(L$):INCR GoIn
		IF L=32 THEN INCR SC
	LOOP UNTIL SC>3 AND L<>32
	SEEK #1,GoIn-1:GET$ #1,1,SET$:CLOSE
	SET$=CHR$(ASC(SET$)-33):RDNO$="":RALF$="":RNUM$=""
	DO
		BF=0
		IF NOT EXIST(FLP2$,"RDNO",0) THEN BF=20:GOSUB ERRA
	LOOP WHILE BF
	OPEN FLP2$+"RDNO" FOR BINARY AS #1
	GET$ #1,100,RDNO$:CLOSE
	DNO=INSTR(RDNO$,SET$):D&=DNO-1
	IF DNO=100 THEN BF=16:GOSUB ERRA
	LOCATE %ActLine,3:PRINT "Using data set no:";DNO
	OPEN FLP2$+"RALF" FOR BINARY AS #1
	SEEK #1,(DNO-1)*256:GET$ #1,256,RALF$
	OPEN FLP2$+"RNUM" FOR BINARY AS #2
	SEEK #2,D&*3000:GET$ #2,3000,RNUM$
	IF INSTR(RNUM$,NULL$) THEN
		BF=14:GOSUB ERRA
		CALL TidyUp
		GOTO ReDec
	END IF
	CLOSE
RETURN
:
SUB AscDec (CG$,RA0(1),RA1(1))
	SHARED CT
	LOCAL EC,TB
	LENCG=LEN(CG$)
	$EVENT OFF
	FOR AD=1 TO LENCG
		IP=ASC(MID$(CG$,AD,1))
		SELECT CASE IP
			CASE 35 TO 126
				EXIT SELECT
			CASE=32
				TB=1:EXIT SELECT
			CASE=33
				TB=2:GOTO 56
			CASE=34
				IF TB=3 THEN
					IF EC=3 THEN EXIT SUB
					INCR EC
				END IF
				TB=3:GOTO 56
			CASE <32, >126
				GOTO 56
		END SELECT
		55:SELECT CASE TB
			CASE 0
				IF IP<64 THEN DECR IP,35
				IF IP>64 THEN DECR IP,36
			CASE 1
				IF CT MOD 5=0 THEN TB=0:GOTO 56
				IP=85
			CASE 2
				IF IP>64 THEN IP=91+IP-36
				IF IP<64 THEN IP=91+IP-35
				TB=0
			CASE 3
				IF IP>64 THEN IP=174+IP-36
				IF IP<64 THEN IP=174+IP-35
				TB=0
		END SELECT
		IP=RA1(IP)
		RL=INT(RND*256)
		PL=IP XOR RL:INCR CT
		PUT$ #1,CHR$(PL)
		IF RL MOD 4=0 THEN RANDOMIZE RA0(RND*256)+(CT-248)
		CT=CT MOD 32767
		IF TB THEN 55
	56:NEXT AD
	$EVENT ON
END SUB
:
SUB KWDEC(LNG,CG$,RA0(1),RA1(1))
	LENCG=LEN(CG$)
	$EVENT OFF
	FOR KD=1 TO LENCG
		RL=INT(RND*256)
		PL=RA1(ASC(MID$(CG$,KD,1))) XOR RL
		PUT$ #1,CHR$(PL)
		IF RL MOD 4=0 THEN RANDOMIZE (KD-RA0(RND*256))+LNG
	NEXT KD
	$EVENT ON
END SUB
:
SUB DECDISK(CG$,RLF$,RNM$)
	LOCAL CP
	LENCG=LEN(CG$)
	$EVENT OFF
	FOR DD=1 TO LENCG
		INCR CP
		CG=ASC(MID$(RLF$,ASC(MID$(CG$,DD,1))+1,1))
		RL=ASC(MID$(RNM$,CP,1))
		PL=CG XOR RL
		RLET$=CHR$(PL)
		PUT$ #1,RLET$
		CP=CP MOD 3000
	NEXT DD
	$EVENT ON
END SUB
:
DECFLUSH:
	LOCATE %SelLine,3
	PRINT "Deciphering finished. Wiping data set";STR$(DNO);"...";
	DO
		BF=0
		IF NOT EXIST(FLP2$,"SETNO",0) THEN BF=20:GOSUB ERRA
	LOOP WHILE BF
	OPEN FLP2$+"RALF" FOR BINARY AS #1
	DRA$=STRING$(256,0):SEEK #1,(DNO-1)*256:PUT$ #1,DRA$
	OPEN FLP2$+"RNUM" FOR BINARY AS #2
	D&=DNO-1
	DRN$=STRING$(3000,0):SEEK #2,D&*3000:PUT$ #2,DRN$
	CLOSE
RETURN
:
NEWDOC:
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	LOCATE 2,3:COLOR %PAPER,ForeG:PRINT "TYPE A NEW DOCUMENT":COLOR ForeG,%PAPER
	LOCATE 4,3:PRINT "Give a name to the document you are about to type"
	LOCATE 6,3:PRINT "or press ESCape for Main Menu"
	LOCATE 8,3:PRINT "Document name: [            ]"
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	DO
		BF=0:Jump=%FALSE:KeyOn=%FALSE
		LOCATE 8,19:PRINT CHR$(219);SPACE$(11)
		Y=8:X=19:LOLIM=33:UPLIM=126:CRS=12
		CALL KEYA(Y,X,LOLIM,UPLIM,CRS,STNG$)
		NM$=STNG$
		IF NM$="" THEN RETURN
		IF CHECKNAME(NM$,BF%)=%BadName THEN GOSUB ERRA
	LOOP WHILE BF
	CALL EDIT(DR$(7),NM$)
RETURN
:
OLDOC:
	ReOld:
	DO
		Heading$="EDIT/READ FILES ON DRIVE "
		HeadX=28
		IF Jump THEN
			CALL ReScreen(HeadX,Heading$,WhichFiles$)
		ELSE
			CALL NewScreen(RAM$,WhichFiles$,Heading$+RAM$)
		END IF
		DO
			BF=0
			DO
				CD=%FALSE
				GOSUB SELECTA
				IF NM$="" OR Jump THEN RETURN
				IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					CD=%TRUE:RAM$=NEWDIR$(NM$)
					CALL ReScreen(HeadX,Heading$,WhichFiles$)
				END IF
			LOOP WHILE CD
			BF=CHECKFILE(NM$,BF%)
			IF INSTR(NM$,"*") THEN BF=%BadName
			IF BF THEN GOSUB ERRA
		LOOP WHILE BF
		RAM$=RTRIM$(CURDIR$,"\")
		CALL EDIT(RAM$,NM$)
	LOOP
RETURN
:
SUB EDIT(RAM$,NM$)
	SHARED FD1,DR$()
	CALL ChPath(DR$(8))
	IF FD1 THEN
		DO
			BF=0
			IF NOT EXIST(DR$(8),DR$(9),0) THEN BF=21:GOSUB ERRA
		LOOP WHILE BF
	END IF
	LOCATE %SelLine,3,0
	SHELL DR$(9)+" "+RAM$+"\"+NM$
	GOSUB BORDER
	CALL ChPath(RAM$)
END SUB
:
RENNA:
	Heading$="RENAME FILES ON DRIVE "
	HeadX=25
	CALL NewScreen(RAM$,WhichFiles$,Heading$+RAM$)
	LOCATE %SelLine,35:PRINT "New name: [            ]"
	ReRen:
	DO
		DO
			BF=0
			DO
				CD=%FALSE
				GOSUB SELECTA
				IF Jump THEN
					LOCATE %SelLine,2:PRINT ClearLine$
					RETURN MENU
				ELSEIF NM$="" THEN
					RETURN
				ELSEIF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					CD=%TRUE:RAM$=NEWDIR$(NM$)
					CALL ReScreen(HeadX,Heading$,WhichFiles$)
				END IF
			LOOP WHILE CD
			IF CHECKFILE(NM$,BF%)=%NoFile THEN GOSUB ERRA
		LOOP WHILE BF
		DO
			BF=0
			LOCATE %SelLine,46:PRINT CHR$(219);SPACE$(11)
			X=46:LOLIM=33:UPLIM=126:CRS=12
			CALL KEYA(%SelLine,X,LOLIM,UPLIM,CRS,STNG$)
			NNM$=UCASE$(STNG$)
			IF NNM$="" THEN RETURN
			IF INSTR(NM$,"*") OR INSTR(NNM$,"*") THEN EXIT LOOP
			IF CHECKNAME(NNM$,%BF)=%BadName THEN GOSUB ERRA
			IF NNM$=NM$ THEN BF=19:GOSUB ERRA
		LOOP WHILE BF
		NAME NM$ AS NNM$
		CALL RePrint(WhichFiles$)
		LOCATE %SelLine,46:PRINT SPACE$(12)
	LOOP
RETURN
:
TERMINAL:
	CALL ChPath(DR$(10))
	IF FD1 THEN
		CALL WarnDisk("PROGRAM")
		DO
			BF=0
			IF NOT EXIST(DR$(10),DR$(11),0) THEN BF=21:GOSUB ERRA
		LOOP WHILE BF
	END IF
	Jump=0
	LOCATE 23,2
	SHELL "REN "+DR$(7)+"* *.Q$} > NUL":RN=%TRUE
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	LOCATE 6,3,0:PRINT "Loading communications program...";
	SHELL DR$(11)
	LOCATE 23,1,0
	ReCom:
	SHELL "REN "+DR$(7)+"*.Q$} * > NUL":RN=%FALSE
	IF SER THEN GOSUB SETBAUD
	GOSUB BORDER
RETURN
:
QUITTA:
	Jump=%FALSE:KeyOn=%FALSE
	CALL WarnDisk("PROGRAM")
	DO
		BF=0
		IF NOT EXIST(DR$(5),"TECOD5.EXE",0) THEN BF=21:GOSUB ERRA
	LOOP WHILE BF
	CALL ChPath(DR$(5))
	CLS:END
RETURN
:
COMPA:
	Heading$="COMPRESS FILES ON DRIVE "
	HeadX=27
	CALL NewScreen(RAM$,WhichFiles$,Heading$+RAM$)
	LOCATE %SelLine,35:PRINT "Name for Compressed file: [            ]"
	ReComp:
	DO
		DO
			BF=0
			DO
				CD=%FALSE
				GOSUB SELECTA
				IF Jump THEN
					LOCATE %SelLine,2:PRINT ClearLine$
					RETURN MENU
				ELSEIF NM$="" THEN
					RETURN
				ELSEIF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					CD=%TRUE:RAM$=NEWDIR$(NM$)
					CALL ReScreen(HeadX,Heading$,WhichFiles$)
				END IF
			LOOP WHILE CD
			IF CHECKFILE(NM$,BF%)=%NoFile THEN GOSUB ERRA
		LOOP WHILE BF
		DO
			BF=0
			LOCATE %SelLine,62:PRINT CHR$(219);SPACE$(11)
			X=62:LOLIM=33:UPLIM=126:CRS=12
			CALL KEYA(%SelLine,X,LOLIM,UPLIM,CRS,STNG$)
			CNM$=UCASE$(STNG$)
			IF CNM$="" THEN RETURN
			IF CHECKNAME(CNM$,NM)=%BadName THEN GOSUB ERRA
		LOOP WHILE BF
		IF FD1 THEN
			CALL WarnDisk("PROGRAM")
			DO
				BF=0
				IF NOT EXIST(DR$(12),"PKPAK.EXE",0) THEN BF=21:GOSUB ERRA
			LOOP WHILE BF
		END IF
		LOCATE %SelLine,3:PRINT "Compressing '";NM$;"'...";
		SHELL DR$(12)+"\PKPAK a "+CNM$+" "+NM$+" > NUL"
		LOCATE %SelLine,3:PRINT SPACE$(32)
		LOCATE %SelLine,62:PRINT SPACE$(12)
		CALL RePrint(WhichFiles$)
	LOOP
RETURN
:
DECOMPA:
	WhichFiles$="*.ARC"
	Heading$="DECOMPRESS FILES ON DRIVE "
	HeadX=29
	CALL NewScreen(RAM$,WhichFiles$,Heading$+RAM$)
	ReDecomp:
	DO
		DO
			BF=0
			DO
				CD=%FALSE
				GOSUB SELECTA
				IF Jump THEN
					WhichFiles$="*.*"
					LOCATE %SelLine,2:PRINT ClearLine$
					CALL RePrint(WhichFiles$)
					RETURN MENU
				END IF
				IF NM$="" THEN RETURN
				IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					CD=%TRUE:RAM$=NEWDIR$(NM$)
					CALL ReScreen(HeadX,Heading$,WhichFiles$)
				END IF
			LOOP WHILE CD
			IF CHECKFILE(NM$,BF%)=%NoFile THEN GOSUB ERRA
		LOOP WHILE BF
		IF FD1 THEN
			CALL WarnDisk("PROGRAM")
			DO
				BF=0
				IF NOT EXIST(DR$(12),"PKUNPAK.EXE",0) THEN BF=21:GOSUB ERRA
			LOOP WHILE BF
		END IF
		LOCATE %SelLine,3:PRINT "Decompressing '";NM$;"'...";
		SHELL DR$(12)+"\PKUNPAK -r "+NM$+" > NUL"
		LOCATE %SelLine,3,32:PRINT SPACE$(32);
		LOCATE %HeadLine,58:PRINT FREESPACE&&(CURDIR$) "bytes free"
	LOOP
RETURN
:
WIPA:
	IF FD1 THEN SOUND 88,8:RETURN
	Heading$="WIPE FILES ON DRIVE "
	HeadX=23
	CALL NewScreen(FLP2$,WhichFiles$,Heading$+FLP2$)
	ReWipe:
	DO
		DO
			BF=0
			DO
				CD=%FALSE
				GOSUB SELECTA
				IF Jump THEN MENU
				IF NM$="" THEN RETURN
				IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					CD=%TRUE:FLP2$=NEWDIR$(NM$)
					CALL ReScreen(HeadX,Heading$,WhichFiles$)
				END IF
			LOOP WHILE CD
			IF CHECKFILE(NM$,BF%)=%NoFile THEN GOSUB ERRA
		LOOP WHILE BF
		SCREEN 0,0,1,1
		CLS:SHELL DR$(12)+"\WIPEFILE "+NM$+" /G"
		SCREEN 0,0,0,0
		CALL RePrint(WhichFiles$)
	LOOP
RETURN
:
WDISK:
	IF FD1 THEN SOUND 88,8:RETURN
	NoDraw=%TRUE
	Heading$="WIPE ERASED & FREE SPACE ON DRIVE "
	HeadX=37
	CALL NewScreen(FLP2$,WhichFiles$,Heading$+FLP2$)
	ReWdisk:
	DO
		DO
			CD=%FALSE
			GOSUB SELECTA
			IF Jump THEN NoDraw=%FALSE:RETURN MENU
			IF NM$="" THEN RETURN
			IF MID$(NM$,2,1)=":" THEN
				CD=%TRUE:FLP2$=LEFT$(NM$,2)
				CALL ReScreen(HeadX,Heading$,WhichFiles$)
			END IF
		LOOP WHILE CD
		SCREEN 0,0,1,1
		CLS:SHELL DR$(12)+"\WIPEDISK "+FLP2$+" /E"
		SCREEN 0,0,0,0
	LOOP
RETURN
:
HIDA:
	Heading$="HIDE FILES ON DRIVE "
	HeadX=23
	CALL NewScreen(FLP2$,WhichFiles$,Heading$+FLP2$)
	ReHide:
	DO
		DO
			BF=0
			DO
				CD=%FALSE
				GOSUB SELECTA
				IF Jump THEN MENU
				IF NM$="" THEN RETURN
				IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					CD=%TRUE:FLP2$=NEWDIR$(NM$)
					CALL ReScreen(HeadX,Heading$,WhichFiles$)
				END IF
			LOOP WHILE CD
			IF CHECKFILE(NM$,BF%)=%NoFile THEN GOSUB ERRA
		LOOP WHILE BF
		ATTRIB NM$,2
		CALL RePrint(WhichFiles$)
	LOOP
RETURN
:
UNHIDA:
	NoDraw=%TRUE
	Heading$="UNHIDE FILES ON DRIVE "
	HeadX=25
	CALL NewScreen(FLP2$,WhichFiles$,Heading$+FLP2$)
	ReUnhide:
	DO
		DO
			CD=%FALSE
			GOSUB SELECTA
			IF Jump THEN NoDraw=%FALSE:RETURN MENU
			IF NM$="" THEN RETURN
			IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
				CD=%TRUE:FLP2$=NEWDIR$(NM$)
				CALL ReScreen(HeadX,Heading$,WhichFiles$)
			END IF
		LOOP WHILE CD
		LOCATE %SelLine,3
		ATTRIB NM$,0
		CALL RePrint(WhichFiles$)
	LOOP
RETURN
:
ALTA:
	Heading$="ALTER TIME/DATE OF FILES ON DRIVE "
	HeadX=37
	CALL NewScreen(RAM$,WhichFiles$,Heading$+RAM$)
	ReAlter:
	DO
		DO
			BF=0
			DO
				CD=%FALSE
				GOSUB SELECTA
				IF Jump THEN RETURN MENU
				IF NM$="" THEN RETURN
				IF MID$(NM$,2,1)=":" OR INSTR(NM$,"\") THEN
					CD=%TRUE:RAM$=NEWDIR$(NM$)
					CALL ReScreen(HeadX,Heading$,WhichFiles$)
				END IF
			LOOP WHILE CD
			IF CHECKFILE(NM$,BF%)=%NoFile THEN GOSUB ERRA
		LOOP WHILE BF
		LOCATE %SelLine,3
		PRINT "New date (mm/dd/yy): [        ]  New time (hh:mm:ss): [        ]"
		DO
			AltErr=%FALSE
			LOCATE %SelLine,25:PRINT CHR$(219);SPACE$(7)
			X=25:LOLIM=47:UPLIM=57:CRS=8
			CALL KEYA(%SelLine,X,LOLIM,UPLIM,CRS,STNG$)
			NewDate$=STNG$
			IF NewDate$="" THEN RETURN
			IF VAL(LEFT$(NewDate$,2))>12 THEN AltErr=%TRUE
			IF VAL(MID$(NewDate$,4,2))>31 THEN AltErr=%TRUE
			IF MID$(NewDate$,3,1)<>"/" THEN AltErr=%TRUE
			IF MID$(NewDate$,6,1)<>"/" THEN AltErr=%TRUE
			IF TALLY(NewDate$,"/")>2 THEN AltErr=%TRUE
			IF AltErr THEN SOUND 88,8
		LOOP WHILE AltErr
		DO
			AltErr=%FALSE
			LOCATE %SelLine,58:PRINT CHR$(219);SPACE$(7)
			X=58:LOLIM=48:UPLIM=58:CRS=8
			CALL KEYA(%SelLine,X,LOLIM,UPLIM,CRS,STNG$)
			NewTime$=STNG$
			IF NewTime$="" THEN RETURN
			IF VAL(LEFT$(NewTime$,2))>24 THEN AltErr=%TRUE
			IF VAL(MID$(NewTime$,4,2))>59 THEN AltErr=%TRUE
			IF VAL(LEFT$(NewTime$,2))>59 THEN AltErr=%TRUE
			IF MID$(NewTime$,3,1)<>":" THEN AltErr=%TRUE
			IF MID$(NewTime$,6,1)<>":" THEN AltErr=%TRUE
			IF TALLY(NewTime$,":")>2 THEN AltErr=%TRUE
			IF AltErr THEN SOUND 88,8
		LOOP WHILE AltErr
		IF FD1 THEN
			CALL WarnDisk("PROGRAM")
			DO
				BF=0
				IF NOT EXIST(DR$(12),"FDATE.COM",0) THEN BF=21:GOSUB ERRA
			LOOP WHILE BF
		END IF
		LOCATE %SelLine,3
		SHELL DR$(12)+"\FDATE "+NM$+" "+NewDate$+" "+NewTime$
		LOCATE %SelLine,2:PRINT ClearLine$
	LOOP
RETURN
:
LISTA:
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	LOCATE 2,3:COLOR %PAPER,ForeG:PRINT "LIST FILES":COLOR ForeG,%PAPER
	LOCATE 4,3:PRINT "ForeG drive number [\subdirectory] to list files on disk"
	LOCATE 6,3:PRINT "or press ESCape for Main Menu"
	LOCATE 8,3:PRINT "Drive number: [                                                            ]"
	DO
		DO
			BF=0:KeyOn=%FALSE
			LOCATE 8,18:PRINT CHR$(219);SPACE$(59)
			Y=8:X=18:LOLIM=33:UPLIM=126:CRS=60
			CALL KEYA(Y,X,LOLIM,UPLIM,CRS,STNG$)
			NM$=UCASE$(RTRIM$(STNG$,"\"))
			IF NM$="" THEN JUMP=0:RETURN
			IF NOT VALIDRIVE(NM$) OR MID$(NM$,2,1)<>":" THEN BF=17
			IF BF THEN GOSUB ERRA
		LOOP WHILE BF
		SCREEN 0,0,1,0
		CLS
		SCREEN 0,0,1,1
		SHELL "DIR "+NM$+"\ /P"
		LOCATE 25,1:PRINT "Press any key to continue . . .";
		CNT$=INPUT$(1)
		SCREEN 0,0,0,0
	LOOP
RETURN
:
CONFIG:
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	DIM DYNAMIC ConfigList$(1:14)
	KeyOn=%FALSE
	FOR N=1 TO 14
		READ ConfigList$(N)
		LOCATE N+1,3
		PRINT CHR$(N+64);TAB(7);ConfigList$(N);TAB(36);"-   ";DR$(N);
	NEXT N
	DATA Colour (7 or 15),"Default ver (D=0, A=1, K=2)","Printer port (Par=0, Ser=1)"
	DATA "No. of drives (>1=0, 1=1)",Path for Coder program,Path for data/message disk
	DATA RAM disk,Path for editor,Name of editor program,Path for comms program
	DATA Name of comms program,Path for utilities
	DATA Path for DOS,Encrypted file extension
	LOCATE %SelLine,3:PRINT "Press letter of option to change";TAB(52);"S - Save changes"
	LOCATE %ActLine,65:PRINT "ESC: Main Menu"
	LOCATE 16,1:PRINT HorLine$
	LOCATE %HeadLine,3:COLOR %PAPER,ForeG:PRINT "CHANGE CONFIGURATION":COLOR ForeG,%PAPER
	DO
		DO
			LOCATE %SelLine,36
			COLOR %FLASH:PRINT CHR$(219):COLOR ForeG
			Choice$=UCASE$(INPUT$(1))
			IF ASC(Choice$)>64 AND ASC(Choice$)<79 THEN
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
							ForeG=VAL(DR$(1)):VER=VAL(DR$(2)):SER=VAL(DR$(3)):FD1=VAL(DR$(4))
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
								PRINT UCASE$(Char$);SPACE$(39-LEN(TempName$))
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
				DO
					BF=0
					IF NOT EXIST(DR$(5),"TECOD5.EXE",0) THEN	BF=21:GOSUB ERRA
				LOOP WHILE BF
				OPEN DR$(5)+"\TECOD5.CNF" FOR OUTPUT AS #1
				FOR N=1 TO 14
					PRINT #1,DR$(N)
				NEXT N
				CLOSE
			END IF
		LOOP UNTIL Choice$=CHR$(%Esc) OR Choice$=CHR$(%Ent)
		IF Choice$=CHR$(%Esc) OR Choice$=CHR$(%Ent) THEN
			ERASE ConfigList$:RESTORE CONFIG
			JUMP=0:EXIT LOOP
		END IF
	LOOP
RETURN
:
ERRA:
	SOUND 88,8:LOCATE ,,0
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	SELECT CASE BF
		CASE 2:ERRMSG$="Disk full"
		CASE 3:ERRMSG$="Unacceptable characters in name"
		CASE 4:ERRMSG$="Unacceptable page number. Acceptable pages: 1-61, 63-92"
		CASE 5:ERRMSG$="Unacceptable page number. Acceptable pages: 1 to 255"
		CASE 6:ERRMSG$="Unacceptable line number. Acceptable lines: 1-31, 33-47"
		CASE 7:ERRMSG$="Unacceptable line number. Acceptable lines: 1-99"
		CASE 8:ERRMSG$="Header missing. File undecipherable"
		CASE 9:ERRMSG$="Un-enciphered or corrupted file. Download file again"
		CASE 11:ERRMSG$="Message too long by "+STR$(LenMsg&-32750)+" bytes for disk version"
		CASE 12:ERRMSG$="Data already used. Re-encipher file"
		CASE 13:ERRMSG$="File cannot be copied to itself"
		CASE 14:ERRMSG$="This file has already been deciphered"
		CASE 15:ERRMSG$="Data finished. Start a new DATA disk"
		CASE 16:ERRMSG$="Last set of data. Start a new DATA disk next time"
		CASE 17:ERRMSG$="Invalid drive number"
		CASE 18:ERRMSG$="File '"+NM$+"' not found"
		CASE 19:ERRMSG$="Copy failed"
		CASE 20:ERRMSG$="Wrong disk. Place DATA disk in drive "+DR$(6)+""
		CASE 21:ERRMSG$="Wrong disk. Place PROGRAM disk in drive"
	END SELECT
	LOCATE 23,((80-LEN(ERRMSG$))\2)-3:COLOR %FLASH:PRINT "** ";:COLOR ForeG
	PRINT ERRMSG$;:COLOR %FLASH:PRINT " **"
	COLOR ForeG:LOCATE 25,15
	PRINT "Press any key to continue. ESCape to return to Menu";
	CLOSE
	CNT$=INPUT$(1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	IF CNT$=CHR$(%Esc) THEN
  	IF M=%AltF9 THEN ERASE ConfigList$:RESTORE CONFIG
		Jump=0:RETURN MENU
	END IF
	IF KeyOn THEN CALL FunKey(KeyFun$())
RETURN
:
ERMESG:
	SOUND 88,8:LOCATE ,,0
	NM$=UCASE$(NM$)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	SELECT CASE ERR
		CASE 53:ERRMSG$="File not found"
		CASE 58:ERRMSG$="File already exists. Use another name"
		CASE 61:ERRMSG$="Out of memory/disk full. Delete files to make space"
		CASE 62:ERRMSG$="You've tried to decipher an un-enciphered file"
		CASE 70:ERRMSG$="Disk write-protected. Switch tab to continue"
		CASE 71:ERRMSG$="Disk not in drive or door open"
		CASE 72:ERRMSG$="Disk corrupted"
		CASE 75:ERRMSG$="Path/File access error"
		CASE 76:ERRMSG$="Path not found"
		CASE ELSE:ERRMSG$="Error number"+STR$(ERR)+" at address"+STR$(ERADR):RR=%TRUE
	END SELECT
	LOCATE 23,((80-LEN(ERRMSG$))\2)-3:COLOR %FLASH:PRINT "** ";:COLOR ForeG
	PRINT ERRMSG$;:COLOR %FLASH:PRINT " **";
	COLOR ForeG:LOCATE 25,15
	PRINT "Press any key to continue. ESCape to return to Menu";
	CLOSE
	CNT$=INPUT$(1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	IF CNT$=CHR$(%Esc) THEN
  	IF M=%AltF9 THEN ERASE ConfigList$:RESTORE CONFIG
		Jump=0:RESUME MENU
	END IF
	IF KeyOn THEN CALL FunKey(KeyFun$())
	IF CD THEN RESUME ContCD
	IF RR THEN RESUME MENU
	SELECT CASE M
		CASE %F1:RESUME ReLoad
		CASE %F2:RESUME ReSave
		CASE %F3:RESUME ReDel
		CASE %F4:RESUME ReEnc
		CASE %F5:RESUME ReDec
		CASE %F7:RESUME ReOld
		CASE %F8:RESUME ReRen
		CASE %F9:RESUME ReCom
		CASE %F10,%AltF10:RESUME QUITTA
		CASE %AltF1:RESUME ReComp
		CASE %AltF2:RESUME ReDecomp
		CASE %AltF3:RESUME ReWipe
		CASE %AltF4:RESUME ReWdisk
		CASE %AltF5:RESUME ReHide
		CASE %AltF6:RESUME ReUnhide
		CASE %AltF7:RESUME ReAlter
	END SELECT
RESUME MENU
:
BORDER:
	CLS
	LOCATE 1,1:PRINT CHR$(201)
	BDR$=STRING$(78,205):LOCATE 1,2:PRINT BDR$
	LOCATE 1,80:PRINT CHR$(187)
	FOR V=2 TO 21:LOCATE V,1:PRINT CHR$(186):LOCATE V,80:PRINT CHR$(186):NEXT V
	LOCATE 22,1:PRINT CHR$(200)
	LOCATE 22,2:PRINT BDR$
	LOCATE 22,80:PRINT CHR$(188)
RETURN
:
SETBAUD:
	SHELL DR$(13)+"\MODE COM1:9600,N,8,1,P > NUL"
RETURN
:
CHARS:
	FOR N=0 TO 28
		SK$(N)=CHR$(35+N)
	NEXT N
	FOR N=29 TO 90
		SK$(N)=CHR$(36+N)
	NEXT N
	FOR N=91 TO 119
		SK$(N)=CHR$(33)+CHR$(N-56)
	NEXT N
	FOR N=120 TO 173
		SK$(N)=CHR$(33)+CHR$(N-55)
	NEXT N
	FOR N=174 TO 202
		SK$(N)=CHR$(34)+CHR$(N-139)
	NEXT N
	FOR N=203 TO 255
		SK$(N)=CHR$(34)+CHR$(N-138)
	NEXT N
RETURN
:
SUB KEYA(Y,X,LOLIM,UPLIM,CRS,STNG$)
	SHARED ForeG,PW
	LOCAL L
	STNG$=""
	DO
		DO
			I=ASC(INPUT$(1))
			SELECT CASE I
				CASE %Ent
					IF L=0 AND PW=%FALSE THEN
						STNG$=""
						EXIT SUB
					END IF
					IF L<CRS THEN
						LOCATE Y,X:PRINT CHR$(32)
						EXIT SUB
					ELSE
						LOCATE Y,X-1:PRINT RIGHT$(STNG$,1)
						EXIT SUB
					END IF
				CASE %Esc
					IF PW=%FALSE THEN
						STNG$=""
						EXIT SUB
					END IF
				CASE %Bsp
					IF L THEN
						STNG$=LEFT$(STNG$,LEN(STNG$)-1)
						LOCATE Y,X-1:PRINT CHR$(219);
						IF L<CRS-1 THEN PRINT " "
						IF L=CRS-1 THEN LOCATE Y,X:PRINT " "
						DECR L:DECR X:EXIT LOOP
					ELSE
						SOUND 88,8
						EXIT LOOP
					END IF
				CASE <LOLIM,>UPLIM
					SOUND 88,8:EXIT LOOP
			END SELECT
			LOCATE Y,X
			IF L<CRS-1 THEN
				IF PW THEN
					PRINT "*";CHR$(219)
				ELSE
					PRINT CHR$(I);CHR$(219)
				END IF
			ELSEIF L=CRS-1 THEN
				COLOR %PAPER,ForeG
				IF PW THEN
					PRINT "*"
				ELSE
					PRINT CHR$(I)
				END IF
				COLOR ForeG,%PAPER
			ELSE
				SOUND 88,8
				EXIT LOOP
			END IF
			STNG$=STNG$+CHR$(I):INCR L:INCR X
		LOOP
	LOOP
END SUB
:
PASSWORD:
	Pass$="":PL=7
	DIM PW$(PL)
	PW$(4)=CHR$(83):PW$(1)=CHR$(84):PW$(5)=CHR$(69):PW$(2)=CHR$(69)
	PW$(3)=CHR$(83):PW$(6)=CHR$(76):PW$(7)=CHR$(66)
	FOR N=1 TO PL:Pass$=Pass$+PW$(N):NEXT N
	ERASE PW$
	CLS:COLOR ForeG,%PAPER
	LOCATE 12,20,0:PRINT "ENTER PASSWORD: [               ]"
	DO
		LOCATE 12,37:PRINT CHR$(219);SPACE$(14)
		Y=12:X=37:LOLIM=32:UPLIM=126:CRS=15
		CALL KEYA(Y,X,LOLIM,UPLIM,CRS,STNG$)
		PSW$=UCASE$(STNG$)
		IF PSW$="" THEN CLS:END
		IF PSW$=Pass$ THEN EXIT LOOP
		SOUND 88,8
	LOOP
	PW=%FALSE
	CLS
RETURN
:
SUB WarnDisk(DISK$)
	SHARED ForeG
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	SOUND 800,5:Jump=0
	WarnMsg$="Ensure "+DISK$+" disk is in drive"
	LOCATE 23,((80-LEN(WarnMsg$))/2)-3:COLOR %FLASH:PRINT "** ";:COLOR ForeG
	PRINT WarnMsg$;:COLOR %FLASH:PRINT " **":COLOR ForeG
	COLOR ForeG:LOCATE 25,15
	PRINT "Press any key to continue. ESCape to return to Menu";
	LOCATE 1,1,0
	CNT$=INPUT$(1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	IF ASC(CNT$)=%Esc THEN MENU
END SUB
:
SUB FunKey(KeyFun$())
	SHARED ForeG,M
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	A=1:X=2
	IF M>103 THEN
		INCR A,10
		COLOR %PAPER,ForeG
		LOCATE 23,3:PRINT "ALT Menu"
		COLOR ForeG,%PAPER
	END IF
	FOR N=A TO A+9
		LOCATE 25,X
		PRINT LTRIM$(STR$(N MOD 10));
		COLOR %PAPER,ForeG
		PRINT KeyFun$(N);
		COLOR ForeG,%PAPER
		INCR X,8
	NEXT N
END SUB
:
SUB GetDirra(WhichFiles$) SHARED
	FF=0:SubD=%FALSE:XF=%FALSE:DTA$=SPACE$(43):NM$=""
	ERASE FileEntry$,FileLen&
	CALL SETDTA(DTA$)
	NM$=FINDFIRST$(WhichFiles$)
	IF NM$="" THEN
		EXIT SUB
	ELSE
		FileEntry$(0)=RIGHT$(NM$,LEN(NM$)-4)
		FileLen&(0)=CVL(LEFT$(NM$,4))
		IF SubD THEN FileEntry$(0)="<"+FileEntry$(0)+">"
		FOR FF=1 TO 139
			NM$=FINDNEXT$
			IF NM$="" THEN EXIT FOR
			FileEntry$(FF)=RIGHT$(NM$,LEN(NM$)-4)
			FileLen&(FF)=CVL(LEFT$(NM$,4))
			IF SubD THEN FileEntry$(FF)="<"+FileEntry$(FF)+">"
		NEXT FF
		ARRAY SORT FileEntry$() FOR FF, TAGARRAY FileLen&()
		IF FF>70 THEN XF=%TRUE
	END IF
END SUB
:
SUB PrintName(FileEntry$())
	SHARED FF,HL,XF,FileLen&()
	LOCATE 2,3:PRINT FileEntry$(0)
	IF HL=0 THEN LOCATE %SelLine,3:PRINT FileEntry$(0);TAB(18);FileLen&(0)
	IF XF THEN
		LOCATE 16,41:PRINT CHR$(180)+"PgDn for more"+CHR$(195)
	ELSE
		LOCATE 16,41:PRINT STRING$(15,196)
	END IF
	IF FF=0 THEN EXIT SUB
	FOR N=1 TO 69
		LOCATE (N\5)+2,((N MOD 5)*16)+3
		PRINT FileEntry$(N)
		IF N=FF THEN EXIT FOR
	NEXT N
END SUB
:
SUB XFILES(FileEntry$())
	SHARED FF,HL,FileLen&()
	LOCATE 2,3:PRINT FileEntry$(70)
	IF HL=0 THEN LOCATE %SelLine,3:PRINT FileEntry$(70);TAB(18);FileLen&(70)
	LOCATE 16,41:PRINT CHR$(180)+"PgUp for more"+CHR$(195)
	IF FF=71 THEN EXIT SUB
	FOR N=1 TO FF-70
		LOCATE (N\5)+2,((N MOD 5)*16)+3
		PRINT FileEntry$(70+N)
	NEXT N
END SUB
:
FUNCTION NEWDIR$(Path$)
	SHARED HL,LH,XFH,XHL
	HL=0:LH=0:XFH=%FALSE:XHL=0
	Path$=UCASE$(RTRIM$(Path$,"\"))
	OldPath$=RTRIM$(CURDIR$,"\")
	IF MID$(Path$,2,1)=":" THEN
		IF INSTR(Path$,"\")=0 THEN
			CHDRIVE Path$
			CHDIR "\"
			EXIT IF
		ELSEIF INSTR(Path$,"\") THEN
			CHDRIVE Path$
			CHDIR RIGHT$(Path$,LEN(Path$)-2)
			EXIT IF
		END IF
	ELSEIF INSTR(Path$,"\") THEN
		IF INSTR(Path$,"\.") THEN
			IF INSTR(Path$,"\..") THEN
				CHDIR ".."
			ELSE
				CHDIR "\"
			END IF
		ELSE
			CHDIR RIGHT$(OldPath$,LEN(OldPath$)-2)+Path$
		END IF
		ContCD:
	END IF
	NEWDIR$=RTRIM$(CURDIR$,"\")
END FUNCTION
:
SELECTA:
	Jump=0:Choice=0:TempName$=""
	IF XFH=%FALSE THEN
		IF FF THEN
			IF XF THEN
				FPOS=69
			ELSE
				FPOS=FF-1
				IF HL>=FPOS THEN HL=FPOS:XHL=HL
			END IF
		END IF
	ELSE
		FPOS=FF-71
		IF HL>=FPOS THEN HL=FPOS:XHL=HL+70
	END IF
	Row=(HL\5)+2:Col=((HL MOD 5)*16)+3
	IF NoDraw=%FALSE THEN
		IF FF THEN
			IF XFH THEN HiLite=HL+70 ELSE HiLite=HL
			COLOR %PAPER,ForeG
			LOCATE Row,Col:PRINT FileEntry$(HiLite)
			COLOR ForeG,%PAPER
			LOCATE %SelLine,3:PRINT SPACE$(30)
			LOCATE %SelLine,3:PRINT FileEntry$(HiLite);TAB(18);FileLen&(HiLite)
		END IF
	ELSE
  	LOCATE %SelLine,3
		IF M=%AltF4 THEN
			PRINT "Press Enter to start";
		ELSE
			PRINT "Enter filename";
		END IF
		PRINT SPACE$(24)
	END IF
	Col=3:LOCATE %SelLine,3,1,0,7
	DO
		DO
			Choice$=INKEY$
		LOOP WHILE Choice$=""
		Choice=ASC(RIGHT$(Choice$,1))
		IF LEN(Choice$)=2 THEN
			IF FF AND NoDraw=%FALSE THEN
				Col=3:LH=HL:TempName$=""
				SELECT CASE Choice
					CASE %RtAr:INCR HL
					CASE %LfAr:DECR HL
					CASE %DnAr:INCR HL,5
					CASE %UpAr:DECR HL,5
					CASE %Home:HL=0
					CASE %End :HL=FPOS
					CASE %PgDn:
						IF XF THEN
							XFH=%TRUE
							FPOS=FF-71
							LOCATE ,,0
							CALL ClearWindow(%TopLeft2,%BottomRight2)
							CALL XFILES(FileEntry$())
							HL=0:LH=HL
						END IF
					CASE %PgUp
						IF XF THEN
							XFH=%FALSE
							FPOS=69
							LOCATE ,,0
							CALL ClearWindow(%TopLeft2,%BottomRight2)
							CALL PRINTNAME(FileEntry$())
							HL=0:LH=HL
						END IF
					CASE %F1 TO %F10, %AltF1 TO %AltF10
						NoDraw=%TRUE
						Jump=Choice
						IF ABS(M-Choice)>9 THEN ChangeMenu=%TRUE ELSE ChangeMenu=%FALSE
						EXIT LOOP
					CASE 30,32,37
						SELECT CASE Choice
							CASE 30
								VER=1:VER$=" ASCII Version "
								GOSUB CHARS
							CASE 32
								VER=0:VER$=" Disk  Version "
							CASE 37
								VER=2:VER$="Keyword Version"
						END SELECT
					COLOR %PAPER,ForeG
					LOCATE %HeadLine,19:PRINT LEFT$(LTRIM$(Ver$),1)
					COLOR ForeG,%PAPER
				END SELECT
				IF HL<0 OR HL>FPOS THEN HL=LH
				XHL=HL:XLH=LH
				IF XFH THEN INCR XHL,70:INCR XLH,70
				LOCATE (LH\5)+2,((LH MOD 5)*16)+3
				COLOR ForeG,%PAPER:PRINT FileEntry$(XLH)
				LOCATE (HL\5)+2,((HL MOD 5)*16)+3
				COLOR %PAPER,ForeG
				PRINT FileEntry$(XHL)
				COLOR ForeG,%PAPER
				IF NoDraw=%FALSE THEN
					LOCATE %SelLine,3:PRINT SPACE$(24):LOCATE %SelLine,3
					PRINT FileEntry$(XHL);TAB(18);FileLen&(XHL)
				ELSE
					PRINT "Enter filename";SPACE$(24)
				END IF
				LOCATE %SelLine,3
				LH=HL
			ELSEIF FF<2 OR NoDraw THEN
				IF Choice=>%F1 AND Choice<=%F10 OR Choice=>%AltF1 AND Choice<=%AltF10 THEN
					Jump=Choice
					IF ABS(M-Choice)>9 THEN ChangeMenu=%TRUE ELSE ChangeMenu=%FALSE
					EXIT LOOP
				END IF
			END IF
		ELSE
			SELECT CASE Choice
				CASE %Ent
					IF TempName$="" THEN
						NM$=FileEntry$(XHL)
						IF ASCII(NM$)=60 THEN
							NM$="\"+REMOVE$(NM$, ANY "<>")
						END IF
					ELSE
						NM$=TempName$
					END IF
					EXIT LOOP
				CASE %Esc:EXIT IF
				CASE %Bsp
					DECR Col
					IF Col<3 THEN Col=3:EXIT SELECT
					LOCATE %SelLine,Col:PRINT CHR$(32):LOCATE %SelLine,Col
					TempName$=LEFT$(TempName$,LEN(TempName$)-1)
				CASE ELSE
					PRINT UCASE$(CHR$(Choice));SPACE$(31-LEN(TempName$))
					INCR Col:LOCATE %SelLine,Col
					TempName$=TempName$+UCASE$(CHR$(Choice))
			END SELECT
		END IF
	LOOP UNTIL (Choice$)=CHR$(%Esc)
	IF Choice$=CHR$(%Esc) THEN
		Jump=0:NM$=""
	END IF
	LOCATE ,,0
RETURN
:
SUB NewScreen(Drive$,WhichFiles$,Heading$)
	SHARED FF,FileEntry$()
	CALL ChPath(Drive$)
	CALL GetDirra(WhichFiles$)
	CALL ScreenDraw(Heading$,Drive$)
	IF FF THEN CALL ShowName(FileEntry$())
END SUB
:
SUB ScreenDraw(Heading$,Drive$) SHARED
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	LOCATE 16,1:PRINT HorLine$
	IF KeyOn THEN CALL FunKey(KeyFun$())
	COLOR %PAPER,ForeG
	LOCATE %HeadLine,3:PRINT Heading$
	COLOR ForeG,%PAPER
	FS&&=FREESPACE&&(CURDIR$):OldFree&&=FS&&
	LOCATE %HeadLine,58:PRINT FS&& "bytes free"
	IF FF=0 THEN
		COLOR %FLASH
		LOCATE 2,3:PRINT "No files!"
		COLOR ForeG
		LOCATE %SelLine,3:PRINT "Change drive?"
	ELSE
		IF NoDraw=%FALSE THEN
			LOCATE %ActLine,3:PRINT Arrows$
		END IF
	END IF
	LOCATE %ActLine,65:PRINT "ESC: Main Menu"
END SUB
:
SUB ReScreen(HeadX,Heading$,WhichFiles$) SHARED
	LOCATE %HeadLine,HeadX:PRINT SPACE$(79-HeadX)
	COLOR %PAPER,ForeG
	IF Jump THEN LOCATE %HeadLine,3:PRINT Heading$
	LOCATE %HeadLine,HeadX:PRINT RTRIM$(CURDIR$,"\")
	COLOR ForeG,%PAPER
	FS&&=FREESPACE&&(CURDIR$)
	IF OldFree&&<>FS&& THEN
		DiskChanged=%TRUE
	ELSE
		DiskChanged=%FALSE
	END IF
	OldFree&&=FS&&
	LOCATE %HeadLine,58:PRINT FS&& "bytes free"
	IF Jump=0 OR DiskChanged THEN
		CALL ClearWindow(%TopLeft2,%BottomRight2)
		LOCATE %SelLine,2:PRINT SPACE$(32)
		CALL RePrint(WhichFiles$)
	END IF
	IF SCREEN(%ActLine,3)<>27 THEN
		IF FF AND NoDraw=%FALSE THEN
			LOCATE %ActLine,3:PRINT Arrows$
		END IF
	ELSEIF FF=0 OR NoDraw=%TRUE THEN
		LOCATE %ActLine,3:PRINT SPACE$(40)
	END IF
	IF ChangeMenu AND KeyOn THEN CALL FunKey(KeyFun$())
END SUB
:
SUB RePrint(WhichFiles$)
	SHARED FF,ForeG,OldFree&&,FileEntry$()
	CALL ClearWindow(%TopLeft2,%BottomRight2)
	CALL GetDirra(WhichFiles$)
	FS&&=FREESPACE&&(CURDIR$)
	LOCATE %HeadLine,58:PRINT FS&& "bytes free"
	OldFree&&=FS&&
	IF FF=0 THEN
		COLOR %FLASH
		LOCATE 2,3:PRINT "No files!"
		COLOR ForeG
		LOCATE %SelLine,3:PRINT "Change drive?"
		LOCATE %ActLine,3:PRINT SPACE$(40)
	ELSE
		CALL ShowName(FileEntry$())
	END IF
END SUB
:
SUB ShowName(FileEntry$())
	SHARED FF,XFH
	IF FF=70 AND XFH THEN XFH=%FALSE
	IF XFH=%FALSE THEN
		CALL PrintName(FileEntry$())
	ELSE
		CALL Xfiles(FileEntry$())
	END IF
END SUB
:
SUB ChPath(Path$)
	CHDRIVE Path$
	IF INSTR (Path$,"\") THEN
		CHDIR RIGHT$(Path$,LEN(Path$)-2)
	ELSE
		CHDIR "\"
	END IF
END SUB
:
SUB TidyUp
	SHARED Arrows$,ClearLine$,KeyFun$()
	LOCATE %SelLine,2:PRINT ClearLine$
	LOCATE %ActLine,3:PRINT Arrows$;TAB(65);"ESC: Main Menu"
	CALL FunKey(KeyFun$())
END SUB
