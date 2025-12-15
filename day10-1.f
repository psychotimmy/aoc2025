      PROGRAM DAY10P1
C
      PARAMETER(MAXTARGETS=5)
      PARAMETER(MAXSWTPOS=24)
      PARAMETER(MAXCOMBOS=4000)
C
      CHARACTER*256 PUZZLE,TSTR
      INTEGER TARGET,L1
      INTEGER SWTPOS(MAXSWTPOS),COMBOS(MAXCOMBOS)
      INTEGER NUMBG,TLEN,TOTAL,CLEN,START,TOT
      INTEGER PARSETARGET
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(A,I8)
C
      WRITE(*,10)"Advent of Code 2025 day 10, part 1"
      WRITE(*,10)" "
C
      OPEN(10,FILE="day10in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      TOTAL=0
   50 CONTINUE
C     Read each input line - character string [ ] ( ) ( ) ... ( ) { }
C     e.g. [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
      READ(UNIT=10,FMT=20,ERR=100,END=100) PUZZLE
      NUM=NUM+1
C     Find the target switch positions as a (binary) number
      TLEN=INDEX(PUZZLE,']')-2
      TSTR=PUZZLE(2:TLEN+1)
      TARGET=PARSETARGET(TSTR,TLEN)
C     WRITE(*,*)"TARGET is ",TARGET
C     Store groups of button pushes as an array of (binary) numbers
      TLEN=(INDEX(PUZZLE,'{')-2)-(INDEX(PUZZLE,']')+2)+1
      TSTR=PUZZLE(INDEX(PUZZLE,']')+2:INDEX(PUZZLE,'{')-2)
      CALL PARSESWT(TSTR,TLEN,SWTPOS,NUMBG)
C     WRITE(*,*)"Number of buttons is ",NUMBG
C     Add up the fewest number of button presses to reach TARGET
      CLEN=0
      START=1
      TOT=0
      CALL CHECKCOMBOS(TARGET,SWTPOS,NUMBG,COMBOS,CLEN,START,TOT)
      TOTAL=TOTAL+TOT
      GOTO 50
  100 CONTINUE
      CLOSE(10)
      WRITE(*,30)"Sum of minimum button presses is ",TOTAL
      END
C
      INTEGER FUNCTION PARSETARGET(TSTR,TLEN)
      CHARACTER*(*) TSTR
      INTEGER TLEN
C
C     Convert the string of . and # to a binary bit representation
C     lsb = the first . or # in the string, msb the last.
C     So ..# = 4 and #.. = 1 (read [..##.] RIGHT to LEFT)
C
      PARSETARGET=0
      DO L1=1,TLEN
        IF (TSTR(L1:L1).EQ.'#') THEN
          PARSETARGET=PARSETARGET+2**(L1-1)
        ENDIF
      ENDDO
      RETURN
      END
C
      SUBROUTINE PARSESWT(TSTR,TLEN,SWTPOS,IDX)
      CHARACTER*(*) TSTR
      INTEGER TLEN
      INTEGER SWTPOS(*),BUTTS(20),IDX
C
      CHARACTER*(TLEN) BUTTONS
      INTEGER BL
C
C     Convert the groups of possible switch toggles to an array 
C     of binary bits organised the opposite way as the TARGET position 
C     i.e. msb = first switch (switch 0), lsb = last switch. 
C     Note: values in TSTR and TLEN are destroyed by this function.
C
      IDX=1
      DO WHILE (TLEN.GT.0)
        BL=INDEX(TSTR,')')-1
C       Add a dummy value and a / to the end so we can use internal
C       list directed i/o to convert button numbers to integers
        BUTTONS=TSTR(2:BL)//",9999,/"
        BL=BL-1
C       WRITE(*,*)BUTTONS(1:BL+7),BL
        READ(BUTTONS,*)(BUTTS(L1),L1=1,20)
        L1=1
        SWTPOS(IDX)=0
        DO WHILE (BUTTS(L1).NE.9999)
C         WRITE(*,*)BUTTS(L1)
          SWTPOS(IDX)=SWTPOS(IDX)+2**BUTTS(L1)
          L1=L1+1
        ENDDO
C       WRITE(*,*)SWTPOS(IDX)
        TLEN=TLEN-BL-3
        TSTR=TSTR(BL+4:)
C       WRITE(*,*)"--->",SWTPOS(IDX)
        IDX=IDX+1
      ENDDO
C     WRITE(*,*)"Parsed ",IDX-1," button groups"
C     Return the number of button groups parsed
      IDX=IDX-1
      RETURN
      END
C
C     Recursive subroutines are an f90 extension
C
      RECURSIVE SUBROUTINE
     +  CHECKCOMBOS(TARGET,VALUES,VLEN,COMBOS,CLEN,START,RES)
      INTEGER TARGET,VALUES(*),VLEN,COMBOS(*),CLEN,START,RES
C
      INTEGER L1,LIGHTS
C
      IF (CLEN.GT.0) THEN
C       WRITE(*,*) "Eval combo of length ",CLEN
C       WRITE(*,*) "Target ",TARGET
C       WRITE(*,*) "Res currently ",RES
C       Toggle the switches according to the button presses
C       Use bitwise XOR to evaluate
        LIGHTS=0
        DO L1=1,CLEN
C         WRITE(*,*)"-->",LIGHTS,COMBOS(L1)
          LIGHTS=XOR(LIGHTS,COMBOS(L1))
        ENDDO
C       If the lights are now the same as target, we have a
C       valid button press combination. Set the number of presses
C       to be CLEN if it's less than the current minimum RES or if
C       RES is currently 0 (i.e. not found). Assumption is the 
C       input always gives at least one valid combination.
        IF (LIGHTS.EQ.TARGET) THEN
          IF ((RES.EQ.0).OR.(CLEN.LT.RES)) RES=CLEN
        ENDIF
      ENDIF
C     
      DO L1=START,VLEN
        IF (CLEN.EQ.0) THEN
          COMBOS(1)=VALUES(L1)
        ELSE
          COMBOS(CLEN+1)=VALUES(L1)
        ENDIF
        CALL CHECKCOMBOS(TARGET,VALUES,VLEN,COMBOS,CLEN+1,L1+1,RES)
      ENDDO
C
      RETURN
      END
