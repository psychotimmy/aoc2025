      PROGRAM DAY10P2
C
      PARAMETER(MAXTARGETS=5)
      PARAMETER(MAXSWTPOS=24)
      PARAMETER(MAXCOMBOS=16)
C
      CHARACTER*256 PUZZLE,TSTR
      INTEGER TARGET,L1,TJB,TJT,JL
      INTEGER SWTPOS(MAXSWTPOS),COMBOS(MAXCOMBOS)
      INTEGER NUMBG,TLEN,TOTAL,INO,CLEN,START,TOT
      INTEGER GOODCOMBOS(20,MAXCOMBOS),PARSEJOLTS,JOLTS(MAXSWTPOS) 
      INTEGER JOLTSNOW(MAXSWTPOS),CL,BEST,FAC,RESULT
      LOGICAL GOOD
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(A,I8)
C
      WRITE(*,10)"Advent of Code 2025 day 10, part 2"
      WRITE(*,10)" "
C
      OPEN(10,FILE="day10in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      TOTAL=0
      INO=0
   50 CONTINUE
C     Read each input line - character string [ ] ( ) ( ) ... ( ) { }
C     e.g. [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
      READ(UNIT=10,FMT=20,ERR=100,END=100) PUZZLE
      INO=INO+1
C     Convert the set of joltages to a (binary) number where any odd
C     value is binary 1 and even values are binary 0.
C
      TJB=INDEX(PUZZLE,'{')+1
      TJT=INDEX(PUZZLE,'}')-1
      TLEN=TJT-TJB+1
C     Add a dummy value and / so we can use internal list-driected i/o
C     to extract the joltages fromt the string of joltages.
      TSTR=PUZZLE(TJB:TJT)//",-1,/"
      READ(TSTR,*)(JOLTS(L1),L1=1,MAXSWTPOS)
C     Save the number of actual joltage values stored
      JL=1
      DO WHILE (JOLTS(JL).NE.-1)
        JL=JL+1
      ENDDO
C     Reduce JL by 1 to get the actual count of joltage values
      JL=JL-1
      TARGET=PARSEJOLTS(TSTR,TLEN)
C     WRITE(*,*)"TARGET is ",TARGET
C     Store groups of button pushes as an array of (binary) numbers
      TLEN=(INDEX(PUZZLE,'{')-2)-(INDEX(PUZZLE,']')+2)+1
      TSTR=PUZZLE(INDEX(PUZZLE,']')+2:INDEX(PUZZLE,'{')-2)
      CALL PARSESWT(TSTR,TLEN,SWTPOS,NUMBG)
C     WRITE(*,*)"Number of buttons is ",NUMBG
C
C     Simplest way to solve is to work backwards from the target
c     joltages to 0 for each one. If we can reach exactly 0 for
C     each joltage then we have a solution - if not, we don't.
C
C     Find every possible combination of buttons that can be
C     pressed to reach our target joltages MOD 2 - e.g.
C     3,5,4,7 = 1,1,0,1 = ##.# - in effect, these are the last
C     button pushed required to solve the puzzle, although we work
C     backwards from the goal so these are the first combinations we
C     find.
C
C     We also need to check for a completely even input and
C     halve it until we get at least one odd joltage!
C
C     FAC keeps track of how many times we've split the input in
C     two, as the number of button presses needed to be multiplied
C     by this factor to get the correct result. If the input has at
C     least one odd value FAC starts at 1, if not FAC doubles until
C     we get a starting combination.
C
      FAC=1
C
   75 CONTINUE
      GOOD=.FALSE.
      DO L1=1,JL
        IF (MOD(JOLTS(L1),2).EQ.1) GOOD=.TRUE.
      ENDDO
      IF (.NOT.GOOD) THEN
C       WRITE(*,*)"Even input - halve it and double FAC!"
        DO L1=1,JL
          JOLTS(L1)=JOLTS(L1)/2
        ENDDO
        FAC=FAC*2
C       ... and update TARGET, as this will have changed
        TARGET=0
        DO L1=1,JL
          IF (MOD(JOLTS(L1),2).EQ.1) THEN
            TARGET=TARGET+2**(L1-1)
          ENDIF
        ENDDO
C       WRITE(*,*)"TARGET now",TARGET
        GOTO 75
      ENDIF
C
C     WRITE(*,*)"Input OK"
C
      CLEN=0
      START=1
      TOT=0
      CALL CHECKCOMBOS(TARGET,SWTPOS,NUMBG,COMBOS,CLEN,START,
     +                 GOODCOMBOS,TOT)
C
C     CHECKGOOD returns a large value if it can't make the joltages
C     work. Any result that does work will have a lower number 
C     of button presses than any failures.
C
C     Start checking from combination 1, repeat until we've
C     checked all possible starting combinations (TOT). Abort if
C     we've got no combinations - something serious has gone wrong.
C
      IF (TOT.EQ.0) STOP 8
C
      BEST=99999
      WRITE(*,*)"Parsing input joltage sequence",INO
      WRITE(*,*)""
      DO CL=1,TOT
C       JOLTS array gets updated in CHECKGOOD - so pass a
C       new copy for each starting combination (f77 is pass by ref)
        DO L1=1,JL
          JOLTSNOW(L1)=JOLTS(L1)
        ENDDO
C       WRITE(*,*) "Joltages start at"
C       WRITE(*,*) (JOLTSNOW(L2),L2=1,JL)
C
        RESULT=GOODCOMBOS(CL,1)*FAC
        CALL CHECKGOOD(GOODCOMBOS,CL,JOLTSNOW,JL,SWTPOS,NUMBG,FAC,
     +                 RESULT)
        WRITE(*,*)"RESULT from combo",CL," is",RESULT
        IF (RESULT.LT.BEST) BEST=RESULT
        WRITE(*,*)"BEST RESULT is now",BEST
        WRITE(*,*)""
      ENDDO
      WRITE(*,*)"===================================================="
      WRITE(*,*)""
C     Assumption is that there is at least 1 way of getting the
C     joltages to the target - give up if this test fails!
      IF (BEST.GE.99999) STOP 8
      TOTAL=TOTAL+BEST
      GOTO 50
  100 CONTINUE
      CLOSE(10)
C
      WRITE(*,30)
     +  "Sum of minimum button presses to reach joltage targets is"
     +  ,TOTAL
      END
C
      INTEGER FUNCTION PARSEJOLTS(TSTR,TLEN)
      CHARACTER*(*) TSTR
      INTEGER TLEN
C
      INTEGER JOLTAGES(24)
C
C     Convert the string of even and odd joltages to a binary bit 
C     representation
C     lsb = the first even or odd in the string, msb the last.
C     So 12,8,7 = 4 and 9,2,6 = 1 
C     (read {even,even,odd,odd} etc RIGHT to LEFT)
C
C     Internal read list directed i/o used, input list already
C     terminated with a dummy value (-1) and a /
C
      READ(TSTR,*)(JOLTAGES(L1),L1=1,24)
      PARSEJOLTS=0
      DO L1=1,16
C       We've finished if we hit -1, exit.
        IF (JOLTAGES(L1).EQ.-1) GOTO 999
C 
        IF (MOD(JOLTAGES(L1),2).EQ.1) THEN
C         Odd value found, set the correct bit in PARSEJOLTS to 1.
          PARSEJOLTS=PARSEJOLTS+2**(L1-1)
        ENDIF
      ENDDO
  999 CONTINUE
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
C     Recursive subroutines are an f90 extension to f77
C
      RECURSIVE SUBROUTINE
     +  CHECKCOMBOS(TARGET,VALUES,VLEN,COMBOS,CLEN,START,
     +              GOODCOMBOS,RES)
      INTEGER TARGET,VALUES(*),VLEN,COMBOS(*),CLEN,START
      INTEGER GOODCOMBOS(20,*),RES
C
      INTEGER L1,LIGHTS
   10 FORMAT(20I8)
C
      IF (CLEN.GT.0) THEN
C       Toggle the switches according to the button presses
C       Use bitwise XOR to evaluate
        LIGHTS=0
        DO L1=1,CLEN
          LIGHTS=XOR(LIGHTS,COMBOS(L1))
        ENDDO
C       If the lights are now the same as target, we have a
C       valid button press combination. Increment RES.
        IF (LIGHTS.EQ.TARGET) THEN
          RES=RES+1
C         Store the number of buttons to press first
          GOODCOMBOS(RES,1)=CLEN
C         Then the buttons
          DO L1=2,CLEN+1
            GOODCOMBOS(RES,L1)=COMBOS(L1-1)
          ENDDO
C         WRITE(*,*)"Valid combination with number of values & values"
C         WRITE(*,10)(GOODCOMBOS(RES,L1),L1=1,CLEN+1)
C         WRITE(*,*)"========================================="
        ENDIF
      ENDIF
C     
      DO L1=START,VLEN
        IF (CLEN.EQ.0) THEN
          COMBOS(1)=VALUES(L1)
        ELSE
          COMBOS(CLEN+1)=VALUES(L1)
        ENDIF
        CALL CHECKCOMBOS(TARGET,VALUES,VLEN,COMBOS,CLEN+1,L1+1,
     +                   GOODCOMBOS,RES)
      ENDDO
C
      RETURN
      END
C
C     Recursive subroutines are an f90 extension to f77
C
      RECURSIVE SUBROUTINE
     +  CHECKGOOD(COM,CL,JOL,JL,SWTPOS,NUMBG,FACTOR,RES)
      INTEGER COM(20,16),CL,JOL(24),JL,SWTPOS(*),NUMBG,FACTOR
      INTEGER RES
C
      INTEGER L1,L2,NEWJOL(24)
      INTEGER NEWTARGET,NEWCOM(20,16),NEWCL,NEWCOMBOS(16)
      INTEGER CLEN,START,TOT,VALUE,BEST,NEWFACTOR,PUSHED
      LOGICAL GOOD
C
C     Copy JOL to NEWJOL as we're going to be updating it
C     f77 is pass by reference
C
      DO L1=1,JL
        NEWJOL(L1)=JOL(L1)
      ENDDO
C
C
C     First, adjust the joltages so that they are all even by
C     subracting the button press values held in COM(CL,2) to
C     COM(CL,1)+1
C
      DO L1=2,COM(CL,1)+1
C       WRITE(*,*) "Pressing ",COM(CL,L1)
        DO L2=1,JL
C         Use a bitwise AND to figure out which buttons are
C         pressed during this combination.
          IF (AND(COM(CL,L1),2**(L2-1)).EQ.2**(L2-1))
     +        NEWJOL(L2)=NEWJOL(L2)-1
        ENDDO
      ENDDO
      PUSHED=COM(CL,1)
C     WRITE(*,*)"Buttons pushed:",PUSHED
C     WRITE(*,*)"Joltages:",(NEWJOL(L1),L1=1,JL)
C
C     Do we have all joltages at 0? If so, return
C
      GOOD=.TRUE.
      DO L1=1,JL
        IF (NEWJOL(L1).NE.0) GOOD=.FALSE.
      ENDDO
      IF (GOOD) THEN
C       WRITE(*,*)"GOOD exit with RES now = ",RES
        RETURN
      ENDIF
C
C     Have we got any negative joltages? If so, return a very
C     large value (99999) as this set of button presses won't work.
C
      GOOD=.TRUE.
      DO L1=1,JL
        IF (NEWJOL(L1).LT.0) THEN
          GOOD=.FALSE.
        ENDIF
      ENDDO
      IF (.NOT.GOOD) THEN
        RES=99999
C       WRITE(*,*)"Negative joltages found"
        RETURN
      ENDIF
C
C     Not all 0. So we halve the input joltages which
C     will all be even numbers at this stage.
C
      NEWFACTOR=FACTOR
  500 CONTINUE
      DO L1=1,JL
        NEWJOL(L1)=NEWJOL(L1)/2
      ENDDO
C     WRITE(*,*)"Joltages are now halved"
C     WRITE(*,*)(NEWJOL(L1),L1=1,JL)
      NEWFACTOR=NEWFACTOR*2
C     WRITE(*,*)"NEWFACTOR is ",NEWFACTOR
C     WRITE(*,*)""
C
C     Next, get the possible combinations that can result in
C     the light pattern (e.g. if the input is 1,2,2,3 the
C     light pattern is #..#, so the only combinations of buttons
C     we need to examine are those that result in that pattern).
C
C     To do this, calculate our new target binary number to use.
C     (a simplified version of the PARSEJOLTS function from part 1)
C
      NEWTARGET=0
      DO L1=1,JL
        IF (MOD(NEWJOL(L1),2).EQ.1) THEN
          NEWTARGET=NEWTARGET+2**(L1-1)
        ENDIF
      ENDDO
C     WRITE(*,*)"NEWTARGET is ",NEWTARGET
C     WRITE(*,*)""
C
C     If they are all even (i.e. NEWTARGET is 0), we halve again 
C     and double NEWFACTOR.
C
      IF (NEWTARGET.EQ.0) GOTO 500
C
C     We now need these combinations of button presses that
C     could work - i.e. the combinations that can give us the
C     button presses to turn all of the new odd joltages even (or 0)
C     again. However, we only want the smallest number of presses
C     that can do this.
C
      CLEN=0
      START=1
      TOT=0
      CALL CHECKCOMBOS(NEWTARGET,SWTPOS,NUMBG,NEWCOMBOS,CLEN,START,
     +                 NEWCOM,TOT)
C
      BEST=99999
C     WRITE(*,*)"Number of new combinations to check is ",TOT
      DO L1=1,TOT
        PUSHED=NEWCOM(L1,1)
        VALUE=RES+PUSHED*NEWFACTOR
C       WRITE(*,*)"Starting value:",VALUE
        CALL CHECKGOOD(NEWCOM,L1,NEWJOL,JL,SWTPOS,NUMBG,NEWFACTOR,
     +                 VALUE)
C       WRITE(*,*)"After combo",L1," VALUE now ",VALUE
        IF (VALUE.LT.BEST) THEN
          BEST=VALUE
        ENDIF
C       WRITE(*,*)"After combo",L1," BEST now ",BEST
      ENDDO
C
C     Set the return result to be the best value found
C
      RES=BEST
C     
      RETURN
      END
