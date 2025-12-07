      PROGRAM DAY7P2
C
      CHARACTER*160 PUZZLE
      COMPLEX SPLITTER(1800)
      INTEGER BEAMPOS(160),DOSPLIT
      INTEGER*8 PATHCOUNT(160),TLS
      INTEGER LINES,NS,POS,L1,BT,TOTAL
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(A,I16,A)
C
      WRITE(*,10)"Advent of Code 2025 day 2, parts 1 & 2"
      WRITE(*,10)" "
C
      OPEN(10,FILE="day7in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      LINES=0
      NS=0
   50 CONTINUE
C     Read each input line
      READ(10,FMT=20,ERR=100,END=100) PUZZLE
      LINES=LINES+1
C     First line has the one and only start point
      IF (LINES.EQ.1) THEN 
        BEAMPOS(1)=INDEX(PUZZLE,'S')
        BT=1
      ENDIF
C     Only every other line has at least one splitter
      POS=INDEX(PUZZLE,'^')
      DO WHILE (POS.NE.0)
        NS=NS+1
        SPLITTER(NS)=CMPLX(LINES,POS)
C       Mark this splitter as recorded and counted
        PUZZLE(POS:POS)='.'
        POS=INDEX(PUZZLE,'^')
      ENDDO
      GOTO 50
  100 CONTINUE
C
C     Iterate through each pair of lines of the tachyon manifold
C     Even = beam only, Odd = splitters
C     PATHCOUNT(x) keeps a track of how many paths go through each
C     column of the grid - calculated by adding the value above
C     each splitter to the columns either side, then zeroing the
C     value above the splitter.
C
      DO L1=1,160
        PATHCOUNT(L1)=0
      ENDDO
      PATHCOUNT(BEAMPOS(1))=1
C
      DO L1=3,LINES,2
        TOTAL=TOTAL+DOSPLIT(L1,SPLITTER,NS,BEAMPOS,BT,PATHCOUNT)
      ENDDO
C
      CLOSE(10)
      WRITE(*,30)"The beam is spilt ",TOTAL," times"
      TLS=PATHCOUNT(1)
      DO L1=2,160
        TLS=TLS+PATHCOUNT(L1)
      ENDDO
      WRITE(*,30)"... generating",TLS," timelines"
      END
C
      FUNCTION DOSPLIT(LINE,SPLITTER,NS,BEAMPOS,BT,PATHCOUNT)
      INTEGER DOSPLIT,LINE,NS,BEAMPOS(*),BT
      INTEGER*8 PATHCOUNT(*)
      COMPLEX SPLITTER(*)
C
      INTEGER BC(BT),TODO,L1,L2,L3
      LOGICAL NOTSPLIT
C
      DOSPLIT=0
C
C     We're going to change the values in BEAMPOS and BT, so 
C     make copies for the loop to work on.
C     
      TODO=BT
      DO L1=1,TODO
        BC(L1)=BEAMPOS(L1)
      ENDDO
C     Reset the beam count
      BT=0
C     For each existing beam in BC
      DO L1=1,TODO
C     Scan through the list of splitters to see if the beam splits
        NOTSPLIT=.TRUE.
        DO L2=1,NS
          IF (SPLITTER(L2).EQ.CMPLX(LINE,BC(L1))) THEN
            NOTSPLIT=.FALSE.
C           Add 1 to the split count
            DOSPLIT=DOSPLIT+1
C           New left beam ... if not already there!
            DO L3=1,BT
              IF (BEAMPOS(BT).EQ.BC(L1)-1) GOTO 100
            ENDDO
            BT=BT+1
            BEAMPOS(BT)=BC(L1)-1
  100       CONTINUE
C           New right beam ... if not already there!
            DO L3=1,BT
              IF (BEAMPOS(BT).EQ.BC(L1)+1) GOTO 200
            ENDDO
            BT=BT+1
            BEAMPOS(BT)=BC(L1)+1
  200       CONTINUE
C
C         Now deal with the PATHCOUNT
C
          PATHCOUNT(BC(L1)-1)=PATHCOUNT(BC(L1)-1)+PATHCOUNT(BC(L1))
          PATHCOUNT(BC(L1)+1)=PATHCOUNT(BC(L1)+1)+PATHCOUNT(BC(L1))
          PATHCOUNT(BC(L1))=0
          ENDIF
        ENDDO
C       Need to keep this beam if it wasn't split ... if not already there!
        IF (NOTSPLIT) THEN
          DO L3=1,BT
            IF (BEAMPOS(L3).EQ.BC(L1)) GOTO 300
          ENDDO
          BT=BT+1
          BEAMPOS(BT)=BC(L1)
  300     CONTINUE
        ENDIF
      ENDDO
      RETURN
      END
