      PROGRAM DAY6P2
C
      CHARACTER*4000 PUZZLE,OPERATORS,COPERANDS(4)
      CHARACTER*4 CNUM
      INTEGER*8 TOTAL,TMUL
      INTEGER L1,L2,L3,L4,NLINES,PZLINE,OPERANDS(4,1000)
      INTEGER NUMCOLS,CPTR,INUM
      LOGICAL FOUNDSPACE
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(A,I16)
   40 FORMAT(I5)
C
      WRITE(*,10)"Advent of Code 2025 day 6, part 2"
      WRITE(*,10)" "
      TOTAL=0
C     Read the puzzle input from the file to get to the last line
C     as it contains the operators
      OPEN(10,FILE="day6in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
C     Need to know how many input lines we have
      NLINES=0
   50 CONTINUE
      READ(10,FMT=20,ERR=999,END=100) PUZZLE
      NLINES=NLINES+1
      GOTO 50
  100 CONTINUE
C     Save the last line - these are our operators
      OPERATORS=PUZZLE
C     Work out how many columns of numbers we have (= number of operators)
C     Note that the contents of PUZZLE is destroyed by this!
C     (Sample file has 4 columns, puzzle input has 1000)
      NUMCOLS=0
      DO L1=1,LEN(PUZZLE)
        IF ((PUZZLE(L1:L1).EQ.'+').OR.(PUZZLE(L1:L1).EQ.'*')) THEN
          PUZZLE(L1:L1)=' '
          NUMCOLS=NUMCOLS+1
        ENDIF
      ENDDO
C     Rewind the input file to read the numbers
      REWIND(10)
  150 CONTINUE
      PZLINE=1
C     For this part of the problem we need to read as characters, sadly
      DO L1=1,NLINES-1
C       We should never hit ERR or END - if we do, give up and goto 999
C       as file format is incorrect
        READ(10,FMT=20,ERR=999,END=999) COPERANDS(PZLINE)
        PZLINE=PZLINE+1
      ENDDO
      CLOSE(10)
C     For the sample input, numbers are no larger than 999, for the
C     puzzle input, 9999. This happens to be PZLINE-1. HOWEVER, we 
C     still need to work out how long the numbers are from the spacing
C     of the operators as we can have a column of no more than 3,2 or 1
C     digits. Sneaky!
C     This also means we need to zero the OPERANDS 2D array and change
C     the logic for the multiplication later on (otherwise we'll get a
C     lot of 0 answers
      DO L1=1,4
        DO L2=1,1000
          OPERANDS(L1,L2)=0
        ENDDO
      ENDDO
C
      CPTR=1
      DO L1=1,NUMCOLS
C       WRITE(*,*)L1,"-----------------"
        DO L2=1,PZLINE
C         Need to work out if the whole column is spaces
C         If so, we move onto the next column as the MAXIMUM
C         size is PZLINE-1 ... it can be less in the puzzle data
          FOUNDSPACE=.TRUE.
          DO L3=1,PZLINE-1
            CNUM(L3:L3)=COPERANDS(L3)(CPTR:CPTR)
            IF (CNUM(L3:L3).NE.' ') FOUNDSPACE=.FALSE.
          ENDDO
          CPTR=CPTR+1
C         If FOUNDSPACE is still TRUE then we have a separator
          IF (FOUNDSPACE) THEN
C           WRITE(*,*)"Stop! ",CNUM(1:L3-1)
C           Ick. Force jump out of the L2 loop.
            GOTO 200
          ELSE
C           WRITE(*,*)"----> ",CNUM(1:L3-1)
            READ(CNUM(1:L3-1),FMT=40)INUM
            OPERANDS(L2,L1)=INUM
          ENDIF
        ENDDO
  200   CONTINUE
      ENDDO
C     Calculate the value of each column, add onto TOTAL
      L1=1
      DO L2=1,LEN(OPERATORS)
        IF (OPERATORS(L2:L2).EQ.'+') THEN
          OPERATORS(L2:L2)=' '
          DO L3=1,PZLINE-1
            TOTAL=TOTAL+INT8(OPERANDS(L3,L1))
          ENDDO
C         WRITE(*,30)"Running total after addition is       ",TOTAL
C         Go to the next input column
          L1=L1+1
        ENDIF
        IF (OPERATORS(L2:L2).EQ.'*') THEN
          OPERATORS(L2:L2)=' '
          TMUL=INT8(OPERANDS(1,L1))
          DO L3=2,PZLINE-1
C           Check we're not multiplying by 0
            IF (OPERANDS(L3,L1).NE.0) THEN
              TMUL=TMUL*INT8(OPERANDS(L3,L1))
            ENDIF
          ENDDO
          TOTAL=TOTAL+TMUL
C         WRITE(*,30)"Running total after multiplication is ",TOTAL
C         Go to the next input column
          L1=L1+1
        ENDIF
      ENDDO
C
      WRITE(*,10)" "
      WRITE(*,30)"Final sum of worksheet columns is     ",TOTAL
  999 CONTINUE
      IF (TOTAL.EQ.0) WRITE(*,10)"Input file error!!"
      END
