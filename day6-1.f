      PROGRAM DAY6P1
C
      CHARACTER*4000 PUZZLE,OPERATORS
      INTEGER*8 TOTAL,TMUL
      INTEGER L1,L2,NLINES,PZLINE,OPERANDS(4,1000),NUMCOLS
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(A,I16)
C
      WRITE(*,10)"Advent of Code 2025 day 6, part 1"
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
C     List directed i/o ftw!
      DO L1=1,NLINES-1
C       We should never hit ERR or END - if we do, give up and goto 999
C       as file format is incorrect
        READ(10,FMT=*,ERR=999,END=999)(OPERANDS(PZLINE,L2),L2=1,NUMCOLS)
        PZLINE=PZLINE+1
      ENDDO
      CLOSE(10)
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
            TMUL=TMUL*INT8(OPERANDS(L3,L1))
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
