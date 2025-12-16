      PROGRAM DAY11P1
C
C     Assume graph is no larger than 700 nodes, each with no
C     more than 25 leaves - parameterised below.
C
      INTEGER MAXNODES,MAXLEAVES
      PARAMETER (MAXNODES=700)
      PARAMETER (MAXLEAVES=25)
C
      CHARACTER*256 PUZZLE
      CHARACTER*3 NODE,LEAVES(MAXLEAVES)
      COMPLEX GRAPH(MAXNODES,MAXLEAVES+1),STR2CMPLX
      LOGICAL VISITED(MAXNODES)
      INTEGER L1,NL,NC,SOURCE,DEST,TOTAL
C
   10 FORMAT(A)
   20 FORMAT(A)
   30 FORMAT(A,I8)
C
      WRITE(*,10)"Advent of Code 2025 day 11, part 1"
      WRITE(*,10)" "
C
      OPEN(10,FILE="day11in.txt",STATUS="OLD",FORM="FORMATTED",
     +     ACCESS="SEQUENTIAL",ACTION="READ")
      NC=0
   50 CONTINUE
      READ(10,FMT=20,ERR=999,END=100) PUZZLE
C     Count the nodes read in
      NC=NC+1
      CALL SPLIT(PUZZLE,NODE,LEAVES,NL)
C     First element of each row of the graph is a complex number,
C     The real element represent the value of the node's name,
C     the imaginary element is set to the number of leaves that
C     the node has (currently 0) by STR2CMPLX
      GRAPH(NC,1)=STR2CMPLX(NODE,0)
C     Record the NC index of the SOURCE node "you" = 251521
      IF (INT(REAL(GRAPH(NC,1))).EQ.251521) SOURCE=NC
C     Remaining elements are the leaves. Leaf name value in the
C     real part of the complex number, imaginary part is 0 (may
C     be useful for solving part 2 perhaps?)
      DO L1=1,NL
        GRAPH(NC,L1+1)=STR2CMPLX(LEAVES(L1),0)
      ENDDO
C     Update the leaf count on the NODE element NC,1
      GRAPH(NC,1)=GRAPH(NC,1)+CMPLX(0,NL)
C
C     WRITE(*,*) NC,(GRAPH(NC,L1),L1=1,NL+1)
      GOTO 50
  100 CONTINUE
      CLOSE(10)
C     Add a node for "out" (DEST) with no leaves
      NC=NC+1
      GRAPH(NC,1)=STR2CMPLX("out",0)
      DEST=NC
C
C     WRITE(*,*) NC,(GRAPH(NC,L1),L1=1,NL+1)
C     Mark every node as not visited
      DO L1=1,MAXNODES
        VISITED(L1)=.FALSE.
      ENDDO
C     Call the DFS graph walking routine to find the number of
C     distinct paths from SOURCE to DEST
      TOTAL=0
      CALL DFS (GRAPH,SOURCE,DEST,VISITED,TOTAL)
C
      WRITE(*,30)"Number of paths from you to out is ",TOTAL
  999 CONTINUE
      END
C
      SUBROUTINE SPLIT(PUZZLE,NODE,LEAVES,NL)
C
C     Splits the string PUZZLE, format xxx: yyy .. zzz
C     into NODE, LEAVES(NL). Contents of PUZZLE destroyed!
C     Assumes every NODE, LEAVES string is exactly 3 characters long
C
      CHARACTER*256 PUZZLE
      CHARACTER*3 NODE,LEAVES(*)
      INTEGER NL
C
C     Fixed format - NODE is the first 3 characters of PUZZLE
      NODE=PUZZLE(1:3)
C     Fixed format - skip to the first leaf past : and <space>
      PUZZLE=PUZZLE(6:)
      NL=0
      DO WHILE (PUZZLE(1:1).NE." ")
        NL=NL+1
        LEAVES(NL)=PUZZLE(1:3)
        PUZZLE=PUZZLE(5:)
      ENDDO
C  
      RETURN
      END
C
      COMPLEX FUNCTION STR2CMPLX(NODE,VAL)
      CHARACTER*3 NODE
      INTEGER VAL
C
      INTEGER L1,NODEVAL
C
      NODEVAL=0
      DO L1=1,3
C       Assumes all node names are lower case! Converts a to 1,
C       b to 2, etc. Multiplies each by 100. So aaa = 10101, 
C       aab = 10102 ... zzz = 262626
        NODEVAL=NODEVAL*100+(ICHAR(NODE(L1:L1))-(ICHAR("a")-1))
      ENDDO
C     Convert to complex number for return
      STR2CMPLX=CMPLX(NODEVAL,0)
      RETURN
      END
C
      RECURSIVE SUBROUTINE DFS(GRAPH,THISNODE,DEST,VISITED,NUMPATHS)
      COMPLEX GRAPH(700,26)
      INTEGER THISNODE,DEST,NUMPATHS
      LOGICAL VISITED(*)
C     
      INTEGER L1,NEIGHBOUR
      COMPLEX NEIGHLEX
      LOGICAL FOUND
C
C     WRITE(*,*)"This node is ",THISNODE,GRAPH(THISNODE,1)
C     WRITE(*,*)"Destination is ",DEST,GRAPH(DEST,1)
C     Exit condition - if THISNODE is DEST, increment NUMPATHS and
C     return.
      IF (THISNODE.EQ.DEST) THEN
        NUMPATHS=NUMPATHS+1
        GOTO 999
      ENDIF
C     Mark THISNODE as VISITED
      VISITED(THISNODE)=.TRUE.
C     Explore all the unvisited neighbours (total neigbours is the
C     imaginary part of GRAPH(THISNODE,1)
      DO L1=1,INT(IMAG(GRAPH(THISNODE,1)))
        NEIGHLEX=GRAPH(THISNODE,L1+1)
C       Find the array index of this neighbour
        FOUND=.FALSE.
        NEIGHBOUR=1
        DO WHILE (.NOT.FOUND)
          IF (INT(REAL(NEIGHLEX)).EQ.
     +        INT(REAL(GRAPH(NEIGHBOUR,1)))) THEN
            FOUND=.TRUE.
          ELSE
            NEIGHBOUR=NEIGHBOUR+1
          ENDIF
        ENDDO
C       WRITE(*,*)"Neighbour index is ",NEIGHBOUR 
        CALL DFS(GRAPH,NEIGHBOUR,DEST,VISITED,NUMPATHS)
      ENDDO
C     Backtrack - mark node as not visited before returning
      VISITED(THISNODE)=.FALSE.
  999 CONTINUE
      RETURN
      END
