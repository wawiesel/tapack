      MODULE SLV_GaussElim_smatCSR

      USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
      USE USR_smatCSR        !!((06-A-USR_smatCSR.f90))
      USE FUN_Sequence       !!((03-A-FUN_Sequence.f90))
      USE FUN_Default        !!((04-A-FUN_Default.f90))

      CONTAINS

      SUBROUTINE SOLVE_GaussElim_smatCSR(A,B,X,MAX,IERR)
!
!
!  NSPIV CALLS NSPIV1 WHICH USES SPARSE GAUSSIAN ELIMINATION WITH
!  COLUMN INTERCHANGES TO SOLVE THE LINEAR SYSTEM A X = B.  THE
!  ELIMINATION PHASE PERFORMS ROW OPERATIONS ON A AND B TO OBTAIN
!  A UNIT UPPER TRIANGULAR MATRIX U AND A VECTOR Y.  THE SOLUTION
!  PHASE SOLVES U X = Y.
!
!
!  INPUT ARGUMENTS---
!
!  N      INTEGER NUMBER OF EQUATIONS AND UNKNOWNS
!
!  IA     INTEGER ARRAY OF N+1 ENTRIES CONTAINING ROW POINTERS TO A
!         (SEE MATRIX STORAGE DESCRIPTION BELOW)
!
!  JA     INTEGER ARRAY WITH ONE ENTRY PER NONZERO IN A, CONTAINING
!         COLUMN NUMBERS OF THE NONZEROES OF A.  (SEE MATRIX STORAGE
!         DESCRIPTION BELOW)
!
!  A      REAL ARRAY WITH ONE ENTRY PER NONZERO IN A, CONTAINING THE
!         ACTUAL NONZEROES.  (SEE MATRIX STORAGE DESCRIPTION BELOW)
!
!  B      REAL ARRAY OF N ENTRIES CONTAINING RIGHT HAND SIDE DATA
!
!  MAX    INTEGER NUMBER SPECIFYING MAXIMUM NUMBER OF OFF-DIAGONAL
!         NONZERO ENTRIES OF U WHICH MAY BE STORED
!
!  R      INTEGER ARRAY OF N ENTRIES SPECIFYING THE ORDER OF THE
!         ROWS OF A (I.E., THE ELIMINATION ORDER FOR THE EQUATIONS)
!
!  C      INTEGER ARRAY OF N ENTRIES SPECIFYING THE ORDER OF THE
!         COLUMNS OF A.  C IS ALSO AN OUTPUT ARGUMENT
!
!  IC     INTEGER ARRAY OF N ENTRIES WHICH IS THE INVERSE OF C
!         (I.E., IC(C(I)) = I).  IC IS ALSO AN OUTPUT ARGUMENT
!
!  ITEMP  INTEGER ARRAY OF 2*N + MAX + 2 ENTRIES, FOR INTERNAL USE
!
!  RTEMP  REAL ARRAY OF N + MAX ENTRIES FOR INTERNAL USE
!
!
!  OUTPUT ARGUMENTS---
!
!  C      INTEGER ARRAY OF N ENTRIES SPECIFYING THE ORDER OF THE
!         COLUMNS OF U.  C IS ALSO AN INPUT ARGUMENT
!
!  IC     INTEGER ARRAY OF N ENTRIES WHICH IS THE INVERSE OF C
!         (I.E., IC(C(I)) = I).  IC IS ALSO AN INPUT ARGUMENT
!
!  X      REAL ARRAY OF N ENTRIES CONTAINING THE SOLUTION VECTOR
!
!  IERR   INTEGER NUMBER WHICH INDICATES ERROR CONDITIONS OR
!         THE ACTUAL NUMBER OF OFF-DIAGONAL ENTRIES IN U (FOR
!         SUCCESSFUL COMPLETION)
!
!         IERR VALUES ARE---
!
!         0 LT IERR             SUCCESSFUL COMPLETION.  U HAS IERR
!                               OFF-DIAGONAL NONZERO ENTRIES
!
!         IERR = 0              ERROR.  N = 0
!
!         -N LE IERR LT 0       ERROR.  ROW NUMBER IABS(IERR) OF A IS
!                               IS NULL
!
!         -2*N LE IERR LT -N    ERROR.  ROW NUMBER IABS(IERR+N) HAS A
!                               DUPLICATE ENTRY
!
!         -3*N LE IERR LT -2*N  ERROR.  ROW NUMBER IABS(IERR+2*N)
!                               HAS A ZERO PIVOT
!
!         -4*N LE IERR LT -3*N  ERROR.  ROW NUMBER IABS(IERR+3*N)
!                               EXCEEDS STORAGE
!
!
!  STORAGE OF SPARSE MATRICES---
!
!  THE SPARSE MATRIX A IS STORED USING THREE ARRAYS IA, JA, AND A.
!  THE ARRAY A CONTAINS THE NONZEROES OF THE MATRIX ROW-BY-ROW, NOT
!  NECESSARILY IN ORDER OF INCREASING COLUMN NUMBER.  THE ARRAY JA
!  CONTAINS THE COLUMN NUMBERS CORRESPONDING TO THE NONZEROES STORED
!  IN THE ARRAY A (I.E., IF THE NONZERO STORED IN A(K) IS IN
!  COLUMN J, THEN JA(K) = J).  THE ARRAY IA CONTAINS POINTERS TO THE
!  ROWS OF NONZEROES/COLUMN INDICES IN THE ARRAY A/JA (I.E.,
!  A(IA(I))/JA(IA(I)) IS THE FIRST ENTRY FOR ROW I IN THE ARRAY A/JA).
!  IA(N+1) IS SET SO THAT IA(N+1) - IA(1) = THE NUMBER OF NONZEROES IN A
!
!
      TYPE(TYPE_smatCSR_Rdp),INTENT(IN) :: A
      REAL(KIND_Rdp),INTENT(IN) :: B(:)
      REAL(KIND_Rdp),INTENT(OUT) :: X(:)
      INTEGER,INTENT(IN) :: MAX
      INTEGER,INTENT(OUT) :: IERR

      REAL(KIND_Rdp),ALLOCATABLE :: RTEMP(:)
      INTEGER,ALLOCATABLE :: R(:),C(:),IC(:)
      INTEGER,ALLOCATABLE :: ITEMP(:)
      INTEGER IU,JU,U,Y,P,N
!
!  SET INDICES TO DIVIDE TEMPORARY STORAGE FOR NSPIV1
!
      N = A%N
      Y = 1
      U = Y + N
      P = 1
      IU = P + N + 1
      JU = IU + N + 1
      ALLOCATE( R(N),C(N),IC(N) )
      R = Sequence(1,1,N)
      C = Sequence(1,1,N)
      IC = Sequence(1,1,N)
      ALLOCATE( ITEMP(2*N + MAX + 2) )
      ALLOCATE( RTEMP(N + MAX) )

!
!  CALL NSPIV1 TO PERFORM COMPUTATIONS
!
      CALL NSPIV1 (A%N,A%IA,A%JA,A%A,B,MAX,R,C,IC,X,RTEMP(Y),ITEMP(P),&
                   ITEMP(IU),ITEMP(JU),RTEMP(U),IERR)


      !wrapup
      DEALLOCATE( ITEMP,RTEMP,R,C,IC )
      RETURN
      END SUBROUTINE


      SUBROUTINE NSPIV1 (N,IA,JA,A,B,MAX,R,C,IC,X,Y,P,IU,JU,U,IERR,&
                         JustFactor )
!
!
!  NSPIV1 USES SPARSE GAUSSIAN ELIMINATION WITH
!  COLUMN INTERCHANGES TO SOLVE THE LINEAR SYSTEM A X = B.  THE
!  ELIMINATION PHASE PERFORMS ROW OPERATIONS ON A AND B TO OBTAIN
!  A UNIT UPPER TRIANGULAR MATRIX U AND A VECTOR Y.  THE SOLUTION
!  PHASE SOLVES U X = Y.
!
!
!  SEE NSPIV FOR DESCRIPTIONS OF ALL INPUT AND OUTPUT ARGUMENTS
!  OTHER THAN THOSE DESCRIBED BELOW
!
!  INPUT ARGUMENTS (USED INTERNALLY ONLY)---
!
!  Y   REAL ARRAY OF N ENTRIES USED TO COMPUTE THE UPDATED
!      RIGHT HAND SIDE
!
!  P   INTEGER ARRAY OF N+1 ENTRIES USED FOR A LINKED LIST.
!      P(N+1) IS THE LIST HEADER, AND THE ENTRY FOLLOWING
!      P(K) IS IN P(P(K)).  THUS, P(N+1) IS THE FIRST DATA
!      ITEM, P(P(N+1)) IS THE SECOND, ETC.  A POINTER OF
!      N+1 MARKS THE END OF THE LIST
!
!  IU  INTEGER ARRAY OF N+1 ENTRIES USED FOR ROW POINTERS TO U
!      (SEE MATRIX STORAGE DESCRIPTION BELOW)
!
!  JU  INTEGER ARRAY OF MAX ENTRIES USED FOR COLUMN NUMBERS OF
!      THE NONZEROES IN THE STRICT UPPER TRIANGLE OF U.  (SEE
!      MATRIX STORAGE DESCRIPTION BELOW)
!
!  U   REAL ARRAY OF MAX ENTRIES USED FOR THE ACTUAL NONZEROES IN
!      THE STRICT UPPER TRIANGLE OF U.  (SEE MATRIX STORAGE
!      DESCRIPTION BELOW)
!
!
!  STORAGE OF SPARSE MATRICES---
!
!  THE SPARSE MATRIX A IS STORED USING THREE ARRAYS IA, JA, AND A.
!  THE ARRAY A CONTAINS THE NONZEROES OF THE MATRIX ROW-BY-ROW, NOT
!  NECESSARILY IN ORDER OF INCREASING COLUMN NUMBER.  THE ARRAY JA
!  CONTAINS THE COLUMN NUMBERS CORRESPONDING TO THE NONZEROES STORED
!  IN THE ARRAY A (I.E., IF THE NONZERO STORED IN A(K) IS IN
!  COLUMN J, THEN JA(K) = J).  THE ARRAY IA CONTAINS POINTERS TO THE
!  ROWS OF NONZEROES/COLUMN INDICES IN THE ARRAY A/JA (I.E.,
!  A(IA(I))/JA(IA(I)) IS THE FIRST ENTRY FOR ROW I IN THE ARRAY A/JA).
!  IA(N+1) IS SET SO THAT IA(N+1) - IA(1) = THE NUMBER OF NONZEROES IN
!  A.  IU, JU, AND U ARE USED IN A SIMILAR WAY TO STORE THE STRICT UPPER
!  TRIANGLE OF U, EXCEPT THAT JU ACTUALLY CONTAINS C(J) INSTEAD OF J
!
!
      REAL(KIND_Rdp) :: A(1),B(1),U(1),X(1),Y(1)
      REAL(KIND_Rdp) :: DK,LKI,ONE,XPV,XPVMAX,YK,ZERO
      INTEGER C(1),IA(1),IC(1),IU(1),JA(1),JU(1),P(1),R(1)
      INTEGER CK,PK,PPK,PV,V,VI,VJ,VK
      LOGICAL,INTENT(IN),OPTIONAL :: JustFactor
      LOGICAL :: JustFactor_
!
!
      JustFactor_ = DEFAULT(.FALSE.,JustFactor)
!
!
      IF (N .EQ. 0) GO TO 1001
!
      ONE = 1.0
      ZERO = 0.0
!
!  INITIALIZE WORK STORAGE AND POINTERS TO JU
!
      DO 10 J=1,N
        X(J) = ZERO
 10     CONTINUE
      IU(1) = 1
      JUPTR = 0
!
!  PERFORM SYMBOLIC AND NUMERIC FACTORIZATION ROW BY ROW
!  VK (VI,VJ) IS THE GRAPH VERTEX FOR ROW K (I,J) OF U
!
      DO 170 K=1,N
!
!  INITIALIZE LINKED LIST AND FREE STORAGE FOR THIS ROW
!  THE R(K)-TH ROW OF A BECOMES THE K-TH ROW OF U.
!
        P(N+1) = N+1
        VK = R(K)
!
!  SET UP ADJACENCY LIST FOR VK, ORDERED IN
!  CURRENT COLUMN ORDER OF U.  THE LOOP INDEX
!  GOES DOWNWARD TO EXPLOIT ANY COLUMNS
!  FROM A IN CORRECT RELATIVE ORDER
!
        JMIN = IA(VK)
        JMAX = IA(VK+1) - 1
        IF (JMIN .GT. JMAX)  GO TO 1002
        J = JMAX
 20       JAJ = JA(J)
          VJ = IC(JAJ)
!
!  STORE A(K,J) IN WORK VECTOR
!
          X(VJ) = A(J)
!  THIS CODE INSERTS VJ INTO ADJACENCY LIST OF VK
          PPK = N+1
 30       PK = PPK
          PPK = P(PK)
          IF (PPK - VJ)  30,1003,40
 40       P(VJ) = PPK
          P(PK) = VJ
          J = J - 1
          IF (J .GE. JMIN) GO TO 20
!
!  THE FOLLOWING CODE COMPUTES THE K-TH ROW OF U
!
        VI = N+1
        YK = B(VK)
 50     VI = P(VI)
        IF (VI .GE. K) GO TO 110
!
!  VI LT VK -- PROCESS THE L(K,I) ELEMENT AND MERGE THE
!  ADJACENCY OF VI WITH THE ORDERED ADJACENCY OF VK
!
        LKI = - X(VI)
        X(VI) = ZERO
!
!  ADJUST RIGHT HAND SIDE TO REFLECT ELIMINATION
!
        YK = YK + LKI * Y(VI)
        PPK = VI
        JMIN = IU(VI)
        JMAX = IU(VI+1) - 1
        IF (JMIN .GT. JMAX)  GO TO 50
        DO 100 J=JMIN,JMAX
          JUJ = JU(J)
          VJ = IC(JUJ)
!
!  IF VJ IS ALREADY IN THE ADJACENCY OF VK,
!  SKIP THE INSERTION
!
          IF (X(VJ) .NE. ZERO)  GO TO 90
!
!  INSERT VJ IN ADJACENCY LIST OF VK.
!  RESET PPK TO VI IF WE HAVE PASSED THE CORRECT
!  INSERTION SPOT.  (THIS HAPPENS WHEN THE ADJACENCY OF
!  VI IS NOT IN CURRENT COLUMN ORDER DUE TO PIVOTING.)
!
          IF (VJ - PPK) 60,90,70
 60       PPK = VI
 70       PK = PPK
          PPK = P(PK)
          IF (PPK - VJ)  70,90,80
 80       P(VJ) = PPK
          P(PK) = VJ
          PPK = VJ
!
!  COMPUTE L(K,J) = L(K,J) - L(K,I)*U(I,J) FOR L(K,I) NONZERO
!  COMPUTE U*(K,J) = U*(K,J) - L(K,I)*U(I,J) FOR U(K,J) NONZERO
!  (U*(K,J) = U(K,J)*D(K,K))
!
 90       X(VJ) = X(VJ) + LKI * U(J)
 100      CONTINUE
        GO TO 50
!
!  PIVOT--INTERCHANGE LARGEST ENTRY OF K-TH ROW OF U WITH
!  THE DIAGONAL ENTRY.
!
!  FIND LARGEST ENTRY, COUNTING OFF-DIAGONAL NONZEROES
!
 110    IF (VI .GT. N) GO TO 1004
        XPVMAX = ABS(X(VI))
        MAXC = VI
        NZCNT = 0
        PV = VI
 120      V = PV
          PV = P(PV)
          IF (PV .GT. N) GO TO 130
          NZCNT = NZCNT + 1
          XPV = ABS(X(PV))
          IF (XPV .LE. XPVMAX) GO TO 120
          XPVMAX = XPV
          MAXC = PV
          MAXCL = V
          GO TO 120
 130    IF (XPVMAX .EQ. ZERO) GO TO 1004
!
!  IF VI = K, THEN THERE IS AN ENTRY FOR DIAGONAL
!  WHICH MUST BE DELETED.  OTHERWISE, DELETE THE
!  ENTRY WHICH WILL BECOME THE DIAGONAL ENTRY
!
        IF (VI .EQ. K) GO TO 140
        IF (VI .EQ. MAXC) GO TO 140
        P(MAXCL) = P(MAXC)
        GO TO 150
 140    VI = P(VI)
!
!  COMPUTE D(K) = 1/L(K,K) AND PERFORM INTERCHANGE.
!
 150    DK = ONE / X(MAXC)
        X(MAXC) = X(K)
        I = C(K)
        C(K) = C(MAXC)
        C(MAXC) = I
        CK = C(K)
        IC(CK) = K
        IC(I) = MAXC
        X(K) = ZERO
!
!  UPDATE RIGHT HAND SIDE.
!
        Y(K) = YK * DK
!
!  COMPUTE VALUE FOR IU(K+1) AND CHECK FOR STORAGE OVERFLOW
!
        IU(K+1) = IU(K) + NZCNT
        IF (IU(K+1) .GT. MAX+1) GO TO 1005
!
!  MOVE COLUMN INDICES FROM LINKED LIST TO JU.
!  COLUMNS ARE STORED IN CURRENT ORDER WITH ORIGINAL
!  COLUMN NUMBER (C(J)) STORED FOR CURRENT COLUMN J
!
        IF (VI .GT. N)  GO TO 170
        J = VI
 160      JUPTR = JUPTR + 1
          JU(JUPTR) = C(J)
          U(JUPTR) = X(J) * DK
          X(J) = ZERO
          J = P(J)
          IF (J .LE. N) GO TO 160
 170    CONTINUE
!
!  BACKSOLVE U X = Y, AND REORDER X TO CORRESPOND WITH A
!
      IF( JustFactor_ )RETURN

      K = N
      DO 200 I=1,N
        YK = Y(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF (JMIN .GT. JMAX)  GO TO 190
        DO 180 J=JMIN,JMAX
          JUJ = JU(J)
          JUJ = IC(JUJ)
          YK = YK - U(J) * Y(JUJ)
 180      CONTINUE
 190    Y(K) = YK
        CK = C(K)
        X(CK) = YK
        K = K-1
 200    CONTINUE
!
!  RETURN WITH IERR = NUMBER OF OFF-DIAGONAL NONZEROES IN U
!
      IERR = IU(N+1) - IU(1)
      RETURN
!
!  ERROR RETURNS
!
!  N = 0
!
 1001 IERR = 0
      RETURN
!
!  ROW K OF A IS NULL
!
 1002 IERR = -K
      RETURN
!
!  ROW K OF A HAS A DUPLICATE ENTRY
!
 1003 IERR = -(N+K)
      RETURN
!
!  ZERO PIVOT IN ROW K
!
 1004 IERR = -(2*N+K)
      RETURN
!
!  STORAGE FOR U EXCEEDED ON ROW K
!
 1005 IERR = -(3*N+K)
      RETURN
      END SUBROUTINE


      SUBROUTINE PREORD(N,IA,R,C,IC)
!
!  PREORD ORDERS THE ROWS OF A BY INCREASING NUMBER OF NONZEROES.
!  THE ROW PERMUTATION IS RETURNED IN R.  C IS SET TO THE IDENTITY.
!
      INTEGER IA(1),R(1),C(1),IC(1)
!
      DO 1 I=1,N
        R(I) = I
        C(I) = I
        IC(I) = I
1       CONTINUE
      DO 5 I = 1,N
5       C(I) = 0
      DO 10 K = 1,N
        KDEG = IA(K+1) - IA(K)
        IF (KDEG .EQ. 0) KDEG = KDEG + 1
        IC(K) = C(KDEG)
        C(KDEG) = K
10      CONTINUE
      I = 0
      DO 30 J = 1,N
        IF (C(J) .EQ. 0) GO TO 30
        K = C(J)
20      I = I + 1
        R(I) = K
        K = IC(K)
        IF (K .GT. 0) GO TO 20
30      CONTINUE
      DO 40 I = 1,N
        C(I) = I
        IC(I) = I
40      CONTINUE
      RETURN
      END SUBROUTINE

      SUBROUTINE RESCHK(N,IA,JA,A,B,X)
!
!  RESCHK COMPUTES THE MAX-NORM AND 2-NORM OF THE RESIDUAL.
!  DOUBLE PRECISION IS USED FOR THE COMPUTATION.
!
      INTEGER IA(1),JA(1)
      REAL(KIND_Rdp) :: A(1),B(1),X(1)
      DOUBLE PRECISION RESID,RESIDM,ROWSUM
      RESID = 0.
      RESIDM = 0.
      DO 20 I=1,N
        ROWSUM = DBLE(B(I))
        JMIN = IA(I)
        JMAX = IA(I+1) - 1
        DO 10 J=JMIN,JMAX
          JAJ = JA(J)
          ROWSUM = ROWSUM - DBLE(A(J)) * DBLE(X(JAJ))
10        CONTINUE
        IF (DABS(ROWSUM) .GT. RESIDM) RESIDM = DABS(ROWSUM)
        RESID = RESID + ROWSUM**2
20      CONTINUE
      RESID = DSQRT(RESID)
      WRITE (6,25) RESID
25    FORMAT (22H 2-NORM OF RESIDUAL = ,D14.7)
      WRITE (6,30) RESIDM
30    FORMAT(24H MAX NORM OF RESIDUAL = ,D14.7)
      RETURN
      END SUBROUTINE


      END MODULE
