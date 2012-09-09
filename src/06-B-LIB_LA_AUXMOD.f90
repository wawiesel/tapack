      MODULE LIB_LA_AUXMOD
      use LIB_LAPACK_BLAS, ONLY: ILAENV_F77 => ILAENV !!((03-A-LIB_LAPACK_BLAS.f))

      CONTAINS
!
      LOGICAL FUNCTION LSAME( CA, CB )
!
!!  PURPOSE
!!  =======
!
!!  LSAME  TESTS IF CA IS THE SAME LETTER AS CB REGARDLESS OF CASE.
!
!!  PARAMETERS
!!  ==========
!
!!  CA      (INPUT) CHARACTER*1
!!  CB      (INPUT) CHARACTER*1
!!          CHARACTERS TO BE COMPARED.
!
!!  .. SCALAR ARGUMENTS ..
      CHARACTER*1, INTENT(IN) :: CA, CB
!!  .. PARAMETERS ..
      INTEGER, PARAMETER      :: IOFF=32
!!  .. LOCAL SCALARS ..
      INTEGER                 :: INTA, INTB, ZCODE
!!  .. INTRINSIC FUNCTIONS ..
      INTRINSIC                  ICHAR
!
!!  .. EXECUTABLE STATEMENTS ..
!
!!  TEST IF THE CHARACTERS ARE EQUAL
!
      LSAME = CA == CB
!
!!  NOW TEST FOR EQUIVALENCE
!
      IF( .NOT.LSAME )THEN
!
!!     use "Z" RATHER THAN "A" SO THAT ASCII CAN BE DETECTED ON PRIME
!!     MACHINES, ON WHICH ICHAR RETURNS A VALUE WITH BIT 8 SET.
!!     ICHAR("A") ON PRIME MACHINES RETURNS 193 WHICH IS THE SAME AS
!!     ICHAR("A") ON AN EBCDIC MACHINE.
!
         ZCODE = ICHAR( "Z" )
!
         INTA = ICHAR( CA )
         INTB = ICHAR( CB )
!
         IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 )THEN
!
!!        ASCII IS ASSUMED - ZCODE IS THE ASCII CODE OF EITHER LOWER OR
!!        UPPER CASE "Z".
!
            IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
            IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
!
         ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 )THEN
!
!!        EBCDIC IS ASSUMED - ZCODE IS THE EBCDIC CODE OF EITHER LOWER OR
!!        UPPER CASE "Z".
!
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.                         &
!!    &       INTA.GE.145 .AND. INTA.LE.153 .OR.                         &
     &       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.                         &
     &       INTB.GE.145 .AND. INTB.LE.153 .OR.                         &
     &       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
!
         ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 )THEN
!
!!        ASCII IS ASSUMED, ON PRIME MACHINES - ZCODE IS THE ASCII CODE
!!        PLUS 128 OF EITHER LOWER OR UPPER CASE "Z".
!
            IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
         ENDIF
         LSAME = INTA == INTB
      ENDIF
      END FUNCTION LSAME


      SUBROUTINE ERINFO(LINFO, SRNAME, INFO, ISTAT)
!
!!  -- LAPACK95 interface driver routine (version 3.0) --
!!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!!     September, 2000
!
!!  .. IMPLICIT STATEMENT ..
         IMPLICIT NONE
!!  .. SCALAR ARGUMENTS ..
         CHARACTER( LEN = * ), INTENT(IN)              :: SRNAME
         INTEGER             , INTENT(IN)              :: LINFO
         INTEGER             , INTENT(OUT), OPTIONAL   :: INFO
         INTEGER             , INTENT(IN), OPTIONAL    :: ISTAT
!!  .. EXECUTABLE STATEMENTS ..
!!         IF( ( LINFO < 0 .AND. LINFO > -200 ) .OR.                     &
!!    &       ( LINFO > 0 .AND. .NOT.PRESENT(INFO) ) )THEN
      IF( ( ( LINFO < 0 .AND. LINFO > -200 ) .OR. LINFO > 0 )           &
     &           .AND. .NOT.PRESENT(INFO) )THEN
        WRITE (*,*) "Program terminated in LAPACK95 subroutine ",SRNAME
        WRITE (*,*) "Error indicator, INFO = ",LINFO
        IF( PRESENT(ISTAT) )THEN
          IF( ISTAT /= 0 ) THEN
            IF( LINFO == -100 )THEN
              WRITE (*,*) "The statement ALLOCATE caUSEs STATUS = ",    &
     &                    ISTAT
            ELSE
              WRITE (*,*) "LINFO = ", LINFO, " not expected"
            END IF
          END IF
        END IF
        STOP
         ELSE IF( LINFO <= -200 ) THEN
           WRITE(*,*) "++++++++++++++++++++++++++++++++++++++++++++++++"
           WRITE(*,*) "*** WARNING, INFO = ", LINFO, " WARNING ***"
           IF( LINFO == -200 )THEN
             WRITE(*,*)                                                 &
     &        "Could not allocate sufficient workspace for the optimum"
             WRITE(*,*)                                                 &
     &        "blocksize, hence the routine may not have performed as"
             WRITE(*,*) "efficiently as possible"
         ELSE
           WRITE(*,*) "Unexpected warning"
         END IF
           WRITE(*,*) "++++++++++++++++++++++++++++++++++++++++++++++++"
        END IF
        IF( PRESENT(INFO) ) THEN
          INFO = LINFO
        END IF
      END SUBROUTINE ERINFO


      INTEGER FUNCTION LA_WS_GELSS( VER, M, N, NRHS )
!
!!  -- LAPACK95 interface driver routine (version 3.0) --
!!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!!     September, 2000
!
!!     .. IMPLICIT STATEMENT ..
      IMPLICIT NONE
!!     .. SCALAR ARGUMENTS ..
      CHARACTER(LEN=1), INTENT(IN) :: VER
      INTEGER, INTENT(IN) :: M, N, NRHS
!!     .. PARAMETERS ..
      CHARACTER(LEN=5), PARAMETER :: NAME1="GELSS", NAME2="GEQRF", NAME3="ORMQR", NAME4="GEBRD", &
                                     NAME5="ORMBR", NAME6="ORGBR", NAME7="GELQF", NAME8="ORMLQ"
!!     .. LOCAL SCALARS ..
      INTEGER :: MNTHR, MINWRK, MAXWRK, MM, BDSPAC
!!     .. INTRINSIC FUNCTIONS ..
      INTRINSIC MAX
!!     .. EXECUTABLE STATEMENTS ..
      MNTHR = ILAENV_F77( 6, VER//NAME1, " ", M, N, NRHS, -1 )
!
!!     COMPUTE WORKSPACE
!!      (NOTE: COMMENTS IN THE CODE BEGINNING "Workspace:" DESCRIBE THE
!!       MINIMAL AMOUNT OF WORKSPACE NEEDED AT THAT POINT IN THE CODE,
!!       AS WELL AS THE PREFERRED AMOUNT FOR GOOD PERFORMANCE.
!!       NB REFERS TO THE OPTIMAL BLOCK SIZE FOR THE IMMEDIATELY
!!       FOLLOWING SUBROUTINE, AS RETURNED BY ILAENV.)
!
      MINWRK = 1
      MAXWRK = 0
      MM = M
      IF( M.GE.N .AND. M.GE.MNTHR ) THEN
!
!!        PATH 1A - OVERDETERMINED, WITH MANY MORE ROWS THAN COLUMNS
!
         MM = N
         MAXWRK = MAX(MAXWRK,N+N*ILAENV_F77(1,VER//NAME2," ",M,N,-1,-1))
         MAXWRK = MAX( MAXWRK, N+NRHS*                                  &
     &            ILAENV_F77( 1, VER//NAME3, "LT", M, NRHS, N, -1 ) )
      END IF
      IF( M.GE.N ) THEN
!
!!        PATH 1 - OVERDETERMINED OR EXACTLY DETERMINED
!
!!        COMPUTE WORKSPACE NEEDE FOR DBDSQR
!
         BDSPAC = MAX( 1, 5*N-4 )
         MAXWRK = MAX( MAXWRK, 3*N+( MM+N )*                            &
     &            ILAENV_F77( 1, VER//NAME4, " ", MM, N, -1, -1 ) )
         MAXWRK = MAX( MAXWRK, 3*N+NRHS*                                &
     &            ILAENV_F77( 1, VER//NAME5, "QLT", MM, NRHS, N, -1 ) )
         MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*                             &
     &            ILAENV_F77( 1, VER//NAME6, "P", N, N, N, -1 ) )
         MAXWRK = MAX( MAXWRK, BDSPAC )
         MAXWRK = MAX( MAXWRK, N*NRHS )
         MINWRK = MAX( 3*N+MM, 3*N+NRHS, BDSPAC )
         MAXWRK = MAX( MINWRK, MAXWRK )
      END IF
      IF( N.GT.M ) THEN
!
!!        COMPUTE WORKSPACE NEEDE FOR DBDSQR
!
         BDSPAC = MAX( 1, 5*M-4 )
         MINWRK = MAX( 3*M+NRHS, 3*M+N, BDSPAC )
         IF( N.GE.MNTHR ) THEN
!
!!           PATH 2A - UNDERDETERMINED, WITH MANY MORE COLUMNS
!!           THAN ROWS
!
            MAXWRK = M + M*ILAENV_F77( 1,VER//NAME7," ",M,N,-1,-1 )
            MAXWRK = MAX( MAXWRK, M*M+4*M+2*M*                          &
     &               ILAENV_F77( 1, VER//NAME4, " ", M, M, -1, -1 ) )
            MAXWRK = MAX( MAXWRK, M*M+4*M+NRHS*                         &
     &               ILAENV_F77( 1,VER//NAME5,"QLT",M,NRHS,M,-1 ) )
            MAXWRK = MAX( MAXWRK, M*M+4*M+( M-1 )*                      &
     &               ILAENV_F77( 1, VER//NAME6, "P", M, M, M, -1 ) )
            MAXWRK = MAX( MAXWRK, M*M+M+BDSPAC )
            IF( NRHS.GT.1 ) THEN
               MAXWRK = MAX( MAXWRK, M*M+M+M*NRHS )
            ELSE
               MAXWRK = MAX( MAXWRK, M*M+2*M )
            END IF
            MAXWRK = MAX( MAXWRK, M+NRHS*                               &
     &               ILAENV_F77( 1, VER//NAME8, "LT", N, NRHS, M, -1 ) )
         ELSE
!
!!           PATH 2 - UNDERDETERMINED
!
            MAXWRK = 3*M+(N+M)*ILAENV_F77(1,VER//NAME4," ",M,N,-1,-1)
            MAXWRK = MAX( MAXWRK, 3*M+NRHS*                             &
     &               ILAENV_F77( 1,VER//NAME5,"QLT",M,NRHS,M,-1 ) )
            MAXWRK = MAX( MAXWRK, 3*M+M*                                &
     &               ILAENV_F77( 1, VER//NAME6, "P", M, N, M, -1 ) )
            MAXWRK = MAX( MAXWRK, BDSPAC )
            MAXWRK = MAX( MAXWRK, N*NRHS )
         END IF
      END IF
      LA_WS_GELSS = MAX( MINWRK, MAXWRK )
      END FUNCTION LA_WS_GELSS


      INTEGER FUNCTION LA_WS_GELS( VER, M, N, NRHS, TRANS )
!
!!  -- LAPACK95 interface driver routine (version 3.0) --
!!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!!     September, 2000
!
!!     .. IMPLICIT STATEMENT ..
      IMPLICIT NONE
!!     .. SCALAR ARGUMENTS ..
      CHARACTER(LEN=1), INTENT(IN) :: TRANS, VER
      INTEGER, INTENT(IN) :: M, N, NRHS
!!     .. PARAMETERS ..
      CHARACTER(LEN=5), PARAMETER :: NAME1="GEQRF", NAME2="ORMQR", NAME3="ORMLQ", NAME4="GELQF"
!!     .. LOCAL SCALARS ..
      INTEGER :: NB, MN
      LOGICAL :: TPSD
!!     .. INTRINSIC FUNCTIONS ..
      INTRINSIC MAX, MIN
!!     .. EXECUTABLE STATEMENTS ..
      MN = MIN( M, N )
      IF( LSAME( TRANS, "N" ) )THEN
        TPSD = .FALSE.
      ELSE
        TPSD = .TRUE.
      ENDIF
      IF( M.GE.N ) THEN
        NB = ILAENV_F77( 1, VER//NAME1, " ", M, N, -1, -1 )
        IF( TPSD ) THEN
          NB = MAX( NB,ILAENV_F77( 1,VER//NAME2,"LN",M,NRHS,N,-1 ) )
        ELSE
          NB = MAX( NB, ILAENV_F77( 1,VER//NAME2,"LT",M,NRHS,N,-1 ) )
        END IF
      ELSE
        NB = ILAENV_F77( 1, VER//NAME4, " ", M, N, -1, -1 )
        IF( TPSD ) THEN
          NB = MAX( NB, ILAENV_F77( 1,VER//NAME3,"LT",N,NRHS,M,-1 ) )
        ELSE
          NB = MAX( NB, ILAENV_F77( 1,VER//NAME3,"LN",N,NRHS,M,-1 ) )
        END IF
      END IF
      LA_WS_GELS = MN + MAX( M, N, NRHS )*NB
      END FUNCTION LA_WS_GELS



      END MODULE LIB_LA_AUXMOD

