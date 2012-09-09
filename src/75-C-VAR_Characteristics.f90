!!# MODULE <<VAR_Characteristcs>>
MODULE VAR_Characteristics

!!## PURPOSE
!! Store variables  <USR_Characteristics> module should use.


!!## EXTERNAL
USE KND_Characteristics !!((03-C-KND_Characteristics.f90))
USE SUB_SAVEn           !!((06-B-SUB_SAVEn.f90))
USE SUB_LOADn           !!((06-B-SUB_LOADn.f90))
USE FUN_NewFile         !!((05-B-FUN_NewFile.f90))
USE FUN_OldFile         !!((06-C-FUN_OldFile.f90))
USE SUB_CLEARn          !!((04-A-SUB_CLEARn.f90))
USE VAR_Units           !!((03-A-VAR_Units.f90))
USE FUN_EXIST           !!((04-B-FUN_EXIST.f90))
USE USR_SparseArray     !!((45-C-USR_SparseArray.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## PUBLIC ACCESS LIST
PUBLIC

LOGICAL,PARAMETER :: NaiveAlloc=.FALSE.
LOGICAL,PARAMETER :: using_refinement = .FALSE.

LOGICAL :: Using_1DCachedArrays = .FALSE.

INTEGER :: Nd = 0
INTEGER :: Nq = 0
INTEGER :: Ns = 0
INTEGER :: Nc = 0
INTEGER :: c  = 0
INTEGER :: CharacteristicSourceOrder = 2

REAL(KIND_qmc),POINTER :: r0   (:) => NULL()
REAL(KIND_qmc),POINTER :: Omega(:) => NULL()
REAL(KIND_qmc)         :: s01      =  1.d0
REAL(KIND_qmc)         :: psi0     =  0.d0

REAL(KIND_qmc) :: DEFAULT_tol       = 1.d-10
INTEGER        :: DEFAULT_Ncoarse   = 0
INTEGER        :: DEFAULT_Ncollapse = 10
INTEGER        :: DEFAULT_Nsamples  = 10

REAL(KIND_qmc),POINTER :: sQ   (:)=>NULL()
INTEGER       ,POINTER :: iQ   (:)=>NULL()
REAL(KIND_qmc),POINTER ::  Q   (:)=>NULL()
REAL(KIND_qmc),POINTER ::  Q1  (:)=>NULL()

REAL(KIND_qmc),POINTER :: ssigma(:)=>NULL()
REAL(KIND_qmc),POINTER ::  sigma(:)=>NULL()
INTEGER       ,POINTER :: isigma(:)=>NULL()

REAL(KIND_qmc),POINTER:: g(:)=>NULL()
REAL(KIND_qmc),POINTER:: h(:)=>NULL()

REAL(KIND_qmc) :: t1,t2

!! * two-dimensional arrays for saving the cells traversed for the source
!!   (iQ), the distances of each traversal (sQ), the cells traversed for the
!!   total cross section (isigma), and the distances of each traversal (ssigma)

INTEGER       ,POINTER :: Cached_iQ(:,:)    =>NULL()
REAL(KIND_qmc),POINTER :: Cached_sQ(:,:)    =>NULL()
REAL(KIND_qmc),POINTER :: Cached_ssigma(:,:)=>NULL()
INTEGER       ,POINTER :: Cached_isigma(:,:)=>NULL()

!! * one-dimensional arrays which correspond to the above (saves memory)
INTEGER       ,POINTER :: Cached_1Q(:)     =>NULL()
INTEGER       ,POINTER :: Cached_1sigma(:) =>NULL()
INTEGER       ,POINTER :: Cached_1iQ(:)    =>NULL()
REAL(KIND_qmc),POINTER :: Cached_1sQ(:)    =>NULL()
REAL(KIND_qmc),POINTER :: Cached_1ssigma(:)=>NULL()
INTEGER       ,POINTER :: Cached_1isigma(:)=>NULL()

!! * cached locations in phase space (r0 and Omega), total distance to
!!   desired vertex (s01) and boundary conditions (psi0) at (r0,Omega)
REAL(KIND_qmc),POINTER :: Cached_r0(:,:)    =>NULL()
REAL(KIND_qmc),POINTER :: Cached_Omega(:,:) =>NULL()
REAL(KIND_qmc),POINTER :: Cached_s01(:)     =>NULL()
REAL(KIND_qmc),POINTER :: Cached_psi0(:)    =>NULL()

INTERFACE IndividualTest1
 MODULE PROCEDURE IndividualTest1_I
 MODULE PROCEDURE IndividualTest1_R
END INTERFACE

INTERFACE IndividualTest2
 MODULE PROCEDURE IndividualTest2_I
 MODULE PROCEDURE IndividualTest2_R
END INTERFACE

CONTAINS

SUBROUTINE WRITE_P1I4(Unit,P1I4)
INTEGER,INTENT(IN) :: Unit
INTEGER,POINTER :: P1I4(:)
!!--begin--
IF( ASSOCIATED(P1I4) )THEN
 WRITE(Unit,*)SIZE(P1I4)
 WRITE(Unit,*)P1I4
ELSE
 WRITE(Unit,*)0
END IF
!!--end--
END SUBROUTINE

SUBROUTINE READ_P1I4(Unit,P1I4)
INTEGER,INTENT(IN) :: Unit
INTEGER,POINTER :: P1I4(:)
INTEGER :: N
!!--begin--
CALL CLEARn(P1I4)
READ(Unit,*)N
IF( N>0 )THEN
 ALLOCATE( P1I4(N) )
 READ(Unit,*)P1I4
END IF
!!--end--
END SUBROUTINE

SUBROUTINE WRITE_P1Rdp(Unit,P1Rdp)
INTEGER,INTENT(IN) :: Unit
REAL(KIND_qmc),POINTER :: P1Rdp(:)
!!--begin--
IF( ASSOCIATED(P1Rdp) )THEN
 WRITE(Unit,*)SIZE(P1Rdp)
 WRITE(Unit,*)P1Rdp
ELSE
 WRITE(Unit,*)0
END IF
!!--end--
END SUBROUTINE

SUBROUTINE READ_P1Rdp(Unit,P1Rdp)
INTEGER,INTENT(IN) :: Unit
REAL(KIND_qmc),POINTER :: P1Rdp(:)
INTEGER :: N
!!--begin--
CALL CLEARn(P1Rdp)
READ(Unit,*)N
IF( N>0 )THEN
 ALLOCATE( P1Rdp(N) )
 READ(Unit,*)P1Rdp
END IF
!!--end--
END SUBROUTINE


SUBROUTINE SAVE_Characteristics(NAME)
CHARACTER(*),INTENT(IN) :: NAME
INTEGER :: Unit
INTEGER,POINTER :: old(:)
!!--begin--

!finalize
IF( Using_1DCachedArrays )THEN

 NULLIFY(old)
 !need this old array because Cached_1sigma will be changed in
 !first call to FINALIZE_SPARSEARRAY
 ALLOCATE( old(SIZE(Cached_1sigma)) )
 old = Cached_1sigma
 CALL FINALIZE_SPARSEARRAY(Cached_1ssigma,Cached_1sigma)
 CALL FINALIZE_SPARSEARRAY(Cached_1isigma,old)
 DEALLOCATE( old )

 ALLOCATE( old(SIZE(Cached_1Q)) )
 old = Cached_1Q
 CALL FINALIZE_SPARSEARRAY(Cached_1sQ,Cached_1Q)
 CALL FINALIZE_SPARSEARRAY(Cached_1iQ,old)
 DEALLOCATE( old )
 NULLIFY( old )

ELSE
 CALL FINALIZE_DENSEARRAY(Cached_ssigma)
 CALL FINALIZE_DENSEARRAY(Cached_sQ)
 CALL FINALIZE_DENSEARRAY(Cached_isigma)
 CALL FINALIZE_DENSEARRAY(Cached_iQ)
END IF

Unit = NewFile(NAME//".Characteristics.dat")
WRITE(Unit,*)Using_1DCachedArrays
WRITE(Unit,*)Nd
WRITE(Unit,*)Nq
WRITE(Unit,*)Ns
WRITE(Unit,*)Nc
!CALL WRITE_P1Rdp(Unit,sQ)
!CALL WRITE_P1Rdp(Unit,Q)
!CALL WRITE_P1Rdp(Unit,ssigma)
!CALL WRITE_P1Rdp(Unit,sigma)
!CALL WRITE_P1Rdp(Unit,g)
!CALL WRITE_P1Rdp(Unit,h)
CLOSE(Unit)

CALL SAVEn(NAME//".Cached_iQ",Cached_iQ)
CALL SAVEn(NAME//".Cached_sQ",Cached_sQ)
CALL SAVEn(NAME//".Cached_ssigma",Cached_ssigma)
CALL SAVEn(NAME//".Cached_isigma",Cached_isigma)
CALL SAVEn(NAME//".Cached_1Q",Cached_1Q)
CALL SAVEn(NAME//".Cached_1sigma",Cached_1sigma)
CALL SAVEn(NAME//".Cached_1iQ",Cached_1iQ)
CALL SAVEn(NAME//".Cached_1sQ",Cached_1sQ)
CALL SAVEn(NAME//".Cached_1ssigma",Cached_1ssigma)
CALL SAVEn(NAME//".Cached_1isigma",Cached_1isigma)
CALL SAVEn(NAME//".Cached_r0",Cached_r0)
CALL SAVEn(NAME//".Cached_Omega",Cached_Omega)
CALL SAVEn(NAME//".Cached_s01",Cached_s01)
CALL SAVEn(NAME//".Cached_psi0",Cached_psi0)
!!--end--
END SUBROUTINE

SUBROUTINE LOAD_Characteristics(NAME)
CHARACTER(*),INTENT(IN) :: NAME
INTEGER :: Unit
!!--begin--

Unit = OldFile(NAME//".Characteristics.dat")
READ(Unit,*)Using_1DCachedArrays
READ(Unit,*)Nd
READ(Unit,*)Nq
READ(Unit,*)Ns
READ(Unit,*)Nc
!CALL READ_P1Rdp(Unit,sQ)
!CALL READ_P1Rdp(Unit,Q)
!CALL READ_P1Rdp(Unit,ssigma)
!CALL READ_P1Rdp(Unit,sigma)
!CALL READ_P1Rdp(Unit,g)
!CALL READ_P1Rdp(Unit,h)
CLOSE(Unit)

CALL LOADn(NAME//".Cached_iQ",Cached_iQ)
CALL LOADn(NAME//".Cached_sQ",Cached_sQ)
CALL LOADn(NAME//".Cached_ssigma",Cached_ssigma)
CALL LOADn(NAME//".Cached_isigma",Cached_isigma)
CALL LOADn(NAME//".Cached_1Q",Cached_1Q)
CALL LOADn(NAME//".Cached_1sigma",Cached_1sigma)
CALL LOADn(NAME//".Cached_1iQ",Cached_1iQ)
CALL LOADn(NAME//".Cached_1sQ",Cached_1sQ)
CALL LOADn(NAME//".Cached_1ssigma",Cached_1ssigma)
CALL LOADn(NAME//".Cached_1isigma",Cached_1isigma)
CALL LOADn(NAME//".Cached_r0",Cached_r0)
CALL LOADn(NAME//".Cached_Omega",Cached_Omega)
CALL LOADn(NAME//".Cached_s01",Cached_s01)
CALL LOADn(NAME//".Cached_psi0",Cached_psi0)
!!--end--
END SUBROUTINE


FUNCTION EXIST_Characteristics(NAME) RESULT(EX)
CHARACTER(*),INTENT(IN) :: NAME
INTEGER :: Unit
LOGICAL :: EX

!!--begin--
EX = .TRUE.
EX = EX .AND.EXIST(NAME//".Characteristics.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_iQ.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_sQ.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_ssigma.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_isigma.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_1Q.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_1sigma.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_1iQ.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_1sQ.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_1ssigma.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_1isigma.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_r0.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_Omega.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_s01.dat")
IF( .NOT.EX )RETURN
EX = EX .AND.EXIST(NAME//".Cached_psi0.dat")
IF( .NOT.EX )RETURN
!!--end--
END FUNCTION


FUNCTION TEST_Characteristics(NAME,Unit_noisy) RESULT(PASS)
!!#### PURPOSE
!! Check the current saved characteristics to see if they
!! match what is saved in files.

CHARACTER(*),INTENT(IN) :: NAME
INTEGER,INTENT(IN),OPTIONAL :: Unit_noisy
LOGICAL :: PASS

INTEGER       ,POINTER :: Cached_iQ0(:,:)
REAL(KIND_qmc),POINTER :: Cached_sQ0(:,:)
REAL(KIND_qmc),POINTER :: Cached_ssigma0(:,:)
INTEGER       ,POINTER :: Cached_isigma0(:,:)
INTEGER       ,POINTER :: Cached_1Q0(:)
INTEGER       ,POINTER :: Cached_1sigma0(:)
INTEGER       ,POINTER :: Cached_1iQ0(:)
REAL(KIND_qmc),POINTER :: Cached_1sQ0(:)
REAL(KIND_qmc),POINTER :: Cached_1ssigma0(:)
INTEGER       ,POINTER :: Cached_1isigma0(:)
REAL(KIND_qmc),POINTER :: Cached_r00(:,:)
REAL(KIND_qmc),POINTER :: Cached_Omega0(:,:)
REAL(KIND_qmc),POINTER :: Cached_s010(:)
REAL(KIND_qmc),POINTER :: Cached_psi00(:)
LOGICAL :: Using_1DCachedArrays0
INTEGER :: Nd0
INTEGER :: Nq0
INTEGER :: Ns0
INTEGER :: Nc0
INTEGER :: Unit
LOGICAL :: test_iQ, test_sQ, test_ssigma, test_isigma
LOGICAL :: test_1Q, test_1sigma, test_1iQ, test_1sQ
LOGICAL :: test_1ssigma, test_1isigma, test_r0
LOGICAL :: test_Omega, test_s01, test_psi0
INTEGER :: i,j,Unit_noisy_,jerr

!!--begin--
IF( .NOT.PRESENT(Unit_noisy) )THEN
 Unit_noisy_ = DEFAULT_OUTPUT_UNIT
ELSE
 Unit_noisy_ = Unit_noisy
END IF

PASS = .TRUE.

Unit = OldFile(NAME//".Characteristics.dat")
READ(Unit,*)Using_1DCachedArrays0
READ(Unit,*)Nd0
READ(Unit,*)Nq0
READ(Unit,*)Ns0
READ(Unit,*)Nc0
CLOSE(Unit)

IF( Unit_Noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST 1DCachedArrays: "//MERGE("PASS","FAIL",Using_1DCachedArrays0.AND.Using_1DCachedArrays)
 WRITE(Unit_noisy_,*)"TEST Nd            : "//MERGE("PASS","FAIL",Nd0==Nd)
 WRITE(Unit_noisy_,*)"TEST Nq            : "//MERGE("PASS","FAIL",Nq0==Nq)
 WRITE(Unit_noisy_,*)"TEST Ns            : "//MERGE("PASS","FAIL",Ns0==Ns)
 WRITE(Unit_noisy_,*)"TEST Nc            : "//MERGE("PASS","FAIL",Nc0==Nc)
END IF


CALL LOADn(NAME//".Cached_iQ",Cached_iQ0)
test_iQ = ALL(Cached_iQ0==Cached_iQ)
PASS = test_iQ.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_iQ     : "//MERGE("PASS","FAIL",test_iQ)
END IF
CALL IndividualTest2(test_iQ,Cached_iQ0,Cached_iQ,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_sQ",Cached_sQ0)
test_sQ = ALL(Cached_sQ0==Cached_sQ)
PASS = test_sQ.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_sQ     : "//MERGE("PASS","FAIL",test_sQ)
END IF
CALL IndividualTest2(test_sQ,Cached_sQ0,Cached_sQ,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_ssigma",Cached_ssigma0)
test_ssigma = ALL(Cached_ssigma0==Cached_ssigma)
PASS = test_ssigma.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_ssigma : "//MERGE("PASS","FAIL",test_ssigma)
END IF
CALL IndividualTest2(test_ssigma,Cached_ssigma0,Cached_ssigma,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_isigma",Cached_isigma0)
test_isigma = ALL(Cached_isigma0==Cached_isigma)
PASS = test_isigma.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_isigma : "//MERGE("PASS","FAIL",test_isigma)
END IF
CALL IndividualTest2(test_isigma,Cached_isigma0,Cached_isigma,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_1Q",Cached_1Q0)
test_1Q = ALL(Cached_1Q0==Cached_1Q)
PASS = test_1Q.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_1Q     : "//MERGE("PASS","FAIL",test_1Q)
END IF
CALL IndividualTest1(test_1Q,Cached_1Q0,Cached_1Q,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_1sigma",Cached_1sigma0)
test_1sigma = ALL(Cached_1sigma0==Cached_1sigma)
PASS = test_1sigma.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_1sigma : "//MERGE("PASS","FAIL",test_1sigma)
END IF
CALL IndividualTest1(test_1sigma,Cached_1sigma0,Cached_1sigma,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_1iQ",Cached_1iQ0)
test_1iQ = ALL(Cached_1iQ0==Cached_1iQ)
PASS = test_1iQ.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_1iQ    : "//MERGE("PASS","FAIL",test_1iQ)
END IF
CALL IndividualTest1(test_1iQ,Cached_1iQ0,Cached_1iQ,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_1sQ",Cached_1sQ0)
test_1sQ = ALL(Cached_1sQ0==Cached_1sQ)
PASS = test_1sQ.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_1sQ    : "//MERGE("PASS","FAIL",test_1sQ)
END IF
CALL IndividualTest1(test_1sQ,Cached_1sQ0,Cached_1sQ,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_1ssigma",Cached_1ssigma0)
test_1ssigma = ALL(Cached_1ssigma0==Cached_1ssigma)
PASS = test_1ssigma.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_1ssigma: "//MERGE("PASS","FAIL",test_1ssigma)
END IF
CALL IndividualTest1(test_1ssigma,Cached_1ssigma0,Cached_1ssigma,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_1isigma",Cached_1isigma0)
test_1isigma = ALL(Cached_1isigma0==Cached_1isigma)
PASS = test_1isigma.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_1isigma: "//MERGE("PASS","FAIL",test_1isigma)
END IF
CALL IndividualTest1(test_1isigma,Cached_1isigma0,Cached_1isigma,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_r0",Cached_r00)
test_r0 = ALL(Cached_r00==Cached_r0)
PASS = test_r0.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_r0     : "//MERGE("PASS","FAIL",test_r0)
END IF
CALL IndividualTest2(test_r0,Cached_r0,Cached_r00,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_Omega",Cached_Omega0)
test_Omega = ALL(Cached_Omega0==Cached_Omega)
PASS = test_Omega.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_Omega  : "//MERGE("PASS","FAIL",test_Omega)
END IF
CALL IndividualTest2(test_Omega,Cached_Omega0,Cached_Omega,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_s01",Cached_s010)
test_s01 = ALL(Cached_s010==Cached_s01)
PASS = test_s01.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_s01    : "//MERGE("PASS","FAIL",test_s01)
END IF
CALL IndividualTest1(test_s01,Cached_s010,Cached_s01,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


CALL LOADn(NAME//".Cached_psi0",Cached_psi00)
test_psi0 = ALL(Cached_psi00==Cached_psi0)
PASS = test_psi0.AND.PASS
IF( Unit_noisy_/=0 )THEN
 WRITE(Unit_noisy_,*)"TEST Cached_psi0   : "//MERGE("PASS","FAIL",test_psi0)
END IF
CALL IndividualTest1(test_s01,Cached_psi00,Cached_psi0,Unit_noisy_)
IF( .NOT.PASS )GOTO 33


33 CONTINUE

DEALLOCATE( Cached_iQ0,Cached_sQ0,Cached_ssigma0,Cached_isigma0,&
            Cached_1Q0,Cached_1sigma0,Cached_1iQ0,Cached_1sQ0,&
            Cached_1ssigma0,Cached_1isigma0,Cached_r00,&
            Cached_Omega0,Cached_s010,Cached_psi00 , STAT=jerr )

!!--end--
END FUNCTION


SUBROUTINE IndividualTest2_R(test,a,a0,Unit_noisy)
LOGICAL,INTENT(IN) :: test
REAL(KIND_qmc),INTENT(IN) :: a(:,:),a0(:,:)
INTEGER,INTENT(IN) :: Unit_noisy
INTEGER :: i,j
!!--begin--
IF( .NOT.test .AND. Unit_noisy/=0 )THEN
 DO i=1,SIZE(a,2)
  DO j=1,SIZE(a,1)
   IF( a(j,i)/=a0(j,i) )THEN
    WRITE(Unit_noisy,"(2E26.16)")a(j,i),a0(j,i)
   END IF
  END DO
 END DO
END IF
!!--end--
END SUBROUTINE



SUBROUTINE IndividualTest1_R(test,a,a0,Unit_noisy)
LOGICAL,INTENT(IN) :: test
REAL(KIND_qmc),INTENT(IN) :: a(:),a0(:)
INTEGER,INTENT(IN) :: Unit_noisy
INTEGER :: i,j
!!--begin--
IF( .NOT.test .AND. Unit_noisy/=0 )THEN
 DO j=1,SIZE(a,1)
  IF( a(j)/=a0(j) )THEN
   WRITE(Unit_noisy,"(2E26.16)")a(j),a0(j)
  END IF
 END DO
END IF
!!--end--
END SUBROUTINE



SUBROUTINE IndividualTest2_I(test,a,a0,Unit_noisy)
LOGICAL,INTENT(IN) :: test
INTEGER,INTENT(IN) :: a(:,:),a0(:,:)
INTEGER,INTENT(IN) :: Unit_noisy
INTEGER :: i,j
!!--begin--
IF( .NOT.test .AND. Unit_noisy/=0 )THEN
 DO i=1,SIZE(a,2)
  DO j=1,SIZE(a,1)
   IF( a(j,i)/=a0(j,i) )THEN
    WRITE(Unit_noisy,"(2E26.16)")a(j,i),a0(j,i)
   END IF
  END DO
 END DO
END IF
!!--end--
END SUBROUTINE



SUBROUTINE IndividualTest1_I(test,a,a0,Unit_noisy)
LOGICAL,INTENT(IN) :: test
INTEGER,INTENT(IN) :: a(:),a0(:)
INTEGER,INTENT(IN) :: Unit_noisy
INTEGER :: i,j
!!--begin--
IF( .NOT.test .AND. Unit_noisy/=0 )THEN
 DO j=1,SIZE(a,1)
  IF( a(j)/=a0(j) )THEN
   WRITE(Unit_noisy,"(2E26.16)")a(j),a0(j)
  END IF
 END DO
END IF
!!--end--
END SUBROUTINE


END MODULE

