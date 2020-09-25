!!# MODULE <<USR_QuasiDiffusion>>
MODULE USR_QuasiDiffusion

!!## PURPOSE
!! The user-module for the quasidiffusion routines.

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes   ,ONLY: KIND_S             !!((01-A-KND_IntrinsicTypes.f90))
USE KND_QuasiDiffusion   ,ONLY: KIND_QDF           !!((02-A-KND_QuasiDiffusion.f90))
USE KND_DiscreteOrdinates,ONLY: KIND_DOR           !!((02-A-KND_DiscreteOrdinates.f90))

!!## EXTERNAL PARAMETERS
USE PAR_QuasiDiffusion                             !!((05-C-PAR_QuasiDiffusion.f90))

!!## GLOBAL ASSIGNMENTS
USE ASN_IntrinsicS                                 !!((05-B-ASN_IntrinsicS.f90))

!!## EXTERNAL PROCEDURES
USE SUB_CLEAR                                      !!((04-A-SUB_CLEAR.f90))
USE SUB_Pause                                      !!((04-B-SUB_Pause.f90))
USE SUB_Reallocate                                 !!((04-B-SUB_Reallocate.f90))
USE SUB_SAVE                                       !!((06-B-SUB_SAVE.f90))
USE SUB_Stop                                       !!((04-B-SUB_Stop.f90))
USE FUN_Default                                    !!((04-A-FUN_Default.f90))
USE FUN_EQUILOC                                    !!((03-A-FUN_EQUILOC.f90))
USE FUN_Error                                      !!((04-A-FUN_Error.f90))
USE FUN_NewFile                                    !!((05-B-FUN_NewFile.f90))
USE FUN_STR                                        !!((05-B-FUN_STR.f90))
USE FUN_VSTR                                       !!((05-B-FUN_VSTR.f90))
USE FUN_Substitute                                 !!((06-C-FUN_Substitute.f90))
USE FUN_IsError                                    !!((05-A-FUN_IsError.f90))
USE FUN_STRTIME                                    !!((06-C-FUN_STRTIME.f90))
USE FUN_Reorder                                    !!((05-A-FUN_Reorder.f90))
USE FUN_xySAREA                                    !!((03-A-FUN_xySAREA.f90))

!!## GLOBAL PRINTING SUBROUTINES
USE PRN_Text                                       !!((07-B-PRN_Text.f90))
USE PRN_Matrix_dmat                                !!((09-B-PRN_Matrix_dmat.f90))
USE PRN_MatrixSymbolic_dmat                        !!((03-A-PRN_MatrixSymbolic_dmat.f90))
USE PRN_Grid2                                      !!((09-B-PRN_Grid2.f90))
USE PRN_Table                                      !!((11-B-PRN_Table.f90))

!!## FORTRAN STANDARDS
USE ISO_varying_string                             !!((03-A-ISO_varying_string.f90))

!!## USER MODULES
!! * feedback user module
USE USR_fdbk                                       !!((08-C-USR_fdbk.f90))

!!## GLOBAL TOOLBOXES
!! * SMLib sparse matrix computation
USE TBX_SMlib                                      !!((22-B-TBX_SMlib.f90))
USE INT_LAPACK1                                    !!((07-B-INT_LAPACK1.f90))
USE INT_LAPACK2                                    !!((08-B-INT_LAPACK2.f90))
USE LIB_SPARSKIT_blassm,ONLY: roscal,amudia,coscal !!((05-A-LIB_SPARSKIT_blassm.f))
USE LIB_SPARSKIT_matvec                            !!((03-A-LIB_SPARSKIT_matvec.f))
USE LIB_SPARSKIT_ilut,ONLY: lusol                  !!((06-A-LIB_SPARSKIT_ilut.f))
USE LIB_SPARSKIT_inout,ONLY: pspltm                !!((03-A-LIB_SPARSKIT_inout.f))

!!## GLOBAL USER MODULES
USE USR_DiscreteOrdinates                          !!((34-B-USR_DiscreteOrdinates.f90))


!!## GLOBAL LIBRARIES
USE LIB_Prompts                                    !!((06-B-LIB_Prompts.f90))
USE LIB_GenericPhrases                             !!((07-B-LIB_GenericPhrases.f90))
USE LIB_Norm                                       !!((04-B-LIB_Norm.f90))
USE LIB_genMoments                                 !!((13-B-LIB_genMoments.f90))

!!## GLOBAL TOOLBOXES
USE TBX_Mesh                                       !!((15-B-TBX_Mesh.f90))
![novis]!USE PLT_Mesh                                                 !!((15-C-PLT_Mesh.f90))

!!## GLOBAL VARIABLES
USE VAR_Units                                      !!((03-A-VAR_Units.f90))
USE VAR_QuasiDiffusion,ONLY: SPARSKIT_lfil,&       !!((46-B-VAR_QuasiDiffusion.f90))
  SPARSKIT_droptol,SPARSKIT_niwk,&
  SPARSKIT_nkryss,SPARSKIT_maxmatvec,&
  Using_AutoIterFix,StandardScaling,PlotSparsityPattern,&
  PlotPrecSparsityPattern,EvaluateConditionNumber,&
  Use_RowScaling,Use_ColScaling,rownorm,colnorm,PrecSpec
USE USR_QDAnalyticTest                             !!((47-C-USR_QDAnalyticTest.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## LOCAL KINDS
INTEGER,PARAMETER :: KIND_AngularFlux = KIND_QDF
INTEGER,PARAMETER :: KIND_ScalarFlux  = KIND_QDF
INTEGER,PARAMETER :: KIND_Quadrature  = KIND_QDF

!!## DEFAULT ACCESS
PRIVATE

!!## PUBLIC ACCESS LIST
PUBLIC :: OUTPUT_QDF
PUBLIC :: MSR_to_CSR
PUBLIC :: NUM_InterfaceEquations
PUBLIC :: MAX_Subfaces
PUBLIC :: EVAL_HalfCellVolumes_Qd
PUBLIC :: BlockOrdering
PUBLIC :: GET_IsMaster
PUBLIC :: SETUP_QDF_MSR
PUBLIC :: SOLVE_NSPIV
PUBLIC :: SOLVE_LAPACK_DENSE
PUBLIC :: SOLVE_SMLIB_BANDED
PUBLIC :: SOLVE_BICGSTAB2
PUBLIC :: SOLVE_SPARSKIT
PUBLIC :: SOLVE_SMLIB_BICGSTAB
PUBLIC :: SOLVE_SMLIB_GMRES
PUBLIC :: SAVERESIDUALINFO
PUBLIC :: ALLOCATE_QDF
PUBLIC :: ESmooth

!!## MODULE PROCEDURES
CONTAINS




SUBROUTINE OUTPUT_QDF( OutputFileBase , iter , A , x , b )
!!#### PURPOSE
!! Output the QDF matrix, solution, and right-hand side.

USE USR_SMlib_MSR                                  !!((17-B-USR_SMlib_MSR.f90))

!!#### REQUIRED INPUT
CHARACTER(*,KIND=KIND_S),INTENT(IN)    :: OutputFileBase
INTEGER                 ,INTENT(IN)    :: iter
TYPE(MSR)               ,INTENT(INOUT) :: A
REAL(KIND_QDF)          ,INTENT(IN)    :: x(:)
REAL(KIND_QDF)          ,INTENT(IN)    :: b(:)

!!#### LOCAL VARIABLES
REAL(KIND_QDF) :: val
INTEGER       :: nrow,ncol
INTEGER       :: Unit_
TYPE(varying_string) :: File
REAL(KIND_QDF),ALLOCATABLE :: A2(:,:)
INTEGER :: N,INFO
REAL(KIND_QDF) :: RCOND,ANORM
INTEGER,ALLOCATABLE :: IWORK(:),IPIV(:)
REAL(KIND_QDF),ALLOCATABLE :: WORK(:)
LOGICAL,PARAMETER :: AllowDense = .FALSE.

!!--begin--
IF( .NOT.Is_ok(A) )THEN
 STOP "QDF"
ELSE
 !1. Print matrix in coordinate format.
 file = Substitute( "A_%p" , (/"%p"/) , (/iter/) )
 file = TRIM(STR(file))//".dat" 
 Unit_ = NewFile( TRIM(STR(file)) , STATUS="Replace" )
 DO nrow=1,SIZE(x)
  DO ncol=1,SIZE(x)
   val = entry(A,nrow,ncol)
   IF( val/=0 )WRITE(Unit_,*)nrow,ncol,val
  END DO
 END DO
 CLOSE(Unit_)

 IF( AllowDense )THEN

  !1.b. Print matrix in dense format.
  ALLOCATE( A2(1:A%N,1:A%N) )
  A2 = A

  file = TRIM(OutputFileBase)//"__Aval_%p"
  file = Substitute( TRIM(STR(file)) , (/"%p"/) , (/iter/) )
  file = TRIM(STR(file))//".txt"
  Unit_ = NewFile( TRIM(STR(file)) , STATUS="Replace" )

  CALL Print_Matrix_dmat(A2,Unit=Unit_,FMT="e9.1")
  CLOSE(Unit_)

  file = TRIM(OutputFileBase)//"__Asym_%p"
  file = Substitute( TRIM(STR(file)) , (/"%p"/) , (/iter/) )
  file = TRIM(STR(file))//".txt" 
  Unit_ = NewFile( TRIM(STR(file)) , STATUS="Replace" )

  CALL Print_MatrixSymbolic_dmat(A2,Unit=Unit_,zero=" ")
  CLOSE(Unit_)

  !2. Print condition number.
  N = A%N
  ALLOCATE(IPIV(N),IWORK(N),WORK(4*N))
  !! get the value of the norm of the matrix
  ANORM = LA_LANGE( '1', N, N, A2 , N, WORK )
  !! do lu factorization
  CALL LA_GETRF( N, N, A2, N, IPIV, INFO )
  !! get condition number
  CALL LA_GECON( '1', N, A2, N, ANORM, RCOND, WORK, IWORK, INFO )

  file = TRIM(OutputFileBase)//"__Acond_%p"
  file = Substitute( TRIM(STR(file)) , (/"%p"/) , (/iter/) )
  file = TRIM(STR(file))//".txt"
  Unit_ = NewFile( TRIM(STR(file)) , STATUS="Replace" , IfOpened="R" )
  write(unit_,"(i6.3,1x,i10.7,1x,e12.5)")iter,N,1._KIND_QDF/RCOND

  DEALLOCATE(IPIV,A2,IWORK,WORK)

 END IF

 !3. Print solution (.dat appended automatically with Save).
 file = Substitute( "x_%p" , (/"%p"/) , (/iter-1/) )
 CALL Save( TRIM(STR(file)) , x )

 !4. Print rhs (.dat appended automatically with Save).
 file = Substitute( "b_%p" , (/"%p"/) , (/iter/) )
 CALL Save( TRIM(STR(file)) , b )


END IF

!clear
CALL CLEAR(file)

!!--end--
END SUBROUTINE



SUBROUTINE SOLVE_SMLIB_banded( A , b , x , fdbk )
!!#### PURPOSE
!! Solve the QDF matrix using SMLIB.


!! * SMLib sparse matrix computation
USE USR_SMlib_Matrix_Arithmetic                    !!((19-B-USR_SMlib_Matrix_Arithmetic.f90))
USE USR_SMlib_ILU                                  !!((20-B-USR_SMlib_ILU.f90))
USE USR_SMlib_CGS                                  !!((20-B-USR_SMlib_CGS.f90))
USE USR_SMlib_Band_LU                              !!((17-B-USR_SMlib_Band_LU.f90))
USE USR_SMlib_Band_Gauss_Solver                    !!((20-B-USR_SMlib_Band_Gauss_Solver.f90))
USE USR_SMlib_GMRES                                !!((20-B-USR_SMlib_GMRES.f90))
USE USR_SMlib_BiCGSTAB                             !!((20-B-USR_SMlib_BiCGSTAB.f90))

!!#### REQUIRED INPUT
TYPE(MSR)     ,INTENT(IN) :: A
REAL(KIND_QDF),INTENT(IN) :: b(:)

!!#### REQUIRED OUTPUT
REAL(KIND_QDF),INTENT(OUT) :: x(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER       :: Inform
TYPE(Band_LU) :: LU

!!--begin--
!! 4.a.1 Factorization.
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] Performing LU &
   &factorization of the sparse coefficient matrix.")
CALL DUMP(fdbk)
CALL Nullify_Matrix( LU )
CALL LU_Factor(A, LU ,inform )

!! 4.a.2 Band LU solution by LINpack
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] Calculating the &
  &low-order solution via direct solution of the &
  &LU system with LINPACK.")
CALL DUMP(fdbk)
CALL SOLVE( LU, b, x )

!! 4.a.3 Deallocate LU matrix.
CALL DEALLOCATE_Matrix(LU)

!!--end--
END SUBROUTINE



SUBROUTINE SOLVE_LAPACK_dense( A , b , x , fdbk )
!!#### PURPOSE
!! Solve the QDF matrix using LAPACK.

!!#### REQUIRED INPUT
TYPE(MSR)     ,INTENT(IN) :: A
REAL(KIND_QDF),INTENT(IN) :: b(:)

!!#### REQUIRED OUTPUT
REAL(KIND_QDF),INTENT(OUT) :: x(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_QDF),ALLOCATABLE :: A2(:,:)
INTEGER :: N,INFO
INTEGER,POINTER :: IPIV(:)
CHARACTER(100) :: msg

!!--begin--
CALL CLEAR(msg)

CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] Performing direct &
   &solve using LAPACK...")

IF( .NOT.Is_ok(A) )THEN
 CALL UPDATE(fdbk_error,fdbk,s="[[QDF]] The coefficient matrix &
   &<A> is not okay. Terminating.")
 RETURN
ELSE

 !! Allocate.
 N = A%N
 ALLOCATE( A2(N,N) , IPIV(N) )

 !! Set RHS.
 x = b

 !! Set matrix.
 A2 = A

 !! Solve.
 CALL LA_GESV( A2, x, IPIV=ipiv, INFO=info )

 !! Deallocate.
 DEALLOCATE(IPIV,A2)

 !generic message
 msg = "[[QDF]] Finished direct solve using LAPACK"

 !add the info message
 CALL AddInfoMessage(info,msg,fdbk)

END IF

!!--end--
END SUBROUTINE


SUBROUTINE AddInfoMessage(info,msg,fdbk)
!!#### PURPOSE
!! Add a message telling the status of
!! a info integer from a legacy code.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: info

!!#### REQUIRED INPUT/OUTPUT
CHARACTER(*),INTENT(INOUT) :: msg

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: status

!!--begin--
!append the info number
msg = msg//"[info="//TRIM(STR(info))//"]"

!append specific info about that number using
!the standard >0 is warning, <0 is error,
!0 is success
IF( Info==0 )THEN
 status = fdbk_comment
 msg = TRIM(msg)//" (success)."
ELSE IF( Info>0 )THEN
 status = fdbk_warning
 msg = TRIM(msg)//" (warning)."
ELSE
 status = fdbk_error
 msg = TRIM(msg)//" (error)."
END IF

!update
CALL UPDATE(status,fdbk,s=TRIM(msg))

!!--end--
END SUBROUTINE


SUBROUTINE SOLVE_bistbl( A , b , x )
!!#### PURPOSE
!! Solve the QDF matrix using bistbl.
USE SUB_bistbl                                     !!((12-B-SUB_bistbl.f))

EXTERNAL :: MatVec_MSR , PreCond001

!!#### REQUIRED INPUT
TYPE(MSR)     ,INTENT(IN) :: A
REAL(KIND_QDF),INTENT(IN) :: b(:)

!!#### REQUIRED OUTPUT
REAL(KIND_QDF),INTENT(INOUT) :: x(:)

!!#### LOCAL VARIABLES
INTEGER :: l,n,mxmv,ldw,ldrw,info
REAL(KIND_QDF),ALLOCATABLE :: work(:,:),rwork(:,:)
INTEGER,ALLOCATABLE :: iwork(:)

!!--begin--
l = 1
n = A%N
mxmv = 1E9

ALLOCATE( work(n,3+2*(l+1)) )
ALLOCATE( rwork(l+1,3+2*(l+1)) )
ALLOCATE( iwork(l+1) )
ldw = 0
ldrw = 0
info = 0
CALL bistbl( l , n , x , b , MatVec_MSR , PreCond001 , 1D-06 , &
  mxmv , work , ldw , rwork , ldrw , iwork , info )
write(*,*)"info=",info

!!--end--
END SUBROUTINE



SUBROUTINE SOLVE_bicgstab2( A , b , x , fdbk )
!!#### PURPOSE
!! Solve the QDF matrix using bicgstab2.
USE SUB_bicgstab2                                  !!((12-B-SUB_bicgstab2.f))
EXTERNAL :: MatVec_MSR

!!#### REQUIRED INPUT
TYPE(MSR)     ,INTENT(IN) :: A
REAL(KIND_QDF),INTENT(IN) :: b(:)

!!#### REQUIRED OUTPUT
REAL(KIND_QDF),INTENT(INOUT) :: x(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: l,n,info,mxmv
REAL(KIND_QDF),ALLOCATABLE :: work(:,:)
REAL(KIND_QDF) :: tol
REAL(KIND_QDF),ALLOCATABLE :: residval(:)
INTEGER,ALLOCATABLE :: residit(:)
CHARACTER(100) :: msg

!!--begin--
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] Performing iterative &
   &solve using BiCGstab(ell=2)")

l = 2
n = A%N
ALLOCATE( work(n,2*l+5) )
info = 0
mxmv = 1E4
ALLOCATE( residval(mxmv) , residit(mxmv) )
residval = ERROR(residval)
residit = ERROR(residit)
tol = 1.0D-5

CALL bicgstab2(okprint=.false.,l=l, n=n, x=x, rhs=b, matvec=MatVec_MSR, &
  nonzero=.true., tol=tol,typestop="abs",mxmv=mxmv, &
  work=work, ldw=SIZE(work), info=info, residval=residval, residit=residit)

msg = "[[QDF]] Finishing iterative solve using BiCGstab(ell=2)"
CALL AddInfoMessage(info,msg,fdbk)

!***output residual info part***
CALL UpdateIterTable( residit, residval )

!clean up
DEALLOCATE( work , residval , residit )

!!--end--
END SUBROUTINE



SUBROUTINE SOLVE_SMLIB_gmres( A , b , x , fdbk )
!!#### PURPOSE
!! Solve the system with SMLIB's gmres.

USE VAR_QuasiDiffusion,ONLY: Fillin,&              !!((46-B-VAR_QuasiDiffusion.f90))
SubSpaceDimension,MaxIterations,Tolerance,PC,&
Initialize,MinIterations,MaxIterations,NumberOfIterations,&
SMLIB_ALL,ConvergenceHistory,ResidualNorm
USE USR_SMlib_GMRES                                !!((20-B-USR_SMlib_GMRES.f90))
USE USR_SMlib_ILU                                  !!((20-B-USR_SMlib_ILU.f90))

!!#### REQUIRED INPUT
TYPE(MSR)     ,INTENT(IN) :: A
REAL(KIND_QDF),INTENT(IN) :: b(:)

!!#### REQUIRED OUTPUT
REAL(KIND_QDF),INTENT(INOUT) :: x(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER,SAVE :: iter=0

!!--begin--
!! 4.b.0. Heuristic settings.
iter              = iter + 1
FillIn            = SQRT(REAL(A%N))/2

!! 4.b.1. Set up the preconditioner data structure.
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Performing symbolic &
  &cancellation with <SMLIB> [FillIn="//TRIM(STR(FillIn))//"].")
!
CALL Nullify_Matrix(PC)
CALL Symbolic_Cancellation( FillIn , A , PC )


!! 4.b.2. Do incomplete factorization preconditioner.
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Generating preconditioner...")
!
CALL ILU( A , PC )


!! 4.b.3. Solve.
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] calculating the &
  &LO solution via GMRES iterative solution with [Tolerance="//&
  TRIM(STR(Tolerance))//"]")
!
CALL GMRES( SubSpaceDimension , x , A , b , PC , &
      init=Initialize , &
      tol=Tolerance , &
      min_it=MinIterations , &
      max_it=MaxIterations , &
      no_of_its=NumberOfIterations , &
      info=SMLIB_ALL , &
      history=ConvergenceHistory , &
      residual_norm=ResidualNorm )

CALL DEALLOCATE_Matrix(PC)

!!--end--
END SUBROUTINE



SUBROUTINE SOLVE_SMLIB_BiCGstab( A , b , x , fdbk )
!!#### PURPOSE
!! Solve the system with SMLIB's BiCGstab.

USE VAR_QuasiDiffusion,ONLY: Fillin,&              !!((46-B-VAR_QuasiDiffusion.f90))
SubSpaceDimension,MaxIterations,Tolerance,PC,&
Initialize,MinIterations,MaxIterations,NumberOfIterations,&
SMLIB_ALL,ConvergenceHistory,ResidualNorm
USE USR_SMlib_BiCGSTAB                             !!((20-B-USR_SMlib_BiCGSTAB.f90))
USE USR_SMlib_ILU                                  !!((20-B-USR_SMlib_ILU.f90))

!!#### REQUIRED INPUT
TYPE(MSR)     ,INTENT(IN) :: A
REAL(KIND_QDF),INTENT(IN) :: b(:)

!!#### REQUIRED OUTPUT
REAL(KIND_QDF),INTENT(INOUT) :: x(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER,SAVE :: iter=0

!!--begin--
!! 4.b.0. Heuristic settings.
iter              = iter + 1
FillIn            = SQRT(REAL(A%N))

!! 4.b.1. Set up the preconditioner data structure.
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] Performing symbolic &
  &cancellation with FillIn="//TRIM(STR(FillIn))//".")
CALL DUMP(fdbk)
!
CALL Nullify_Matrix(PC)
CALL Symbolic_Cancellation( FillIn , A , PC )


!! 4.b.2. Do incomplete factorization preconditioner.
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] Preconditioner generation.")
CALL DUMP(fdbk)
!
CALL ILU( A , PC )


!! 4.b.3. Solve.
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] Calculating the &
  &low-order solution via BiCGstab iterative solution with Tolerance="//&
  TRIM(STR(Tolerance))//".")
CALL DUMP(fdbk)
!
CALL BiCGStab( x , A , b , PC , &
      init=Initialize , &
      tol=Tolerance , &
      min_it=MinIterations , &
      max_it=MaxIterations , &
      no_of_its=NumberOfIterations , &
      info=SMLIB_ALL , &
      history=ConvergenceHistory , &
      residual_norm=ResidualNorm )
!
CALL DEALLOCATE_Matrix(PC)

!!--end--
END SUBROUTINE


SUBROUTINE Init_SPARSKIT(n,lfil,droptol,niwk,nkryss,maxmatvec)
INTEGER,INTENT(IN) :: n
INTEGER,INTENT(OUT) :: lfil,niwk,nkryss,maxmatvec
REAL(KIND_QDF),INTENT(OUT) :: droptol

!!--begin--

 !1. set fill level
 IF( IsError(SPARSKIT_lfil) )THEN
  lfil = 1+int(real(n)/16.)
 ELSE
  lfil = SPARSKIT_lfil
 END IF
 IF( lfil>n )lfil=n

 !2. set drop tolerance
 IF( IsError(SPARSKIT_droptol) )THEN
  droptol = sqrt(epsilon(droptol))
 ELSE
  droptol = SPARSKIT_droptol
 END IF
 IF( droptol<REAL(0,KIND(droptol)) )droptol=REAL(0,KIND(droptol))

 !3. approximate work space needed
 IF( IsError(SPARSKIT_niwk) )THEN
  niwk = 10
 ELSE
  niwk = SPARSKIT_niwk
 END IF

 !5. krylov subspace dim
 IF( IsError(SPARSKIT_nkryss) )THEN
  nkryss = 50
 ELSE
  nkryss = SPARSKIT_nkryss
 END IF
 IF( nkryss>n )nkryss=n

 !6. maximum matrix-vector multiplies
 IF( IsError(SPARSKIT_maxmatvec) )THEN
  maxmatvec = 300
 ELSE IF( SPARSKIT_maxmatvec<0 )THEN
  maxmatvec = ABS(SPARSKIT_maxmatvec)*n
 ELSE
  maxmatvec = SPARSKIT_maxmatvec
 END IF

!!--end--
END SUBROUTINE



SUBROUTINE EVAL_CondNumber_ILU(n,iao,jao,ao,alu,jlu,ju,RCONDA,RCONDP)
USE USR_smatCSR                                    !!((06-A-USR_smatCSR.f90))
USE LIB_SPARSKIT_formats                           !!((04-A-LIB_SPARSKIT_formats.f))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: n
INTEGER,POINTER    :: iao(:),jao(:)
INTEGER,INTENT(IN) :: jlu(:),ju(:)
REAL(KIND_QDF),POINTER :: ao(:)
REAL(KIND_QDF),INTENT(IN) :: alu(:)
REAL(KIND_QDF),INTENT(OUT) :: RCONDA,RCONDP

!!#### LOCAL VARIABLES
REAL(KIND_QDF),ALLOCATABLE :: A2(:,:)
INTEGER :: INFO,col
REAL(KIND_QDF) :: RCOND,ANORM,eyemat(n)
INTEGER,ALLOCATABLE :: IWORK(:),IPIV(:)
REAL(KIND_QDF),ALLOCATABLE :: WORK(:)
TYPE(TYPE_smatCSR_Rdp) :: A
INTEGER :: Unit
INTEGER :: lines(0:0)=0
!!--begin--
write(*,*)"n=",n
write(*,*)"n^2=",n**2
write(*,*)"before ipiv"
ALLOCATE(IPIV(N))
write(*,*)"before iwork"
ALLOCATE(IWORK(N))
write(*,*)"before work"
ALLOCATE(WORK(4*N))
write(*,*)"before a2"
ALLOCATE( A2(1:N,1:N) )
write(*,*)"before nullify"
CALL NULLIFY( A )

!"STAGE 1: Matrix condition number"

!"construct A from pointers"
!"to get matrix in dense form"
A%n  =  n
write(*,*)"after n"
A%ia => iao
write(*,*)"after ia"
A%ja => jao
write(*,*)"after ja"
A%A  => ao
write(*,*)"after ao"

!A2 = A
write(*,*)"overloaded equals converts from CSR to dense"
!try Saad's conversion
CALL csrdns(n,n,ao,jao,iao,A2,n,INFO)

!"get rid of CSR"
A%ia => NULL()
A%ja => NULL()
A%A  => NULL()

!"now we just have dense matrix A2 that is equal to A"
IF( PlotSparsityPattern )THEN
 Unit=NewFile("A_sparsity.ps")
 CALL pspltm(N,N,0,jao,iao," ",0,3.0,"in",0,lines,Unit)
 CLOSE(Unit)
END IF

!" get the value of the norm of the matrix"
ANORM = LA_LANGE( '1', N, N, A2 , N, WORK )
!" do lu factorization"
CALL LA_GETRF( N, N, A2, N, IPIV, INFO )
!" get condition number"
CALL LA_GECON( '1', N, A2, N, ANORM, RCOND, WORK, IWORK, INFO )
RCONDA=1._KIND_QDF/RCOND



!"STAGE 2: Preconditioned Matrix condition number"

!"construct A from pointers"
!"to get matrix in dense form"
A%n  =  n
write(*,*)"after n"
A%ia => iao
write(*,*)"after ja"
A%ja => jao
A%A  => ao

!A2 = A
write(*,*)"overloaded equals converts from CSR to dense"
!try Saad's conversion
CALL csrdns(n,n,ao,jao,iao,A2,n,INFO)

!"get rid of CSR"
A%ia => NULL()
A%ja => NULL()
A%A  => NULL()

!"now use incomplete LU factors"
DO col=1,N
 CALL lusol( N , A2(:,col) , A2(:,col) , alu,jlu,ju )
END DO

IF( PlotPrecSparsityPattern )THEN
 eyemat = 0.d0
 DO col=1,N
  eyemat(col) = 1.d0
  CALL lusol( N , A2(:,col) , eyemat , alu,jlu,ju )
  eyemat(col) = 0.d0
 END DO
 A = A2
 Unit=NewFile("Minv_sparsity.ps")
 CALL pspltm(N,N,0,A%ja,A%ia," ",0,3.0,"in",0,lines,Unit)
 CLOSE(Unit)
 CALL DEALLOCATE(A)
 CALL NULLIFY(A)
END IF

!" get the value of the norm of the matrix"
ANORM = LA_LANGE( '1', N, N, A2 , N, WORK )
!" do lu factorization"
CALL LA_GETRF( N, N, A2, N, IPIV, INFO )
!" get new condition number"
CALL LA_GECON( '1', N, A2, N, ANORM, RCOND, WORK, IWORK, INFO )
RCONDP=1._KIND_QDF/RCOND

!"cleanup"
write(*,*)"before deall"
DEALLOCATE(IPIV,A2,IWORK,WORK)
write(*,*)"after deall"

!!--end--
END SUBROUTINE



SUBROUTINE SOLVE_SPARSKIT( Solver_ , A , b , x , fdbk )
!!#### PURPOSE
!! Solve the QDF matrix using SPARSKIT's solvers.
USE LIB_SPARSKIT_formats                           !!((04-A-LIB_SPARSKIT_formats.f))
USE LIB_SPARSKIT_iters                             !!((08-A-LIB_SPARSKIT_iters.f))
USE LIB_SPARSKIT_itaux,ONLY: &                     !!((07-A-LIB_SPARSKIT_itaux.f))
  runrc,sparskit_dump=>dump ,ilut,ilutp,ilud,iludp
USE KND_IntrinsicTypes,ONLY: KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
USE VAR_TAPACK,ONLY: tolPhiVInf                    !!((66-C-VAR_TAPACK.f90))
USE LIB_Norm                                       !!((04-B-LIB_Norm.f90))

!!#### REQUIRED INPUT
![waw] changed to INOUT because want to apply scaling
!      in place
CHARACTER(*),INTENT(IN) :: Solver_
TYPE(MSR)     ,INTENT(INOUT) :: A
REAL(KIND_QDF),INTENT(INOUT) :: b(:)

!!#### REQUIRED OUTPUT
REAL(KIND_QDF),INTENT(INOUT) :: x(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: n
REAL(KIND_QDF),ALLOCATABLE :: work(:)
REAL(KIND_QDF),POINTER :: ao(:)
INTEGER       ,POINTER :: iao(:),jao(:)
REAL(KIND_QDF),ALLOCATABLE :: alu(:)
INTEGER       ,ALLOCATABLE :: jlu(:),ju(:)
REAL(KIND_QDF),ALLOCATABLE :: w(:)
INTEGER       ,ALLOCATABLE :: jw(:)
REAL(KIND_QDF),ALLOCATABLE :: residval(:)
INTEGER,ALLOCATABLE :: residit(:)
REAL(KIND_QDF) :: guess(SIZE(x)),RCONDA,RCONDP
REAL(KIND_QDF) :: bmax,absL2Err,relL2Err
INTEGER       ,SAVE :: ipar(16),ierr,niwk,nkryss,lfil,maxmatvec,mbloc
REAL(KIND_QDF),SAVE :: fpar(16),droptol,reltol,abstol,permtol,alph
LOGICAL       ,SAVE :: SolverParamsInitialized=.FALSE.
LOGICAL :: AutoIterFix
INTEGER,SAVE :: iter=0
INTEGER :: lb,m,nwork,Unitx,k,lendiag
REAL :: t1,t2,dt
CHARACTER(7) :: Prec_
INTEGER,ALLOCATABLE :: iperm(:),idiag(:)
INTEGER :: Unit,nnzprec
REAL(KIND_QDF),ALLOCATABLE :: diag_eqn(:),diag_unk(:),diagvals(:)
INTEGER :: lines(0:0)=0
!!--begin--

iter = iter + 1
AutoIterFix = .FALSE.
ALLOCATE( diag_eqn(A%n) , diag_unk(A%n) )

Prec_ = PrecSpec

!initialize
1111 CONTINUE

!set initial guess
guess = x

NULLIFY( ao,iao,jao )
n = A%n
ierr= 0

bmax = 1._KIND_QDF
IF( StandardScaling )THEN
 bmax = MAXVAL(ABS(b))
 IF( bmax==REAL(0,KIND(bmax)) )THEN
  bmax=1._KIND_QDF
 END IF
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Scaling the low-order &
  & matrix and RHS by [bmax="//TRIM(STR(bmax,"(Es14.4)"))//"].")
 b = b/bmax
 A%A = A%A/bmax
END IF

!set tolerances
abstol = EPSILON(bmax)*bmax
reltol = 0.1d0*tolPhiVInf

!get matrix in CSR format
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Converting to CSR sparse&
 & matrix format from MSR")
CALL MSR_to_CSR( A , ao , jao , iao , Test=.FALSE. , Sort=.FALSE.)


!col scaling
IF( Use_ColScaling )THEN
 !CALL coscal(nrow,job,nrm,a,ja,ia,diag,b,jb,ib,ierr)
 CALL coscal(N,1,colnorm,ao,jao,iao,diag_unk,ao,jao,iao,ierr)
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Exiting coscal &
 & with error [errint="//TRIM(STR(ierr))//"]")
END IF

!row scaling
IF( Use_RowScaling )THEN
 !CALL roscal(nrow,job,nrm,a,ja,ia,diag,b,jb,ib,ierr)
 CALL roscal(N,1,rownorm,ao,jao,iao,diag_eqn,ao,jao,iao,ierr)
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Exiting roscal &
 & with error [errint="//TRIM(STR(ierr))//"]")
 !apply row scaling to RHS
 b = b*diag_eqn
END IF

!plot matrix sparsity pattern
IF( PlotSparsityPattern )THEN
 Unit=NewFile("A_sparsity.ps")
 CALL pspltm(N,N,0,jao,iao," ",0,3.0,"in",0,lines,Unit)
 CLOSE(Unit)
END IF

IF( .NOT.SolverParamsInitialized )THEN

 CALL Init_SPARSKIT(n,lfil,droptol,niwk,nkryss,&
   maxmatvec)

 SolverParamsInitialized = .TRUE.

END IF

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] SPARSKIT:: maximum number of&
 &matrix-vector multiplies is [maxmatvec="//TRIM(STR(maxmatvec))//"].")

!setup-1
ALLOCATE( ju(n),w(n+1),jw(2*n) )

!setup-3
m  = nkryss  !krylov subspace dimension
lb = m+1     !parameter DQGMRES needs
SELECT CASE(TRIM(Solver_))
 CASE("BCGSTAB") ; nwork=8*n
 CASE("CG")      ; nwork=5*n
 CASE("CGNR")    ; nwork=5*n
 CASE("BCG")     ; nwork=7*n
 CASE("DBCG")    ; nwork=11*n
 CASE("TFQMR")   ; nwork=11*n
 CASE("FOM")     ; nwork=(n+3)*(m+2) + (m+1)*m/2
 CASE("GMRES")   ; nwork=(n+3)*(m+2) + (m+1)*m/2
 CASE("FGMRES")  ; nwork=2*n*(m+1) + (m+1)*m/2 + 3*m + 2
 CASE("DQGMRES") ; nwork=n + lb * (2*n+4)
 CASE DEFAULT
  CALL UpdateAndDump(fdbk_error,fdbk,s="[[QDF]] SPARSKIT: Solver &
    &not recognized ("//TRIM(Solver_)//")")
END SELECT
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] SPARSKIT: Allocating &
 & workspace [nwork="//TRIM(STR(nwork))//"]")
ALLOCATE( work(nwork) )

!setup-3
ALLOCATE( residval(maxmatvec) , residit(maxmatvec) )
residval = ERROR(residval(1))
residit  = ERROR(residit(1))


!come here if don't have enough space
433 CONTINUE
ALLOCATE( alu(niwk) , jlu(niwk) )

!come here to try a better preconditioner
434 CONTINUE

!! ILU calculation
!** ilut preconditioner **
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Calculating [Prec="//TRIM(Prec_)//"]&
 & with [Fillin="//TRIM(STR(lfil))//"], &
 & [DropTol="//TRIM(STR(droptol))//"], and [niwk="//TRIM(STR(niwk))//"]")
CALL CPU_TIME(t1)
!********************************************************
SELECT CASE(TRIM(Prec_))

 CASE("ILUT")
   call ilut (n,ao,jao,iao,lfil,droptol              ,alu,jlu,ju,niwk,w,jw,ierr)

 CASE("ILUTP")
   mbloc = n
   permtol = 0.1_KIND_QDF
   IF( .NOT.ALLOCATED(iperm) )THEN
    ALLOCATE( iperm(2*n) )
    DO k=1,n
     iperm(k) = k
    END DO
   END IF
   call ilutp(n,ao,jao,iao,lfil,droptol,permtol,mbloc,alu,jlu,ju,niwk,w,jw,iperm,ierr)

 CASE("ILUD")
   alph = 1.d0
   call ilud (n,ao,jao,iao,alph,droptol              ,alu,jlu,ju,niwk,w,jw,ierr)

 CASE("ILUDP")
   mbloc = n
   permtol = 0.1_KIND_QDF
   alph = 1.d0
   IF( .NOT.ALLOCATED(iperm) )THEN
    ALLOCATE( iperm(2*n) )
    DO k=1,n
     iperm(k) = k
    END DO
   END IF
   call iludp(n,ao,jao,iao,alph,droptol,permtol,mbloc,alu,jlu,ju,niwk,w,jw,iperm,ierr)

 CASE("DIAG")
   ALLOCATE( diagvals(n) , idiag(n) )
   !CALL getdia (nrow,ncol,job,a,ja,ia,len,diag,idiag,ioff)
   CALL getdia (n,n,0,ao,jao,iao,lendiag,diagvals,idiag,0)
   DO k=1,n
    IF( diagvals(k) == 0.d0 )THEN
     alu(k) = 1.d0
    ELSE
     alu(k) = 1.d0/diagvals(k)
    END IF
    jlu(k) = 0
    ju(k)  = 0
   END DO
   DEALLOCATE( diagvals , idiag )

  CASE("NONE")
    DO k=1,n
     alu(k) = 1.d0
     jlu(k) = 0
     ju(k)  = 0
    END DO

END SELECT
!********************************************************
CALL CPU_TIME(t2)
dt = t2-t1
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Exiting [Prec="//TRIM(Prec_)//"]&
 & with error [errint="//TRIM(STR(ierr))//"]&
 & and prec. gen. [time="//TRIM(STRTIME(dt))//"]")

nnzprec = n
do k = 1, n
 nnzprec = nnzprec + ju(k)-jlu(k)
end do

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] The number of nonzeros is &
  &[nnz="//TRIM(STR(iao(n+1)-iao(1)))//"] and the size of &
  &the preconditioner is [nnzprec="//TRIM(STR(nnzprec))//"]")

!permute RHS
IF( ALLOCATED(iperm) )THEN
 b = Reorder(b,iperm(n+1:2*n),"L")
END IF

!!redo the calculation if we run out of memory
SELECT CASE(ierr)
 CASE(-2) ; CALL UpdateAndDump(fdbk_comment,fdbk,s=&
                        "[[QDF]] insufficient memory for L &
                        &preconditioner---will try harder.")
                        niwk = niwk*1.2
                        DEALLOCATE( alu,jlu )
                        GOTO 433

 CASE(-3) ; CALL UpdateAndDump(fdbk_comment,fdbk,s=&
                        "[[QDF]] insufficient memory for U &
                        &preconditioner---will try harder.")
                        niwk = niwk*1.2
                        DEALLOCATE( alu,jlu )
                        GOTO 433
END SELECT

IF( EvaluateConditionNumber )THEN
 CALL EVAL_CondNumber_ILU(n,iao,jao,ao,alu,jlu,ju,RCONDA,RCONDP)
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] SPARSKIT: Condition numbers &
  &[cond(A)="//TRIM(STR(RCONDA,"(Es10.4)"))//"] and &
  &[cond(Minv*A)="//TRIM(STR(RCONDP,"(Es10.4)"))//"]")
END IF


!** iterative solver driver**
!initialize
ipar    = 0
fpar    = 0
ipar(1) = 0         !initialize
ipar(2) = 1         !left preconditioning=1,right=2
ipar(3) = 2         !stopping criteria || residual || <= rtol * || rhs || + atol
ipar(4) = nwork     !workspace
ipar(5) = nkryss    !Krylov subspace dim
ipar(6) = maxmatvec !maximum number of matvecs
fpar(1) = reltol    !relative error tolerance for rhs
fpar(2) = abstol    !absolute error tolerance for error

!come here to iterate more
663 CONTINUE

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] SPARSKIT: Using &
 &[solver="//TRIM(Solver_)//"]  with [precond='left'], &
 &[stopcrit= ||resid|| <= reltol*||rhs|| + abstol ] with tolerances &
 &[abstol="//TRIM(STR(fpar(2),"(Es8.1)"))//"] and &
 &[reltol="//TRIM(STR(fpar(1),"(Es8.1)"))//"] .")

CALL CPU_TIME(t1)

SELECT CASE(TRIM(Solver_))
 CASE("BCGSTAB")
   call runrc(n,b,x,ipar,fpar,work,guess,ao,jao,iao,alu,jlu,ju,BCGSTAB,&
    okprint=.TRUE.,printunit=OutputUnit(fdbk),residval=residval, residit=residit)
 CASE("CG")
   call runrc(n,b,x,ipar,fpar,work,guess,ao,jao,iao,alu,jlu,ju,CG,&
    okprint=.TRUE.,printunit=OutputUnit(fdbk),residval=residval, residit=residit)
 CASE("CGNR")
   call runrc(n,b,x,ipar,fpar,work,guess,ao,jao,iao,alu,jlu,ju,CGNR,&
    okprint=.TRUE.,printunit=OutputUnit(fdbk),residval=residval, residit=residit)
 CASE("BCG")
   call runrc(n,b,x,ipar,fpar,work,guess,ao,jao,iao,alu,jlu,ju,BCG,&
    okprint=.TRUE.,printunit=OutputUnit(fdbk),residval=residval, residit=residit)
 CASE("DBCG")
   call runrc(n,b,x,ipar,fpar,work,guess,ao,jao,iao,alu,jlu,ju,DBCG,&
    okprint=.TRUE.,printunit=OutputUnit(fdbk),residval=residval, residit=residit)
 CASE("TFQMR")
   call runrc(n,b,x,ipar,fpar,work,guess,ao,jao,iao,alu,jlu,ju,TFQMR,&
    okprint=.TRUE.,printunit=OutputUnit(fdbk),residval=residval, residit=residit)
 CASE("FOM")
   call runrc(n,b,x,ipar,fpar,work,guess,ao,jao,iao,alu,jlu,ju,FOM,&
    okprint=.TRUE.,printunit=OutputUnit(fdbk),residval=residval, residit=residit)
 CASE("GMRES")
   call runrc(n,b,x,ipar,fpar,work,guess,ao,jao,iao,alu,jlu,ju,GMRES,&
    okprint=.TRUE.,printunit=OutputUnit(fdbk),residval=residval, residit=residit)
 CASE("FGMRES")
   call runrc(n,b,x,ipar,fpar,work,guess,ao,jao,iao,alu,jlu,ju,FGMRES,&
    okprint=.TRUE.,printunit=OutputUnit(fdbk),residval=residval, residit=residit)
 CASE("DQGMRES")
   call runrc(n,b,x,ipar,fpar,work,guess,ao,jao,iao,alu,jlu,ju,DQGMRES,&
    okprint=.TRUE.,printunit=OutputUnit(fdbk),residval=residval, residit=residit)
END SELECT

CALL CPU_TIME(t2)
dt = t2-t1
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Finished iterations in [time="//TRIM(STRTIME(dt))//"]")

!if the last residual is greater than tolerance then we increase
!the fill-in and decrease the drop-tolerance and try again.
!again

!put residual in guess
CALL amux (n, x, guess, ao,jao,iao)
guess = guess - b

absL2err = bmax*NormEll2(guess)
relL2Err = absL2err/NormEll2(x)
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Iterative errors are &
  &[relL2Err="//TRIM(STR(relL2Err,"(Es8.2)"))//"] and &
  &[absL2Err="//TRIM(STR(absL2Err,"(Es8.2)"))//"].")

!contingent upon residual criteria chosen
AutoIterFix = Using_AutoIterFix .AND. &
  ( (relL2Err>reltol .AND. absL2Err>abstol) .OR. ipar(1)<0)

IF( AutoIterFix )THEN

 !if we are making good progress
 IF( ipar(1)==0 )THEN

  guess = x
  fpar(1) = fpar(1)/10._KIND_QDF
  fpar(2) = fpar(2)/10._KIND_QDF
  CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Attempting to automatically&
    & refine the low-order solution ...")
  GOTO 663

 ELSE

  !increase fill in by the factor (1+its/maxits) and add 1 for good measure
  lfil = REAL(lfil)*( 1. + REAL(ipar(7))/REAL(ipar(6)) ) + 1.

  !decrease droptol by the factor (2+tol/resid)
  droptol = droptol/( 2.*(1.+abstol/fpar(6)) )

  CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] inadequate iterative solve---&
   &will try harder by redoing ILUT factorization.")

  !unpermute Ax and b
  IF( ALLOCATED(iperm) )THEN
   b = Reorder( b , iperm(n+1:2*n) , "R")
   x = Reorder( x , iperm(n+1:2*n) , "R")
   do k=iao(1),iao(n+1)-1
    jao(k) = iperm(n+jao(k))
   enddo
  END IF
  GOTO 434
 END IF

!***output residual info part***
ELSE

 CALL UpdateIterTable( residit, residval )
 !unpermute Ax and b
 IF( ALLOCATED(iperm) )THEN
  b = Reorder( b , iperm(n+1:2*n) , "R")
  x = Reorder( x , iperm(n+1:2*n) , "R")
  do k=iao(1),iao(n+1)-1
   jao(k) = iperm(n+jao(k))
  enddo
 END IF

 IF( Use_RowScaling )THEN
  b = b/diag_eqn
 END IF
 IF( Use_ColScaling )THEN
  x = x*diag_unk
 END IF

 IF( StandardScaling )THEN
  CALL UpdateAndDump(fdbk_comment,fdbk,s="[[QDF]] Un-scaling the low-order &
   & matrix.")
   A%A = A%A*bmax
   b   = b*bmax
 END IF

END IF

!wrapup
DEALLOCATE( alu,jlu,work,residval,residit,diag_unk,diag_eqn )
DEALLOCATE( w , jw )
IF( ALLOCATED(iperm) )THEN
 DEALLOCATE(iperm)
END IF

!!--end--
END SUBROUTINE




SUBROUTINE SOLVE_nspiv(A,b,x,fdbk)
!!#### PURPOSE
!! Solve the CSR system using <nspiv>, a sparse
!! gauss elimination routine.

USE KND_IntrinsicTypes,ONLY: KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
USE SLV_GaussElim_smatCSR                          !!((07-B-SLV_GaussElim_smatCSR.f90))

!!#### REQUIRED INPUT
TYPE(MSR)                  :: A
REAL(KIND_QDF),INTENT(IN)  :: b(:)
REAL(KIND_QDF),INTENT(OUT) :: x(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: IERR,N
TYPE(TYPE_smatCSR_Rdp)     :: A2
INTEGER,SAVE :: MAX=0

!!--begin--
A2%N = A%N
N = A%N

CALL MSR_to_CSR( A , A2%a , A2%ja , A2%ia )

IF( MAX==0 )MAX = NNZ(A)
333 CONTINUE
CALL SOLVE_GaussElim_smatCSR(A2,b,x,MAX,IERR)

IF( IERR>0 )THEN
 CALL UPDATE(fdbk_comment,fdbk, s="[[QDF]] <nspiv> successful completion")
ELSE IF( IERR==0 )THEN
 CALL UPDATE(fdbk_error,fdbk, s="[[QDF]] <nspiv> error: N=0")
ELSE IF( IERR>=-N )THEN
 CALL UPDATE(fdbk_error,fdbk, s="[[QDF]] <nspiv> error: row "//&
   TRIM(STR(ABS(IERR)))//" is null")
ELSE IF( IERR>=-2*N )THEN
 CALL UPDATE(fdbk_error,fdbk, s="[[QDF]] <nspiv> error: row "//&
   TRIM(STR(ABS(IERR+N)))//" has a duplicate")
ELSE IF( IERR>=-3*N )THEN
 CALL UPDATE(fdbk_error,fdbk, s="[[QDF]] <nspiv> error: row "//&
   TRIM(STR(ABS(IERR+2*N)))//" has a zero pivot")
ELSE IF( IERR>=-4*A%N )THEN
 CALL UPDATE(fdbk_comment,fdbk, s="[[QDF]] <nspiv> warning: row "//&
   TRIM(STR(ABS(IERR+3*N)))//" exceeds storage with MAX="//TRIM(STR(MAX))//&
   ", retrying with MAX="//TRIM(STR(MAX*3/2)) )
 MAX = MAX*3/2
 GO TO 333
END IF

!!--end--
END SUBROUTINE




SUBROUTINE Reorder_CSR(a,ja,ia,perm)
!!#### PURPOSE
!! Reorder the CSR array by permutation vector.

USE LIB_SPARSKIT_unary                             !!((03-A-LIB_SPARSKIT_unary.f))

!!#### REQUIRED INPUT
REAL(KIND_QDF),POINTER :: a(:)
INTEGER       ,POINTER :: ia(:),ja(:)
INTEGER,INTENT(IN) :: perm(:)


!!#### LOCAL VARIABLES
REAL(KIND_QDF),POINTER :: aw(:)
INTEGER       ,POINTER :: iaw(:),jaw(:)
INTEGER :: n

!!--begin--
!allocate
n = SIZE(ia)-1
ALLOCATE( aw(SIZE(a)) , jaw(SIZE(ja))  , iaw(SIZE(ia)))

!reorder
CALL rperm (n,a,ja,ia,aw,jaw,iaw,perm,job=1)

a = aw
ja = jaw
ia = iaw

!wrapup
DEALLOCATE( aw , jaw , iaw )

!!--end--
END SUBROUTINE



SUBROUTINE getOrderRCM_MSR(A,perm)
!!#### PURPOSE
!! Get an RCM ordering from an MSR matrix.

USE LIB_rcm                                        !!((03-A-LIB_rcm.f90))

!!#### REQUIRED INPUT
TYPE(MSR),INTENT(IN) :: A

!!#### REQUIRED OUTPUT
INTEGER,POINTER :: perm(:)

!!#### LOCAL VARIABLES
INTEGER :: node_num,adj_max,i,c,iaft,ibef,adj_num
INTEGER,POINTER :: adj(:),adj_row(:),perm_inv(:)

!!--begin--
node_num = A%N
adj_max = NNZ(A) + node_num
ALLOCATE( adj(adj_max) , adj_row(node_num+1) )
CALL adj_set ( node_num, adj_max, adj_num, adj_row, adj,-1, -1 )
DO i=1,node_num
 DO c = A % JA (i), A % JA (i+1) - 1
  CALL adj_set ( node_num, adj_max, adj_num, adj_row, adj, i , A % JA (c) )
 END DO
END DO

!calculate original properties
ibef = adj_bandwidth ( node_num, adj_num, adj_row, adj )
write(*,"(a,i0)")"bandwidth = ",ibef

!reordering
ALLOCATE( perm(1:node_num) )
call genrcm ( node_num, adj_num, adj_row, adj, perm )

!calculate permuted properties
allocate( perm_inv(node_num) )
call perm_inverse ( node_num, perm, perm_inv )
iaft = adj_perm_bandwidth ( node_num, adj_num, adj_row, adj,perm, perm_inv )
write(*,*)"permuted bandwidth =",iaft

DEALLOCATE( perm_inv , adj , adj_row )
!!--end--
END SUBROUTINE



SUBROUTINE MSR_to_CSR( A , ao , jao , iao , Test , Sort )
!!#### PURPOSE
!! Convert MSR to CSR conversion routine with optional
!! test by passing an MSR matrix, converting to CSR, and then
!! back to MSR to check for equality with the original.

USE LIB_SPARSKIT_formats                           !!((04-A-LIB_SPARSKIT_formats.f))
USE USR_SMlib_MSR                                  !!((17-B-USR_SMlib_MSR.f90))
USE USR_SMlib_CSR                                  !!((17-B-USR_SMlib_CSR.f90))

!!#### REQUIRED INPUT
TYPE(MSR),INTENT(IN) :: A

!!#### REQUIRED OUTPUT
REAL(KIND_QDF),POINTER :: ao(:)
INTEGER       ,POINTER :: jao(:),iao(:)

!!#### OPTIONAL INPUT
LOGICAL,INTENT(IN),OPTIONAL :: Test,Sort

!!#### LOCAL VARIABLES
TYPE(CSR) :: A2
TYPE(MSR) :: A3
LOGICAL :: Pass0,Pass1,Pass2,Pass3,Pass,Test_,Sort_
INTEGER :: n,nn
INTEGER       ,POINTER :: iwk(:)
REAL(KIND_QDF),POINTER :: wk(:)
LOGICAL,POINTER :: mask_ja(:),mask_a(:)

!!--begin--
CALL NULLIFY_Matrix(A2)
CALL NULLIFY_Matrix(A3)

!enable testing
Test_ = DEFAULT(.FALSE.,Test)
Sort_ = DEFAULT(.TRUE.,Sort)

!set CSR matrix through [SMLIB] overloaded
!"=" conversion
n = A%N
nn = nnz(A)
A2 = A !overloaded equals

!set pointers to CSR values (we just return arrays instead of structure)
ao  => A2%a
iao => A2%ia
jao => A2%ja

!simple test of whether conversion works
IF( Test_ )THEN

 !check if CSR matrix is okay
 Pass0 = Is_Ok(A2)
 WRITE(*,*)"Is_Ok(CSR)=",Is_Ok(A2)

 !initialize a3
 ALLOCATE( a3%a(SIZE(a%a)) , a3%ja(SIZE(a%ja)) )
 a3%n  = n
 a3%a  = 0._KIND_QDF
 a3%ja = 0

 !convert CSR (a2) back to MSR (a3)
 iwk => NULL()
 wk => NULL()
 ALLOCATE( wk(n) , iwk(n+1) )
 ![sparskit] conversion routine
 CALL csrmsr (n , ao , jao , iao , A3%a , A3%ja , wk , iwk)

 !see if a(MSR)->a2(CSR)->a3(MSR) yields a==a3
 Pass1 = (A3%n==A%n)
 allocate( mask_ja(size(a%ja)) , mask_a(size(a%a)) )
 mask_ja = A3%ja==A%ja
 !this position has no meaning
 mask_ja(n+1) = .TRUE.
 mask_ja(size(mask_ja)) = .TRUE.
 Pass2 = ALL(mask_ja)
 mask_a = A3%a==A%a
 !neither does this one
 mask_a(SIZE(mask_a))=.TRUE.
 Pass3 = ALL(mask_a)
 Pass = Pass1.AND.Pass2.AND.Pass3.AND.Pass0
 WRITE(*,*)"The MSR-CSR matrix conversion test: "//MERGE("PASS","FAIL",Pass)

 !deallocate test matrix
 CALL DEALLOCATE_Matrix( A3 )
 DEALLOCATE( wk , iwk )
 DEALLOCATE( mask_a , mask_ja )

END IF

!nullify CSR components
NULLIFY( A2%a,A2%ja,A2%ia )

!sort by columns if asked for
IF( Sort_ )THEN
 ALLOCATE( iwk(max ( n+1, 2*nn )) )
 CALL csort (n,ao,jao,iao,iwk,values=.TRUE.)
 DEALLOCATE( iwk )
END IF

!!--end--
END SUBROUTINE



SUBROUTINE UpdateIterTable( residit , residval )
!!#### PURPOSE
!! Updates the iteration table (table of residuals
!! for iterative low order solution).

!!#### GLOBAL VARIABLES
USE VAR_QuasiDiffusion,ONLY: IterTable             !!((46-B-VAR_QuasiDiffusion.f90))
USE FUN_SIZEa                                      !!((06-B-FUN_SIZEa.f90))

!!#### REQUIRED INPUT
REAL(KIND_QDF),INTENT(IN) :: residval(:)
INTEGER,INTENT(IN) :: residit(:)

!!#### LOCAL VARIABLES
INTEGER,SAVE :: Ns=1,Np=1
INTEGER :: NResid

!increment transport iteration
Ns = Ns + 1

!get maximum number of iteration spots used so far (plus the header row)
Nresid = SIZEa(residit)
Np = MAX(Nresid,Np)+1

!(re)allocate table if needed
IF( .NOT.ASSOCIATED(IterTable) )THEN
 ALLOCATE( IterTable(Np,Ns) )
 CALL CLEAR(IterTable)
 IterTable(1,1 ) = "p"
ELSE
 IF( Np>SIZE(IterTable,1) )THEN
  CALL REALLOCATE( IterTable , (/Np-SIZE(IterTable,1),0/) , &
    fill=REPEAT(" ",LEN(IterTable)) )
 END IF
 IF( Ns>SIZE(IterTable,2) )THEN
  CALL REALLOCATE( IterTable , (/0,Ns-SIZE(IterTable,2)/) , &
    fill=REPEAT(" ",LEN(IterTable)) )
 END IF
END IF

!update column heading
IterTable(1,Ns) = "resid"//TRIM(STR(Ns-1))//"_p"

!naive updates (assume iteration numbers don't change---only more can
!be added)
!1) update iteration numbers
IterTable(2:Nresid+1,1 ) = STR(residit,"(I4)")
!1) update iteration residuals
IterTable(2:Nresid+1,Ns) = STR(residval,"(Es9.2)")

END SUBROUTINE


SUBROUTINE ALLOCATE_QDF(Ng,Ni,Nj,Nk,Njbdry,&
  VExx,VEyy,VExy,&
  FExx,FEyy,FExy,&
  CExx,CEyy,CExy,&
  VExx_times_Phi,VEyy_times_Phi,VExy_times_Phi,&
  FExx_times_Phi,FEyy_times_Phi,FExy_times_Phi,&
  CExx_times_Phi,CEyy_times_Phi,CExy_times_Phi,&
  ScalarFluxIN,CurrentIN,C)
!!#### PURPOSE
!! Allocate variables according to passed sizes.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Ng,Ni,Nj,Nk,Njbdry

!!#### REQUIRED OUTPUT
!! * vert Eddington factors <EddingtonV>
REAL(KIND_ScalarFlux),POINTER :: VExx(:,:)
REAL(KIND_ScalarFlux),POINTER :: VEyy(:,:)
REAL(KIND_ScalarFlux),POINTER :: VExy(:,:)
!
!! * face Eddington factors <EddingtonF>
REAL(KIND_ScalarFlux),POINTER :: FExx(:,:)
REAL(KIND_ScalarFlux),POINTER :: FEyy(:,:)
REAL(KIND_ScalarFlux),POINTER :: FExy(:,:)
!
!! * cell Eddington factors <EddigntonC>
REAL(KIND_ScalarFlux),POINTER :: CExx(:,:)
REAL(KIND_ScalarFlux),POINTER :: CEyy(:,:)
REAL(KIND_ScalarFlux),POINTER :: CExy(:,:)
!
!! * actual eddington tensors
REAL(KIND_AngularFlux),POINTER :: VExx_times_Phi(:,:),VEyy_times_Phi(:,:),VExy_times_Phi(:,:)
REAL(KIND_AngularFlux),POINTER :: FExx_times_Phi(:,:),FEyy_times_Phi(:,:),FExy_times_Phi(:,:)
REAL(KIND_AngularFlux),POINTER :: CExx_times_Phi(:,:),CEyy_times_Phi(:,:),CExy_times_Phi(:,:)
!
!! * boundary factors <C>
REAL(KIND_ScalarFlux),POINTER :: C(:,:)
!
!! * "incoming" scalar fluxes <ScalarFluxIN>
REAL(KIND_ScalarFlux),POINTER :: ScalarFluxIN(:,:)
!
!! * "incoming" currents <CurrentIN>
REAL(KIND_ScalarFlux),POINTER :: CurrentIN(:,:)
!

!!--begin--
IF( .NOT.ASSOCIATED(VExx) )THEN
 ALLOCATE( VExx(Ng,Nk),VEyy(Ng,Nk),VExy(Ng,Nk) )
 VExx=0.d0
 VEyy=0.d0
 VExy=0.d0
END IF

IF( .NOT.ASSOCIATED(VExx_times_Phi) )THEN
 ALLOCATE( VExx_times_Phi(Ng,Nk) , VEyy_times_Phi(Ng,Nk) , VExy_Times_Phi(Ng,Nk) )
 VExx_times_Phi=0.d0
 VEyy_times_Phi=0.d0
 VExy_times_Phi=0.d0
END IF

!! Allocation check: xx face Eddington factors <FExx>.
IF( .NOT.ASSOCIATED(FExx) )THEN
 ALLOCATE( FExx(1:Ng,1:Nj) )
 FExx=0.d0
END IF

!! Allocation check: yy face Eddington factors <FEyy>.
IF( .NOT.ASSOCIATED(FEyy) )THEN
 ALLOCATE( FEyy(1:Ng,1:Nj) )
 FEyy=0.d0
END IF

!! Allocation check: xy face Eddington factors <FExy>.
IF( .NOT.ASSOCIATED(FExy) )THEN
 ALLOCATE( FExy(1:Ng,1:Nj) )
 FExy=0.d0
END IF

!! Allocation check: xx cell Eddington factors <CExx>.
IF( .NOT.ASSOCIATED(CExx) )THEN
 ALLOCATE( CExx(1:Ng,1:Ni) )
 CExx=0.d0
END IF

!! Allocation check: yy cell Eddington factors <CEyy>.
IF( .NOT.ASSOCIATED(CEyy) )THEN
 ALLOCATE( CEyy(1:Ng,1:Ni) )
 CEyy=0.d0
END IF

!! Allocation check: xy cell Eddington factors <CExy>.
IF( .NOT.ASSOCIATED(CExy) )THEN
 ALLOCATE( CExy(1:Ng,1:Ni) )
 CExy=0.d0
END IF

!! Allocation check: incoming scalar Flux on boundary faces
!! (partial angular integral of angular Flux over incoming
!! directions) <ScalarFluxIN>.
IF( .NOT.ASSOCIATED(ScalarFluxIN) )THEN
 ALLOCATE( ScalarFluxIN(1:Ng,1:Njbdry) )
 ScalarFluxIN=0.d0
END IF

!! Allocation check: incoming Current on boundary faces
!! (partial angular integral of angular Flux over incoming
!! directions) <CurrentIN>.
IF( .NOT.ASSOCIATED(CurrentIN) )THEN
 ALLOCATE( CurrentIN(1:Ng,1:Njbdry) )
 CurrentIN=0.d0
END IF

!! Allocation check: boundary factor on boundary faces
!! (partial angular integral of angular Flux over incoming
!! directions) <C>.
IF( .NOT.ASSOCIATED(C) )THEN
 ALLOCATE( C(1:Ng,1:Njbdry) )
 C=0.d0
END IF

!! Allocation check: xx face Eddington factors <FExx_times_Phi>.
IF( .NOT.ASSOCIATED(FExx_times_Phi) )THEN
 ALLOCATE( FExx_times_Phi(1:Ng,1:Nj) )
 FExx_times_Phi=0.d0
END IF

!! Allocation check: yy face Eddington factors <FEyy_times_Phi>.
IF( .NOT.ASSOCIATED(FEyy_times_Phi) )THEN
 ALLOCATE( FEyy_times_Phi(1:Ng,1:Nj) )
 FEyy_times_Phi=0.d0
END IF

!! Allocation check: xy face Eddington factors <FExy_times_Phi>.
IF( .NOT.ASSOCIATED(FExy_times_Phi) )THEN
 ALLOCATE( FExy_times_Phi(1:Ng,1:Nj) )
 FExy_times_Phi=0.d0
END IF

!! Allocation check: xx cell Eddington factors <CExx_times_Phi>.
IF( .NOT.ASSOCIATED(CExx_times_Phi) )THEN
 ALLOCATE( CExx_times_Phi(1:Ng,1:Ni) )
 CExx_times_Phi=0.d0
END IF

!! Allocation check: yy cell Eddington factors <CEyy_times_Phi>.
IF( .NOT.ASSOCIATED(CEyy_times_Phi) )THEN
 ALLOCATE( CEyy_times_Phi(1:Ng,1:Ni) )
 CEyy_times_Phi=0.d0
END IF

!! Allocation check: xy cell Eddington factors <CExy_times_Phi>.
IF( .NOT.ASSOCIATED(CExy_times_Phi) )THEN
 ALLOCATE( CExy_times_Phi(1:Ng,1:Ni) )
 CExy_times_Phi=0.d0
END IF

!!--end--
END SUBROUTINE






SUBROUTINE SETUP_QDF_MSR( NQDF , NZMAX , A , x , b )
!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: NQDF,NZMAX

!!#### REQUIRED OUTPUT
TYPE(MSR),INTENT(INOUT) :: A
REAL(KIND_QDF),POINTER :: x(:),b(:)

!!--begin--
!! Nullify the matrices we will use.
CALL Nullify_Matrix( A )

!! Allocate the coefficient matrix.
CALL Allocate_Matrix( A , N=NQDF , NZMAX=NZMAX )

!! Allocate the RHS.
ALLOCATE( b(1:NQDF) ) ; b = 0._KIND_QDF

!! Allocate the solution vector.
ALLOCATE( x(1:NQDF) ) ; x = 1.E-10_KIND_QDF

!!--end--
END SUBROUTINE



SUBROUTINE Reallocate_Eqn(cols,vals,N)
!!#### PURPOSE
!! Reallocate the sparse matrix equation specifiers
!! <cols> and <vals> to size <N>.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: N

!!#### REQUIRED INPUT/OUTPUT
INTEGER      ,POINTER :: cols(:)
REAL(KIND_QDF),POINTER :: vals(:)

!!--begin--
IF( .NOT.ASSOCIATED(cols) )THEN
 ALLOCATE( cols(1:N) )
ELSE IF( SIZE(cols)/=N )THEN
 DEALLOCATE( cols )
 ALLOCATE( cols(1:N) )
END IF
cols = Error(cols)

IF( .NOT.ASSOCIATED(vals) )THEN
 ALLOCATE( vals(1:N) )
ELSE IF( SIZE(vals)/=N )THEN
 DEALLOCATE( vals )
 ALLOCATE( vals(1:N) )
END IF
vals = Error(vals)

!!--end--
END SUBROUTINE



SUBROUTINE Esmooth( w , &
   CExy0 , EBxy0 , ERxy0 , ETxy0 , ELxy0 , &
   CExx0 , EBxx0 , ERxx0 , ETxx0 , ELxx0 , &
   CEyy0 , EByy0 , ERyy0 , ETyy0 , ELyy0 )
!!#### PURPOSE
!! Smooths the face E's in a cell by averaging (linearly)
!! with the cell-average E.

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: w

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),INTENT(INOUT) :: &
   CExy0 , EBxy0 , ERxy0 , ETxy0 , ELxy0 , &
   CExx0 , EBxx0 , ERxx0 , ETxx0 , ELxx0 , &
   CEyy0 , EByy0 , ERyy0 , ETyy0 , ELyy0

!!--begin--

 !never do it
 RETURN

 !eddingtons-xy
 EBxy0 = CExy0*w + (1._KIND_AngularFlux-w)*EBxy0
 ERxy0 = CExy0*w + (1._KIND_AngularFlux-w)*ERxy0
 ETxy0 = CExy0*w + (1._KIND_AngularFlux-w)*ETxy0
 ELxy0 = CExy0*w + (1._KIND_AngularFlux-w)*ELxy0

 !eddingtons-xx
 EBxx0 = CExx0*w + (1._KIND_AngularFlux-w)*EBxx0
 ERxx0 = CExx0*w + (1._KIND_AngularFlux-w)*ERxx0
 ETxx0 = CExx0*w + (1._KIND_AngularFlux-w)*ETxx0
 ELxx0 = CExx0*w + (1._KIND_AngularFlux-w)*ELxx0

 !eddingtons-yy
 EByy0 = CEyy0*w + (1._KIND_AngularFlux-w)*EByy0
 ERyy0 = CEyy0*w + (1._KIND_AngularFlux-w)*ERyy0
 ETyy0 = CEyy0*w + (1._KIND_AngularFlux-w)*ETyy0
 ELyy0 = CEyy0*w + (1._KIND_AngularFlux-w)*ELyy0

!!--end--
END SUBROUTINE


!!### EVALUATION SUBROUTINE: \<EVAL_HalfCellVolumes_Qd\>
SUBROUTINE EVAL_HalfCellVolumes_Qd(Mesh,kBL,kBR,kTR,kTL,jB,jR,jT,jL,i,&
  VB,VR,VT,VL,Vi,Method)

!!#### PURPOSE
!! Evaluate half cell volumes for quadrilaterals from the Mesh and a list
!! of indices.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: kBL,kBR,kTR,kTL
INTEGER        ,INTENT(IN) :: jB,jR,jT,jL
INTEGER        ,INTENT(IN) :: i

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: Method

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),INTENT(OUT) :: VB,VR,VT,VL,Vi

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: VBR,VTR,VBL,VTL

!!--begin--
Vi = 0._KIND_MSH
VB = 0._KIND_MSH
VR = 0._KIND_MSH
VT = 0._KIND_MSH
VL = 0._KIND_MSH

SELECT CASE(Method)

 CASE( QDF_FaceBisect )

 !get the volume of a cell
 Vi = CellVolume(Mesh,i)

 !get the volume of the bottom half-cell
 VB = xySAREA_Qg( RESHAPE((/Vert(Mesh,kBL),&
                    Vert(Mesh,kBR),&
                    FaceCentroid(Mesh,jR),&
                    FaceCentroid(Mesh,jL) /),(/2,4/)) )

 !get the volume of the right half-cell
 VR = xySAREA_Qg( RESHAPE((/FaceCentroid(Mesh,jB),&
                    Vert(Mesh,kBR),&
                    Vert(Mesh,kTR),&
                    FaceCentroid(Mesh,jT) /),(/2,4/)) )

 !get the volume of the top half-cell
 VT = xySAREA_Qg( RESHAPE((/FaceCentroid(Mesh,jL),&
                    FaceCentroid(Mesh,jR),&
                    Vert(Mesh,kTR),&
                    Vert(Mesh,kTL) /),(/2,4/)) )

 !get the volume of the left half-cell
 VL = xySAREA_Qg( RESHAPE((/Vert(Mesh,kBL),&
                    FaceCentroid(Mesh,jB),&
                    FaceCentroid(Mesh,jT),&
                    Vert(Mesh,kTL) /),(/2,4/)) )

 CASE( QDF_Corners )

 !get the volume of a cell
 Vi = CellVolume(Mesh,i)

 !get the volumes of the corner cells
 VBR = xySAREA_Qg( RESHAPE((/CellCentroid(Mesh,i),&
                     FaceCentroid(Mesh,jB),&
                     Vert(Mesh,kBR),&
                     FaceCentroid(Mesh,jR) /),(/2,4/)) )
 VTR = xySAREA_Qg( RESHAPE((/CellCentroid(Mesh,i),&
                    FaceCentroid(Mesh,jR),&
                     Vert(Mesh,kTR),&
                     FaceCentroid(Mesh,jT) /),(/2,4/)) )
 VTL = xySAREA_Qg( RESHAPE((/CellCentroid(Mesh,i),&
                     FaceCentroid(Mesh,jT),&
                     Vert(Mesh,kTL),&
                    FaceCentroid(Mesh,jL) /),(/2,4/)) )
 VBL = xySAREA_Qg( RESHAPE((/CellCentroid(Mesh,i),&
                     FaceCentroid(Mesh,jL),&
                                        Vert(Mesh,kBL),&
                     FaceCentroid(Mesh,jB) /),(/2,4/)) )

!get the volume of the bottom half-cell
 VB =  VBR + VBL

 !get the volume of the right half-cell
 VR =  VBR + VTR

!get the volume of the top half-cell
 VT =  VTL + VTR

!get the volume of the left half-cell
 VL =  VTL + VBL

END SELECT

!!--end--
END SUBROUTINE


SUBROUTINE BlockOrdering(Mesh,Ng,row_reorder,col_reorder)
!!#### PURPOSE
!! Reorder unknowns (columns) according to a
!! strategy which leads to blocks of unknowns.

!!#### LIMITATIONS
!! Assumes that the faces are ordered with cells.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER,INTENT(IN) :: Ng

!!#### REQUIRED INPUT/OUTPUT
INTEGER,INTENT(INOUT) :: row_reorder(:),col_reorder(:)

INTEGER :: n,j0,i,j,ni,nj
!!--begin--
ni = NUM_Cells(Mesh)
nj = NUM_Faces(Mesh)

n = 0
j0 = 0
DO i=1,Ni
 n = n + 1
 col_reorder(i) = n
 DO j=1,NUM_Faces(Mesh%Cells(i))
  n = n + 1
  j0 = j0 + 1
  col_reorder(Ni+j0) = n
  n = n + 1
  col_reorder(Ni+Nj+j0) = n
 END DO
END DO
!!--end--
END SUBROUTINE


FUNCTION NUM_InterfaceEquations(Interfaces) RESULT(NUM)
!!#### PURPOSE
!! Determine the number of interface equations:
!!   * 1 strong equation per subface
!!   * 1 weak equation per interface

!!#### REQUIRED INPUT
TYPE(TYPE_Interfaces),INTENT(IN) :: Interfaces(:)

!!#### REQUIRED OUTPUT
INTEGER :: NUM

!!#### LOCAL VARIABLES
INTEGER :: jn0

!!--begin--

!count the weak equations
NUM = SIZE(Interfaces)

!count the strong equations
DO jn0=1,SIZE(Interfaces)
 NUM = NUM + SIZE(Interfaces(jn0)%subs)
END DO

!!--end--
END FUNCTION



FUNCTION MAX_Subfaces(Interfaces) RESULT(MAX_)
!!#### PURPOSE
!! Determine the maximum number of subfaces
!! of any interface.

!!#### REQUIRED INPUT
TYPE(TYPE_Interfaces),INTENT(IN) :: Interfaces(:)

!!#### REQUIRED OUTPUT
INTEGER :: MAX_

!!#### LOCAL VARIABLES
INTEGER :: jn0

!!--begin--
!init
MAX_ = 0

!loop over the interfaces
DO jn0=1,SIZE(Interfaces)
 MAX_ = MAX(MAX_,SIZE(Interfaces(jn0)%subs))
END DO

!!--end--
END FUNCTION


SUBROUTINE GET_IsMaster(IsMaster,Interfaces)
!!#### PURPOSE
!! Get the <IsMaster> mask, dictating whether or not each
!! face is a master face or not.

!!#### REQUIRED INPUT
TYPE(TYPE_Interfaces),INTENT(IN) :: Interfaces(:)

!!#### REQUIRED OUTPUT
LOGICAL :: IsMaster(:)

!!#### LOCAL VARIABLES
INTEGER :: jn0

!!--begin--
!! IN ORDER TO ALWAYS HAVE ENTRIES ON THE DIAGONALS
!! MUST KNOW MASTER FACES SO CAN SWAP ROWS
!! for the QDF_Flux interface
!! conditions and the Moment1 equations!!!
IsMaster = .FALSE.

![waw] Do not consider boundary faces master faces!
DO jn0=1,SIZE(Interfaces)
 IsMaster(Interfaces(jn0)%master) = .TRUE.
END DO

!!--end--
END SUBROUTINE


SUBROUTINE OUTPUT_ILU(iter,PC)
!!#### PURPOSE
!! Visualize the incomplete LU preconditioner matrices
!! for the Iterative solution of the QDF system.

!!#### MODULES
USE FUN_NewFile                                    !!((05-B-FUN_NewFile.f90))
USE PRN_MatrixSymbolic_dmat                        !!((03-A-PRN_MatrixSymbolic_dmat.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: iter

!!#### OPTIONAL INPUT
TYPE(LU_CSR_MSR),INTENT(IN),OPTIONAL :: PC

!!#### LOCAL VARIABLES
REAL(KIND_QDF),ALLOCATABLE :: L_(:,:),U_(:,:)
INTEGER :: unit
TYPE(varying_string) :: file
LOGICAL,PARAMETER :: VisL=.TRUE.,VisU=.TRUE.
!!--begin--
!! visualize the LU preconditioners
IF( VisL .AND. PRESENT(PC) )THEN
 ALLOCATE( L_(PC%L%N,PC%L%N) )
 L_ = PC%L
 file = "PCILU-L_%p"
 file = Substitute( TRIM(STR(file)) , (/"%p"/) , (/iter/) )
 file = TRIM(STR(file))//".dat"
 unit = NewFile(STR(file),STATUS="Replace",IfOpened="R")
 CALL PRINT_MatrixSymbolic_dmat( L_ , unit=unit , zero=" ")
 CLOSE(unit)
 DEALLOCATE( L_ )
END IF

IF( VisU .AND. PRESENT(PC) )THEN
 ALLOCATE( U_(PC%U%N,PC%U%N) )
 U_ = PC%U
 file = "PCILU-U_%p"
 file = Substitute( TRIM(STR(file)) , (/"%p"/) , (/iter/) )
 file = TRIM(STR(file))//".dat"
 unit = NewFile(TRIM(STR(file)),STATUS="Replace",IfOpened="R")
 CALL PRINT_MatrixSymbolic_dmat( U_ , unit=unit , zero=" ")
 CLOSE(unit)
 DEALLOCATE( U_ )
END IF


!!--end--
END SUBROUTINE


SUBROUTINE SaveResidualInfo(iter,res,relres,&
  maxloc_res,maxval_res,iter_res,&
  maxloc_relres,maxval_relres,iter_relres,&
  maxloc_relres0,maxval_relres0,&
  maxloc_res0,maxval_res0,&
  fdbk)
!!#### PURPOSE
!! Saves residual information, overwriting maximums.

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: iter
REAL(KIND_QDF),INTENT(IN) :: res(:),relres(:)

!!#### REQUIRED OUTPUT
REAL(KIND_QDF),INTENT(INOUT) :: maxval_res,maxval_relres
INTEGER       ,INTENT(INOUT) :: maxloc_res,maxloc_relres,iter_res,iter_relres
REAL(KIND_QDF),INTENT(OUT)   :: maxval_res0,maxval_relres0
INTEGER       ,INTENT(OUT)   :: maxloc_res0,maxloc_relres0

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!--begin--

!! Update the current maximum residual for this iteration.
maxloc_res0    = MAXLOC(res,1)
maxval_res0    = res(maxloc_res0)
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] On iteration number "//TRIM(STR(iter))//&
  ",row "//TRIM(STR(maxloc_res0))//&
  " had the maximum residual of "//TRIM(STR(maxval_res0))//".")
CALL DUMP(fdbk)

!! Update the current maximum relative residual for this iteration.
maxloc_relres0 = MAXLOC(relres,1)
maxval_relres0 = res(maxloc_relres0)
CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] On iteration number "//TRIM(STR(iter))//&
  ",row "//TRIM(STR(maxloc_relres0))//&
  " had the maximum relative residual of "//TRIM(STR(maxval_relres0))//".")
CALL DUMP(fdbk)


!! Check if we have a new maximum residual for this run.
IF( maxval_res0>maxval_res )THEN
 iter_res = iter
 maxloc_res = maxloc_res0
 maxval_res = maxval_res0
 CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] On iteration number "//TRIM(STR(iter))//&
   ",row "//TRIM(STR(maxloc_res))//&
   " achieved a new maximum residual for this run of "//&
   TRIM(STR(maxval_res))//".")
 CALL DUMP(fdbk)
END IF


!! Check if we have a new maximum relative residual for this run.
IF( maxval_relres0>maxval_relres )THEN
 iter_relres = iter
 maxloc_relres = maxloc_relres0
 maxval_relres = maxval_relres0
 CALL UPDATE(fdbk_comment,fdbk,s="[[QDF]] On iteration number "//TRIM(STR(iter))//&
   ",row "//TRIM(STR(maxloc_relres))//&
   " achieved a new maximum relative residual for this run of "//&
   TRIM(STR(maxval_relres))//".")
 CALL DUMP(fdbk)
END IF

!!--end--
END SUBROUTINE


END MODULE




SUBROUTINE MatVec_MSR(n,x,y)
!!#### PURPOSE
!! Provides an external subroutine to
!! do matrix multiply of low order coefficient
!! matrix, y=A*x.

USE USR_SMlib_MSR                                  !!((17-B-USR_SMlib_MSR.f90))
USE VAR_QuasiDiffusion,ONLY: A,KIND_QDF            !!((46-B-VAR_QuasiDiffusion.f90))

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: n
REAL(KIND_QDF),INTENT(IN) :: x(1:n)

!!#### REQUIRED OUTPUT
REAL(KIND_QDF),INTENT(OUT) :: y(1:n)

!!--begin--
y = A*x
!!--end--
END SUBROUTINE


SUBROUTINE PreCond001(n,x)
!!#### PURPOSE
!! Simple diagonal preconditioning.

USE USR_SMlib_MSR                                  !!((17-B-USR_SMlib_MSR.f90))
USE VAR_QuasiDiffusion,ONLY: A,KIND_QDF            !!((46-B-VAR_QuasiDiffusion.f90))

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN)    :: n
REAL(KIND_QDF),INTENT(INOUT) :: x(1:n)
!!--begin--
x = x/A%A(1:A%N)

!!--end--
END SUBROUTINE


