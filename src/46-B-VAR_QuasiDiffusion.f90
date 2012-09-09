!!# MODULE <<VAR_QuasiDiffusion>>
MODULE VAR_QuasiDiffusion

!!## PURPOSE
!! Variables for the quasi-diffusion toolbox.

!!## EXTERNAL KINDS
USE KND_MoCshort                   !!((03-A-KND_MoCshort.f90))
USE KND_QuasiDiffusion             !!((02-A-KND_QuasiDiffusion.f90))
USE KND_Mesh                       !!((05-B-KND_Mesh.f90))
USE KND_ScalarFluxes               !!((02-A-KND_ScalarFluxes.f90))

!!## EXTERNAL PARAMETERS
USE PAR_Units                      !!((02-A-PAR_Units.f90))
USE PAR_QuasiDiffusion             !!((05-C-PAR_QuasiDiffusion.f90))

!!## GLOBAL USER MODULES
USE USR_Mesh                       !!((14-B-USR_Mesh.f90))
USE USR_SMlib_Band_LU              !!((17-B-USR_SMlib_Band_LU.f90))
USE USR_SMlib_MSR                  !!((17-B-USR_SMlib_MSR.f90))
USE USR_SMlib_LU_CSR_MSR           !!((18-B-USR_SMlib_LU_CSR_MSR.f90))
USE USR_SMlib_Iteration_Defaults   !!((17-B-USR_SMlib_Iteration_Defaults.f90))
USE TBX_Mesh,ONLY: TYPE_Interfaces !!((15-B-TBX_Mesh.f90))


!!## DEFAULT ACCESS
PUBLIC


!!## GLOBAL VARIABLES
!! * sparase matrix in Modified-Storage-Row format, <A>, to use
!!   to solve linear system of QDF equations, $Ax=b$.
!! * vector of unknowns, <x>
!! * vector of RHS, <b>
!! * band-LU factorization, <LU>
!! * preconditions, <PC>
TYPE(MSR)                :: A
REAL(KIND_QDF)  ,POINTER :: x(:) => NULL()
REAL(KIND_QDF)  ,POINTER :: b(:) => NULL()
TYPE(LU_CSR_MSR)         :: PC
!
!! * low-order mesh
TYPE(TYPE_Mesh),POINTER :: LO_Mesh=>NULL()
!
!! * face mapping from high to low-order and back
INTEGER,POINTER :: FaceMapping(:)=>NULL()
!
!! * list of boundary faces <Jbdry>
!! * list of interior faces <Jinte>
!! * list of domain faces <Jdomn>
INTEGER,POINTER :: Jbdry(:) => NULL()
INTEGER,POINTER :: Jinte(:) => NULL()
INTEGER,POINTER :: Jdomn(:) => NULL()
!! * offsets
INTEGER,POINTER :: offsets(:)=>NULL()
!
!! * which formulation of the system of equations to use
INTEGER         :: System = QDF_System02
!
!! * solver to use
INTEGER         :: Solver = QDF_Iterative
!
!! * subspace dimension (GMRES only), SMLIB => <k>
!! * level of fill in for the iterative solvers, SMLIB => <p>
!! * initialize iterative solver solution by value in <x>, SMLIB => <init>
!! * tolerance, SMLIB => <tol>
!! * minimum number of iterations, SMLIB => <min_it>
!! * maximum number of iterations, SMLIB => <max_it>
!! * convergence history, SMLIB => <history>
!! * number of iterations used, SMLIB => <no_of_its>
!! * amount of info to print, SMLIB => <info>
!! * residual norm, SMLIB => <residual_norm>
INTEGER        :: SubSpaceDimension  = 10
INTEGER        :: FillIn             = 0
LOGICAL        :: Initialize         = .TRUE.
REAL(KIND_QDF) :: Tolerance          = 1.0E-5_KIND_QDF
INTEGER        :: MinIterations      = 1
INTEGER        :: Info               = SMLIB_ALL
INTEGER        :: MaxIterations      = 20
TYPE(ConvHist) :: ConvergenceHistory
INTEGER        :: NumberOfIterations
REAL(KIND_QDF) :: ResidualNorm
!
!! * subsolver to use (only for Solver=QDF_Iterative)
INTEGER         :: SubSolver = QDF_SPARSKIT_BiCGstab
!
!! * number of equations
!! * maximum number of nonzero elements in sparse matrx
INTEGER         :: NQDF,NZMAX
!
!! * residuals
REAL(KIND_QDF),POINTER :: relres(:),res(:)
REAL(KIND_QDF)         :: residmat
INTEGER                :: maxloc_relres,iter_relres
INTEGER                :: maxloc_res,iter_res
REAL(KIND_QDF)         :: maxval_relres,maxval_res
INTEGER                :: maxloc_relres0,maxloc_res0
REAL(KIND_QDF)         :: maxval_relres0,maxval_res0
!
!! * variables for <TBX_QuasiDiffusion>
INTEGER       :: Nj=-1,Nk=-1,Ni=-1,Njbdry=-1
!
!! * whether to interactively display information
LOGICAL       :: Interactive = .FALSE.
!! * whether or not the run is noisy
LOGICAL       :: Noisy = .FALSE.
!! * file/unit to dump noise to
CHARACTER(80) :: File_noisy = ""
INTEGER       :: Unit_noisy = window_unit
!! * whether to output coefficient matrix
LOGICAL :: OutputCoefficientMatrix = .FALSE.
LOGICAL :: PrintTestFunctionals = .FALSE.
!
!! * external sources
REAL(KIND_QDF),POINTER :: AvgExtSource(:,:)=>NULL()
REAL(KIND_QDF),POINTER :: FaceAvgExtSource1(:,:,:)=>NULL()
!
!! * interface variables for low-order/high-order mesh mapping
TYPE(TYPE_Interfaces),POINTER :: LO_Interfaces(:)=>NULL()
!
!! * reordering strategies and iterative convergence table
CHARACTER(32),POINTER :: IterTable(:,:)=>NULL()
INTEGER      ,POINTER :: ReorderStrategies(:)=>NULL()
!
!! * sparskit options variables
INTEGER        :: SPARSKIT_lfil      = -HUGE(1)
REAL(KIND_QDF) :: SPARSKIT_droptol   = -HUGE(1._KIND_QDF)
INTEGER        :: SPARSKIT_niwk      = -HUGE(1)
INTEGER        :: SPARSKIT_nkryss    = -HUGE(1)
INTEGER        :: SPARSKIT_maxmatvec = -HUGE(1)
!
!! * options for iterative procedure
LOGICAL         :: DiffusionOverride(2)
LOGICAL         :: EOverride(2)
REAL(KIND_QDF)  :: EOverrideVals(3)
LOGICAL         :: COverride(2)
REAL(KIND_QDF)  :: COverrideVals
INTEGER         :: MaxLOIterations=10000
!
!! * whether to automatically attempt to fix the iteration process
LOGICAL         :: Using_AutoIterFix = .FALSE.
!
!! * hanging-node mesh interface handling
INTEGER         :: InterfaceType=QDF_SC_plus_WF
!
!! * half-cell shapes
INTEGER         :: HalfCellShape=QDF_FaceBisect
!
!! * low order boundary conditions boundary conditions <LOBC(1:Njd)>
!! * fixed boundary condition values, <FixBdryVals(1:Njd)>
INTEGER,POINTER :: LOBC(:)=>NULL()
REAL(KIND_QDF),POINTER :: FixBdryVals(:)=>NULL()
!
!! * dump transport data before each iteration
LOGICAL :: DumpTransportData=.FALSE.
!
!! * print the residuals of the low order solve out
LOGICAL :: PrintResiduals=.FALSE.
!
!! * cell scalar flux function options
INTEGER :: CellFunctionMethod = QDF_LINEAR_GAUSS
INTEGER :: NonlinearFixup     = QDF_NONE

!ilut params
LOGICAL :: StandardScaling=.TRUE.
LOGICAL :: PlotSparsityPattern=.FALSE.
LOGICAL :: PlotPrecSparsityPattern=.FALSE.
LOGICAL :: EvaluateConditionNumber=.FALSE.
LOGICAL :: Use_RowScaling=.FALSE.
LOGICAL :: Use_ColScaling=.FALSE.
INTEGER :: rownorm=1,colnorm=1
CHARACTER(7) :: PrecSpec="ILUT"

END MODULE
