!!# USER MODULE <<USR_TAPACK>>
MODULE USR_TAPACK

!!## PURPOSE
!! The user routines that support TAPACK.

!!## MODULES
USE TBX_Mesh              !!((15-B-TBX_Mesh.f90))
USE USR_fdbk              !!((08-C-USR_fdbk.f90))
USE KND_ScalarFluxes      !!((02-A-KND_ScalarFluxes.f90))
USE KND_AngularFluxes     !!((02-A-KND_AngularFluxes.f90))
USE KND_Currents          !!((02-A-KND_Currents.f90))
USE KND_DiscreteOrdinates !!((02-A-KND_DiscreteOrdinates.f90))
USE LIB_genMoments        !!((13-B-LIB_genMoments.f90))
USE PAR_QuasiDiffusion    !!((05-C-PAR_QuasiDiffusion.f90))
USE USR_QDAnalyticTest    !!((47-C-USR_QDAnalyticTest.f90))
USE KND_IntrinsicTypes    !!((01-A-KND_IntrinsicTypes.f90))
USE USR_Source            !!((35-B-USR_Source.f90))
USE PAR_TAPACK            !!((05-C-PAR_TAPACK.f90))
USE FUN_EXIST             !!((04-B-FUN_EXIST.f90))
USE FUN_Default           !!((04-A-FUN_Default.f90))
USE FUN_STR               !!((05-B-FUN_STR.f90))
USE SUB_CLEAR             !!((04-A-SUB_CLEAR.f90))

!!## DEFAULTS
IMPLICIT NONE
PRIVATE

TYPE TYPE_megatable
 CHARACTER(32)          :: spec
 CHARACTER(32)          :: note
 INTEGER                :: i=0
 INTEGER                :: j=0
 INTEGER                :: k=0
 INTEGER                :: l=0
 INTEGER                :: m=0
 INTEGER                :: g=0
 REAL(KIND_AngularFlux) :: r(3)=(/0.d0,0.d0,0.d0/)
 REAL(KIND_AngularFlux) :: Omega(3)=(/0.d0,0.d0,0.d0/)
 REAL(KIND_AngularFlux) :: E=0.d0
 CHARACTER(32)          :: Region
END TYPE TYPE_megatable


PUBLIC :: UPDATE_HI_ScalarFluxF
PUBLIC :: UPDATE_HI_ScalarFluxC
PUBLIC :: UPDATE_HI_CurrentFN
PUBLIC :: UPDATE_EddingtonF
PUBLIC :: UPDATE_EddingtonC
PUBLIC :: UPDATE_AngularFluxF
PUBLIC :: UPDATE_AngularFluxC
PUBLIC :: TYPE_megatable
PUBLIC :: REALLOCATE_megatable
PUBLIC :: RECONSTRUCT_CurrentC
PUBLIC :: UPDATE_BoundaryFactors
PUBLIC :: UPDATE_Eddingtons
PUBLIC :: CHECK_TotSourcePositive
PUBLIC :: EVAL_BalanceEquation1
PUBLIC :: CHECK_ScalarBalanceEquation
PUBLIC :: UPDATE_CellFunction

!INTERFACE REALLOCATE
! MODULE PROCEDURE REALLOCATE_megatable
!END INTERFACE


!!## CONTAINED PROCEDURES
CONTAINS



SUBROUTINE REALLOCATE_megatable(megatable,dn)
TYPE(TYPE_megatable),POINTER :: megatable(:)
INTEGER,INTENT(IN) :: dn
TYPE(TYPE_megatable),POINTER :: old(:)
INTEGER :: n,i

!!--begin--

IF( .NOT.ASSOCIATED(megatable) )THEN
 ALLOCATE( megatable(dn) )
ELSE
 n = SIZE(megatable)
 ALLOCATE( old(n) )
 DO i=1,n
  old(i) = megatable(i)
  old(i)%spec = megatable(i)%spec
  old(i)%region = megatable(i)%region
  old(i)%note = megatable(i)%note
 END DO
 DEALLOCATE( megatable )

 ALLOCATE(megatable(n+dn))
 DO i=1,n
  megatable(i) = old(i)
  megatable(i)%spec = old(i)%spec
  megatable(i)%region = old(i)%region
  megatable(i)%note = old(i)%note
 END DO
 DEALLOCATE( old )
END IF

!!--end--
END SUBROUTINE




!!### SUBROUTINE: <UPDATE_HI_CurrentFN>
SUBROUTINE UPDATE_HI_CurrentFN( HI_CurrentFN , HI_CurrentV , &
  Mesh , fdbk , varname , key , set_key )

!!#### PURPOSE
!! Update the face-average normal currents.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: HI_CurrentFN(:,:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),POINTER    :: HI_CurrentV(:,:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*) ,INTENT(IN),OPTIONAL :: varname
CHARACTER(32),INTENT(IN),OPTIONAL :: key,set_key

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="HI_CurrentFN"

!!#### LOCAL VARIABLES
CHARACTER(32) :: local_key=""
INTEGER       :: j,g,k
REAL(KIND_AngularFlux),POINTER :: HI_CurrentVN(:,:)

!!--begin--
INCLUDE "48-C-USR_TAPACK__Update.f90.sup"

IF( .NOT.ASSOCIATED(HI_CurrentFN) )THEN
 ALLOCATE( HI_CurrentFN(SIZE(HI_CurrentV,2),NUM_Faces(Mesh)) )
END IF

ALLOCATE( HI_CurrentVN( SIZE(HI_CurrentV,2),SIZE(HI_CurrentV,3) ) )

DO j=1,SIZE(HI_CurrentFN,2)
 DO g=1,SIZE(HI_CurrentFN,1)
  DO k=1,SIZE(HI_CurrentVN,2)
   HI_CurrentVN(g,k) = DOT_PRODUCT( HI_CurrentV(:,g,k) , FaceNormal(Mesh,j) )
  END DO
  HI_CurrentFN(g,j) = FaceAverage( Mesh , j , HI_CurrentVN(g,:) )
 END DO
END DO

DEALLOCATE( HI_CurrentVN )

!!--end--
END SUBROUTINE




!!### SUBROUTINE: <<UPDATE_HI_ScalarFluxF>>
SUBROUTINE UPDATE_HI_ScalarFluxF( HI_ScalarFluxF , HI_ScalarFluxV , &
  Mesh , fdbk , varname , key , set_key )

!!#### PURPOSE
!! Update the face-average values for the HI_ScalarFlux.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: HI_ScalarFluxF(:,:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),POINTER    :: HI_ScalarFluxV(:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*) ,INTENT(IN),OPTIONAL :: varname
CHARACTER(32),INTENT(IN),OPTIONAL :: key,set_key

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="HI_ScalarFluxF"

!!#### LOCAL VARIABLES
CHARACTER(32) :: local_key=""
INTEGER :: j,g,n,k

!!--begin--

INCLUDE "48-C-USR_TAPACK__Update.f90.sup"

IF( .NOT.ASSOCIATED(HI_ScalarFluxF) )THEN
 ALLOCATE( HI_ScalarFluxF(SIZE(HI_ScalarFluxV,1),NUM_Faces(Mesh)) )
 HI_ScalarFluxF = 0.d0
END IF

DO j=1,SIZE(HI_ScalarFluxF,2)
 DO g=1,SIZE(HI_ScalarFluxF,1)
  HI_ScalarFluxF(g,j) = FaceAverage( Mesh , j , HI_ScalarFluxV(g,:) )
 END DO
END DO


!!--end--
END SUBROUTINE



!!### SUBROUTINE <<UPDATE_HI_ScalarFluxC>>
SUBROUTINE UPDATE_HI_ScalarFluxC( HI_ScalarFluxC , HI_ScalarFluxF , HI_ScalarFluxV , &
  Mesh , fdbk , varname , key , set_key )
!!#### PURPOSE
!! Update the cell-average values for the HI_ScalarFluxC.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_ScalarFlux),POINTER :: HI_ScalarFluxC(:,:)

!!#### REQUIRED INPUT
REAL(KIND_ScalarFlux),POINTER :: HI_ScalarFluxF(:,:)
REAL(KIND_ScalarFlux),POINTER :: HI_ScalarFluxV(:,:)
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*) ,INTENT(IN),OPTIONAL :: varname
CHARACTER(32),INTENT(IN),OPTIONAL :: key,set_key

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="HI_ScalarFluxC"

!!#### LOCAL VARIABLES
CHARACTER(32) :: local_key=""
INTEGER :: i,g

!!--begin--
INCLUDE "48-C-USR_TAPACK__Update.f90.sup"

IF( .NOT.ASSOCIATED(HI_ScalarFluxC) )THEN
 ALLOCATE( HI_ScalarFluxC(SIZE(HI_ScalarFluxF,1),NUM_Cells(Mesh)) )
END IF

DO i=1,SIZE(HI_ScalarFluxC,2)
 DO g=1,SIZE(HI_ScalarFluxF,1)
  HI_ScalarFluxC(g,i) = CellAverage( Mesh , i , &
    HI_ScalarFluxV(g,:) , HI_ScalarFluxF(g,:) , Method="LinTri" )
 END DO
END DO

!!--end--
END SUBROUTINE




SUBROUTINE UPDATE_AngularFluxF( AngularFluxF ,&
  AngularFluxV , Mesh , fdbk , varname , key , set_key )
!!#### PURPOSE
!! Update the face average values for the angular Flux.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: AngularFluxF(:,:,:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),POINTER    :: AngularFluxV(:,:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*) ,INTENT(IN),OPTIONAL :: varname
CHARACTER(32),INTENT(IN),OPTIONAL :: key,set_key

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="Face Angular Flux"

!!#### LOCAL VARIABLES
CHARACTER(32) :: local_key=""
INTEGER :: m,j,g

!!--begin--
INCLUDE "48-C-USR_TAPACK__Update.f90.sup"

DO m=1,SIZE(AngularFluxF,3)
 DO j=1,SIZE(AngularFluxF,2)
  DO g=1,SIZE(AngularFluxF,1)
   AngularFluxF(g,j,m) = FaceAverage( Mesh , j , AngularFluxV(g,:,m) )
  END DO
 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_EddingtonF( ExxF,EyyF,ExyF ,&
  Exx,Eyy,Exy , Mesh , fdbk , varname , key , set_key )
!!#### PURPOSE
!! Update the face-average values for the Eddington
!! factors.


!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: ExxF(:,:)
REAL(KIND_AngularFlux),POINTER :: EyyF(:,:)
REAL(KIND_AngularFlux),POINTER :: ExyF(:,:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),POINTER :: Exx(:,:)
REAL(KIND_AngularFlux),POINTER :: Eyy(:,:)
REAL(KIND_AngularFlux),POINTER :: Exy(:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*) ,INTENT(IN),OPTIONAL :: varname
CHARACTER(32),INTENT(IN),OPTIONAL :: key,set_key

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="Face Eddingtons"

!!#### LOCAL VARIABLES
CHARACTER(32) :: local_key=""
INTEGER :: j,g

!!--begin--
INCLUDE "48-C-USR_TAPACK__Update.f90.sup"

DO j=1,SIZE(ExxF,2)
 DO g=1,SIZE(ExxF,1)
  ExxF(g,j) = FaceAverage( Mesh , j , Exx(g,:) )
  EyyF(g,j) = FaceAverage( Mesh , j , Eyy(g,:) )
  ExyF(g,j) = FaceAverage( Mesh , j , Exy(g,:) )
 END DO
END DO

!!--end--
END SUBROUTINE



SUBROUTINE UPDATE_EddingtonC( ExxC,EyyC,ExyC ,&
  ExxF,EyyF,ExyF , ExxV,EyyV,ExyV , Mesh , fdbk , varname , key , set_key )
!!#### PURPOSE
!! Update the face-average values for the Eddington
!! factors.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: ExxC(:,:)
REAL(KIND_AngularFlux),POINTER :: EyyC(:,:)
REAL(KIND_AngularFlux),POINTER :: ExyC(:,:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),POINTER :: ExxF(:,:)
REAL(KIND_AngularFlux),POINTER :: EyyF(:,:)
REAL(KIND_AngularFlux),POINTER :: ExyF(:,:)
REAL(KIND_AngularFlux),POINTER :: ExxV(:,:)
REAL(KIND_AngularFlux),POINTER :: EyyV(:,:)
REAL(KIND_AngularFlux),POINTER :: ExyV(:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*) ,INTENT(IN),OPTIONAL :: varname
CHARACTER(32),INTENT(IN),OPTIONAL :: key,set_key

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="Cell Eddingtons"

!!#### LOCAL VARIABLES
CHARACTER(32) :: local_key=""
INTEGER :: i,g

!!--begin--
INCLUDE "48-C-USR_TAPACK__Update.f90.sup"

DO i=1,SIZE(ExxC,2)
 DO g=1,SIZE(ExxC,1)
  ExxC(g,i) = CellAverage( Mesh , i , ExxV(g,:) , ExxF(g,:) )
  EyyC(g,i) = CellAverage( Mesh , i , EyyV(g,:) , EyyF(g,:) )
  ExyC(g,i) = CellAverage( Mesh , i , ExyV(g,:) , ExyF(g,:) )
 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_AngularFluxC( AngularFluxC ,&
  AngularFluxF , AngularFluxV , Mesh , fdbk , varname , key , set_key )
!!#### PURPOSE
!! Update the cell average values for the angular Flux.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: AngularFluxC(:,:,:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxF(:,:,:)
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxV(:,:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*) ,INTENT(IN),OPTIONAL :: varname
CHARACTER(32),INTENT(IN),OPTIONAL :: key,set_key

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="Cell Angular Flux"

!!#### LOCAL VARIABLES
CHARACTER(32) :: local_key=""
INTEGER :: m,i,g

!!--begin--
INCLUDE "48-C-USR_TAPACK__Update.f90.sup"

DO m=1,SIZE(AngularFluxC,3)
 DO i=1,SIZE(AngularFluxC,2)
  DO g=1,SIZE(AngularFluxC,1)
   AngularFluxC(g,i,m) = CellAverage( Mesh , i , AngularFluxV(g,:,m) , AngularFluxF(g,:,m) )
  END DO
 END DO
END DO

!!--end--
END SUBROUTINE


FUNCTION RECONSTRUCT_CurrentC( Mesh , i , CurrentFN ) RESULT(vecJ)
USE INT_LAPACK2           !!((08-B-INT_LAPACK2.f90))
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
INTEGER,INTENT(IN) :: i
REAL(KIND_Current),INTENT(IN) :: CurrentFN(:)
REAL(KIND_Current) :: vecJ(2)
INTEGER :: j,n,Nn,info
REAL(KIND_MSH) :: FN(2),FC(2),A(4,4),b(4,1),CC(2)
!!--begin--

CC = CellCentroid(Mesh,i)

!!assemble coefficient matrix
Nn = NUM_Faces(Mesh%Cells(i))
DO n=1,Nn
 j = Mesh%Cells(i)%FaceList(n)
 FN = FaceNormal(Mesh,j)
 FC = FaceCentroid(Mesh,ABS(j))-CC
 A(n,1:4) = (/FN(1)*FC(1),FN(2)*FC(2),FN(1),FN(2)/)
 b(n,1)   = CurrentFN( ABS(j) )
END DO

!solve the system
CALL LA_GESV( A(1:Nn,1:4), b(1:Nn,1), INFO=info )
vecJ = (/b(3,1),b(4,1)/)

!!--end--
END FUNCTION


FUNCTION RECONSTRUCT_CurrentC_Approx( Mesh , i , CurrentFN ) RESULT(vecJ)
USE INT_LAPACK2           !!((08-B-INT_LAPACK2.f90))
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
INTEGER,INTENT(IN) :: i
REAL(KIND_Current),INTENT(IN) :: CurrentFN(:)
REAL(KIND_Current) :: vecJ(2)
INTEGER :: j,n,Nn,info
REAL(KIND_MSH) :: FN(2),A(4,2),b(4,1)
!!--begin--

!!assemble coefficient matrix
Nn = NUM_Faces(Mesh%Cells(i))
DO n=1,Nn
 j = Mesh%Cells(i)%FaceList(n)
 FN = FaceNormal(Mesh,j)
 A(n,1:2) = (/FN(1),FN(2)/)
 b(n,1)   = CurrentFN( ABS(j) )
END DO

!solve the system with least squares
CALL LA_GELS( A(1:Nn,1:2), b(1:Nn,1), INFO=info )
vecJ = (/b(3,1),b(4,1)/)

!!--end--
END FUNCTION




SUBROUTINE UPDATE_SecondMoment( Kxx , Kyy , Kxy , AngularFlux , &
  Ordinates , Weights , fdbk , varname , key , set_key )
!!#### PURPOSE
!! Update the second moment factors.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: Kxx(:,:),Kyy(:,:),Kxy(:,:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFlux(:,:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*) ,INTENT(IN),OPTIONAL :: varname
CHARACTER(32),INTENT(IN),OPTIONAL :: key,set_key

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="Second Moment"

!!#### LOCAL VARIABLES
CHARACTER(32)  :: local_key=""
INTEGER        :: n,g,mp,Nn
REAL(KIND_DOR) :: K(6)

!!--begin--
INCLUDE "48-C-USR_TAPACK__Update.f90.sup"

!get number of terms
Nn = SIZE(AngularFlux,2)

!get E
DO n=1,SIZE(AngularFlux,2)
 DO g=1,SIZE(AngularFlux,1)
  K = Moment2( AngularFlux(g,n,:) , Ordinates , Weights )
  Kxx(g,n) = K(1)
  Kyy(g,n) = K(2)
  Kxy(g,n) = K(4) !Kzz=K(3)
 END DO
END DO

!!--end--
END SUBROUTINE



!!### UPDATE SUBROUTINE: UPDATE_Eddingtons
SUBROUTINE UPDATE_Eddingtons( &
  FactorGenerationStyle , &
  FactorEvalMethod , &
  AngularFluxV , &
  AngularFluxF , &
  AngularFluxC , &
  ExxV,EyyV,ExyV,&
  ExxF,EyyF,ExyF,&
  ExxC,EyyC,ExyC,&
  KxxV,KyyV,KxyV,&
  KxxF,KyyF,KxyF,&
  KxxC,KyyC,KxyC,&
  Ordinates,Weights,&
  Mesh,&
  fdbk,&
  DiffusionOverride,EOverride,EOverrideVals)

!!#### PURPOSE
!! Update the Eddington Factor field.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: AngularFluxF(:,:,:)
REAL(KIND_AngularFlux),POINTER :: AngularFluxC(:,:,:)
REAL(KIND_AngularFlux),POINTER :: ExxV(:,:),EyyV(:,:),ExyV(:,:)
REAL(KIND_AngularFlux),POINTER :: ExxF(:,:),EyyF(:,:),ExyF(:,:)
REAL(KIND_AngularFlux),POINTER :: ExxC(:,:),EyyC(:,:),ExyC(:,:)
REAL(KIND_AngularFlux),POINTER :: KxxV(:,:),KyyV(:,:),KxyV(:,:)
REAL(KIND_AngularFlux),POINTER :: KxxF(:,:),KyyF(:,:),KxyF(:,:)
REAL(KIND_AngularFlux),POINTER :: KxxC(:,:),KyyC(:,:),KxyC(:,:)

!!#### REQUIRED INPUT
INTEGER               ,INTENT(IN) :: FactorGenerationStyle
INTEGER               ,INTENT(IN) :: FactorEvalMethod
REAL(KIND_AngularFlux),POINTER    :: AngularFluxV(:,:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
LOGICAL               ,OPTIONAL,INTENT(IN) :: DiffusionOverride
LOGICAL               ,OPTIONAL,INTENT(IN) :: EOverride
REAL(KIND_AngularFlux),OPTIONAL,INTENT(IN) :: EOverrideVals(:)

!!#### LOCAL VARIABLES
LOGICAL :: DiffusionOverride_,EOverride_
INTEGER,SAVE :: entry_num=0
REAL(KIND_MSH) :: fc(2),cc(2)
INTEGER        :: i,j,j_

!!--begin--

DiffusionOverride_ = Default(.FALSE.,DiffusionOverride)
EOverride_         = Default(.FALSE.,EOverride)

!! Override whatever method we chose and set E's to
!! the diffusion coefficients.
IF( EOverride_ )THEN

 ExxV = EOverrideVals(1)
 EyyV = EOverrideVals(2)
 ExyV = EOverrideVals(3)
 ExxF = EOverrideVals(1)
 EyyF = EOverrideVals(2)
 ExyF = EOverrideVals(3)
 ExxC = EOverrideVals(1)
 EyyC = EOverrideVals(2)
 ExyC = EOverrideVals(3)
 RETURN

ELSE IF( DiffusionOverride_ )THEN

 ExxV = 1._KIND_DOR/3._KIND_DOR
 EyyV = 1._KIND_DOR/3._KIND_DOR
 ExyV = 0._KIND_DOR
 ExxF = 1._KIND_DOR/3._KIND_DOR
 EyyF = 1._KIND_DOR/3._KIND_DOR
 ExyF = 0._KIND_DOR
 ExxC = 1._KIND_DOR/3._KIND_DOR
 EyyC = 1._KIND_DOR/3._KIND_DOR
 ExyC = 0._KIND_DOR
 RETURN

END IF


!! Choose an E generation strategy.
SELECT CASE( FactorGenerationStyle )

 CASE(TAP_EvalPsiFirst)
   CALL UPDATE_Eddingtons_PsiFirst( FactorEvalMethod , &
     AngularFluxV , &
     AngularFluxF , &
     AngularFluxC , &
     ExxF,EyyF,ExyF,&
     ExxC,EyyC,ExyC,&
     KxxF,KyyF,KxyF,&
     KxxC,KyyC,KxyC,&
     Ordinates, Weights, &
     Mesh,&
     fdbk)

 CASE(TAP_EvalFactorFirst  )
   CALL UPDATE_Eddingtons_FactorFirst( FactorEvalMethod , &
     KxxV,KyyV,KxyV,&
     ExxV,EyyV,ExyV ,&
     ExxF,EyyF,ExyF,&
     ExxC,EyyC,ExyC,&
     AngularFluxV,&
     Ordinates,Weights,&
     Mesh,&
     fdbk)

 CASE(TAP_PsiKnown)
   CALL UPDATE_Eddingtons_PsiKnown( &
     AngularFluxV , &
     AngularFluxF , &
     AngularFluxC , &
     ExxF,EyyF,ExyF,&
     ExxC,EyyC,ExyC,&
     KxxF,KyyF,KxyF,&
     KxxC,KyyC,KxyC,&
     Ordinates, Weights, &
     Mesh,&
     fdbk)

END SELECT

!!--end--
END SUBROUTINE





SUBROUTINE UPDATE_Eddingtons_PsiFirst( FactorEvalMethod , &
     AngularFluxV , &
     AngularFluxF , &
     AngularFluxC , &
     ExxF,EyyF,ExyF,&
     ExxC,EyyC,ExyC,&
     KxxF,KyyF,KxyF,&
     KxxC,KyyC,KxyC,&
     Ordinates, Weights , &
     Mesh,&
     fdbk)
!!#### PURPOSE
!! Update the Eddington Factor field, using the PsiFirst
!! strategy of determining the field for <Psi> first, and
!! then taking the appropriate moments at those points
!! to evaluate the <E>s.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: AngularFluxF(:,:,:)
REAL(KIND_AngularFlux),POINTER :: AngularFluxC(:,:,:)
REAL(KIND_AngularFlux),POINTER :: ExxF(:,:),EyyF(:,:),ExyF(:,:)
REAL(KIND_AngularFlux),POINTER :: ExxC(:,:),EyyC(:,:),ExyC(:,:)
REAL(KIND_AngularFlux),POINTER :: KxxF(:,:),KyyF(:,:),KxyF(:,:)
REAL(KIND_AngularFlux),POINTER :: KxxC(:,:),KyyC(:,:),KxyC(:,:)

!!#### REQUIRED INPUT
INTEGER               ,INTENT(IN) :: FactorEvalMethod
REAL(KIND_AngularFlux),POINTER    :: AngularFluxV(:,:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_ScalarFlux)  :: PhiF(1:SIZE(AngularFluxF,1),1:SIZE(AngularFluxF,2))
REAL(KIND_ScalarFlux)  :: PhiC(1:SIZE(AngularFluxC,1),1:SIZE(AngularFluxC,2))
INTEGER                :: j,i,g,m

!!--begin--

!update face-average angular flux
CALL UPDATE_AngularFluxF( AngularFluxF , AngularFluxV , &
  Mesh , fdbk , varname="Face Angular Flux" )

!0.5 update face-average eddington factors (using face-average angular Flux)
CALL UPDATE_SecondMoment( &
  KxxF , KyyF , KxyF , &
  AngularFluxF , &
  Ordinates , Weights , &
  fdbk , varname="Face Second Moment" )

!calculate normalization factor
!Moment0 is too slow
!DO j=1,SIZE(AngularFluxF,2)
! DO g=1,SIZE(AngularFluxF,1)
!  PhiF(g,j) = Moment0( AngularFluxF(g,j,:) , Ordinates , Weights )
! END DO
!END DO
PhiF=0.d0
DO m=1,SIZE(AngularFluxF,3)
    DO j=1,SIZE(AngularFluxF,2)
        DO g=1,SIZE(AngularFluxF,1)
            PhiF(g,j) = PhiF(g,j) + AngularFluxF(g,j,m)*Weights(m)
        END DO
    END DO
END DO

!fixup for zero
ExxF = MERGE(KxxF/PhiF,0._KIND_AngularFlux,PhiF/=0._KIND_AngularFlux)
EyyF = MERGE(KyyF/PhiF,0._KIND_AngularFlux,PhiF/=0._KIND_AngularFlux)
ExyF = MERGE(KxyF/PhiF,0._KIND_AngularFlux,PhiF/=0._KIND_AngularFlux)


!0.6 update cell-average angular Flux using face-average
CALL UPDATE_AngularFluxC( AngularFluxC , AngularFluxF , AngularFluxV , &
  Mesh , fdbk , varname="Cell Angular Flux")


!0.7 update cell-average eddington factors (using cell-average angular Flux)
CALL UPDATE_SecondMoment( &
  KxxC , KyyC , KxyC , &
  AngularFluxC , Ordinates, Weights , &
  fdbk , varname="Cell Second Moment")


!calculate the scalar flux at the center
!Too slow
! DO i=1,SIZE(AngularFluxC,2)
!  DO g=1,SIZE(AngularFluxC,1)
!   PhiC(g,i) = Moment0( AngularFluxC(g,i,:) , Ordinates , Weights )
!  END DO
! END DO
PhiC=0.d0
DO m=1,SIZE(AngularFluxC,3)
    DO i=1,SIZE(AngularFluxC,2)
        DO g=1,SIZE(AngularFluxC,1)
            PhiC(g,i) = PhiC(g,i) + AngularFluxC(g,i,m)*Weights(m)
        END DO
    END DO
END DO
!fixup for zeros
ExxC = MERGE(KxxC/PhiC,1._KIND_AngularFlux/3._KIND_AngularFlux,PhiC/=0._KIND_AngularFlux)
EyyC = MERGE(KyyC/PhiC,1._KIND_AngularFlux/3._KIND_AngularFlux,PhiC/=0._KIND_AngularFlux)
ExyC = MERGE(KxyC/PhiC,0._KIND_AngularFlux,PhiC/=0._KIND_AngularFlux)

!!--end--
END SUBROUTINE



! 
! FUNCTION paraface2(y1,y2,yavg,L,xbar) RESULT(PPara)
! !!#### PURPOSE
! !! Construct a piecewise parabolic polynomial interpolant of
! !! two endpoint values and an average value.
! !! same as: PParaInterpolant_Karpov
! 
! !!#### REQUIRED INPUT
! REAL(KIND_Rdp),INTENT(IN) :: y1,y2,yavg,L
! 
! !!#### REQUIRED OUTPUT
! REAL(KIND_Rdp),INTENT(OUT) :: xbar
! REAL(KIND_Rdp) :: PPara(3,2)
! 
! !!#### LOCAL VARIABLES
! REAL(KIND_Rdp) :: ymin,ymax,r,dy,lstarl,lstarr
! LOGICAL :: Noisy_=.FALSE.
! 
! !!--begin--
! 
! xbar = ERROR(xbar)
! PPara = ERROR(xbar)
! 
! 
! ymin = MIN(y1,y2)
! ymax = MAX(y1,y2)
! 
! IF( yavg>=ymax )THEN
!  !possibly non-monotonic (parabola based on ymax only)
!  PPara(1,1) = ymax
!  PPara(2,1) = (-6*ymax + 6*yavg)/(L)
!  PPara(3,1) = ( 6*ymax - 6*yavg)/(L**2)
!  IF( Noisy_ )THEN
!     WRITE(*,*)"WARNING: discontinuous yavg>=ymax interpolant used."
!  END IF
! 
! ELSE IF( yavg<=ymin )THEN
!  !possibly non-monotonic (parabola based on ymin only)
!  PPara(1,1) = ymin
!  PPara(2,1) = (-6*ymin + 6*yavg)/(L)
!  PPara(3,1) = ( 6*ymin - 6*yavg)/(L**2)
!  IF( Noisy_ )THEN
!     WRITE(*,*)"WARNING: discontinuous yavg<=ymin interpolant used."
!  END IF
! 
! ELSE
!  !monotonic
! 
!  !if we have a dy that vanishes
!  dy = y2-y1
! 
!  IF( dy==0._KIND_qmc )THEN
!   PPara(1,1) = y1
!   PPara(2,1) = 0._KIND_qmc
!   PPara(3,1) = 0._KIND_qmc
! 
!  !dy is normal
!  ELSE
! 
!   !get the left lstar
!   lstarl =     3*L*(y2-yavg)/(y2-y1)
!   !get the right lstar
!   lstarr = L - 3*L*(y1-yavg)/(y1-y2)
! 
!   IF( 0._KIND_qmc<lstarl .AND. lstarl<L )THEN
!    !use left piecewise parabolic
!    r = (y2 - y1)/(lstarl**2)     !evaluate at l=lstarl
!    PPara(1,1) = y1               !y1
!    PPara(2,1) = 2*r*lstarl       !2*y2-2*y1
!    PPara(3,1) = -r               !y1-y2
!    PPara(1,2) = y2               !=y2 as it should
!    PPara(2,2) = 0._KIND_qmc
!    PPara(3,2) = 0._KIND_qmc
!    xbar = lstarl
! 
!   ELSE IF( 0._KIND_qmc<lstarr .AND. lstarr<L )THEN
!    !use right piecewise parabolic
!    r = (y1 - y2)/((L-lstarr)**2)
!    PPara(1,1) = y1
!    PPara(2,1) = 0._KIND_qmc
!    PPara(3,1) = 0._KIND_qmc              !evaluate at l=lstarr
!    PPara(1,2) = y1 - r*lstarr**2         !y1 - r*lstarr**2 
!    PPara(2,2) = 2*r*lstarr               !2*r*lstarr**2
!    PPara(3,2) = -r                       !-r*lstarr**2 ---> y1
!    xbar = lstarr                         !at L
!                                          !y1 - r*(L**2-2*lstarr*L+lstarr**2)=y1-r*(L-lstarr)**2 = y2
!   ELSE
!    !use center parabolic
!    PPara(1,1) = y1
!    PPara(2,1) = (-2*y2 - 4*y1 + 6*yavg)/(L)
!    PPara(3,1) = ( 3*y1 + 3*y2 - 6*yavg)/(L**2)
! 
!   END IF
! 
!  END IF
! 
! END IF
! 
! !!--end--
! END FUNCTION
! 

SUBROUTINE paraface(y1,y2,yAvg,L,f0,fx,fx2) 
REAL(8) :: y1,y2,yAvg,L,f0,fx,fx2
  !parabolic interpolant
  fx2 = ( 3*y1 - 6*yAvg + 3*y2 )/L**2
  fx  = (-4*y1 + 6*yAvg - 2*y2 )/L
  f0  = y1
END SUBROUTINE paraface





SUBROUTINE lincell(Mesh,i,yc,yf,f0,fx,fy)
USE FUN_CellGradient_Gauss

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_MSH) ,INTENT(IN) :: yc
REAL(KIND_MSH) ,INTENT(IN) :: yf(:)

!!#### REQUIRED OUTPUT
REAL(KIND_MSH),INTENT(OUT) :: f0,fx,fy

!!#### LOCAL VARIABLES
REAL(KIND_MSH) :: A(2,10),Fa(10)
INTEGER :: Nd,Na,n,j
REAL(KIND_MSH) :: VC,Lin(3)

!!--begin--

Nd = Mesh%NDim
Na = NUM_Faces(Mesh%Cells(i))


DO n=1,Na
 j = Mesh%Cells(i)%FaceList(n)
 A(:,n) = FaceNormal(Mesh,j)*FaceArea(Mesh,ABS(j))
 Fa(n)  = yf(ABS(j))
END DO

Vc = Mesh%Cells(i)%CellVolume

!get the gradient and the function (the special thing about the Gauss approximation
!is that the center value and the average are the same.
Lin(1) = yc
Lin(2:3) = CellGradient_Gauss( Nd , Na , Vc , A(1:Nd,1:Na) , Fa(1:Na) )

f0=Lin(1)
fx=Lin(2)
fy=Lin(3)

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_Eddingtons_PsiKnown( &
     AngularFluxV , &
     AngularFluxF , &
     AngularFluxC , &
     ExxF,EyyF,ExyF,&
     ExxC,EyyC,ExyC,&
     KxxF,KyyF,KxyF,&
     KxxC,KyyC,KxyC,&
     Ordinates, Weights , &
     Mesh,&
     fdbk)
!!#### PURPOSE
!! Update the Eddington Factor field, using the PsiFirst
!! strategy of determining the field for <Psi> first, and
!! then taking the appropriate moments at those points
!! to evaluate the <E>s.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: ExxF(:,:),EyyF(:,:),ExyF(:,:)
REAL(KIND_AngularFlux),POINTER :: ExxC(:,:),EyyC(:,:),ExyC(:,:)
REAL(KIND_AngularFlux),POINTER :: KxxF(:,:),KyyF(:,:),KxyF(:,:)
REAL(KIND_AngularFlux),POINTER :: KxxC(:,:),KyyC(:,:),KxyC(:,:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),POINTER    :: AngularFluxV(:,:,:)
REAL(KIND_AngularFlux),POINTER    :: AngularFluxF(:,:,:)
REAL(KIND_AngularFlux),POINTER    :: AngularFluxC(:,:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
INTEGER                :: m,j,i,g,k,k1,k2,Nk
REAL(KIND_ScalarFlux)  :: KxxV(1:SIZE(AngularFluxV,1),1:SIZE(AngularFluxV,2))
REAL(KIND_ScalarFlux)  :: KyyV(1:SIZE(AngularFluxV,1),1:SIZE(AngularFluxV,2))
REAL(KIND_ScalarFlux)  :: KxyV(1:SIZE(AngularFluxV,1),1:SIZE(AngularFluxV,2))
REAL(KIND_ScalarFlux)  :: PhiV(1:SIZE(AngularFluxV,1),1:SIZE(AngularFluxV,2))
REAL(KIND_ScalarFlux)  :: PhiF(1:SIZE(AngularFluxF,1),1:SIZE(AngularFluxF,2))
REAL(KIND_ScalarFlux)  :: PhiC(1:SIZE(AngularFluxC,1),1:SIZE(AngularFluxC,2))
REAL(KIND_ScalarFlux)  :: kxx_1,kxx_x,kxx_y,kxx_x2
REAL(KIND_ScalarFlux)  :: kyy_1,kyy_x,kyy_y,kyy_x2
REAL(KIND_ScalarFlux)  :: kxy_1,kxy_x,kxy_y,kxy_x2
REAL(KIND_ScalarFlux)  :: phi_1,phi_x,phi_y,phi_x2
REAL(KIND_ScalarFlux)  :: L,x,y
INTEGER :: ix,iy
LOGICAL :: Using_E_As_Function=.FALSE.

!!--begin--

KxxV=0.d0
KxyV=0.d0
KyyV=0.d0
PhiV=0.d0
DO m=1,SIZE(AngularFluxV,3)
    DO k=1,SIZE(AngularFluxV,2)
        DO g=1,SIZE(AngularFluxV,1)
            KxxV(g,k) = KxxV(g,k) + AngularFluxV(g,k,m)*Weights(m)*(Ordinates(1,m)**2)
            KxyV(g,k) = KxyV(g,k) + AngularFluxV(g,k,m)*Weights(m)*(Ordinates(1,m)*Ordinates(2,m))
            KyyV(g,k) = KyyV(g,k) + AngularFluxV(g,k,m)*Weights(m)*(Ordinates(2,m)**2)
            PhiV(g,k) = PhiV(g,k) + AngularFluxV(g,k,m)*Weights(m)
        END DO
    END DO
END DO


KxxF=0.d0
KxyF=0.d0
KyyF=0.d0
PhiF=0.d0
DO m=1,SIZE(AngularFluxF,3)
    DO j=1,SIZE(AngularFluxF,2)
        DO g=1,SIZE(AngularFluxF,1)
            KxxF(g,j) = KxxF(g,j) + AngularFluxF(g,j,m)*Weights(m)*(Ordinates(1,m)**2)
            KxyF(g,j) = KxyF(g,j) + AngularFluxF(g,j,m)*Weights(m)*(Ordinates(1,m)*Ordinates(2,m))
            KyyF(g,j) = KyyF(g,j) + AngularFluxF(g,j,m)*Weights(m)*(Ordinates(2,m)**2)
            PhiF(g,j) = PhiF(g,j) + AngularFluxF(g,j,m)*Weights(m)
        END DO
    END DO
END DO


KxxC=0.d0
KxyC=0.d0
KyyC=0.d0
PhiC=0.d0
DO m=1,SIZE(AngularFluxC,3)
    DO i=1,SIZE(AngularFluxC,2)
        DO g=1,SIZE(AngularFluxC,1)
            KxxC(g,i) = KxxC(g,i) + AngularFluxC(g,i,m)*Weights(m)*(Ordinates(1,m)**2)
            KxyC(g,i) = KxyC(g,i) + AngularFluxC(g,i,m)*Weights(m)*(Ordinates(1,m)*Ordinates(2,m))
            KyyC(g,i) = KyyC(g,i) + AngularFluxC(g,i,m)*Weights(m)*(Ordinates(2,m)**2)
            PhiC(g,i) = PhiC(g,i) + AngularFluxC(g,i,m)*Weights(m)
        END DO
    END DO
END DO


!calculate eddington
DO j=1,SIZE(AngularFluxF,2)
    
    Nk = SIZE(Mesh%Faces(abs(j))%VertList)
    k1 = Mesh%Faces(abs(j))%VertList(1)
    k2 = Mesh%Faces(abs(j))%VertList(Nk)
    L  = FaceArea(Mesh,j)

    DO g=1,SIZE(AngularFluxF,1)
        IF( PhiF(g,j)/=0.0d0 )THEN
            IF( Using_E_As_Function )THEN
            CALL paraface(PhiV(g,k1),PhiV(g,k2),PhiF(g,j),L,phi_1,phi_x,phi_x2) 

            CALL paraface(KxxV(g,k1),KxxV(g,k2),KxxF(g,j),L,kxx_1,kxx_x,kxx_x2) 
            ExxF(g,j) = FaceAverage_F( Mesh , j , exx_face_function )
            
            CALL paraface(KyyV(g,k1),KyyV(g,k2),KyyF(g,j),L,kyy_1,kyy_x,kyy_x2) 
            EyyF(g,j) = FaceAverage_F( Mesh , j , eyy_face_function )
            
            CALL paraface(KxyV(g,k1),KxyV(g,k2),KxyF(g,j),L,kxy_1,kxy_x,kxy_x2) 
            ExyF(g,j) = FaceAverage_F( Mesh , j , exy_face_function )
            ELSE
                ExxF(g,j)=KxxF(g,j)/PhiF(g,j)
                EyyF(g,j)=KyyF(g,j)/PhiF(g,j)
                ExyF(g,j)=KxyF(g,j)/PhiF(g,j)            
            END IF
        ELSE
            ExxF(g,j) = 1._KIND_AngularFlux/3._KIND_AngularFlux
            EyyF(g,j) = 1._KIND_AngularFlux/3._KIND_AngularFlux
            ExyF(g,j) = 0._KIND_AngularFlux
        END IF
    END DO
END DO



!calculate eddington
DO i=1,SIZE(AngularFluxC,2)
    DO g=1,SIZE(AngularFluxC,1)
        IF( PhiC(g,i)/=0.0d0 )THEN
            IF( Using_E_As_Function )THEN
            CALL lincell(Mesh,i,PhiC(g,i),PhiF(g,:),phi_1,phi_x,phi_y)
            
            CALL lincell(Mesh,i,KxxC(g,i),KxxF(g,:),kxx_1,kxx_x,kxx_y)

            if( i==1 )then
                !for fine grid output
                OPEN(11,file='myexx1_fine.dat',status='replace')
                DO ix=0,20
                    DO iy=0,20
                        x=ix/20.d0
                        y=iy/20.d0
                        WRITE(11,"(1x,1Es14.5)",ADVANCE='no')exx_cell_function(x,y)
                    END DO
                    WRITE(11,"(a)",ADVANCE='yes')''
                END DO
                CLOSE(11)
            end if

            ExxC(g,i) = CellAverage_F( Mesh , i , exx_cell_function )
            
            CALL lincell(Mesh,i,KyyC(g,i),KyyF(g,:),kyy_1,kyy_x,kyy_y)
            EyyC(g,i) = CellAverage_F( Mesh , i , eyy_cell_function )

            CALL lincell(Mesh,i,KxyC(g,i),KxyF(g,:),kxy_1,kxy_x,kxy_y)
            ExyC(g,i) = CellAverage_F( Mesh , i , exy_cell_function )
            ELSE
                ExxC(g,i)=KxxC(g,i)/PhiC(g,i)
                EyyC(g,i)=KyyC(g,i)/PhiC(g,i)
                ExyC(g,i)=KxyC(g,i)/PhiC(g,i)
            END IF
        ELSE
            ExxC(g,i) = 1._KIND_AngularFlux/3._KIND_AngularFlux
            EyyC(g,i) = 1._KIND_AngularFlux/3._KIND_AngularFlux
            ExyC(g,i) = 0._KIND_AngularFlux
        END IF
    END DO
END DO


!!--end--
CONTAINS

function exx_cell_function(x,y) result(v)
real(8),intent(in) :: x,y
real(8) :: v
v= ( kxx_1 + kxx_x*x + kxx_y*y ) / ( phi_1 + phi_x*x + phi_y*y ) 
!v=KxxC(g,i)/PhiC(g,i)
end function exx_cell_function

function exy_cell_function(x,y) result(v)
real(8),intent(in) :: x,y
real(8) :: v
v= ( kxy_1 + kxy_x*x + kxy_y*y ) / ( phi_1 + phi_x*x + phi_y*y ) 
!v=KxyC(g,i)/PhiC(g,i)
end function exy_cell_function

function eyy_cell_function(x,y) result(v)
real(8),intent(in) :: x,y
real(8) :: v
v= ( kyy_1 + kyy_x*x + kyy_y*y ) / ( phi_1 + phi_x*x + phi_y*y ) 
!v=KyyC(g,i)/PhiC(g,i)
end function eyy_cell_function


function exx_face_function(x) result(v)
real(8),intent(in) :: x
real(8) :: v
v= ( kxx_1 + kxx_x*x + kxx_x2*(x**2) ) / ( phi_1 + phi_x*x + phi_x2*(x**2) ) 
v=KxxF(g,j)/PhiF(g,j)
end function exx_face_function

function exy_face_function(x,y) result(v)
real(8),intent(in) :: x,y
real(8) :: v
v= ( kxy_1 + kxy_x*x + kxy_x2*(x**2) ) / ( phi_1 + phi_x*x + phi_x2*(x**2) ) 
v=KxyF(g,j)/PhiF(g,j)
end function exy_face_function

function eyy_face_function(x,y) result(v)
real(8),intent(in) :: x,y
real(8) :: v
v= ( kyy_1 + kyy_x*x + kyy_x2*(x**2) ) / ( phi_1 + phi_x*x + phi_x2*(x**2) ) 
v=KyyF(g,j)/PhiF(g,j)
end function eyy_face_function

END SUBROUTINE

! SUBROUTINE UPDATE_Eddingtons_PsiKnown( &
!      AngularFluxV , &
!      AngularFluxF , &
!      AngularFluxC , &
!      ExxF,EyyF,ExyF,&
!      ExxC,EyyC,ExyC,&
!      KxxF,KyyF,KxyF,&
!      KxxC,KyyC,KxyC,&
!      Ordinates, Weights , &
!      Mesh,&
!      fdbk)
! !!#### PURPOSE
! !! Update the Eddington Factor field, using the PsiFirst
! !! strategy of determining the field for <Psi> first, and
! !! then taking the appropriate moments at those points
! !! to evaluate the <E>s.
! 
! !!#### REQUIRED INPUT/OUTPUT
! REAL(KIND_AngularFlux),POINTER :: ExxF(:,:),EyyF(:,:),ExyF(:,:)
! REAL(KIND_AngularFlux),POINTER :: ExxC(:,:),EyyC(:,:),ExyC(:,:)
! REAL(KIND_AngularFlux),POINTER :: KxxF(:,:),KyyF(:,:),KxyF(:,:)
! REAL(KIND_AngularFlux),POINTER :: KxxC(:,:),KyyC(:,:),KxyC(:,:)
! 
! !!#### REQUIRED INPUT
! REAL(KIND_AngularFlux),POINTER    :: AngularFluxV(:,:,:)
! REAL(KIND_AngularFlux),POINTER    :: AngularFluxF(:,:,:)
! REAL(KIND_AngularFlux),POINTER    :: AngularFluxC(:,:,:)
! REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)
! TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
! 
! !!#### OPTIONAL INPUT/OUTPUT
! TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk
! 
! !!#### LOCAL VARIABLES
! INTEGER                :: m,j,i,g
! REAL(KIND_ScalarFlux)  :: PhiF(1:SIZE(AngularFluxF,2),1:SIZE(AngularFluxF,2))
! REAL(KIND_ScalarFlux)  :: PhiC(1:SIZE(AngularFluxC,2),1:SIZE(AngularFluxC,2))
! 
! !!--begin--
! 
! !0.5 update face-average second moments
! ! CALL UPDATE_SecondMoment( &
! !   KxxF , KyyF , KxyF , &
! !   AngularFluxF , &
! !   Ordinates , Weights , &
! !   fdbk , varname="Face Second Moments" )
! KxxF=0.d0
! KxyF=0.d0
! KyyF=0.d0
! DO m=1,SIZE(AngularFluxF,3)
!  DO j=1,SIZE(AngularFluxF,2)
!   DO g=1,SIZE(AngularFluxF,1)
!    KxxF(g,j) = KxxF(g,j) + AngularFluxF(g,j,m)*Weights(m)*(Ordinates(1,m)**2)
!    KxyF(g,j) = KxyF(g,j) + AngularFluxF(g,j,m)*Weights(m)*(Ordinates(1,m)*Ordinates(2,m))
!    KyyF(g,j) = KyyF(g,j) + AngularFluxF(g,j,m)*Weights(m)*(Ordinates(2,m)**2)
!   END DO
!  END DO
! END DO
! 
! PhiF=0.d0
! DO m=1,SIZE(AngularFluxF,3)
!     DO j=1,SIZE(AngularFluxF,2)
!         DO g=1,SIZE(AngularFluxF,1)
!             PhiF(g,j) = PhiF(g,j) + AngularFluxF(g,j,m)*Weights(m)
!         END DO
!     END DO
! END DO
! 
! !calculate eddington
! DO j=1,SIZE(AngularFluxF,2)
!  DO g=1,SIZE(AngularFluxF,1)
!   IF( PhiF(g,j)/=0.0d0 )THEN
!    ExxF(g,j) = (KxxF(g,j))/(PhiF(g,j))
!    EyyF(g,j) = (KyyF(g,j))/(PhiF(g,j))
!    ExyF(g,j) = (KxyF(g,j))/(PhiF(g,j))
!   ELSE
!    ExxF(g,j) = 1._KIND_AngularFlux/3._KIND_AngularFlux
!    EyyF(g,j) = 1._KIND_AngularFlux/3._KIND_AngularFlux
!    ExyF(g,j) = 0._KIND_AngularFlux
!   END IF
!  END DO
! END DO
! 
! !0.6 update cell-average angular Flux using face-average
! !CALL UPDATE_AngularFluxC( AngularFluxC , AngularFluxF , AngularFluxV , &
! !  Mesh , fdbk , varname="Cell Angular Flux")
! 
! 
! !0.7 update cell-average eddington factors (using cell-average angular Flux)
! ! CALL UPDATE_SecondMoment( &
! !   KxxC , KyyC , KxyC , &
! !   AngularFluxC , Ordinates, Weights , &
! !   fdbk , varname="Cell Second Moments")
! 
! KxxC=0.d0
! KxyC=0.d0
! KyyC=0.d0
! DO m=1,SIZE(AngularFluxC,3)
!  DO i=1,SIZE(AngularFluxC,2)
!   DO g=1,SIZE(AngularFluxC,1)
!    KxxC(g,i) = KxxC(g,i) + AngularFluxC(g,i,m)*Weights(m)*(Ordinates(1,m)**2)
!    KxyC(g,i) = KxyC(g,i) + AngularFluxC(g,i,m)*Weights(m)*(Ordinates(1,m)*Ordinates(2,m))
!    KyyC(g,i) = KyyC(g,i) + AngularFluxC(g,i,m)*Weights(m)*(Ordinates(2,m)**2)
!   END DO
!  END DO
! END DO
! 
! PhiC=0.d0
! DO m=1,SIZE(AngularFluxC,3)
!     DO i=1,SIZE(AngularFluxC,2)
!         DO g=1,SIZE(AngularFluxC,1)
!             PhiC(g,i) = PhiC(g,i) + AngularFluxC(g,i,m)*Weights(m)
!         END DO
!     END DO
! END DO
! 
! !calculate eddington
! DO i=1,SIZE(AngularFluxC,2)
!  DO g=1,SIZE(AngularFluxC,1)
!   IF( PhiC(g,i)/=0.0d0 )THEN
!    ExxC(g,i) = (KxxC(g,i))/(PhiC(g,i))
!    EyyC(g,i) = (KyyC(g,i))/(PhiC(g,i))
!    ExyC(g,i) = (KxyC(g,i))/(PhiC(g,i))
!   ELSE
!    ExxC(g,i) = 1._KIND_AngularFlux/3._KIND_AngularFlux
!    EyyC(g,i) = 1._KIND_AngularFlux/3._KIND_AngularFlux
!    ExyC(g,i) = 0._KIND_AngularFlux
!   END IF
!  END DO
! END DO
! 
! !!--end--
! END SUBROUTINE


SUBROUTINE UPDATE_Eddingtons_FactorFirst( FactorEvalMethod , &
     KxxV,KyyV,KxyV,&
     ExxV,EyyV,ExyV ,&
     ExxF,EyyF,ExyF,&
     ExxC,EyyC,ExyC,&
     AngularFluxV,&
     Ordinates,Weights,&
     Mesh,&
     fdbk)
!!#### PURPOSE
!! Update the Eddington Factor field, using the EFirst
!! strategy taking the appropriate moments of <Psi> and
!! then determining <E> at the appropriate points.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: KxxV(:,:),KyyV(:,:),KxyV(:,:)
REAL(KIND_AngularFlux),POINTER :: ExxV(:,:),EyyV(:,:),ExyV(:,:)
REAL(KIND_AngularFlux),POINTER :: ExxF(:,:),EyyF(:,:),ExyF(:,:)
REAL(KIND_AngularFlux),POINTER :: ExxC(:,:),EyyC(:,:),ExyC(:,:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),POINTER   :: AngularFluxV(:,:,:)
INTEGER              ,INTENT(IN) :: FactorEvalMethod
REAL(KIND_DOR)       ,INTENT(IN) :: Ordinates(:,:),Weights(:)
TYPE(TYPE_Mesh)      ,INTENT(IN) :: Mesh

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
REAL(KIND_ScalarFlux) :: Phi
INTEGER :: g,k

!!--begin--

!update Eddington factors
CALL UPDATE_SecondMoment( KxxV , KyyV , &
  KxyV , AngularFluxV , &
  Ordinates, Weights, &
  fdbk , varname="Vertex Second Moments" )

DO g=1,SIZE(AngularFluxV,1)
 DO k=1,SIZE(AngularFluxV,2)
  Phi = Moment0( AngularFluxV(g,k,:),Ordinates,Weights )
  IF( Phi/=0.d0 )THEN
   ExxV(g,k) = (KxxV(g,k))/(Phi)
   EyyV(g,k) = (KyyV(g,k))/(Phi)
   ExyV(g,k) = (KxyV(g,k))/(Phi)
  ELSE
   ExxV(g,k) = 1._KIND_AngularFlux/3._KIND_AngularFlux
   EyyV(g,k) = 1._KIND_AngularFlux/3._KIND_AngularFlux
   ExyV(g,k) = 0._KIND_AngularFlux
  END IF
 END DO
END DO

!update face-average eddingtons from Verts
CALL UPDATE_EddingtonF( ExxF,EyyF,ExyF , ExxV,EyyV,ExyV , &
  Mesh , fdbk , varname="Face Eddingtons" )

!update cell-average eddingtons from verts and faces
CALL UPDATE_EddingtonC( ExxC,EyyC,ExyC , ExxF,EyyF,ExyF , &
 ExxV,EyyV,ExyV , Mesh , fdbk , varname="Cell Eddingtons" )

!!--end--
END SUBROUTINE



SUBROUTINE UPDATE_ScalarFluxIN_PsiFirst( ScalarFluxIN , Jbdry , &
  AngularFluxF , Ordinates , Weights , Mesh , JDomn , LOBC , &
  fdbk , varname )

!!#### PURPOSE
!! Update the incoming scalar Flux.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: ScalarFluxIN(:,:)
INTEGER               ,POINTER :: Jbdry(:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxF(:,:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
INTEGER               ,INTENT(IN) :: JDomn(:)
INTEGER               ,POINTER :: LOBC(:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)

!!#### LOCAL VARIABLES
INTEGER :: m,n,g
INTEGER :: mp,j_,jd
REAL(KIND_DOR) :: Normal(1:NUM_Dimensions(Mesh))

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*),INTENT(IN),OPTIONAL :: varname

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="incoming Scalar Flux"

!!--begin--
CALL UPDATE(fdbk_comment,fdbk,s="[[TAP]] Updating "//TRIM(DEFAULT(varname0,varname)))

!get denomenator factor
DO j_=1,SIZE(Jbdry)

 !get domain face
 jd = Jdomn(j_)

 !get fixed boundary conditions
 IF( ASSOCIATED(LOBC) )THEN

  SELECT CASE( LOBC(jd) )
   CASE( QDF_functionPhi )
    DO g=1,SIZE(ScalarFluxIN,1)
     ScalarFluxIN(g,j_) = EXACT_PhiF(Mesh,Jbdry(j_))
    END DO
    CYCLE
   CASE( QDF_functionMixed )
    DO g=1,SIZE(ScalarFluxIN,1)
     ScalarFluxIN(g,j_) = EXACT_PhiF(Mesh,Jbdry(j_))
    END DO
    CYCLE
  END SELECT

 END IF

 !determine incoming scalar flux
 Normal = FaceNormal( Mesh , Jbdry(j_) )
 DO g=1,SIZE(AngularFluxF,1)
  ScalarFluxIN(g,j_) = Moment0_IN( AngularFluxF(g,Jbdry(j_),:) , &
     Ordinates , Weights , (/Normal,0._KIND_AngularFlux/) )
 END DO

END DO

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_ScalarFluxIN_VFirst( ScalarFluxIN , Jbdry , &
  AngularFluxV , Ordinates , Weights , Mesh , JDomn , LOBC , &
  fdbk , varname )
!!#### PURPOSE
!! Update the incoming scalar Flux.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: ScalarFluxIN(:,:)
INTEGER               ,POINTER :: Jbdry(:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxV(:,:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
INTEGER               ,INTENT(IN) :: JDomn(:)
INTEGER               ,POINTER :: LOBC(:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*),INTENT(IN),OPTIONAL :: varname

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="incoming Scalar Flux"

!!#### LOCAL VARIABLES
REAL(KIND_DOR) :: Normal(1:NUM_Dimensions(Mesh))
INTEGER :: j,jb,jd,k,g,Ng,Nk,Njd,Njb
REAL(KIND_DOR),ALLOCATABLE :: PhiIN(:,:)
!!--begin--
CALL UPDATE(fdbk_comment,fdbk,s="[[TAP]] Updating "//TRIM(DEFAULT(varname0,varname)))

!get number of domain faces
Njd = NUM_DomainFaces(Mesh)
Nk  = NUM_Verts(Mesh)
Njb = SIZE(Jbdry)
Ng  = SIZE(AngularFluxV,1)
ALLOCATE( PhiIN(Ng,Nk) )

!for each domain face
DO jd=1,Njd

 !get normal for this domain face
 Normal = DomainFaceNormal( Mesh , jd )

 !for each boundary vert on that domain
 PhiIN = 0._KIND_AngularFlux
 DO k=1,Nk
  IF( .NOT.IsBoundaryVert(Mesh,k,jd) )CYCLE
  DO g=1,Ng
   PhiIN(g,k) = Moment0_IN( AngularFluxV(g,k,:) , &
        Ordinates , Weights ,(/Normal,0._KIND_AngularFlux/) )
  END DO
 END DO

 !for each boundary face
 DO jb=1,Njb
  j = Jbdry(jb)
  IF( .NOT.IsBoundaryFace(Mesh,j,jd) )CYCLE

  !get from low-order boundary condition
  IF( ASSOCIATED(LOBC) )THEN
   SELECT CASE( LOBC(jd) )
    CASE( QDF_functionPhi )
     DO g=1,Ng
      ScalarFluxIN(g,jb) = EXACT_PhiF(Mesh,j)
     END DO
     CYCLE

    CASE( QDF_functionMixed)
     DO g=1,Ng
      ScalarFluxIN(g,jb) = EXACT_PhiF(Mesh,j)
     END DO
     CYCLE
   END SELECT
  END IF

  !low-order boundary condition not recognized
  DO g=1,Ng
   ScalarFluxIN(g,jb) = FaceAverage(Mesh,j,PhiIN(g,:))
  END DO

 END DO

END DO

DEALLOCATE( PhiIN )

!!--end--
END SUBROUTINE



SUBROUTINE UPDATE_CurrentIN_VFirst( CurrentIN , Jbdry , &
  AngularFluxV , Ordinates , Weights , &
  Mesh , JDomn, LOBC , fdbk , varname )

!!#### PURPOSE
!! Update the Gold'in factors.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: CurrentIN(:,:)
INTEGER               ,POINTER :: Jbdry(:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxV(:,:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
INTEGER               ,INTENT(IN) :: JDomn(:)
INTEGER               ,POINTER :: LOBC(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*),INTENT(IN),OPTIONAL :: varname

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="incoming Current"

!!#### LOCAL VARIABLES
INTEGER :: j,jd,k,g,Ng,Nk,Njd,jb,Njb,m
REAL(KIND_DOR) :: Normal(1:NUM_Dimensions(Mesh))
REAL(KIND_DOR),ALLOCATABLE :: JIN(:,:)

!!--begin--

CALL UPDATE(fdbk_comment,fdbk,s="[[TAP]] Updating "//TRIM(DEFAULT(varname0,varname)))

!get number of domain faces
Njd = NUM_DomainFaces(Mesh)
Nk  = NUM_Verts(Mesh)
Njb = SIZE(Jbdry)
Ng  = SIZE(AngularFluxV,1)
ALLOCATE( JIN(Ng,Nk) )

!for each domain face
DO jd=1,Njd


 !get normal for this domain face
 Normal = DomainFaceNormal( Mesh , jd )

 !for each boundary vert on that domain
 JIN = 0._KIND_AngularFlux
 DO k=1,Nk
  IF( .NOT.IsBoundaryVert(Mesh,k,jd) )CYCLE
  DO g=1,SIZE(AngularFluxV,1)
   JIN(g,k) = Moment1_NormalIN( AngularFluxV(g,k,:) , Ordinates, Weights &
         ,(/Normal,0._KIND_AngularFlux/) )
  END DO

 END DO

 !for each boundary face
 DO jb=1,Njb
  j = Jbdry(jb)
  IF( .NOT.IsBoundaryFace(Mesh,j,jd) )CYCLE

  !try to apply low order boundary condition function
  IF( ASSOCIATED(LOBC) )THEN

   SELECT CASE( LOBC(jd) )

    CASE( QDF_functionJ )
     DO g=1,SIZE(CurrentIN,1)
      CurrentIN(g,jb) = EXACT_JFN(Mesh,j)
     END DO
     CYCLE

    CASE( QDF_functionMixed )
     DO g=1,SIZE(CurrentIN,1)
      CurrentIN(g,jb) = EXACT_JFN(Mesh,j)
     END DO
     CYCLE

   END SELECT

  END IF

  !if we don't have a Low Order Boundary Condition (LOBC)
  !or it wasn't recognized above
  DO g=1,Ng
   CurrentIN(g,jb) = FaceAverage(Mesh,j,JIN(g,:))
  END DO

 END DO

END DO

DEALLOCATE( JIN )

!!--end--
END SUBROUTINE



SUBROUTINE UPDATE_CurrentIN_PsiFirst( CurrentIN , Jbdry , &
  AngularFluxF , Ordinates , Weights , &
  Mesh , JDomn , LOBC , fdbk , varname )
!!#### PURPOSE
!! Update the Gold'in factors.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: CurrentIN(:,:)
INTEGER              ,POINTER :: Jbdry(:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxF(:,:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
INTEGER               ,INTENT(IN) :: JDomn(:)
INTEGER               ,POINTER :: LOBC(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*),INTENT(IN),OPTIONAL :: varname

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="incoming Current"

!!#### LOCAL VARIABLES
INTEGER        :: g,j,jb,jd,m
REAL(KIND_DOR) :: Normal(1:NUM_Dimensions(Mesh))

!!--begin--

CALL UPDATE(fdbk_comment,fdbk,s="[[TAP]] Updating "//TRIM(DEFAULT(varname0,varname)))

!get denomenator factor
DO jb=1,SIZE(Jbdry)

 j  = Jbdry(jb)
 jd = Jdomn(jb)

 !get boundary conditions specified by a function
 IF( ASSOCIATED(LOBC) )THEN

  SELECT CASE( LOBC(jd) )

   CASE( QDF_functionJ )
    DO g=1,SIZE(CurrentIN,1)
     CurrentIN(g,jb) = EXACT_JFN(Mesh,j)
    END DO
    CYCLE

   CASE( QDF_functionMixed )
    DO g=1,SIZE(CurrentIN,1)
     CurrentIN(g,jb) = EXACT_JFN(Mesh,j)
    END DO
    CYCLE
  END SELECT

 END IF

 !boundary conditions not specified or not recognized
 Normal = FaceNormal( Mesh , j )
 DO g=1,SIZE(AngularFluxF,1)
  CurrentIN(g,jb) = Moment1_NormalIN( AngularFluxF(g,j,:) , &
    Ordinates , Weights , Normal )
 END DO

END DO

!!--end--
END SUBROUTINE



SUBROUTINE UPDATE_Goldin_PsiFirst( C , Jbdry , AngularFluxF , &
  Ordinates , Weights , Mesh , fdbk , varname )
!!#### PURPOSE
!! Update the Gold"in factors.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: C(:,:)
INTEGER               ,POINTER :: Jbdry(:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxF(:,:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)


!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*),INTENT(IN),OPTIONAL :: varname

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="Boundary Factors of Gold'in "

!!#### LOCAL VARIABLES
INTEGER :: m,n,g,jb
REAL(KIND_DOR)  :: Normal(1:NUM_Dimensions(Mesh))
REAL(KIND_DOR),ALLOCATABLE :: PhiOUT(:,:),JOUT(:,:)
INTEGER :: j,Ng,Njb

!!--begin--
CALL UPDATE(fdbk_comment,fdbk,s="[[TAP]] updating "//TRIM(DEFAULT(varname0,varname)))

Ng = SIZE(AngularFluxF,1)
Njb = SIZE(Jbdry)

ALLOCATE( JOUT(Ng,Njb) , PhiOUT(Ng,Njb) )

!get Gold"in boundary factors
DO jb=1,Njb

 !get face
 j = JBdry(jb)

 !the surface normals for faces on the boundary are always OUTWARD normals
 Normal = FaceNormal( Mesh , j )

 DO g=1,Ng
  !calculate the 0th moment over outgoing directions
  PhiOUT(g,jb) = Moment0_OUT( AngularFluxF(g,j,:) , &
    Ordinates , Weights , (/Normal,0._KIND_AngularFlux/) )

  !calculate the 1st moment over outgoing directions
  JOUT(g,jb) = Moment1_NormalOUT( AngularFluxF(g,j,:) , &
    Ordinates , Weights , (/Normal,0._KIND_AngularFlux/) )

  !calculate the boundary factor
  IF( PhiOUT(g,jb)/=0.d0 )THEN
   C(g,jb) = (JOUT(g,jb))/(PhiOUT(g,jb))
  ELSE
   C(g,jb) = 0.5_KIND_AngularFlux
  END IF
 END DO

END DO

!!--end--
END SUBROUTINE



SUBROUTINE UPDATE_Goldin_VFirst( C , Jbdry , AngularFluxV , &
  Ordinates , Weights , Mesh , fdbk , varname )
!!#### PURPOSE
!! Update the Gold"in factors.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: C(:,:)
INTEGER               ,POINTER :: Jbdry(:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),INTENT(IN) :: AngularFluxV(:,:,:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
!! * variable names to use instead of default
CHARACTER(*),INTENT(IN),OPTIONAL :: varname

!!#### LOCAL PARAMETERS
!! * this makes maximum varname 50 characters because DEFAULT() function
!!   makes output string length based on the only required input, the
!!   default value (first argument)---not the optional value (second argument)
CHARACTER(50),PARAMETER :: varname0="Boundary Factors of Gold'in "

!!#### LOCAL VARIABLES
INTEGER :: m,n,g,jb
REAL(KIND_DOR)  :: Normal(1:NUM_Dimensions(Mesh))
REAL(KIND_DOR),ALLOCATABLE :: PhiOUT(:,:),JOUT(:,:),BFac(:,:)
INTEGER :: j,Ng,Njb,k,Nk,Njd,jd
REAL(KIND_AngularFlux),ALLOCATABLE :: Test(:,:)
!!--begin--

CALL UPDATE(fdbk_comment,fdbk,s="[[TAP]] Updating "//TRIM(DEFAULT(varname0,varname)))

Ng = SIZE(AngularFluxV,1)
Nk = SIZE(AngularFluxV,2)
Njb = SIZE(Jbdry)
Njd = NUM_DomainFaces(Mesh)

ALLOCATE( JOUT(Ng,Nk) , PhiOUT(Ng,Nk) )
ALLOCATE( BFac(Ng,Nk) )

!for each domain face
DO jd=1,Njd

 !get normal for this domain face
 Normal = DomainFaceNormal( Mesh , jd )

 !for each boundary vert on that domain
 JOUT   = 0._KIND_AngularFlux
 PhiOUT = 0._KIND_AngularFlux
 DO k=1,Nk
  IF( .NOT.IsBoundaryVert(Mesh,k,jd) )CYCLE

  DO g=1,SIZE(AngularFluxV,1)
   PhiOUT(g,k) = Moment0_OUT( AngularFluxV(g,k,:) , &
     Ordinates,Weights,(/Normal,0._KIND_AngularFlux/))
   JOUT(g,k)   = Moment1_NormalOUT( AngularFluxV(g,k,:) , &
     Ordinates,Weights,(/Normal,0._KIND_AngularFlux/))

   IF( PhiOUT(g,k)/=0.d0 )THEN
    BFac(g,k) = JOUT(g,k)/PhiOUT(g,k)
   ELSE
    BFac(g,k) = 0.5_KIND_AngularFlux
   END IF

  END DO



 END DO
 !need to print out PhiOUT,JOUT,BFac on vertices

 !for each boundary face
 DO jb=1,Njb
  j = Jbdry(jb)
  IF( .NOT.IsBoundaryFace(Mesh,j,jd) )CYCLE
  DO g=1,Ng
   C(g,jb) = FaceAverage(Mesh,j,BFac(g,:))
  END DO
 END DO
 !need to print out C on faces
END DO

!wrapup
DEALLOCATE( JOUT,PHIOUT,BFac )

!!--end--
END SUBROUTINE


!!### SUBROUTINE: UPDATE_BoundaryFactors
SUBROUTINE UPDATE_BoundaryFactors( &
  BFGenerationStyle , &
  BFEvalMethod , &
  AngularFluxF , &
  ScalarFluxIN , &
  CurrentIN , &
  C , Jbdry , AngularFluxV , &
  Ordinates, Weights , &
  Mesh , JDomn , LOBC , fdbk , &
  DiffusionOverride,COverride,COverrideVals)

!!#### PURPOSE
!! Update the values of the boundary factors using
!! a specified BFGenerationStyle and BFEvalMethod.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: ScalarFluxIN(:,:)
REAL(KIND_AngularFlux),POINTER :: CurrentIN(:,:)
REAL(KIND_AngularFlux),POINTER :: C(:,:)
INTEGER               ,POINTER :: Jbdry(:)
REAL(KIND_AngularFlux),POINTER :: AngularFluxF(:,:,:)

!!#### REQUIRED INPUT
INTEGER               ,INTENT(IN) :: BFGenerationStyle
INTEGER               ,INTENT(IN) :: BFEvalMethod
REAL(KIND_AngularFlux),POINTER    :: AngularFluxV(:,:,:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
INTEGER               ,INTENT(IN) :: JDomn(:)
INTEGER               ,POINTER    :: LOBC(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
LOGICAL               ,OPTIONAL,INTENT(IN) :: DiffusionOverride
LOGICAL               ,OPTIONAL,INTENT(IN) :: COverride
REAL(KIND_AngularFlux),OPTIONAL,INTENT(IN) :: COverrideVals

!!--begin--

!! Choose a BF generation strategy.
SELECT CASE( BFGenerationStyle )

 CASE(TAP_EvalPsiFirst)
   CALL UPDATE_BF_PsiFirst( BFEvalMethod , &
     AngularFluxV , &
     AngularFluxF , &
     C,&
     ScalarFluxIN,&
     CurrentIN,&
     JBdry,&
     Ordinates,Weights,&
     Mesh,JDomn,LOBC,&
     fdbk,&
     DiffusionOverride,&
     COverride,CoverrideVals)

 CASE(TAP_EvalFactorFirst  )
   CALL UPDATE_BF_FactorFirst( BFEvalMethod , &
     AngularFluxV,&
     C,&
     ScalarFluxIN,&
     CurrentIN,&
     JBdry,&
     Ordinates,Weights,&
     Mesh,JDomn,LOBC,&
     fdbk,&
     DiffusionOverride,&
     COverride,CoverrideVals)

 CASE(TAP_PsiKnown)
   CALL UPDATE_BF_PsiKnown( &
     AngularFluxV , &
     AngularFluxF , &
     C,&
     ScalarFluxIN,&
     CurrentIN,&
     JBdry,&
     Ordinates,Weights,&
     Mesh,JDomn,LOBC,&
     fdbk,&
     DiffusionOverride,&
     COverride,CoverrideVals)


END SELECT

!!--end--
END SUBROUTINE

!##SUBROUTINE: UPDATE_BF_FactorFirst
SUBROUTINE UPDATE_BF_FactorFirst( &
     BFEvalMethod , &
     AngularFluxV,&
     C,ScalarFluxIN,CurrentIN,&
     JBdry,&
     Ordinates,Weights,&
     Mesh,JDomn,LOBC,&
     fdbk,&
     DiffusionOverride,&
     COverride,COverrideVals)
!!#### PURPOSE
!! Update the Boundary Factor field, using the BFFirst
!! strategy of determining the field for <BFs> first, and
!! then averaging them over faces.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: C(:,:)
REAL(KIND_AngularFlux),POINTER :: ScalarFluxIN(:,:)
REAL(KIND_AngularFlux),POINTER :: CurrentIN(:,:)

!!#### REQUIRED INPUT
INTEGER               ,INTENT(IN) :: BFEvalMethod
REAL(KIND_AngularFlux),POINTER    :: AngularFluxV(:,:,:)
INTEGER               ,POINTER    :: Jbdry(:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
INTEGER               ,INTENT(IN) :: JDomn(:)
INTEGER               ,POINTER    :: LOBC(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
LOGICAL       ,INTENT(IN),OPTIONAL :: DiffusionOverride,COverride
REAL(KIND_DOR),INTENT(IN),OPTIONAL :: COverrideVals

!!#### LOCAL VARIABLES
LOGICAL :: DiffusionOverride_,COverride_
INTEGER :: n


!!--begin--

!! 1. calculate incoming scalar Flux
CALL UPDATE_ScalarFluxIN_VFirst( ScalarFluxIN , Jbdry , &
  AngularFluxV , Ordinates , Weights , Mesh , JDomn , LOBC,fdbk )

!! 2. calculate incoming Current
CALL UPDATE_CurrentIN_VFirst( CurrentIN , Jbdry , &
  AngularFluxV , Ordinates , Weights , Mesh , JDomn , LOBC,fdbk )


!! 3. Override whatever method we chose and set E's to
!!    the diffusion coefficients.
DiffusionOverride_ = Default(.FALSE.,DiffusionOverride)
COverride_         = Default(.FALSE.,COverride)

!! 3.1 override the values of C with some user-specified values
IF( COverride_ )THEN

 IF( PRESENT(COverrideVals) )THEN
  C = COverrideVals
 ELSE
  C = 1._KIND_DOR/2._KIND_DOR
 END IF


!! 3.2 override the value by the diffusion value
ELSE IF( DiffusionOverride_ )THEN

 C = 1._KIND_DOR/2._KIND_DOR

!! 3.3 calculate the values of C as per Goldin's method
ELSE

 CALL UPDATE_Goldin_VFirst( C , Jbdry , AngularFluxV , &
  Ordinates , Weights , Mesh , fdbk )

END IF


!!--end--
END SUBROUTINE



!##SUBROUTINE: UPDATE_BF_PsiFirst
SUBROUTINE UPDATE_BF_PsiFirst( BFEvalMethod , &
     AngularFluxV , &
     AngularFluxF , &
     C,&
     ScalarFluxIN,&
     CurrentIN,&
     JBdry,&
     Ordinates,Weights,&
     Mesh,JDomn,LOBC,&
     fdbk,DiffusionOverride,Coverride,CoverrideVals)
!!#### PURPOSE
!! Update the Boundary Factor field, using the PsiFirst
!! strategy of determining the field for <Psi> first, and
!! then taking the appropriate moments at those points
!! to evaluate the <BF>s.

!!#### REQUIRED INPUT/OUTPUT
REAL(KIND_AngularFlux),POINTER :: AngularFluxF(:,:,:)
REAL(KIND_AngularFlux),POINTER :: C(:,:)
REAL(KIND_AngularFlux),POINTER :: ScalarFluxIN(:,:)
REAL(KIND_AngularFlux),POINTER :: CurrentIN(:,:)

!!#### REQUIRED INPUT
INTEGER               ,INTENT(IN) :: BFEvalMethod
REAL(KIND_AngularFlux),POINTER    :: AngularFluxV(:,:,:)
INTEGER               ,POINTER    :: Jbdry(:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
INTEGER               ,INTENT(IN) :: JDomn(:)
INTEGER               ,POINTER :: LOBC(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
LOGICAL       ,INTENT(IN),OPTIONAL :: DiffusionOverride,COverride
REAL(KIND_DOR),INTENT(IN),OPTIONAL :: COverrideVals

!!#### LOCAL VARIABLES
LOGICAL :: DiffusionOverride_,COverride_


!!--begin--
!update face-average angular flux
CALL UPDATE_AngularFluxF( AngularFluxF , AngularFluxV , &
  Mesh , fdbk , varname="Face Angular Flux" )

!0.2 calculate incoming scalar Flux
CALL UPDATE_ScalarFluxIN_PsiFirst( ScalarFluxIN , Jbdry , &
  AngularFluxF , Ordinates , Weights , Mesh , JDomn , LOBC , fdbk )

!0.3 calculate incoming Current
CALL UPDATE_CurrentIN_PsiFirst( CurrentIN , Jbdry , &
  AngularFluxF , Ordinates , Weights , Mesh , JDomn , LOBC , fdbk )

!! 3. Override whatever method we chose and set E's to
!!    the diffusion coefficients.
DiffusionOverride_ = Default(.FALSE.,DiffusionOverride)
COverride_         = Default(.FALSE.,COverride)

!! 3.1 override the values of C with some user-specified values
IF( COverride_ )THEN

 IF( PRESENT(COverrideVals) )THEN
  C = COverrideVals
 ELSE
  C = 1._KIND_DOR/2._KIND_DOR
 END IF


!! 3.2 override the value by the diffusion value
ELSE IF( DiffusionOverride_ )THEN

 C = 1._KIND_DOR/2._KIND_DOR

!! 3.3 calculate the values of C as per Goldin's method
ELSE

 !0.4 update Goldin boundary factors
CALL UPDATE_Goldin_PsiFirst( C , Jbdry , AngularFluxF , &
  Ordinates , Weights , Mesh , fdbk )

END IF


!!--end--
END SUBROUTINE



!##SUBROUTINE: UPDATE_BF_PsiKnown
SUBROUTINE UPDATE_BF_PsiKnown( &
     AngularFluxV , &
     AngularFluxF , &
     C,ScalarFluxIN,CurrentIN,&
     JBdry,&
     Ordinates,Weights,&
     Mesh,JDomn,LOBC,&
     fdbk,DiffusionOverride,Coverride,CoverrideVals)
!!#### PURPOSE
!! Update the Boundary Factor field, using known angular fluxes.

!!#### REQUIRED INPUT/OUTPUT

REAL(KIND_AngularFlux),POINTER :: C(:,:)
REAL(KIND_AngularFlux),POINTER :: ScalarFluxIN(:,:)
REAL(KIND_AngularFlux),POINTER :: CurrentIN(:,:)

!!#### REQUIRED INPUT
REAL(KIND_AngularFlux),POINTER    :: AngularFluxF(:,:,:)
REAL(KIND_AngularFlux),POINTER    :: AngularFluxV(:,:,:)
INTEGER               ,POINTER    :: Jbdry(:)
REAL(KIND_DOR)        ,INTENT(IN) :: Ordinates(:,:),Weights(:)
TYPE(TYPE_Mesh)       ,INTENT(IN) :: Mesh
INTEGER               ,INTENT(IN) :: JDomn(:)
INTEGER               ,POINTER :: LOBC(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_Fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### OPTIONAL INPUT
LOGICAL       ,INTENT(IN),OPTIONAL :: DiffusionOverride,COverride
REAL(KIND_DOR),INTENT(IN),OPTIONAL :: COverrideVals

!!#### LOCAL VARIABLES
LOGICAL :: DiffusionOverride_,COverride_

!!--begin--

!0.2 calculate incoming scalar Flux
CALL UPDATE_ScalarFluxIN_PsiFirst( ScalarFluxIN , Jbdry , &
  AngularFluxF , Ordinates , Weights , Mesh , JDomn , LOBC , fdbk )

!0.3 calculate incoming Current
CALL UPDATE_CurrentIN_PsiFirst( CurrentIN , Jbdry , &
  AngularFluxF , Ordinates , Weights , Mesh , JDomn , LOBC , fdbk )

!! 3. Override whatever method we chose and set E's to
!!    the diffusion coefficients.
DiffusionOverride_ = Default(.FALSE.,DiffusionOverride)
COverride_         = Default(.FALSE.,COverride)

!! 3.1 override the values of C with some user-specified values
IF( COverride_ )THEN

 IF( PRESENT(COverrideVals) )THEN
  C = COverrideVals
 ELSE
  C = 1._KIND_DOR/2._KIND_DOR
 END IF


!! 3.2 override the value by the diffusion value
ELSE IF( DiffusionOverride_ )THEN

 C = 1._KIND_DOR/2._KIND_DOR

!! 3.3 calculate the values of C as per Goldin's method
ELSE

 !0.4 update Goldin boundary factors
CALL UPDATE_Goldin_PsiFirst( C , Jbdry , AngularFluxF , &
  Ordinates , Weights , Mesh , fdbk )

END IF


!!--end--
END SUBROUTINE


!!### SUBROUTINE <<CHECK_TotSourcePositive>>
SUBROUTINE CHECK_TotSourcePositive(Mesh,TotSourceCellFunction,fdbk)
!!#### PURPOSE
!! Check the total source for positivity.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_Rdp) ,INTENT(IN) :: TotSourceCellFunction(:,:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk

!!#### LOCAL VARIABLES
INTEGER,POINTER :: VertSet(:)
INTEGER         :: i,g,n,k
REAL(KIND_Rdp)  :: CC(Mesh%NDim),V(Mesh%NDim)
REAL(KIND_Rdp)  :: TotSource

!!--begin--

NULLIFY( VertSet )
CALL UPDATE(fdbk_comment,fdbk,s="[[TAP]] Checking positivity of the source...")

DO i=1,NUM_Cells(Mesh)
 CC = CellCentroid(Mesh,i)
 DO g=1,SIZE(TotSourceCellFunction,2)

  CALL GET_CellVertBoundarySet(Mesh,i,VertSet)

  DO n=1,SIZE(VertSet)

   k = VertSet(n)
   V = Vert(Mesh,k)

   TotSource = EVAL_SourceFunction( TotSourceCellFunction(:,g,i) , V-CC )

   IF( TotSource<0._KIND_Rdp )THEN
    CALL UPDATE(fdbk_warning,fdbk,s="[[TAP]] CHECK_TotSourcePositive::&
      & [test=FAIL] for cell [i="//TRIM(STR(i))//"] vertex&
      & [k="//TRIM(STR(k))//"] at location&
      & [R="//coords( V )//"].")
   END IF

  END DO

  CALL CLEAR(VertSet)

 END DO
END DO

!!--end--
END SUBROUTINE




SUBROUTINE CHECK_ScalarBalanceEquation(Mesh,&
  ScalarFluxC,CurrentFN,TotSourceCellFunction,MacT,l_,fdbk,&
  reltol,abstol,Tests,caller)
USE LIB_Norm              !!((04-B-LIB_Norm.f90))

TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_AngularFlux) :: CurrentFN(:,:) !CurrentF(:,:,:)
REAL(KIND_AngularFlux) :: ScalarFluxC(:,:),TotSourceCellFunction(:,:,:),MacT(:,:)
INTEGER                :: l_(:)
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
CHARACTER(2),INTENT(IN),OPTIONAL :: Tests(:)
REAL(KIND_AngularFlux),INTENT(IN),OPTIONAL :: reltol,abstol
CHARACTER(*),INTENT(IN),OPTIONAL :: caller

!!#### LOCAL VARIABLES
INTEGER :: g,i,n,j
LOGICAL :: Pass
REAL(KIND_AngularFlux) :: reltol_,abstol_
CHARACTER(2)           :: Tests_(6)
CHARACTER(2),PARAMETER :: ALL_TESTS(6)=(/"Ai","A1","A2","Ri","R1","R2"/)
REAL(KIND_AngularFlux) :: tol,result
CHARACTER(26) :: var,caller_
INTEGER :: N1,N2
REAL(KIND_AngularFlux),ALLOCATABLE :: maxavg(:,:),abserr(:,:)

!!--begin--
caller_ = DEFAULT("[[TAP]]",caller)

IF( PRESENT(Tests) )THEN
 DO n=1,MAX(SIZE(Tests),6)
  Tests_(n) = Tests(n)
 END DO
ELSE
 Tests_ = ALL_TESTS
END IF

reltol_ = Default(1.d-10,reltol)
abstol_ = Default(1.d-16,abstol)
N1=SIZE(ScalarFluxC,1)
N2=SIZE(ScalarFluxC,2)
ALLOCATE( abserr(N1,N2) )
ALLOCATE( maxavg(N1,N2) )

Pass = .TRUE.
DO g=1,N1
 DO i=1,N2
   abserr(g,i) = EVAL_BalanceEquation1(Mesh,i,ScalarFluxC(g,i),&
       CurrentFN(g,:),TotSourceCellFunction(:,g,i),MacT(g,l_(i)),&
       maxavg=maxavg(g,i))
 END DO
END DO

DO n=1,6
 IF( Tests_(n)=="  " )EXIT
 SELECT CASE(Tests_(n))
  CASE("Ai") ; tol = abstol
               result = NormInfty(abserr)
               var="abs. error (infinity norm)"
  CASE("A1") ; tol = abstol
               result = NormEll1(abserr)
               var="abs. error (L1 norm)"
  CASE("A2") ; tol = abstol
               result = NormEll2(abserr)
               var="abs. error (L2 norm)"
  CASE("Ri") ; tol = reltol
               result = NormInfty(abserr/(maxavg+1.d-20))
               var="rel. error (infinity norm)"
  CASE("R1") ; tol = reltol
               result = NormEll1(abserr/(1.d-20+NormEll1(maxavg)))
               var="rel. error (L1 norm)"
               Pass=result<=reltol_
  CASE("R2") ; tol = reltol
               result = NormEll2(abserr/(1.d-20+NormEll2(maxavg)))
               var="rel. error (L2 norm)"
               Pass=result<=reltol_
 END SELECT
 Pass=result<=tol

 CALL UpdateAndDump(fdbk_comment,fdbk,s=TRIM(caller_)//" Scalar flux cell &
   &balance check "//TRIM(var)//" [result="//TRIM(STR(result,"(Es8.2)"))//"] &
   &using [tol="//TRIM(STR(tol,"(Es8.2)"))//"] we have &
   &[test="//MERGE("PASS","FAIL",Pass)//"].")

END DO


!!--end--
END SUBROUTINE



FUNCTION EVAL_BalanceEquation1(Mesh,i,&
  PhiC,JFN,QFunc,MacT,relerr,maxavg) RESULT(Balance)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER :: i
REAL(KIND_AngularFlux) :: JFN(:) !JF(:,:)
REAL(KIND_AngularFlux) :: PhiC,MacT
REAL(KIND_AngularFlux) :: AreaVec(Mesh%NDim),Vol
REAL(KIND_AngularFlux) :: Balance,QFunc(:),FBal
REAL(KIND_AngularFlux),INTENT(OUT),OPTIONAL :: relerr,maxavg
INTEGER :: n,j
REAL(KIND_AngularFlux) :: maxavg_,Q

!!--begin--
Vol     = CellVolume(Mesh,i)
Balance = 0._KIND_AngularFlux
maxavg_ = 0._KIND_AngularFlux

DO n=1,NUM_Faces_CellIndex(Mesh,i)
 j = Mesh%Cells(i)%FaceList(n)
 !AreaVec  = FaceNormal(Mesh,j)*FaceArea(Mesh,ABS(j))
 !FBal = DOT_PRODUCT(AreaVec,JF(:,ABS(j)))
 !change to just need face-average normal component
 IF( j>0 )THEN
  FBal = JFN(j)*FaceArea(Mesh,j)
 ELSE
  FBal = -JFN(ABS(j))*FaceArea(Mesh,ABS(j))
 END IF

 Balance = Balance + FBal
 IF( PRESENT(relerr).OR. PRESENT(maxavg) )THEN
  maxavg_ = MAX(maxavg_,ABS(FBal))
 END IF
END DO

Q = EVAL_SourceAverage(Mesh,i,QFunc)
Balance = Balance + PhiC*MacT*Vol - Q*Vol

maxavg_ = MAXVAL( ABS( (/PhiC,Q,maxavg_/) ) )
IF( maxavg_==0._KIND_AngularFlux )THEN
 maxavg_=TINY(0._KIND_AngularFlux)
END IF

IF( PRESENT(relerr) )THEN
 relerr = ABS(Balance)/maxavg_
END IF
IF( PRESENT(maxavg) )THEN
 maxavg = maxavg_
END IF

!--end--
END FUNCTION



SUBROUTINE UPDATE_CellFunction(Mesh,&
  ScalarFluxC,ScalarFluxF,&
  ScalarFluxCellFunction,Method,NonlinearFixup,fdbk)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxF(:,:)
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxC(:,:)
REAL(KIND_MSH),INTENT(OUT) :: ScalarFluxCellFunction(:,:,:)
CHARACTER(*),INTENT(IN) :: Method,NonlinearFixup
TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
INTEGER :: Norder
CHARACTER(64) :: Method_,NonlinearFixup_

!!--begin--

IF( Mesh%NDim/=2 )THEN
 CALL UpdateAndDump(fdbk_error,fdbk,s="The only number of dimensions&
  &supported is 2.")
END IF

Method_ = Method
NonlinearFixup_ = NonlinearFixup

Norder = SIZE(ScalarFluxCellFunction,1)
IF( Norder==1 )THEN
 Method_ = "Flat"
ELSE IF( Norder/=3 )THEN

 CALL UpdateAndDump(fdbk_error,fdbk,s="In UPDATE_CellFunction &
  &only a linear expansion is supported")

ELSE

 SELECT CASE(TRIM(Method_))
  CASE("Flat"); CALL UPDATE_CellFunction_Flat(Mesh,&
                      ScalarFluxC,ScalarFluxF,&
                      ScalarFluxCellFunction)
  CASE("LLS")  ; CALL UPDATE_CellFunction_LLS(Mesh,&
                      ScalarFluxC,ScalarFluxF,&
                      ScalarFluxCellFunction)
  CASE("Gauss"); CALL UPDATE_CellFunction_Gauss(Mesh,&
                      ScalarFluxC,ScalarFluxF,&
                      ScalarFluxCellFunction)
 END SELECT

 SELECT CASE(TRIM(NonlinearFixUp_))
  CASE("Tau"); CALL FIXUP_CellFunction_Tau(Mesh,ScalarFLuxC,ScalarFluxF,&
                    ScalarFluxCellFunction)
  CASE("TauN"); CALL FIXUP_CellFunction_TauN(Mesh,ScalarFLuxC,ScalarFluxF,&
                    ScalarFluxCellFunction)
  CASE("TauDN"); CALL FIXUP_CellFunction_TauDN(Mesh,ScalarFLuxC,ScalarFluxF,&
                    ScalarFluxCellFunction)
  CASE("OI3"); CALL FIXUP_CellFunction_OI3(Mesh,ScalarFLuxC,ScalarFluxF,&
                    ScalarFluxCellFunction)
  CASE("Switch"); CALL FIXUP_CellFunction_Switch(Mesh,ScalarFLuxC,ScalarFluxF,&
                    ScalarFluxCellFunction)
  CASE("Min0"); CALL FIXUP_CellFunction_Min0(Mesh,ScalarFLuxC,ScalarFluxF,&
                    ScalarFluxCellFunction)
 END SELECT

END IF

!!--end--
END SUBROUTINE


SUBROUTINE FIXUP_CellFunction_Tau(Mesh,&
  ScalarFluxC,ScalarFluxF,&
  ScalarFluxCellFunction)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxF(:,:)
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxC(:,:)
REAL(KIND_MSH),INTENT(INOUT) :: ScalarFluxCellFunction(:,:,:)
INTEGER :: Ng,i,g
INTEGER,PARAMETER :: p=2
REAL(KIND_MSH) :: tau,measure,rdim,last2,last3
INTEGER :: count=0
!!--begin--
count=count+1
!nonlinear coarse-mesh fix-up
Ng = SIZE(ScalarFluxC,1)
rdim = 1.d0/REAL(Mesh%Ndim,KIND_MSH)
DO i=1,NUM_Cells(Mesh)

 measure = CellVolume(Mesh,i)**(rdim)

 DO g=1,Ng

  tau = 1.d0/(1.d0 + &
        (measure*ScalarFluxCellFunction(1,g,i))**p + &
        (measure*ScalarFluxCellFunction(2,g,i))**p)

  last2=ScalarFluxCellFunction(2,g,i)
  last3=ScalarFluxCellFunction(3,g,i)

  ScalarFluxCellFunction(1,g,i) = (1.d0-tau)*ScalarFluxC(g,i) + &
                                  (     tau)*ScalarFluxCellFunction(1,g,i)
  ScalarFluxCellFunction(2:3,g,i) = (tau)*ScalarFluxCellFunction(2:3,g,i)
  WRITE(79,'(2i5,f15.6,es23.16,4es12.4)')count,i,measure,tau,&
       last2,ScalarFluxCellFunction(2,g,i),last3,ScalarFluxCellFunction(3,g,i)
 END DO
END DO

!!--end--
END SUBROUTINE



SUBROUTINE FIXUP_CellFunction_TauN(Mesh,&
  ScalarFluxC,ScalarFluxF,&
  ScalarFluxCellFunction)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxF(:,:)
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxC(:,:)
REAL(KIND_MSH),INTENT(INOUT) :: ScalarFluxCellFunction(:,:,:)
INTEGER :: Ng,i,g
INTEGER,PARAMETER :: p=2
REAL(KIND_MSH) :: tau,mx,my,rdim,last2,last3,fx,fy
INTEGER :: count=0
!!--begin--
count=count+1
!nonlinear coarse-mesh fix-up
Ng = SIZE(ScalarFluxC,1)
rdim = 1.d0/REAL(Mesh%Ndim,KIND_MSH)
DO i=1,NUM_Cells(Mesh)

 mx = CellVolume(Mesh,i)**(rdim)
 my=mx
 DO g=1,Ng
  fx=mx*ScalarFluxCellFunction(2,g,i)/ScalarFluxCellFunction(1,g,i)
  fy=my*ScalarFluxCellFunction(3,g,i)/ScalarFluxCellFunction(1,g,i)
  tau = 1.d0/(1.d0 + fx**p + fy**p )

  last2=ScalarFluxCellFunction(2,g,i)
  last3=ScalarFluxCellFunction(3,g,i)

  ScalarFluxCellFunction(1,g,i) = (1.d0-tau)*ScalarFluxC(g,i) + &
                                  (     tau)*ScalarFluxCellFunction(1,g,i)
  ScalarFluxCellFunction(2:3,g,i) = (tau)*ScalarFluxCellFunction(2:3,g,i)
  WRITE(79,'(2i5,f15.6,es23.16,4es12.4)')count,i,mx,tau,&
       last2,ScalarFluxCellFunction(2,g,i),last3,ScalarFluxCellFunction(3,g,i)
 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE FIXUP_CellFunction_TauDN(Mesh,&
  ScalarFluxC,ScalarFluxF,&
  ScalarFluxCellFunction)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxF(:,:)
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxC(:,:)
REAL(KIND_MSH),INTENT(INOUT) :: ScalarFluxCellFunction(:,:,:)
INTEGER :: Ng,i,g
INTEGER,PARAMETER :: p=2
REAL(KIND_MSH) :: tau,mx,my,rdim,last2,last3,fx,fy
REAL(KIND_MSH) :: minx,miny,maxx,maxy,V(2),C(2)
INTEGER,POINTER :: VertSet(:)
INTEGER :: count=0
INTEGER :: k_,k
!!--begin--
count=count+1
!nonlinear coarse-mesh fix-up
Ng = SIZE(ScalarFluxC,1)
rdim = 1.d0/REAL(Mesh%Ndim,KIND_MSH)
DO i=1,NUM_Cells(Mesh)

 NULLIFY( VertSet )
 CALL GET_CellVertBoundarySet(Mesh,i,VertSet)
 minx=HUGE(1.d0)
 miny=HUGE(1.d0)
 maxx=-HUGE(1.d0)
 maxy=-HUGE(1.d0)
 DO k_=1,SIZE(Vertset)
   k = VertSet(k_)
   V = Vert(Mesh,k)
   maxx = MAX(maxx,V(1))
   minx = MIN(minx,V(1))
   maxy = MAX(maxy,V(2))
   miny = MIN(miny,V(2))
 END DO
 DEALLOCATE( VertSet )
 mx=maxx-minx
 my=maxy-miny

 DO g=1,Ng
  fx=mx*ScalarFluxCellFunction(2,g,i)/ScalarFluxCellFunction(1,g,i)
  fy=my*ScalarFluxCellFunction(3,g,i)/ScalarFluxCellFunction(1,g,i)
  tau = 1.d0/(1.d0 + fx**p + fy**p )

  last2=ScalarFluxCellFunction(2,g,i)
  last3=ScalarFluxCellFunction(3,g,i)

  ScalarFluxCellFunction(1,g,i) = (1.d0-tau)*ScalarFluxC(g,i) + &
                                  (     tau)*ScalarFluxCellFunction(1,g,i)
  ScalarFluxCellFunction(2:3,g,i) = (tau)*ScalarFluxCellFunction(2:3,g,i)
  WRITE(79,'(2i5,2f15.6,es23.16,4es12.4)')count,i,mx,my,tau,&
       last2,ScalarFluxCellFunction(2,g,i),last3,ScalarFluxCellFunction(3,g,i)
 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE FIXUP_CellFunction_oi3(Mesh,&
  ScalarFluxC,ScalarFluxF,&
  ScalarFluxCellFunction)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxF(:,:)
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxC(:,:)
REAL(KIND_MSH),INTENT(INOUT) :: ScalarFluxCellFunction(:,:,:)
INTEGER :: Ng,i,g
INTEGER,PARAMETER :: r=4
REAL(KIND_MSH),PARAMETER :: eps=1.d-6
REAL(KIND_MSH) :: hx,hy,V1(2),V2(2),w0,w1,epsr,denom,OI3
INTEGER,POINTER :: VertSet(:)
INTEGER :: k1,k2,k1_,k2_
!!--begin--

!nonlinear coarse-mesh fix-up
Ng = SIZE(ScalarFluxC,1)
DO i=1,NUM_Cells(Mesh)
 NULLIFY( VertSet )
 CALL GET_CellVertBoundarySet(Mesh,i,VertSet)
 hx=0.d0
 hy=0.d0
 DO k1_=1,SIZE(Vertset)
  DO k2_=1,SIZE(VertSet)
   IF( k1_==k2_ )CYCLE
   k1 = VertSet(k1_)
   k2 = VertSet(k2_)
   V1 = Vert(Mesh,k1)
   V2 = Vert(mesh,k2)
   hx = MAX(V1(1)-V2(1),hx)
   hy = MAX(V1(2)-V2(2),hy)
  END DO
 END DO
 DEALLOCATE( VertSet )

 DO g=1,Ng
  OI3 = SQRT(CellVolume(Mesh,i)*((ScalarFluxCellFunction(2,g,i)/hx)**2 + &
                                 (ScalarFluxCellFunction(3,g,i)/hy)**2 ) )
  epsr = eps**(-r)
  denom = (eps+OI3)**(-r) + epsr
  w0 = epsr/denom
  w1 = (eps+OI3)**(-r)/denom

  ScalarFluxCellFunction(:,g,i) = w0*ScalarFluxC(g,i) + &
                                  w1*ScalarFluxCellFunction(:,g,i)
 END DO

END DO

!!--end--
END SUBROUTINE


!revert to flat if any vert is negative
SUBROUTINE FIXUP_CellFunction_Switch(Mesh,&
  ScalarFluxC,ScalarFluxF,&
  ScalarFluxCellFunction)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxF(:,:)
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxC(:,:)
REAL(KIND_MSH),INTENT(INOUT) :: ScalarFluxCellFunction(:,:,:)
INTEGER :: Ng,i,g
REAL(KIND_MSH) :: c(2),v(2)
INTEGER,POINTER :: VertSet(:)
INTEGER :: k
REAL(KIND_MSH) :: min_val,eval_vert
!!--begin--


!nonlinear coarse-mesh fix-up
Ng = SIZE(ScalarFluxC,1)
DO i=1,NUM_Cells(Mesh)
 NULLIFY( VertSet )
 CALL GET_CellVertBoundarySet(Mesh,i,VertSet)
 c=CellCentroid(Mesh,i)
 DO g=1,Ng
    min_val=0.d0
    DO k=1,SIZE(Vertset)
    v=Vert(Mesh,VertSet(k))
    eval_vert=ScalarFluxCellFunction(1,g,i)+&
                ScalarFluxCellFunction(2,g,i)*(v(1)-c(1))+&            
                ScalarFluxCellFunction(3,g,i)*(v(2)-c(2))
    min_val=MIN(eval_vert,min_val)
    END DO
    IF( min_val<0.d0 )THEN
        ScalarFluxCellFunction(2:3,g,i)=0.d0
        ScalarFluxCellFunction(1,g,i)=ScalarFluxC(g,i)
    END IF
 END DO
 DEALLOCATE(VertSet)
END DO

!!--end--
END SUBROUTINE


!make most negative vert 0, without changing average
SUBROUTINE FIXUP_CellFunction_min0(Mesh,&
  ScalarFluxC,ScalarFluxF,&
  ScalarFluxCellFunction)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxF(:,:)
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxC(:,:)
REAL(KIND_MSH),INTENT(INOUT) :: ScalarFluxCellFunction(:,:,:)
INTEGER :: Ng,i,g
REAL(KIND_MSH) :: a,b,c,cc(2),v(2),d
INTEGER,POINTER :: VertSet(:)
INTEGER :: k
REAL(KIND_MSH) :: min_val,eval_vert
!!--begin--


!nonlinear coarse-mesh fix-up
Ng = SIZE(ScalarFluxC,1)
DO i=1,NUM_Cells(Mesh)
 NULLIFY( VertSet )
 CALL GET_CellVertBoundarySet(Mesh,i,VertSet)
 cc=CellCentroid(Mesh,i) 
 DO g=1,Ng
    min_val=0.d0
    a=ScalarFluxCellFunction(2,g,i)
    b=ScalarFluxCellFunction(3,g,i)
    c=ScalarFluxCellFunction(1,g,i)
    DO k=1,SIZE(Vertset)
        v=Vert(Mesh,VertSet(k))
        eval_vert=a*(v(1)-cc(1))+b*(v(2)-cc(2))+c
        min_val=MIN(eval_vert,min_val)
    END DO
    IF( min_val<0.d0 )THEN
        d=-c/(min_val-c)
        ScalarFluxCellFunction(2,g,i)=a*d
        ScalarFluxCellFunction(3,g,i)=b*d
    END IF
 END DO
 DEALLOCATE(VertSet)
END DO

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_CellFunction_LLS(Mesh,&
  ScalarFluxC,ScalarFluxF,&
  ScalarFluxCellFunction)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxF(:,:)
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxC(:,:)
REAL(KIND_MSH),INTENT(OUT) :: ScalarFluxCellFunction(:,:,:)
INTEGER :: Ng,i,g

!!--begin--

Ng = SIZE(ScalarFluxC,1)
DO i=1,NUM_Cells(Mesh)
 DO g=1,Ng
  ScalarFluxCellFunction(:,g,i) = &
    LinearCellFunction_LLS(Mesh,i,ScalarFluxC(g,i),ScalarFluxF(g,:))
 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_CellFunction_Flat(Mesh,&
  ScalarFluxC,ScalarFluxF,&
  ScalarFluxCellFunction)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxF(:,:)
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxC(:,:)
REAL(KIND_MSH),INTENT(OUT) :: ScalarFluxCellFunction(:,:,:)
INTEGER :: Ng,i,g

!!--begin--

Ng = SIZE(ScalarFluxC,1)
DO i=1,NUM_Cells(Mesh)
 DO g=1,Ng
  ScalarFluxCellFunction(1,g,i) = ScalarFluxC(g,i)
  ScalarFluxCellFunction(2,g,i) = 0._KIND_Msh
  ScalarFluxCellFunction(3,g,i) = 0._KIND_Msh
 END DO
END DO

!!--end--
END SUBROUTINE



SUBROUTINE UPDATE_CellFunction_Gauss(Mesh,&
  ScalarFluxC,ScalarFluxF,&
  ScalarFluxCellFunction)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxF(:,:)
REAL(KIND_MSH),INTENT(IN)  :: ScalarFluxC(:,:)
REAL(KIND_MSH),INTENT(OUT) :: ScalarFluxCellFunction(:,:,:)
INTEGER :: Ng,i,g

!!--begin--

Ng = SIZE(ScalarFluxC,1)
DO i=1,NUM_Cells(Mesh)
 DO g=1,Ng
  ScalarFluxCellFunction(:,g,i) = &
    LinearCellFunction_Gauss(Mesh,i,ScalarFluxC(g,i),ScalarFluxF(g,:))
 END DO
END DO

!!--end--
END SUBROUTINE


END MODULE
