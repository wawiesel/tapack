!!# MODULE <<USR_Source>>
MODULE USR_Source

!!## PURPOSE
!! The source user-routine repository.


!!## EXTERNAL KINDS
USE KND_Mesh,ONLY: KIND_MSH                  !!((05-B-KND_Mesh.f90))


!!## EXTERNAL PARAMETERS
USE KND_Source,ONLY: KIND_ExtSource          !!((02-A-KND_Source.f90))
USE KND_AngularFluxes,ONLY: KIND_AngularFlux !!((02-A-KND_AngularFluxes.f90))


!!## USER MODULES
!! * feedback user module
USE USR_fdbk                                 !!((08-C-USR_fdbk.f90))
USE USR_Mesh                                 !!((14-B-USR_Mesh.f90))
USE TBX_Mesh                                 !!((15-B-TBX_Mesh.f90))
USE SUB_CLEAR                                !!((04-A-SUB_CLEAR.f90))
USE SUB_CLEARn                               !!((04-A-SUB_CLEARn.f90))

!!## MODULES
USE FUN_Default                              !!((04-A-FUN_Default.f90))
USE FUN_xyEVAL                               !!((05-A-FUN_xyEVAL.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## TYPE DECLARATION
TYPE TYPE_Source
 REAL(KIND_ExtSource),POINTER :: strength0(:)   => NULL() !constant component
 REAL(KIND_ExtSource),POINTER :: strength1(:,:) => NULL() !linear component
 INTEGER                      :: shape          =  0
 REAL(KIND_MSH)      ,POINTER :: coeff(:,:)     => NULL()
ENDTYPE


!!## INTERFACE for Reallocate
INTERFACE Reallocate
 MODULE PROCEDURE Reallocate_Source
END INTERFACE

INTERFACE UPDATE_TotSource
 MODULE PROCEDURE UPDATE_TotSource_1Mom
 MODULE PROCEDURE UPDATE_TotSource_NMom
END INTERFACE

!!## EXTERNAL PARAMETERS
INTEGER,PARAMETER :: LEN_source=12


!!## PUBLIC ACCESS LIST
PUBLIC :: TYPE_Source
PUBLIC :: Reallocate_Source
PUBLIC :: KIND_ExtSource
PUBLIC :: LEN_Source
PUBLIC :: UPDATE_TotSource
PUBLIC :: UPDATE_TotSource_1Mom
PUBLIC :: UPDATE_TotSource_NMom
PUBLIC :: EVAL_SourceAverage
PUBLIC :: EVAL_SourceFunction


!!## CONTAINED PROCEDURES
CONTAINS



!!### SUBROUTINE <Reallocate_Source>
SUBROUTINE Reallocate_Source( Source , dn , NG , Nd )

!!#### PURPOSE
!! Reallocate the source type plus the
!! strength component.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Source),POINTER :: Source(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: dn
INTEGER,INTENT(IN) :: NG
INTEGER,OPTIONAL,INTENT(IN) :: Nd

!!#### LOCAL VARIABLES
TYPE(TYPE_Source),POINTER :: COPY_Source(:)
INTEGER :: Nd_
INTEGER :: i

!!--begin--

!nullify
NULLIFY( COPY_Source )

!number of dimensions of linear part of source
Nd_ = DEFAULT(3,Nd)

!point COPY_Source to old source
IF( ASSOCIATED(Source) )THEN
 COPY_Source => Source
END IF

!point old source to null
Source => NULL()

!if it was associated then reallocate
IF( ASSOCIATED(COPY_Source) )THEN

 !allocate new source
 ALLOCATE( Source(1:dn+SIZE(COPY_Source)) )

 !copy loop
 DO i=1,SIZE(COPY_Source)
  Source(i) = COPY_Source(i)
 END DO
 !destroy old source
 NULLIFY( COPY_Source )

 !new data loop allocation
 DO i=SIZE(COPY_Source)+1,SIZE(Source)
  ALLOCATE( Source(i)%strength0(1:NG) )
  Source(i)%strength0 = 0
  ALLOCATE( Source(i)%strength1(Nd_,1:NG) )
  Source(i)%strength1 = 0
 END DO

ELSE

 !allocate the source
 ALLOCATE( Source(1:dn) )

 !allocate the source strength
 DO i=1,dn
  ALLOCATE( Source(i)%strength0(1:NG) )
  Source(i)%strength0 = 0
  ALLOCATE( Source(i)%strength1(Nd_,1:NG) )
  Source(i)%strength1 = 0
 END DO

END IF

!!--end--
END SUBROUTINE



FUNCTION EVAL_SourceAverage(Mesh,i,SourceCellFunction0) RESULT(AvgSource)
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: i
REAL(KIND_AngularFlux),INTENT(IN) :: SourceCellFunction0(:) !(index)
REAL(KIND_AngularFlux) :: AvgSource
!!--begin--
IF( SIZE(SourceCellFunction0)==1 )THEN
 AvgSource = SourceCellFunction0(1)
ELSE IF( SIZE(SourceCellFunction0)==3 )THEN
 AvgSource = CellAverage_Lin(Mesh,i,SourceCellFunction0(:))
ELSE
 WRITE(*,*)"ERROR in EVAL_AverageSource: must have flat or linear source."
 STOP
END IF
!!--end--
END FUNCTION


FUNCTION EVAL_SourceFunction(SourceCellFunction0,r) RESULT(Source)
REAL(KIND_AngularFlux),INTENT(IN) :: SourceCellFunction0(:) !(index)
REAL(KIND_AngularFlux),INTENT(IN) :: r(:)
REAL(KIND_AngularFlux) :: Source
!!--begin--
IF( SIZE(SourceCellFunction0)==1 )THEN
 Source = SourceCellFunction0(1)
ELSE IF( SIZE(SourceCellFunction0)==3 )THEN
 Source = xyEVAL_Lin2P( SourceCellFunction0 , r )
ELSE
 WRITE(*,*)"ERROR in EVAL_SourceFunction: must have flat or linear source."
 STOP
END IF
!!--end--
END FUNCTION


SUBROUTINE UPDATE_TotSource_NMom( Mesh,Ng,ExtSourceCellFunction , &
  Coeff_Moment , MaterialMap , Moment , TotSourceCellFunction )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: Ng
REAL(KIND_AngularFlux),INTENT(IN) :: ExtSourceCellFunction(:,:,:) !(index,g,i)
REAL(KIND_AngularFlux),INTENT(IN) :: Coeff_Moment(:,:,:) !(g,l,moment)
INTEGER               ,INTENT(IN) :: MaterialMap(:) !l=MaterialMap(i)
REAL(KIND_AngularFlux),INTENT(IN) :: Moment(:,:,:,:) !(index,g,i,moment)
REAL(KIND_AngularFlux),POINTER :: TotSourceCellFunction(:,:,:) !(index,g,i)

INTEGER :: mom,i,g,index
!!--begin--

IF( SIZE(TotSourceCellFunction)/=SIZE(Moment(:,:,:,1)) )THEN
 CALL CLEAR( TotSourceCellFunction )
END IF

IF( .NOT.ASSOCIATED(TotSourceCellFunction) )THEN
 ALLOCATE( TotSourceCellFunction(SIZE(Moment,1),SIZE(Moment,2),SIZE(Moment,3)) )
END IF

TotSourceCellFunction = REAL(0,KIND(TotSourceCellFunction))

DO mom=1,SIZE(Coeff_Moment,3)

 DO i=1,NUM_Cells(Mesh)
  DO g=1,Ng
   DO index=1,SIZE(Moment,1)
    TotSourceCellFunction(index,g,i) = TotSourceCellFunction(index,g,i) + &
       Coeff_Moment(g,MaterialMap(i),mom)*Moment(index,g,i,mom) + ExtSourceCellFunction(index,g,i)
   END DO
  END DO
 END DO

END DO

!!--end--
END SUBROUTINE


SUBROUTINE UPDATE_TotSource_1Mom( Mesh,Ng,ExtSourceCellFunction , &
  Coeff_Moment , MaterialMap , Moment , TotSourceCellFunction )
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
INTEGER        ,INTENT(IN) :: Ng
REAL(KIND_AngularFlux),INTENT(IN) :: ExtSourceCellFunction(:,:,:) !(index,g,i)
REAL(KIND_AngularFlux),INTENT(IN) :: Coeff_Moment(:,:) !(g,l)
INTEGER               ,INTENT(IN) :: MaterialMap(:) !l=MaterialMap(i)
REAL(KIND_AngularFlux),INTENT(IN) :: Moment(:,:,:) !(index,g,i)
REAL(KIND_AngularFlux),POINTER :: TotSourceCellFunction(:,:,:) !(index,g,i)

INTEGER :: i,g,index
!!--begin--

IF( .NOT.ASSOCIATED(TotSourceCellFunction) )THEN
 ALLOCATE( TotSourceCellFunction(SIZE(Moment,1),SIZE(Moment,2),SIZE(Moment,3)) )
END IF

IF( SIZE(TotSourceCellFunction)/=SIZE(Moment(:,:,:)) )THEN
 CALL CLEARn( TotSourceCellFunction )
 ALLOCATE( TotSourceCellFunction(SIZE(Moment,1),SIZE(Moment,2),SIZE(Moment,3)) )
END IF

TotSourceCellFunction = REAL(0,KIND(TotSourceCellFunction))

DO i=1,NUM_Cells(Mesh)
 DO g=1,Ng
  DO index=1,SIZE(Moment,1)
   TotSourceCellFunction(index,g,i) = TotSourceCellFunction(index,g,i) + &
     Coeff_Moment(g,MaterialMap(i))*Moment(index,g,i) + ExtSourceCellFunction(index,g,i)
  END DO
 END DO
END DO


!!--end--
END SUBROUTINE




END MODULE
