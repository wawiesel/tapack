!!# USER MODULE <<USR_ExplicitCell>>
MODULE USR_ExplicitCell

!!## PURPOSE
!! Provides a user module for basic operations on ExplicitCell
!! types.

!!## EXTERNAL KINDS
USE KND_Mesh                  !!((05-B-KND_Mesh.f90))

!!## EXTERNAL PARAMETERS
USE PAR_ComputationalGeometry !!((02-A-PAR_ComputationalGeometry.f90))

!!## GLOBAL TOOLBOXES
USE TBX_ComputationalGeometry !!((09-A-TBX_ComputationalGeometry.f90))

!!## USER MODULES
USE USR_Face                  !!((11-B-USR_Face.f90))
USE USR_Cell                  !!((12-B-USR_Cell.f90))
USE USR_ExplicitFace          !!((12-B-USR_ExplicitFace.f90))
USE USR_fdbk                  !!((08-C-USR_fdbk.f90))

!!## EXTERNAL PROCEDURES
USE FUN_Reorder               !!((05-A-FUN_Reorder.f90))
USE FUN_Reverse               !!((04-A-FUN_Reverse.f90))
USE FUN_Error                 !!((04-A-FUN_Error.f90))
USE FUN_NewUnit               !!((04-B-FUN_NewUnit.f90))
USE FUN_Default               !!((04-A-FUN_Default.f90))
USE FUN_STR                   !!((05-B-FUN_STR.f90))
USE FUN_IsError
USE ISO_varying_string        

!!## GLOBAL LIBRARIES
USE LIB_Prompts               !!((06-B-LIB_Prompts.f90))
USE LIB_GenericPhrases        !!((07-B-LIB_GenericPhrases.f90))
USE LIB_ALLOCATE_Shape        !!((03-A-LIB_ALLOCATE_Shape.f90))

!!## GLOBAL PRINTING
USE PRN_Text                  !!((07-B-PRN_Text.f90))



!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE

!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_ ="USR_ExplicitCell"
CHARACTER(*),PARAMETER :: file_="13-B-USR_ExplicitCell.f90"
PRIVATE :: mod_,file_



!!## TYPE DEFINITION for explicit cells
TYPE TYPE_ExplicitCell

 PRIVATE

 INTEGER       ,POINTER :: NDim      => NULL()
 INTEGER       ,POINTER :: CellShape => NULL()
 REAL(KIND_MSH),POINTER :: coeff(:)  => NULL()

 LOGICAL :: tmp

END TYPE

INTERFACE CLEAN
 MODULE PROCEDURE CLEAN_ExplicitCell
END INTERFACE

INTERFACE CLEAN_Xc
 MODULE PROCEDURE CLEAN_ExplicitCell
END INTERFACE

INTERFACE IsQs
 MODULE PROCEDURE IsQs_Xc
END INTERFACE

INTERFACE IsQr
 MODULE PROCEDURE IsQr_Xc
END INTERFACE

INTERFACE IsQg
 MODULE PROCEDURE IsQg_Xc
END INTERFACE

INTERFACE ExplicitCell
 MODULE PROCEDURE ExplicitCell_
END INTERFACE

INTERFACE NUM_Dimensions
 MODULE PROCEDURE NUM_Dimensions_Xc
END INTERFACE

INTERFACE ASSIGNMENT(=)
 MODULE PROCEDURE ASSIGN_Xc
END INTERFACE

INTERFACE OPERATOR(==)
 MODULE PROCEDURE EQUALITY_Xc
END INTERFACE

!!## TYPE
PUBLIC :: TYPE_ExplicitCell

!!## ASSIGNMENT
PUBLIC :: ASSIGNMENT(=)

!!## EQUALITY TEST
PUBLIC :: OPERATOR(==)

!!## PARAMETERS
PUBLIC :: KEY_CellShape
PUBLIC :: Qs_,Qr_,Qg_,Tr_,Pg_

!!## GEOMETRY ROUTINES
PUBLIC :: Interior_XcP
PUBLIC :: Interior_XcPs
PUBLIC :: Centroid_Xc
PUBLIC :: Volume_Xc
PUBLIC :: POLYGON_Xc

!!## CONSTRUCTOR
PUBLIC :: ExplicitCell

!!## INQUIRY FUNCTIONS
PUBLIC :: IsQs,IsQr,IsQg
PUBLIC :: IsApprox      ,IsApprox_Xf2
PUBLIC :: IsSamePlane   ,IsSamePlane_Xf2
PUBLIC :: NUM_Dimensions,NUM_Dimensions_Xc
PUBLIC :: NUM_Points

!!## DESTRUCTOR
PUBLIC :: CLEAN , CLEAN_Xc



CONTAINS



SUBROUTINE ASSIGN_Xc( Xc_a , Xc_b )
!!#### PURPOSE
!! Assign one cell to another.

!!#### REQUIRED ASSIGNMENT VARIABLES
TYPE(TYPE_ExplicitCell),INTENT(INOUT) :: Xc_a
TYPE(TYPE_ExplicitCell),INTENT(IN)    :: Xc_b

!!--begin--

CALL CLEAN_Xc( Xc_a , .false. )

ALLOCATE( Xc_a%NDim , Xc_a%CellShape , Xc_a%coeff(SIZE(Xc_b%coeff)) )

Xc_a%NDim      = Xc_b%NDim
Xc_a%CellShape = Xc_b%CellShape
Xc_a%coeff     = Xc_b%coeff

CALL CLEAN_Xc( Xc_b , .true. )

!!--end--
END SUBROUTINE


PURE ELEMENTAL FUNCTION EQUALITY_Xc( Xc_a , Xc_b ) &
  RESULT(EQUALITY)
!!#### PURPOSE
!! Strict equality of two explicit cells.

!!#### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Xc_a
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Xc_b

!!#### REQUIRED OUTPUT
LOGICAL :: EQUALITY

!!--begin--

EQUALITY = .FALSE.

IF( Xc_a%CellShape==Xc_b%CellShape )THEN
 IF( Xc_a%NDim==Xc_b%CellShape )THEN
  IF( ALL(Xc_a%coeff==Xc_b%coeff) )THEN
   EQUALITY = .TRUE.
  END IF
 END IF
END IF

!!--end--
END FUNCTION



PURE FUNCTION NDIM_Xc(Coord,Xc) RESULT(NDIM)
!!###### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Xc
INTEGER,INTENT(IN) :: Coord
!!###### REQUIRED OUTPUT
INTEGER :: NDIM
CHARACTER(*),PARAMETER :: proc_="NDIM_Xc"
!!--begin--
SELECT CASE(Coord)

 CASE(CGE_REC)

  SELECT CASE( Xc%CellShape )
   CASE(Tr_) ; NDIM = SIZE(Xc%coeff)/3
   CASE(Qs_,Qr_,Qg_) ; NDIM = SIZE(Xc%coeff)/4
   !HACK ALERT
   CASE(Pg_)        ; NDIM = 2
   CASE DEFAULT
     NDIM=0
     !CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
     !  " The CellShape was not recognized!",LineIndents=(/7,7/))

  END SELECT

END SELECT

END FUNCTION


FUNCTION CENTROID_Xc( Xc , AREA ) RESULT( P )
!!###### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Xc
REAL(KIND_MSH)         ,INTENT(IN) :: AREA

!!###### REQUIRED OUTPUT
REAL(KIND_MSH) :: P(NDIM_Xc(CGE_REC,Xc))

!!###### LOCAL VARIABLES
INTEGER :: N
CHARACTER(*),PARAMETER :: proc_="CENTROID_Xc"
TYPE(varying_string) :: VS

!!--begin--
SELECT CASE( NDIM_Xc(CGE_REC,Xc) )

 CASE(2)

  SELECT CASE( Xc%CellShape )
   CASE(Qs_,Qr_,Qg_,Pg_,Tr_)
     N = SIZE(Xc%coeff)/2
     P = xyCENTROID_Pg( N , RESHAPE( Xc%coeff , xySHAPE_Pg(:,N) ) , AREA )
   CASE DEFAULT
     VS=MODPROC(mod_,proc_)
     CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
       " The CellShape was not recognized!",LineIndents=(/7,7/))

  END SELECT

 CASE(3)
   VS=MODPROC(mod_,proc_)
   CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
     " 3D has not been installed yet!",LineIndents=(/7,7/))

END SELECT
!!--end--
END FUNCTION


FUNCTION VOLUME_Xc( Xc ) RESULT( VOLUME )
!!###### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Xc

!!###### REQUIRED OUTPUT
REAL(KIND_MSH) :: VOLUME

!!###### LOCAL VARIABLES
INTEGER :: N
CHARACTER(*),PARAMETER :: proc_="VOLUME_Xc"
TYPE(varying_string) :: VS

!!--begin--
SELECT CASE( NDIM_Xc(CGE_REC,Xc) )

 CASE(2)

  SELECT CASE( Xc%CellShape )
   CASE(Qs_,Qr_,Qg_,Pg_,Tr_)
     N = SIZE(Xc%coeff)/2
     VOLUME = xySAREA_Pg( N , RESHAPE( Xc%coeff , xySHAPE_Pg(:,N) ) )
   CASE DEFAULT
     VS=MODPROC(mod_,proc_)
     CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
      " The CellShape was not recognized!",LineIndents=(/7,7/))

  END SELECT

 CASE(3)
   VS=MODPROC(mod_,proc_)
   CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
     " 3D has not been installed yet!",LineIndents=(/7,7/))

END SELECT
!!--end--
END FUNCTION


SUBROUTINE CLEAN_ExplicitCell( Xc , only_tmp )
!!#### PURPOSE
!! Deallocate the variable Xc (an explicit
!! representation of a Cell.)

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_ExplicitCell) :: Xc

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: only_tmp

!!#### LOCAL VARIABLES
LOGICAL :: clean_tmp

!!--begin--

IF( ASSOCIATED(Xc%NDim) )THEN
 DEALLOCATE( Xc%NDim , Xc%CellShape , Xc%coeff )
END IF
NULLIFY(    Xc%NDim , Xc%CellShape , Xc%coeff )

clean_tmp = .false.
if ( present(only_tmp) ) clean_tmp = only_tmp
 if ( .not. clean_tmp .or. Xc%tmp ) then
  if ( associated( Xc%ndim) ) then
    deallocate( Xc%ndim )
        deallocate( Xc%coeff )
        deallocate( Xc%cellshape )
 end if
end if

!!--end--
END SUBROUTINE


PURE ELEMENTAL FUNCTION NUM_Points( Xc ) RESULT(NUM)
!!#### PURPOSE
!! Return the number of points in the cell.

!!#### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Xc

!!#### REQUIRED OUTPUT
INTEGER :: NUM

!!--begin--
IF( ASSOCIATED(Xc%coeff) )THEN
 NUM = SIZE(Xc%coeff)/Xc%NDim
ELSE
 NUM = 0
END IF

!!--end--
END FUNCTION


ELEMENTAL FUNCTION NUM_Dimensions_Xc( Xc ) RESULT(NUM)
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Xc
INTEGER :: NUM
NUM = Xc%NDim
END FUNCTION


FUNCTION POLYGON_Xc( Xc , n ) RESULT(Pg)
!!#### PURPOSE
!! Return the polygon of the cell.

!!#### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Xc

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: n

!!#### REQUIRED OUTPUT
REAL(KIND_MSH) :: Pg(NUM_Dimensions(Xc),NUM_Points(Xc))

!!#### LOCAL VARIABLES
INTEGER :: n_,i,j
CHARACTER(*),PARAMETER :: proc_="POLYGON_Xc"
TYPE(varying_string) :: VS

!!--begin--

n_ = DEFAULT( SIZE(Pg,1)*SIZE(Pg,2) , n )

SELECT CASE( Xc%NDim )

  CASE(1)
    SELECT CASE( Xc%CellShape )
      CASE( Pg_ )  ; Pg(1,:) = (/Xc%coeff(1),Xc%coeff(2)/)
      CASE DEFAULT ; Pg = ERROR(1._KIND_MSH)
    END SELECT

  CASE(2)
    SELECT CASE( Xc%CellShape )
      CASE( Tr_,Qs_,Qr_,Qg_,Pg_ ) ; Pg = xyPOLYGON_( Xc%coeff(1:n_) )
      CASE DEFAULT ; Pg = ERROR(1._KIND_MSH)
    END SELECT

  CASE(3)
    VS=MODPROC(mod_,proc_)
    CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
      " 3D has not been installed yet!",LineIndents=(/7,7/))

END SELECT

IF( ANY(IsError(Pg)) )THEN
 VS=MODPROC(mod_,proc_)
 CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
   " The CellShape was not recognized!",LineIndents=(/7,7/))
END IF

!!--end--
END FUNCTION



FUNCTION INTERIOR_XcP( Cell , P , P_centroid , tol , IncludeEdges ) RESULT(INTERIOR)
!!###### PURPOSE
!! Check if point <P> is in <Cell>.

!!###### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN)  :: Cell
REAL(KIND_MSH)           ,INTENT(IN)  :: P(:)
REAL(KIND_MSH)           ,INTENT(IN)  :: P_centroid(:)
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: tol
LOGICAL,INTENT(IN),OPTIONAL :: IncludeEdges

!!###### REQUIRED OUTPUT
LOGICAL :: INTERIOR

!!###### LOCAL VARIABLES
CHARACTER(*),PARAMETER :: proc_="INTERIOR_XcP"
INTEGER :: N
TYPE(varying_string) :: VS

!!--begin--
SELECT CASE( NDIM_P(CGE_REC,P) )

 CASE(1)
  VS=MODPROC(mod_,proc_)
  CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
   " 1D has not been installed yet!",LineIndents=(/7,7/))

 CASE(2) ;
  SELECT CASE(Cell%CellShape)
   !triangle
   CASE(Tr_)
     INTERIOR = xyINTERIOR_PgP( 3 , RESHAPE(Cell%coeff,xySHAPE_Tr) , P , P_centroid , &
           tol=tol , IncludeEdges=IncludeEdges )

   !square
   CASE(Qs_)
     INTERIOR = xyINTERIOR_PgP( 4 , RESHAPE(Cell%coeff,xySHAPE_Qs) , P , P_centroid , &
           tol=tol , IncludeEdges=IncludeEdges )

   !rectangle
   CASE(Qr_)
     INTERIOR = xyINTERIOR_PgP( 4 , RESHAPE(Cell%coeff,xySHAPE_Qr) , P , P_centroid , &
           tol=tol , IncludeEdges=IncludeEdges )

   !general quadrilateral
   CASE(Qg_)
     INTERIOR = xyINTERIOR_PgP( 4 , RESHAPE(Cell%coeff,xySHAPE_Qg) , P , P_centroid , &
           tol=tol , IncludeEdges=IncludeEdges )

   !polygon
   CASE(Pg_)
     N = SIZE(Cell%coeff)/2
     INTERIOR = xyINTERIOR_PgP( N , RESHAPE(Cell%coeff,xySHAPE_Pg(:,N)) , P , P_centroid , &
           tol=tol , IncludeEdges=IncludeEdges )

   CASE DEFAULT
    VS=MODPROC(mod_,proc_)
    CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
      " The CellShape was not recognized!",LineIndents=(/7,7/))

  END SELECT

 CASE(3)
   VS=MODPROC(mod_,proc_)
   CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
   " 3D has not been installed yet!",LineIndents=(/7,7/))

END SELECT

!!--end--
END FUNCTION


FUNCTION INTERIOR_XcPs( Cell , Ps , P_centroid ) RESULT(INTERIOR)
!!###### PURPOSE
!! Check if plane segment <Ps> is in <Cell>.

!!###### NOTES
!! For the 2D case, the Ps is a single line segment.
!! For the 3D case, the Ps is a closed 2D shape.

!!###### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN)  :: Cell
REAL(KIND_MSH)         ,INTENT(IN)  :: Ps(:,:)
REAL(KIND_MSH)         ,INTENT(IN)  :: P_centroid(:)

!!###### REQUIRED OUTPUT
LOGICAL :: INTERIOR

!!###### LOCAL VARIABLES
CHARACTER(*),PARAMETER :: proc_="INTERIOR_XcPs"
INTEGER :: NDim,N
TYPE(varying_string) :: VS

!!--begin--
!determine number of dimensions
NDim = SIZE(Ps,1)

SELECT CASE(NDim)

 CASE( 1 )
  VS=MODPROC(mod_,proc_)
  CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
    " 1D has not been installed yet!",LineIndents=(/7,7/))

 CASE( 2 )

  !in order of increasing complexity
  SELECT CASE(Cell%CellShape)
   !triangle
   CASE(Tr_)
   !square
   CASE(Qc_)
   !rectangle
   CASE(Qr_)
   !quadrilateral
   CASE(Qg_)
   !polygon
   CASE(Pg_)
    N = NUM_Points( Cell )
    INTERIOR = xyINTERIOR_PgLs( N , RESHAPE( Cell%coeff , &
                                         xySHAPE_Pg(:,N) ) , &
                                         Ps , &
                                         P_centroid )
   CASE DEFAULT
    VS=MODPROC(mod_,proc_)
    CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
     " the CellShape was not recognized!",LineIndents=(/7,7/))

  END SELECT

 CASE( 3 )
   VS=MODPROC(mod_,proc_)
   CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
     " 3D has not been installed yet!",LineIndents=(/7,7/))

 CASE DEFAULT

END SELECT

!!--end--
END FUNCTION


PURE FUNCTION IsQg_Xc( Cell ) RESULT(IsQg)
!!###### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Cell
!!###### REQUIRED OUTPUT
LOGICAL :: IsQg
!!--begin--
IsQg = ANY(Cell%CellShape==(/Qs_,Qr_,Qg_/))
!!--end--
END FUNCTION

PURE FUNCTION IsQr_Xc( Cell ) RESULT(IsQr)
!!###### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Cell
!!###### REQUIRED OUTPUT
LOGICAL :: IsQr
!!--begin--
IsQr = ANY(Cell%CellShape==(/Qs_,Qr_/))
!!--end--
END FUNCTION

PURE FUNCTION IsQs_Xc( Cell ) RESULT(IsQs)
!!###### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Cell
!!###### REQUIRED OUTPUT
LOGICAL :: IsQs
!!--begin--
IsQs = Cell%CellShape==Qs_
!!--end--
END FUNCTION

PURE FUNCTION IsTr_Xc( Cell ) RESULT(IsTr)
!!###### REQUIRED INPUT
TYPE(TYPE_ExplicitCell),INTENT(IN) :: Cell
!!###### REQUIRED OUTPUT
LOGICAL :: IsTr
!!--begin--
IsTr = Cell%CellShape==Tr_
!!--end--
END FUNCTION


FUNCTION ExplicitCell_( Cell , Faces , Verts ) RESULT(Xc)
!!###### PURPOSE
!! Create an explicit Cell from the implicit Cell,
!! a list of implicit faces, and some verts.

!!###### REQUIRED INPUT
TYPE(TYPE_Cell),INTENT(IN) :: Cell
TYPE(TYPE_Face),INTENT(IN) :: Faces(:)
REAL(KIND_MSH) ,INTENT(IN) :: Verts(:,:)

!!###### REQUIRED OUTPUT
TYPE(TYPE_ExplicitCell) :: Xc

!!###### LOCAL VARIABLES
INTEGER :: j,j_,Nj_,SIZE_Pg
TYPE(TYPE_ExplicitFace),POINTER :: Xf(:)
REAL(KIND_MSH),POINTER :: Lsx(:,:,:),Pg(:,:)
INTEGER,POINTER :: FL(:)
CHARACTER(*),PARAMETER :: proc_="ExplicitCell"
TYPE(varying_string) :: VS
!!--begin--

CALL CLEAN( Xc )

NULLIFY( Xf,Pg,Lsx,FL )

!get dimension
ALLOCATE( Xc%NDim )
Xc%NDim = SIZE(Verts,1)

!set the Cellshape to the same
ALLOCATE( Xc%Cellshape )
Xc%Cellshape = CellShape(Cell)

SELECT CASE(Xc%NDim)
 CASE(1)
   VS=MODPROC(mod_,proc_)
   CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
     " 1D has not been installed yet!",LineIndents=(/7,7/))

 CASE(2)
  SELECT CASE(Xc%CellShape)

   CASE(Tr_,Pg_,Qs_,Qr_,Qg_) ;

    !assuming always line segments (planar segments)
    !using general quadrilateral for anything and treating
    Nj_ = NUM_Faces(Cell)

    ALLOCATE( FL(1:Nj_) )
    FL = FaceList(Cell)

    ALLOCATE( Xf(Nj_) )

    CALL ALLOCATE_Lsx( 2 , CGE_REC , Lsx , Nj_ )

    DO j_=1,Nj_
     Xf(j_) = ExplicitFace( Faces(ABS(FL(j_))) , Verts , Flip=FL(j_)<0 )
     Lsx(:,:,j_) = LINESEGMENT_Xf( Xf(j_) )
    END DO

    ALLOCATE( Pg(xySHAPE_Pg(1,Nj_),xySHAPE_Pg(2,Nj_)) )
    Pg = xyPOLYGON_Lsx( Nj_ , Lsx )

    SIZE_Pg = xySHAPE_Pg(1,Nj_)*xySHAPE_Pg(2,Nj_)

    ALLOCATE( Xc%coeff(SIZE_Pg) )
    Xc%coeff = RESHAPE( Pg , (/SIZE_Pg/) )

    DO j_=1,Nj_
      CALL CLEAN( Xf(j_) )
    END DO
    DEALLOCATE( Lsx , Xf , Pg , FL )
    NULLIFY( Lsx , Xf , Pg , FL )


   CASE DEFAULT
    VS=MODPROC(mod_,proc_)
    CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
     " the CellShape was not recognized!",LineIndents=(/7,7/))

  END SELECT


 CASE( 3 )
   VS=MODPROC(mod_,proc_)
   CALL UpdateAndDump(fdbk_error,s=STR(VS)//&
    " 1D has not been installed yet!",LineIndents=(/7,7/))

END SELECT

!!--end--
END FUNCTION


END MODULE
