!!# USER MODULE: <USR_ExplicitFace>
MODULE USR_ExplicitFace

!!## PURPOSE
!! Defines the explicit face type.

!!## EXTERNAL KINDS
USE KND_Mesh                               !!((05-B-KND_Mesh.f90))

!!## USER MODULES
USE USR_Face                               !!((11-B-USR_Face.f90))

!!## TOOLBOXES
USE TBX_ComputationalGeometry              !!((09-A-TBX_ComputationalGeometry.f90))

!!## GLOBAL ROUTINES
USE FUN_Reorder                            !!((05-A-FUN_Reorder.f90))
USE FUN_Reverse                            !!((04-A-FUN_Reverse.f90))
USE FUN_Error                              !!((04-A-FUN_Error.f90))
USE FUN_NewUnit                            !!((04-B-FUN_NewUnit.f90))
USE FUN_NDIM                               !!((08-B-FUN_NDIM.f90))
USE FUN_IsApprox,ONLY: IsApprox_=>IsApprox !!((03-A-FUN_IsApprox.f90))
USE FUN_Default                            !!((04-A-FUN_Default.f90))
USE FUN_xyPLANE                            !!((05-B-FUN_xyPLANE.f90))

!!## GLOBAL LIBRARIES
USE LIB_Prompts                            !!((06-B-LIB_Prompts.f90))
USE LIB_GenericPhrases                     !!((07-B-LIB_GenericPhrases.f90))

!!## GLOBAL PRINTING
USE PRN_Text                               !!((07-B-PRN_Text.f90))

!!## GLOBAL FLOW CONTROL
USE SUB_Pause                              !!((04-B-SUB_Pause.f90))
USE SUB_Stop                               !!((04-B-SUB_Stop.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## IDENTIFICATION
CHARACTER(*),PARAMETER :: mod_ ="USR_ExplicitFace"
CHARACTER(*),PARAMETER :: file_="08-B-USR_ExplicitFace.f90"
PRIVATE :: mod_,file_


!!## TYPE DEFINITION for explicit faces
TYPE TYPE_ExplicitFace

 PRIVATE

 INTEGER       ,POINTER :: NDim      => NULL()
 INTEGER       ,POINTER :: FaceShape => NULL()
 REAL(KIND_MSH),POINTER :: coeff(:)  => NULL()

 LOGICAL :: tmp = .FALSE.

END TYPE


INTERFACE CLEAN
 MODULE PROCEDURE CLEAN_Xf
END INTERFACE

INTERFACE IsPs
 MODULE PROCEDURE IsPs_Xf
END INTERFACE

INTERFACE IsApprox
 MODULE PROCEDURE IsApprox_Xf2
END INTERFACE

INTERFACE ExplicitFace
 MODULE PROCEDURE ExplicitFace_
END INTERFACE

INTERFACE IsSamePlane
 MODULE PROCEDURE IsSamePlane_Xf2
END INTERFACE

INTERFACE NUM_Dimensions
 MODULE PROCEDURE NUM_Dimensions_Xf
END INTERFACE

INTERFACE NDim
 MODULE PROCEDURE NUM_Dimensions_Xf
END INTERFACE

INTERFACE NDim_Xf
 MODULE PROCEDURE NUM_Dimensions_Xf
END INTERFACE

INTERFACE ASSIGNMENT(=)
 MODULE PROCEDURE ASSIGN_Xf
END INTERFACE

INTERFACE OPERATOR(==)
 MODULE PROCEDURE EQUALITY_Xf
END INTERFACE

!!## TYPE
PUBLIC :: TYPE_ExplicitFace

!!## ASSIGNMENT
PUBLIC :: ASSIGNMENT(=)

!!## EQUALITY TEST
PUBLIC :: OPERATOR(==)

!!## PARAMETERS
PUBLIC :: KEY_FaceShape , Ps_
PUBLIC :: KEY_EdgeShape , Straight_

!!## GEOMETRY ROUTINES
PUBLIC :: Intersect_XfRy
PUBLIC :: Intersect_XfLn
PUBLIC :: Intersect_XfPn
PUBLIC :: Interior_PnXf
PUBLIC :: Centroid_Xf
PUBLIC :: PLANE_Xf
PUBLIC :: LINESEGMENT_Xf

!!## CONSTRUCTOR
PUBLIC :: ExplicitFace

!!## INQUIRY FUNCTIONS
PUBLIC :: IsPs          ,IsPs_Xf
PUBLIC :: IsApprox      ,IsApprox_Xf2
PUBLIC :: IsSamePlane   ,IsSamePlane_Xf2
PUBLIC :: NUM_Dimensions,NUM_Dimensions_Xf

!!## DESTRUCTOR
PUBLIC :: CLEAN , CLEAN_Xf

CONTAINS


ELEMENTAL FUNCTION IsPs_Xf( Xf )
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf
LOGICAL :: IsPs_Xf
IsPs_Xf = Xf%FaceShape==Ps_
END FUNCTION

ELEMENTAL FUNCTION NUM_Dimensions_Xf( Xf ) RESULT(NUM)
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf
INTEGER :: NUM
NUM = Xf%NDim
END FUNCTION


FUNCTION INTERIOR_PnXf( Pn , Xf , &
  tolinf , toleuc , norminf , normeuc ) RESULT(INTERIOR)
!!## PURPOSE
!! Check if an explicit face is part of (interior) to a plane.

!!## REQUIRED INPUT
!! @ the plane <Pn>
!! @ the explicit face <Xf>
REAL(KIND_MSH)         ,INTENT(IN) :: Pn(:)
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf

!!## OPTIONAL INPUT
!! @ Infinity-norm tolerance <tolinf>
!! @ Euclidean-norm tolerance <toleuc>
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: tolinf
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: toleuc

!!## OPTIONAL OUTPUT
!! @ Infinity-norm <norminf>
!! @ Euclidean-norm <normeuc>
REAL(KIND_MSH),OPTIONAL,INTENT(OUT) :: norminf
REAL(KIND_MSH),OPTIONAL,INTENT(OUT) :: normeuc

!!## REQUIRED OUTPUT
!! @ whether the face is a subset <INTERIOR>
LOGICAL :: INTERIOR

!!## LOCAL VARIABLES
REAL(KIND_MSH) :: tolinf_,toleuc_
REAL(KIND_MSH) :: norminf_,normeuc_
REAL(KIND_MSH) :: SDIST
INTEGER     :: k
REAL(KIND_MSH),POINTER :: Ls(:,:)

!!--begin--
!initialize
INTERIOR = .FALSE.

!initialize infintity tolerance
IF( PRESENT(tolinf) )THEN
 tolinf_ = tolinf
ELSE
 tolinf_ = 0._KIND_MSH
END IF

!initialize euclidean tolerance
IF( PRESENT(toleuc) )THEN
 toleuc_ = toleuc**2
ELSE
 toleuc_ = EPSILON(0._KIND_MSH)
END IF

!choose action based on number of dimensions of the input plane
SELECT CASE( NDIM_Pn(CGE_REC,Pn) )
 CASE(2)

  SELECT CASE( Xf%FaceShape )
   CASE(Ps_)

    norminf_ = 0._KIND_MSH
    normeuc_ = 0._KIND_MSH

    CALL ALLOCATE_Ls( 2 , CGE_REC , Ls )
    Ls = xyLINESEGMENT_( Xf%Coeff )
    DO k=1,2
     !get signed distance
     SDIST = xySDIST_PnP( Pn , Ls(:,k) )

     !infinity norm criterion
     norminf_ = MAX(norminf_,ABS(SDIST))

     !euclidean norm criterion
     normeuc_ = normeuc_ + SDIST**2
    END DO
    DEALLOCATE( Ls )

    INTERIOR = (norminf_<tolinf_)
    INTERIOR = (normeuc_<toleuc_)

  END SELECT

 CASE(3)
  CALL PRINT_Text(s="USR_ExplicitFace::PnXf, no 3D yet.")
  CALL Stop(s="FATAL ERROR")

END SELECT

!return optional output
IF( PRESENT(norminf) )THEN
 norminf = norminf_
END IF

IF( PRESENT(normeuc) )THEN
 normeuc = SQRT(normeuc_)
END IF

!!--end--
END FUNCTION


PURE FUNCTION CENTROID_Xf( Xf ) RESULT( P )
!!## REQUIRED INPUT
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf

!!## REQUIRED OUTPUT
REAL(KIND_MSH) :: P(NUM_Dimensions(Xf))

!!--begin--
SELECT CASE( NUM_Dimensions(Xf) )

 CASE(2) ;

  SELECT CASE( Xf%FaceShape )
   CASE( Ps_ )   ; P = xyCENTROID_Ls( RESHAPE( Xf%coeff(:) , xySHAPE_Ls ) )
   CASE DEFAULT  ; P = ERROR( 1._KIND_MSH )
  END SELECT

 CASE(3) ;
  P = ERROR( 1._KIND_MSH )

END SELECT

!!--end--
END FUNCTION



FUNCTION INTERSECT_XfPn( Xf , Pn , IncludeEdges , &
  P_intersect , key ) RESULT(INTERSECT)
!!## PURPOSE
!! Determine the intersection of an explicit face and a plane.

!!## REQUIRED INPUT
!! @ explicit face <Xf>
!! @ plane <Pn>
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf
REAL(KIND_MSH)           ,INTENT(IN) :: Pn(:)

!!## OPTIONAL INPUT
!! @ if the edges of the face are included in the intersection
!!   check <IncludeEdges>
LOGICAL,OPTIONAL,INTENT(IN) :: IncludeEdges

!!## OPTIONAL OUTPUT
!! @ point of intersection <P_intersect>
!! @ key to give extra info about intersection <key>
!!  -1 = no intersection
!!   0 = intersection through edge
!!   1 = intersection through middle
REAL(KIND_MSH),OPTIONAL,INTENT(OUT) :: P_intersect(SIZE(Pn)-1)
INTEGER    ,OPTIONAL,INTENT(OUT) :: key

!!## REQUIRED OUTPUT
!! @ whether or not the intersection happens <INTERSECT>
LOGICAL :: INTERSECT

!!## LOCAL VARIABLES
REAL(KIND_MSH) :: Ln(1:SIZE(Pn)-1,1:2)
INTEGER     :: k

!!--begin--
SELECT CASE( Xf%NDim )
 CASE(2)
  SELECT CASE(Xf%FaceShape)
   CASE(Ps_)
    !would like a line-segment/plane intersection here instead
    Ln(:,1) = -Pn(1:2)*Pn(3)
    Ln(:,2) = xyPERPCCW_U( Pn(1:2) )
    INTERSECT = xyINTERSECT_LnLs( Ln , RESHAPE(Xf%Coeff,xySHAPE_Ls) , IncludeEnds=SPREAD(IncludeEdges,1,2) , P_intersect=P_intersect , key=key )
  END SELECT
END SELECT
!!--end--
END FUNCTION



PURE ELEMENTAL FUNCTION EQUALITY_Xf( Xf_a , Xf_b ) &
  RESULT(EQUALITY)
!!## PURPOSE
!! Strict equality of two explicit faces.

!!## REQUIRED INPUT
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf_a
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf_b

!!## REQUIRED OUTPUT
LOGICAL :: EQUALITY

!!--begin--

EQUALITY = .FALSE.

IF( Xf_a%FaceShape==Xf_b%FaceShape )THEN
 IF( Xf_a%NDim==Xf_b%FaceShape )THEN
  IF( ALL(Xf_a%coeff==Xf_b%coeff) )THEN
   EQUALITY = .TRUE.
  END IF
 END IF
END IF

!!--end--
END FUNCTION



PURE ELEMENTAL FUNCTION IsApprox_Xf2( Xf_a , Xf_b , &
  tol , reltol) RESULT(IsApprox)
!!## PURPOSE
!! Check if two explicit faces are approximately equal.

!!## REQUIRED INPUT
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf_a
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf_b

!!## OPTIONAL INPUT
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: tol,reltol

!!## REQUIRED OUTPUT
LOGICAL :: IsApprox

!!--begin--

IsApprox = .FALSE.

IF( Xf_a%FaceShape==Xf_b%FaceShape )THEN
 IF( Xf_a%NDim==Xf_b%NDim )THEN
  IF( IsApprox_(Xf_a%coeff,Xf_b%coeff,tol,reltol) )THEN
   IsApprox = .TRUE.
  END IF
 END IF
END IF

!!--end--
END FUNCTION




FUNCTION IsSamePlane_Xf2( Xf_a , Xf_b , &
  tol , reltol) RESULT(IsSamePlane)

!!## PURPOSE
!! Check if two explicit faces share the same plane
!! representation and could be combined.

!!## REQUIRED INPUT
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf_a
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf_b

!!## OPTIONAL INPUT
REAL(KIND_MSH),INTENT(IN),OPTIONAL :: tol,reltol

!!## REQUIRED OUTPUT
LOGICAL :: IsSamePlane

!!## LOCAL VARIABLES
REAL(KIND_MSH) :: Pn_a(1:3),Pn_b(1:3)

!!--begin--

IsSamePlane = .FALSE.

IF( Xf_a%NDim==Xf_b%NDim )THEN
 IF( Xf_a%FaceShape==Ps_ .AND. Xf_b%FaceShape==Ps_ )THEN
  Pn_a = PLANE_Xf( Xf_a )
  Pn_b = PLANE_Xf( Xf_b )
  IsSamePlane = IsApprox_(Pn_a,Pn_b,tol,reltol)
 END IF
END IF

!!--end--
END FUNCTION



SUBROUTINE CLEAN_Xf( Xf , only_tmp )
!!## PURPOSE
!! Clean up an explicit face.

!!## REQUIRED INPUT/OUTPUT
TYPE(TYPE_ExplicitFace) :: Xf

!!## OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: only_tmp

!!## LOCAL VARIABLES
LOGICAL :: clean_tmp

!!--begin--

IF( ASSOCIATED(Xf%NDim) )THEN
 DEALLOCATE( Xf%NDim , Xf%FaceShape , Xf%coeff )
END IF
NULLIFY(    Xf%NDim , Xf%FaceShape , Xf%coeff )

clean_tmp = .false.
if ( present(only_tmp) ) clean_tmp = only_tmp
 if ( .not. clean_tmp .or. Xf%tmp ) then
  if ( associated( Xf%ndim) ) then
    deallocate( Xf%ndim )
        deallocate( Xf%coeff )
        deallocate( Xf%faceshape )
 end if
end if

!!--end--
END SUBROUTINE



SUBROUTINE ASSIGN_Xf( Xf_a , Xf_b )
!!## PURPOSE
!! Assign one face to another

!!## REQUIRED ASSIGNMENT VARIABLES
TYPE(TYPE_ExplicitFace),INTENT(INOUT) :: Xf_a
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf_b

!!--begin--

CALL CLEAN_Xf( Xf_a , .false. )

ALLOCATE( Xf_a%NDim , Xf_a%FaceShape , Xf_a%coeff(SIZE(Xf_b%coeff)) )

Xf_a%NDim      = Xf_b%NDim
Xf_a%FaceShape = Xf_b%FaceShape
Xf_a%coeff     = Xf_b%coeff

CALL CLEAN_Xf( Xf_b , .true. )

!!--end--
END SUBROUTINE



PURE FUNCTION PLANE_Xf( Xf ) RESULT(Pn)
!!## PURPOSE
!! Return the 3D plane of the surface, or the error number
!! if no such plane exists for this surface.

!!## REQUIRED INPUT
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf

!!## REQUIRED OUTPUT
REAL(KIND_MSH) :: Pn(Xf%Ndim+1)

!!--begin--
SELECT CASE( Xf%NDim )

  CASE(1)
    SELECT CASE( Xf%FaceShape )
      CASE( Ps_ )  ; Pn = (/SIGN(Xf%coeff(1),1._KIND_MSH),0._KIND_MSH,0._KIND_MSH,0._KIND_MSH/)
      CASE DEFAULT ; Pn = ERROR(1._KIND_MSH)
    END SELECT

  CASE(2)
    SELECT CASE( Xf%FaceShape )
      CASE( Ps_ )  ; Pn = (/xyPLANE_P2(  RESHAPE( Xf%Coeff , xySHAPE_Ls )  ) ,0._KIND_MSH/)
      CASE DEFAULT ; Pn = ERROR(1._KIND_MSH)
    END SELECT

  CASE(3)
    SELECT CASE( Xf%FaceShape )
      CASE( Ps_ )  ; Pn = xyzPLANE_P3( RESHAPE( Xf%Coeff(1:9) , xySHAPE_Tr )  )
      CASE DEFAULT ; Pn = ERROR(1._KIND_MSH)
    END SELECT

END SELECT
!!--end--
END FUNCTION


PURE FUNCTION NUM_Ls( Xf ) RESULT(NUM)
!!## PURPOSE
!! Return the number of line segments in the face.

!!## REQUIRED INPUT
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf

!!## REQUIRED OUTPUT
INTEGER :: NUM

!!--begin--
IF( ASSOCIATED(Xf%coeff) )THEN
 NUM = SIZE(Xf%coeff)/Xf%NDim
ELSE
 NUM = 0
END IF

!!--end--
END FUNCTION



PURE FUNCTION LINESEGMENT_Xf( Xf , n ) RESULT(Lsx)
!!## PURPOSE
!! Return the linesegments of the face.

!!## REQUIRED INPUT
TYPE(TYPE_ExplicitFace),INTENT(IN) :: Xf

!!## OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: n

!!## REQUIRED OUTPUT
REAL(KIND_MSH) :: Lsx(NUM_Dimensions(Xf),2)

!!## LOCAL VARIABLES
INTEGER :: n_,i,j

!!--begin--

n_ = DEFAULT( 1 , n )

SELECT CASE( Xf%NDim )

  CASE(1)
    SELECT CASE( Xf%FaceShape )
      CASE( Ps_ )  ; Lsx(1,:) = (/Xf%coeff(1),Xf%coeff(2)/)
      CASE DEFAULT ; Lsx = ERROR(1._KIND_MSH)
    END SELECT

  CASE(2)
    SELECT CASE( Xf%FaceShape )
      CASE( Ps_ )  ; i = 1+4*(n_-1)
                         j = 4*n_
                                         Lsx(:,:) = xyLINESEGMENT_( Xf%coeff(i:j) )
      CASE DEFAULT ; Lsx = ERROR(1._KIND_MSH)
    END SELECT

  CASE(3)
    SELECT CASE( Xf%FaceShape )
      CASE( Ps_ )  ; Lsx = ERROR(1._KIND_MSH) !RESHAPE(Xf%Coeff,xyzSHAPE_Ls)
      CASE DEFAULT ; Lsx = ERROR(1._KIND_MSH)
    END SELECT

END SELECT

!!--end--
END FUNCTION


FUNCTION INTERSECT_XfRy( Xf , Ry , DIST , tol ) RESULT(INTERSECT)
!!## PURPOSE
!! Determine the intersection of a ray, Ry, with a Xf.

!!## DEPENDENCIES
USE FUN_NewFile                            !!((05-B-FUN_NewFile.f90))

!!## REQUIRED INPUT
TYPE(TYPE_ExplicitFace),INTENT(IN)  :: Xf
REAL(KIND_MSH)           ,INTENT(IN)  :: Ry(:,:)

!!## REQUIRED OUTPUT
LOGICAL :: INTERSECT

!!## OPTIONAL OUTPUT
REAL(KIND_MSH),OPTIONAL,INTENT(OUT) :: DIST
REAL(KIND_MSH),OPTIONAL,INTENT(IN) :: tol

!!## LOCAL VARIABLES
INTEGER                :: m,k,j
INTEGER                :: Unit
LOGICAL     ,PARAMETER :: NoisyMode_ = .FALSE.
CHARACTER(*),PARAMETER :: proc_="INTERSECT_XfRy"
!!--begin--
IF( NoisyMode_ )THEN
 Unit = NewFile(FILE=proc_//".txt",STATUS="Replace")
END IF


SELECT CASE( NUM_Dimensions(Xf) )

 CASE(2)

  SELECT CASE(Xf%FaceShape)

   CASE(Ps_)

    IF( NoisyMode_ )WRITE(Unit,*)"   Ray base     :",Ry(:,1)
    IF( NoisyMode_ )WRITE(Unit,*)"   Ray direction:",Ry(:,2)
    IF( NoisyMode_ )WRITE(Unit,*)"   LineSegment start:",Xf%coeff(1:2)
    IF( NoisyMode_ )WRITE(Unit,*)"   LineSegment end  :",Xf%coeff(3:4)

    INTERSECT = xyINTERSECT_RyLs( Ry , RESHAPE(Xf%coeff,xySHAPE_Ls) , DIST , tol=tol )

    IF( NoisyMode_ )WRITE(Unit,*)"   INTERSECTION: ",INTERSECT
    IF( NoisyMode_ )WRITE(Unit,*)"    DISTANCE TO: ",DIST


  END SELECT
END SELECT

!!--end--
END FUNCTION



FUNCTION INTERSECT_XfLn( Xf , Ln , DIST , tol ) RESULT(INTERSECT)
!!## PURPOSE
!! Determine the intersection of a line, Ln, with a Xf.

!!## DEPENDENCIES
USE FUN_NewFile                            !!((05-B-FUN_NewFile.f90))

!!## REQUIRED INPUT
TYPE(TYPE_ExplicitFace),INTENT(IN)  :: Xf
REAL(KIND_MSH)         ,INTENT(IN)  :: Ln(:,:)

!!## REQUIRED OUTPUT
LOGICAL :: INTERSECT

!!## OPTIONAL OUTPUT
REAL(KIND_MSH),OPTIONAL,INTENT(OUT) :: DIST
REAL(KIND_MSH),OPTIONAL,INTENT(IN)  :: tol

!!## LOCAL VARIABLES
INTEGER                :: m,k,j
INTEGER                :: Unit
LOGICAL     ,PARAMETER :: NoisyMode_ = .FALSE.
CHARACTER(*),PARAMETER :: proc_="INTERSECT_XfLn"
!!--begin--
IF( NoisyMode_ )THEN
 Unit = NewFile(FILE=proc_//".txt",STATUS="Replace")
END IF


SELECT CASE( NUM_Dimensions(Xf) )

 CASE(2)

  SELECT CASE(Xf%FaceShape)

   CASE(Ps_)

    IF( NoisyMode_ )WRITE(Unit,*)"   Line base     :",Ln(:,1)
    IF( NoisyMode_ )WRITE(Unit,*)"   Line direction:",Ln(:,2)
    IF( NoisyMode_ )WRITE(Unit,*)"   LineSegment start:",Xf%coeff(1:2)
    IF( NoisyMode_ )WRITE(Unit,*)"   LineSegment end  :",Xf%coeff(3:4)

    INTERSECT = xyINTERSECT_LnLs( Ln , RESHAPE(Xf%coeff,xySHAPE_Ls) , DIST , tol=tol )

    IF( NoisyMode_ )WRITE(Unit,*)"   INTERSECTION: ",INTERSECT
    IF( NoisyMode_ )WRITE(Unit,*)"    DISTANCE TO: ",DIST


  END SELECT
END SELECT

!!--end--
END FUNCTION



PURE FUNCTION ExplicitFace_( Face , Verts , Flip ) RESULT(Xf)
!!## PURPOSE
!! Create an explicit face.

!!## REQUIRED INPUT
TYPE(TYPE_Face),INTENT(IN) :: Face
REAL(KIND_MSH) ,INTENT(IN) :: Verts(:,:)

!!## OPTIONAL INPUT
!! The face ordering is reversed if <Flip=.TRUE.>.
LOGICAL,INTENT(IN),OPTIONAL :: Flip

!!## REQUIRED OUTPUT
TYPE(TYPE_ExplicitFace) :: Xf

!!## LOCAL VARIABLES
INTEGER :: k1,k2,NDim_
LOGICAL :: Flip_
INTEGER,ALLOCATABLE :: VL(:)

!!--begin--

ALLOCATE( VL(1:NUM_Verts(Face)) )

NULLIFY( Xf%NDim , Xf%FaceShape )

!options handling
Flip_ = DEFAULT(.FALSE.,Flip)

!set number of dimensions
ALLOCATE( Xf%NDim )
Xf%NDim = SIZE(Verts,1)


!set the Faceshape to the same
ALLOCATE( Xf%Faceshape )
Xf%Faceshape = FaceShape(Face)


!determine coefficients
NDim_ = NUM_Dimensions(Xf)

SELECT CASE( NDim_ )

 !1D
 CASE(1)

  NULLIFY( Xf%Coeff )
  ALLOCATE( Xf%Coeff(NDim_) )
  Xf%coeff = ERROR(1._KIND_MSH)


 !2D
 CASE(2)

  SELECT CASE( Xf%Faceshape )

   CASE(Ps_)

    !allocate
    NULLIFY( Xf%Coeff )
        ALLOCATE( Xf%coeff(SUM(xySHAPE_Ls)) )

    !get the two indices of the left and right verts
    VL = VertList(Face)
    k1 = VL(1)
    k2 = VL(SIZE(VL))

    !get the explicit face coefficients as the line segment that connects the two verts
    IF( Flip_ )THEN
     Xf%coeff = RESHAPE(xyLINESEGMENT_PP( Verts(:,k2) , Verts(:,k1) ) ,(/NDim_*2/))
    ELSE
     Xf%coeff = RESHAPE(xyLINESEGMENT_PP( Verts(:,k1) , Verts(:,k2) ) ,(/NDim_*2/))
    END IF

   CASE DEFAULT
    NULLIFY( Xf%Coeff )
        ALLOCATE( Xf%Coeff(NDim_) )
    Xf%Coeff = ERROR(1._KIND_MSH)

  END SELECT

 !3D
 CASE(3)

   NULLIFY( Xf%Coeff )
   ALLOCATE( Xf%Coeff(NDim_) )
   Xf%Coeff = ERROR(1._KIND_MSH)


 !>3D
 CASE DEFAULT

   ALLOCATE( Xf%Coeff(NDim_) )
   Xf%Coeff = ERROR(1._KIND_MSH)


END SELECT

DEALLOCATE( VL )


!!--end--
END FUNCTION


END MODULE
