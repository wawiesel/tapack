!!# LIBRARY MODULE: <<LIB_genMoments>>
MODULE LIB_genMoments

!!## PURPOSE
!! Contains a library of moment-taking routines, meant for
!! discrete ordinates integration of functions on the sphere,
!! for the most general case.

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PROCEDURE OVERLOADING
!! * Moment0
INTERFACE Moment0
 MODULE PROCEDURE Moment0_Rsp
 MODULE PROCEDURE Moment0_Rdp
END INTERFACE
!! * Moment0 with incoming directions only
INTERFACE Moment0_IN
 MODULE PROCEDURE Moment0_IN_Rsp
 MODULE PROCEDURE Moment0_IN_Rdp
END INTERFACE
!! * Moment0 With outgoing directions only
INTERFACE Moment0_OUT
 MODULE PROCEDURE Moment0_OUT_Rsp
 MODULE PROCEDURE Moment0_OUT_Rdp
END INTERFACE
!! * Moment1
INTERFACE Moment1
 MODULE PROCEDURE Moment1_Rsp
 MODULE PROCEDURE Moment1_Rdp
END INTERFACE
!! * Moment1 for a single dimension
INTERFACE Moment1_Dim
 MODULE PROCEDURE Moment1_Dim_Rsp
 MODULE PROCEDURE Moment1_Dim_Rdp
END INTERFACE
!! * Moment1 with incoming directions only
INTERFACE Moment1_IN
 MODULE PROCEDURE Moment1_IN_Rsp
 MODULE PROCEDURE Moment1_IN_Rdp
END INTERFACE
!! * Moment1 With outgoing directions only
INTERFACE Moment1_OUT
 MODULE PROCEDURE Moment1_OUT_Rsp
 MODULE PROCEDURE Moment1_OUT_Rdp
END INTERFACE
!! * Moment1 for a single dimension
!!   with incoming directions only
INTERFACE Moment1_DimIN
 MODULE PROCEDURE Moment1_DimIN_Rsp
 MODULE PROCEDURE Moment1_DimIN_Rdp
END INTERFACE
!! * Moment1 for a single dimension
!!   with outgoing directions only
INTERFACE Moment1_DimOUT
 MODULE PROCEDURE Moment1_DimOUT_Rsp
 MODULE PROCEDURE Moment1_DimOUT_Rdp
END INTERFACE
!! * Moment1 with normal vector in integral
INTERFACE Moment1_Normal
 MODULE PROCEDURE Moment1_Normal_Rsp
 MODULE PROCEDURE Moment1_Normal_Rdp
END INTERFACE
!! * Moment1 with normal and integration over
!!   inward directions only
INTERFACE Moment1_NormalIN
 MODULE PROCEDURE Moment1_NormalIN_Rsp
 MODULE PROCEDURE Moment1_NormalIN_Rdp
END INTERFACE
!! * Moment1 with normal and integration over
!!   outward directions only
INTERFACE Moment1_NormalOUT
 MODULE PROCEDURE Moment1_NormalOUT_Rsp
 MODULE PROCEDURE Moment1_NormalOUT_Rdp
END INTERFACE
!! * Moment2  (tensor stored in 1D array, diagonal first, then left
!!             to right off-diagonal elements on lower triangular part)
INTERFACE Moment2
 MODULE PROCEDURE Moment2_Rsp
 MODULE PROCEDURE Moment2_Rdp
END INTERFACE
!! * Moment2 for a single dimension pair
INTERFACE Moment2_Dim
 MODULE PROCEDURE Moment2_Dim_Rsp
 MODULE PROCEDURE Moment2_Dim_Rdp
END INTERFACE
!! * Renormalize a quadrature set
INTERFACE RENORMALIZE
! MODULE PROCEDURE RENORMALIZE_Rsp
 MODULE PROCEDURE RENORMALIZE_Rdp
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: Moment0
PUBLIC :: Moment0_IN
PUBLIC :: Moment0_OUT
PUBLIC :: Moment1
PUBLIC :: Moment1_Dim
PUBLIC :: Moment1_DimIN
PUBLIC :: Moment1_DimOUT
PUBLIC :: Moment1_Normal
PUBLIC :: Moment1_NormalIN
PUBLIC :: Moment1_NormalOUT
PUBLIC :: Moment2
PUBLIC :: Moment2_Dim

PUBLIC :: PRINT_genMomentTest
PUBLIC :: PRINT_QuadratureSet

PUBLIC :: TEST_prodMoment
PUBLIC :: Renormalize

!!## CONTAINED PROCEDURES
CONTAINS


!!### PURE FUNCTION: <Moment0>
PURE FUNCTION Moment0_Rsp( f , x , w ) RESULT(Moment0)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment0.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment0.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment0_Rdp( f , x , w ) RESULT(Moment0)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment0.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment0.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: <Moment1>
PURE FUNCTION Moment1_Rsp( f , x , w ) RESULT(Moment1)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment1_Rdp( f , x , w ) RESULT(Moment1)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: <Moment2>
PURE FUNCTION Moment2_Rsp( f , x , w ) RESULT(Moment2)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment2.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment2.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION Moment2_Rdp( f , x , w ) RESULT(Moment2)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment2.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment2.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: <Moment1_Dim>
PURE FUNCTION Moment1_Dim_Rsp( f , x , w , Dim ) RESULT(Moment1_Dim)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_Dim.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_Dim.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment1_Dim_Rdp( f , x , w , Dim ) RESULT(Moment1_Dim)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_Dim.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_Dim.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: <Moment2_Dim>
PURE FUNCTION Moment2_Dim_Rsp(  f , x , w , Dim1,Dim2 ) RESULT(Moment2_Dim)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment2_Dim.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment2_Dim.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment2_Dim_Rdp(  f , x , w , Dim1,Dim2 ) RESULT(Moment2_Dim)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment2_Dim.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment2_Dim.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: <Moment1_Normal>
PURE FUNCTION Moment1_Normal_Rsp( f , x , w , u ) RESULT(Moment1_Normal)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_Normal.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_Normal.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment1_Normal_Rdp( f , x , w , u ) RESULT(Moment1_Normal)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_Normal.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_Normal.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: <Moment1_NormalIN>
PURE FUNCTION Moment1_NormalIN_Rsp( f , x , w , u ) RESULT(Moment1_NormalIN)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_NormalIN.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_NormalIN.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment1_NormalIN_Rdp( f , x , w , u ) RESULT(Moment1_NormalIN)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_NormalIN.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_NormalIN.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: <Moment1_NormalOUT>
PURE FUNCTION Moment1_NormalOUT_Rsp( f , x , w , u ) RESULT(Moment1_NormalOUT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_NormalOUT.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_NormalOUT.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment1_NormalOUT_Rdp( f , x , w , u ) RESULT(Moment1_NormalOUT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_NormalOUT.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_NormalOUT.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: <Moment0_OUT>
PURE FUNCTION Moment0_OUT_Rsp( f , x , w , u ) RESULT(Moment0_OUT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment0_OUT.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment0_OUT.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment0_OUT_Rdp( f , x , w , u ) RESULT(Moment0_OUT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment0_OUT.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment0_OUT.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: <Moment0_IN>
PURE FUNCTION Moment0_IN_Rsp( f , x , w , u ) RESULT(Moment0_IN)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment0_IN.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment0_IN.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment0_IN_Rdp( f , x , w , u ) RESULT(Moment0_IN)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment0_IN.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment0_IN.f90.bdy"
!!--end--
END FUNCTION




!!### PURE FUNCTION: <Moment1_IN>
PURE FUNCTION Moment1_IN_Rsp( f , x , w , u ) RESULT(Moment1_IN)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_IN.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_IN.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment1_IN_Rdp( f , x , w , u ) RESULT(Moment1_IN)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_IN.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_IN.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: <Moment1_OUT>
PURE FUNCTION Moment1_OUT_Rsp( f , x , w , u ) RESULT(Moment1_OUT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_OUT.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_OUT.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment1_OUT_Rdp( f , x , w , u ) RESULT(Moment1_OUT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_OUT.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_OUT.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: <Moment1_DimIN>
PURE FUNCTION Moment1_DimIN_Rsp( f , x , w , Dim , u ) RESULT(Moment1_DimIN)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_DimIN.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_DimIN.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment1_DimIN_Rdp( f , x , w , Dim , u ) RESULT(Moment1_DimIN)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_DimIN.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_DimIN.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: <Moment1_DimOUT>
PURE FUNCTION Moment1_DimOUT_Rsp( f , x , w , Dim , u ) RESULT(Moment1_DimOUT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_DimOUT.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_DimOUT.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION Moment1_DimOUT_Rdp( f , x , w , Dim , u ) RESULT(Moment1_DimOUT)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments.f90.hdr"
INCLUDE "13-B-LIB_genMoments__Moment1_DimOUT.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__Moment1_DimOUT.f90.bdy"
!!--end--
END FUNCTION


SUBROUTINE TEST_prodMoment(Unit)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE SUB_genQuadrature_FROM_prod               !!((07-B-SUB_genQuadrature_FROM_prod.f90))
USE SUB_prodQuadrature                        !!((07-B-SUB_prodQuadrature.f90))

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: aorder,porder,errint,m,Nm
CHARACTER(100) :: errmsg
REAL(KIND_R),POINTER :: wa(:),xp(:),wp(:),xa(:,:),x(:,:),w(:)
INTEGER,POINTER :: m_map(:,:)

!!--begin--

!initialize
NULLIFY( wa,xa,wp,xp,m_map,x,w )


!get the orders
WRITE(*,*)"<aOrder> <pOrder>"
READ(*,*)aOrder,pOrder

!get the product quadrature
CALL prodQuadrature( "GL" , aOrder , &
                     "dGL" , pOrder , &
                      xa , xp , wa , wp , &
                      errint , errmsg , &
                      UniformZGeometry=.FALSE. )

!get the general quadrature
CALL genQuadrature_FROM_prod( xa , xp , wa , wp , &
  m_map , x , w , errint , errmsg )

CALL PRINT_genMomentTest(x,w,Unit)

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_QuadratureSet(x,w,Unit)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE PRN_RowVectors                            !!((12-C-PRN_RowVectors.f90))
USE PAR_Constants_Rdp,ONLY: c_1_by_4_times_PI !!((02-A-PAR_Constants_Rdp.f90))
USE FUN_STR                                   !!((05-B-FUN_STR.f90))
USE VAR_Units                                 !!((03-A-VAR_Units.f90))
USE FUN_Default                               !!((04-A-FUN_Default.f90))

REAL(KIND_R),POINTER :: x(:,:),w(:)
INTEGER,INTENT(IN),OPTIONAL :: Unit

INTEGER :: Unit_,i
REAL(KIND_R),POINTER :: f(:)
REAL(KIND_R) :: coeff,resid,tol
LOGICAL :: Pass
REAL(KIND_R),POINTER :: xplusw(:,:)
CHARACTER(10),POINTER :: NAME_x(:)

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

!print row vectors
WRITE(Unit_,*)" * Directions $\vec{\Omega}$ and weights $w$"
ALLOCATE( xplusw(SIZE(x,1)+1,SIZE(x,2)) ,NAME_x(SIZE(x,2)) )
xplusw(1:SIZE(x,1),:) = x
xplusw(1+SIZE(x,1),:) = w
DO i=1,SIZE(x,2)
 NAME_x(i) = TRIM(STR(i))
END DO
CALL PRINT_RowVectors( xplusw , unit=Unit_ , NAME_x=NAME_x , &
  NAME_i=(/"Ox(m)","Oy(m)","Oz(m)","w(m) "/) , NAME_index="m" )
DEALLOCATE( xplusw , NAME_x )
!!--end--
END SUBROUTINE

SUBROUTINE PRINT_genMomentTest(x,w,Unit)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE PRN_RowVectors                            !!((12-C-PRN_RowVectors.f90))
USE PAR_Constants_Rdp,ONLY: c_1_by_4_times_PI !!((02-A-PAR_Constants_Rdp.f90))
USE FUN_STR                                   !!((05-B-FUN_STR.f90))
USE VAR_Units                                 !!((03-A-VAR_Units.f90))
USE FUN_Default                               !!((04-A-FUN_Default.f90))

REAL(KIND_R),POINTER :: x(:,:),w(:)
INTEGER,INTENT(IN),OPTIONAL :: Unit
INTEGER :: Unit_,i
REAL(KIND_R),POINTER :: f(:)
REAL(KIND_R) :: coeff,resid,tol
LOGICAL :: Pass
REAL(KIND_R),POINTER :: xplusw(:,:)
CHARACTER(10),POINTER :: NAME_x(:)

!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

WRITE(Unit_,*)" * Diagnostic Tests of Moment Taking Routines"
!** set the test function <f>
ALLOCATE( f(SIZE(w)) )
f = c_1_by_4_times_Pi

!** Test of 0th moment integration such that full=half1+half2
tol = -1._KIND_R
Pass = TEST_Mom0f( f,x,w,tol=tol,resid=resid )
WRITE(Unit_,"(1x,a)")"Mom0f: "//MERGE("PASS","FAIL",Pass)//"("//TRIM(STR(resid,"(Es7.1)"))//&
  "/"//TRIM(STR(tol,"(Es7.1)"))//")"

!** Test of 1st moment integration such that full=half1+half2
tol = -1._KIND_R
Pass = TEST_Mom1f( f,x,w,tol=tol,resid=resid )
WRITE(Unit_,"(1x,a)")"Mom1f: "//MERGE("PASS","FAIL",Pass)//"("//TRIM(STR(resid,"(Es7.1)"))//&
  "/"//TRIM(STR(tol,"(Es7.1)"))//")"

!** Test of 1st moment integration such that getting each dimension individually
!   is the same as returning the full vector
tol = -1._KIND_R
Pass = TEST_Mom1d( f,x,w,tol=tol,resid=resid )
WRITE(Unit_,"(1x,a)")"Mom1d: "//MERGE("PASS","FAIL",Pass)//"("//TRIM(STR(resid,"(Es7.1)"))//&
  "/"//TRIM(STR(tol,"(Es7.1)"))//")"

!** Test of 2nd moment integration such that getting each dimension individually
!   is the same as returning the full tensor
tol = -1._KIND_R
Pass = TEST_Mom2d( f,x,w,tol=tol,resid=resid )
WRITE(Unit_,"(1x,a)")"Mom2d: "//MERGE("PASS","FAIL",Pass)//"("//TRIM(STR(resid,"(Es7.1)"))//&
  "/"//TRIM(STR(tol,"(Es7.1)"))//")"


!** print tables where we check the moments
WRITE(Unit_,*)" * Full Integral Check"
CALL PRINT_MomentsCheck0(x,w,Unit=Unit_)
WRITE(Unit_,*)" * Half Integral Check (+/- x half-spaces)"
CALL PRINT_MomentsCheck1(x,w,Unit=Unit_,u=(/1._KIND_R,0._KIND_R,0._KIND_R/))
WRITE(Unit_,*)" * Half Integral Check (+/- y half-spaces)"
CALL PRINT_MomentsCheck1(x,w,Unit=Unit_,u=(/0._KIND_R,1._KIND_R,0._KIND_R/))
WRITE(Unit_,*)" * Half Integral Check (+/- z half-spaces)"
CALL PRINT_MomentsCheck1(x,w,Unit=Unit_,u=(/0._KIND_R,0._KIND_R,1._KIND_R/))
WRITE(Unit_,*)" * Half Integral with Normal Check (+/- x half-spaces)"
CALL PRINT_MomentsCheck2(x,w,Unit=Unit_,u=(/1._KIND_R,0._KIND_R,0._KIND_R/))
WRITE(Unit_,*)" * Half Integral with Normal Check (+/- y half-spaces)"
CALL PRINT_MomentsCheck2(x,w,Unit=Unit_,u=(/0._KIND_R,1._KIND_R,0._KIND_R/))
WRITE(Unit_,*)" * Half Integral with Normal Check (+/- z half-spaces)"
CALL PRINT_MomentsCheck2(x,w,Unit=Unit_,u=(/0._KIND_R,0._KIND_R,1._KIND_R/))



!!--end--
END SUBROUTINE

SUBROUTINE PRINT_MomentsCheck0( x,w,Unit )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp                         !!((02-A-PAR_Constants_Rdp.f90))
USE FUN_STR                                   !!((05-B-FUN_STR.f90))
USE PRN_Table                                 !!((11-B-PRN_Table.f90))
USE SUB_CLEAR                                 !!((04-A-SUB_CLEAR.f90))
REAL(KIND_R),INTENT(IN) :: x(:,:),w(SIZE(x,2))
REAL(KIND_R) :: f(SIZE(x,2))
INTEGER,INTENT(IN),OPTIONAL :: Unit
CHARACTER(64),ALLOCATABLE :: T(:,:)
REAL(KIND_R) :: m0,m1(3),m2(6)

!!--begin--

ALLOCATE( T(11,5) )
CALL CLEAR(T)
T(2:,1 ) = (/"0  ","1x ","1y ","1z ","2xx","2yy","2zz","2xy","2xz","2yz"/)
T(1 ,2:) = (/" 1/4*pi","Ox/4*pi","Oy/4*pi","Oz/4*pi"/)

f = c_1_by_4_times_PI
CALL UpdateColumn(2)

f = x(1,:)*c_1_by_4_times_PI
CALL UpdateColumn(3)

f = x(2,:)*c_1_by_4_times_PI
CALL UpdateColumn(4)

f = x(3,:)*c_1_by_4_times_PI
CALL UpdateColumn(5)

CALL PRINT_Table(T,Unit=Unit)

DEALLOCATE( T )

!!--end--
CONTAINS

SUBROUTINE UpdateColumn(col)
INTEGER,INTENT(IN) :: col

m0 = Moment0(f,x,w)
m1 = Moment1(f,x,w)
m2 = Moment2(f,x,w)
T(02,col) = STR(m0,"(F)")
T(03,col) = STR(m1(1),"(F)")
T(04,col) = STR(m1(2),"(F)")
T(05,col) = STR(m1(3),"(F)")
T(06,col) = STR(m2(1),"(F)")
T(07,col) = STR(m2(2),"(F)")
T(08,col) = STR(m2(3),"(F)")
T(09,col) = STR(m2(4),"(F)")
T(10,col) = STR(m2(5),"(F)")
T(11,col) = STR(m2(6),"(F)")

END SUBROUTINE

END SUBROUTINE


SUBROUTINE RENORMALIZE_Rdp(x,w)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(INOUT) :: x(:,:),w(SIZE(x,2))
REAL(KIND_R) :: c_4pi,sum_w,norm_x
INTEGER :: m
!!--begin--
c_4pi = 8*ACOS(0._KIND_R)

!make sure weights add up to 4pi
sum_w = SUM(w)
w = c_4pi*w/sum_w

!make sure the directions are unit vectors
DO m=1,SIZE(x,2)
 norm_x = SQRT(SUM(x(:,m)**2))
 x(:,m) = x(:,m)/norm_x
END DO

!!--end--
END SUBROUTINE

SUBROUTINE PRINT_MomentsCheck1( x,w,Unit,u)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp                         !!((02-A-PAR_Constants_Rdp.f90))
USE FUN_STR                                   !!((05-B-FUN_STR.f90))
USE PRN_Table                                 !!((11-B-PRN_Table.f90))
USE SUB_CLEAR                                 !!((04-A-SUB_CLEAR.f90))
USE FUN_Default                               !!((04-A-FUN_Default.f90))
REAL(KIND_R),INTENT(IN) :: x(:,:),w(SIZE(x,2))
REAL(KIND_R),INTENT(IN),OPTIONAL :: u(SIZE(x,1))
REAL(KIND_R) :: f(SIZE(x,2))
INTEGER,INTENT(IN),OPTIONAL :: Unit
CHARACTER(64),ALLOCATABLE :: T(:,:)
REAL(KIND_R) :: m0,mi(3),mo(3),u_(3)

!!--begin--
u_ = DEFAULT( (/1._KIND_R,0._KIND_R,0._KIND_R/) , u )

ALLOCATE( T(9,5) )
CALL CLEAR(T)
T(2:,1 ) = (/"IN-0  ","IN-1x ","IN-1y ","IN-1z ","OUT-0 ","OUT-1x","OUT-1y","OUT-1z"/)
T(1 ,2:) = (/" 1/4*pi","Ox/4*pi","Oy/4*pi","Oz/4*pi"/)

f = c_1_by_4_times_PI
CALL UpdateColumn(2)

f = x(1,:)*c_1_by_4_times_PI
CALL UpdateColumn(3)

f = x(2,:)*c_1_by_4_times_PI
CALL UpdateColumn(4)

f = x(3,:)*c_1_by_4_times_PI
CALL UpdateColumn(5)

CALL PRINT_Table(T,Unit=Unit)

DEALLOCATE( T )

!!--end--
CONTAINS

SUBROUTINE UpdateColumn(col)
INTEGER,INTENT(IN) :: col

mi = Moment1_IN (f,x,w,u_)
mo = Moment1_OUT(f,x,w,u_)


m0 = Moment0_IN(f,x,w,u_)
T(02,col) = STR(m0,"(F)")
T(03,col) = STR(mi(1),"(F)")
T(04,col) = STR(mi(2),"(F)")
T(05,col) = STR(mi(3),"(F)")

m0 = Moment0_OUT(f,x,w,u_)
T(06,col) = STR(m0,"(F)")
T(07,col) = STR(mo(1),"(F)")
T(08,col) = STR(mo(2),"(F)")
T(09,col) = STR(mo(3),"(F)")

END SUBROUTINE

END SUBROUTINE



SUBROUTINE PRINT_MomentsCheck2( x,w,Unit,u)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp                         !!((02-A-PAR_Constants_Rdp.f90))
USE FUN_STR                                   !!((05-B-FUN_STR.f90))
USE PRN_Table                                 !!((11-B-PRN_Table.f90))
USE SUB_CLEAR                                 !!((04-A-SUB_CLEAR.f90))
USE FUN_Default                               !!((04-A-FUN_Default.f90))
REAL(KIND_R),INTENT(IN) :: x(:,:),w(SIZE(x,2))
REAL(KIND_R),INTENT(IN),OPTIONAL :: u(SIZE(x,1))
REAL(KIND_R) :: f(SIZE(x,2))
INTEGER,INTENT(IN),OPTIONAL :: Unit
CHARACTER(64),ALLOCATABLE :: T(:,:)
REAL(KIND_R) :: m,mi,mo,u_(3)

!!--begin--
u_ = DEFAULT( (/1._KIND_R,0._KIND_R,0._KIND_R/) , u )

ALLOCATE( T(4,5) )
CALL CLEAR(T)
T(2:,1 ) = (/"1    ","IN-1 ","OUT-1"/)
T(1 ,2:) = (/" 1/4*pi","Ox/4*pi","Oy/4*pi","Oz/4*pi"/)

f = c_1_by_4_times_PI
CALL UpdateColumn(2)

f = x(1,:)*c_1_by_4_times_PI
CALL UpdateColumn(3)

f = x(2,:)*c_1_by_4_times_PI
CALL UpdateColumn(4)

f = x(3,:)*c_1_by_4_times_PI
CALL UpdateColumn(5)

CALL PRINT_Table(T,Unit=Unit)

DEALLOCATE( T )

!!--end--
CONTAINS

SUBROUTINE UpdateColumn(col)
INTEGER,INTENT(IN) :: col

m  = Moment1_Normal   (f,x,w,u_)
mi = Moment1_NormalIN (f,x,w,u_)
mo = Moment1_NormalOUT(f,x,w,u_)

T(02,col) = STR(m ,"(F)")
T(03,col) = STR(mi,"(F)")
T(04,col) = STR(mo,"(F)")

END SUBROUTINE

END SUBROUTINE




FUNCTION TEST_Mom0f( f,x,w,tol,resid ) RESULT(Pass)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments__TEST.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__TEST.f90.sup"
INCLUDE "13-B-LIB_genMoments__TEST_Mom0f.f90.bdy"
INCLUDE "13-B-LIB_genMoments__TEST.f90.wup"
!!--end--
END FUNCTION



FUNCTION TEST_Mom1f( f,x,w,tol,resid ) RESULT(Pass)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments__TEST.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__TEST.f90.sup"
INCLUDE "13-B-LIB_genMoments__TEST_Mom1f.f90.bdy"
INCLUDE "13-B-LIB_genMoments__TEST.f90.wup"
!!--end--
END FUNCTION



FUNCTION TEST_Mom1d( f,x,w,tol,resid ) RESULT(Pass)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments__TEST.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__TEST.f90.sup"
INCLUDE "13-B-LIB_genMoments__TEST_Mom1d.f90.bdy"
INCLUDE "13-B-LIB_genMoments__TEST.f90.wup"
!!--end--
END FUNCTION



FUNCTION TEST_Mom2d( f,x,w,tol,resid ) RESULT(Pass)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-LIB_genMoments__TEST.f90.hdr"
!!--begin--
INCLUDE "13-B-LIB_genMoments__TEST.f90.sup"
INCLUDE "13-B-LIB_genMoments__TEST_Mom2d.f90.bdy"
INCLUDE "13-B-LIB_genMoments__TEST.f90.wup"
!!--end--
END FUNCTION


END MODULE
