!!# MODULE <<FUN_NormEll2Sqr>>
MODULE FUN_NormEll2Sqr

!!## PURPOSE
!! Defines the $\ell^2$ norm of <A> squared, with optional weight <W>.


!!## DETAILS
!! The $\ell^2$-squared norm is just the $\ell^2$ norm without
!! the square root
!!
!! $$ [\ell^2(A)]^2 = \sum_i [ A_i ]^2 W_i  $$
!!
!! Where for example $A_i$ could be average values
!! $A_i = \frac{1}{\Delta x_i} \int_{a_i}^{b_i} A(x) dx$
!! and the weights $W_i = \Delta x_i$ returned from a finite
!! volume solution of a PDE.


!!## USAGE
!
!       s = NormEll2Sqr( A [,W] )
!

!!## REFERENCES
!
![1] Weisstein, Eric W. "L2-Norm." From MathWorld--A Wolfram Web Resource.
!    http://mathworld.wolfram.com/L2-Norm.html


!!## OWNER
![1] Wieselquist, William W. (william.wieselquist AT gmail.com)
!    July 8, 2006 - present


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE



!!## NormEll2Sqr PROCEDURE OVERLOADING
INTERFACE NormEll2Sqr
 MODULE PROCEDURE NormEll2Sqr_A1_Rsp
 MODULE PROCEDURE NormEll2Sqr_A2_Rsp
 MODULE PROCEDURE NormEll2Sqr_A3_Rsp
 MODULE PROCEDURE NormEll2Sqr_A4_Rsp
 MODULE PROCEDURE NormEll2Sqr_A5_Rsp
 MODULE PROCEDURE NormEll2Sqr_A6_Rsp
 MODULE PROCEDURE NormEll2Sqr_A7_Rsp

 MODULE PROCEDURE NormEll2Sqr_A1_Rdp
 MODULE PROCEDURE NormEll2Sqr_A2_Rdp
 MODULE PROCEDURE NormEll2Sqr_A3_Rdp
 MODULE PROCEDURE NormEll2Sqr_A4_Rdp
 MODULE PROCEDURE NormEll2Sqr_A5_Rdp
 MODULE PROCEDURE NormEll2Sqr_A6_Rdp
 MODULE PROCEDURE NormEll2Sqr_A7_Rdp
END INTERFACE
INTERFACE NormEll2Sqr
 MODULE PROCEDURE NormEll2Sqr_A1W1_Rsp
 MODULE PROCEDURE NormEll2Sqr_A2W2_Rsp
 MODULE PROCEDURE NormEll2Sqr_A3W3_Rsp
 MODULE PROCEDURE NormEll2Sqr_A4W4_Rsp
 MODULE PROCEDURE NormEll2Sqr_A5W5_Rsp
 MODULE PROCEDURE NormEll2Sqr_A6W6_Rsp
 MODULE PROCEDURE NormEll2Sqr_A7W7_Rsp

 MODULE PROCEDURE NormEll2Sqr_A1W1_Rdp
 MODULE PROCEDURE NormEll2Sqr_A2W2_Rdp
 MODULE PROCEDURE NormEll2Sqr_A3W3_Rdp
 MODULE PROCEDURE NormEll2Sqr_A4W4_Rdp
 MODULE PROCEDURE NormEll2Sqr_A5W5_Rdp
 MODULE PROCEDURE NormEll2Sqr_A6W6_Rdp
 MODULE PROCEDURE NormEll2Sqr_A7W7_Rdp
END INTERFACE



!!## PUBLIC ACCESS LIST
PUBLIC :: NormEll2Sqr



!## MODULE PROCEDURES
CONTAINS


!!### L-2 Norm for single precision
PURE FUNCTION NormEll2Sqr_A1_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A2_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A3_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A4_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A5_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A6_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A7_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION



!!### L-2 Norm for double precision
PURE FUNCTION NormEll2Sqr_A1_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A2_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A3_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A4_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A5_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A6_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A7_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 )
!!--end--
END FUNCTION




!!### L-2 Norm for single precision
PURE FUNCTION NormEll2Sqr_A1W1_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:),W(:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A2W2_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:),W(:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A3W3_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:),W(:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A4W4_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:),W(:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A5W5_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:),W(:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A6W6_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:),W(:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A7W7_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:,:),W(:,:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION



!!### L-2 Norm for double precision
PURE FUNCTION NormEll2Sqr_A1W1_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:),W(:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A2W2_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:),W(:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A3W3_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:),W(:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A4W4_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:),W(:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A5W5_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:),W(:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A6W6_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:),W(:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION

PURE FUNCTION NormEll2Sqr_A7W7_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:,:),W(:,:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = SUM( ( A )**2 * W )
!!--end--
END FUNCTION



END MODULE
