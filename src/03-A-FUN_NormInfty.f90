!!# MODULE <<FUN_NormInfty>>
MODULE FUN_NormInfty

!!## PURPOSE
!! Defines the $L^\infty$ norm of <A>, with optional weight <W>.


!!## DETAILS
!! The $L^\infty$ norm is a discrete norm of the form.
!!
!! $$ L^\infty(A) = \max_i | A_i | W_i $$
!!
!! Where for example $A_i$ could be average values
!! $A_i = \frac{1}{\Delta x_i} \int_{a_i}^{b_i} A(x) dx$
!! and the weights $W_i = \Delta x_i$ returned from a finite
!! volume solution of a PDE.


!!## USAGE
!
!       s = NormInfty( A [,W] )
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



!!## NormInfty PROCEDURE OVERLOADING
INTERFACE NormInfty
 MODULE PROCEDURE NormInfty_A1_Rsp
 MODULE PROCEDURE NormInfty_A2_Rsp
 MODULE PROCEDURE NormInfty_A3_Rsp
 MODULE PROCEDURE NormInfty_A4_Rsp
 MODULE PROCEDURE NormInfty_A5_Rsp
 MODULE PROCEDURE NormInfty_A6_Rsp
 MODULE PROCEDURE NormInfty_A7_Rsp

 MODULE PROCEDURE NormInfty_A1_Rdp
 MODULE PROCEDURE NormInfty_A2_Rdp
 MODULE PROCEDURE NormInfty_A3_Rdp
 MODULE PROCEDURE NormInfty_A4_Rdp
 MODULE PROCEDURE NormInfty_A5_Rdp
 MODULE PROCEDURE NormInfty_A6_Rdp
 MODULE PROCEDURE NormInfty_A7_Rdp
END INTERFACE
INTERFACE NormInfty
 MODULE PROCEDURE NormInfty_A1W1_Rsp
 MODULE PROCEDURE NormInfty_A2W2_Rsp
 MODULE PROCEDURE NormInfty_A3W3_Rsp
 MODULE PROCEDURE NormInfty_A4W4_Rsp
 MODULE PROCEDURE NormInfty_A5W5_Rsp
 MODULE PROCEDURE NormInfty_A6W6_Rsp
 MODULE PROCEDURE NormInfty_A7W7_Rsp

 MODULE PROCEDURE NormInfty_A1W1_Rdp
 MODULE PROCEDURE NormInfty_A2W2_Rdp
 MODULE PROCEDURE NormInfty_A3W3_Rdp
 MODULE PROCEDURE NormInfty_A4W4_Rdp
 MODULE PROCEDURE NormInfty_A5W5_Rdp
 MODULE PROCEDURE NormInfty_A6W6_Rdp
 MODULE PROCEDURE NormInfty_A7W7_Rdp
END INTERFACE



!!## PUBLIC ACCESS LIST
PUBLIC :: NormInfty



!## MODULE PROCEDURES
CONTAINS


!!### L-2 Norm for single precision
PURE FUNCTION NormInfty_A1_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A2_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A3_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A4_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A5_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A6_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A7_Rsp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION



!!### L-2 Norm for double precision
PURE FUNCTION NormInfty_A1_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A2_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A3_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A4_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A5_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A6_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A7_Rdp(A) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS( A ) )
!!--end--
END FUNCTION




!!### L-2 Norm for single precision
PURE FUNCTION NormInfty_A1W1_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:),W(:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A2W2_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:),W(:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A3W3_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:),W(:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A4W4_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:),W(:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A5W5_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:),W(:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A6W6_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:),W(:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A7W7_Rsp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:,:),W(:,:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION



!!### L-2 Norm for double precision
PURE FUNCTION NormInfty_A1W1_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:),W(:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A2W2_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:),W(:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A3W3_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:),W(:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A4W4_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:),W(:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A5W5_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:),W(:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A6W6_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:),W(:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION

PURE FUNCTION NormInfty_A7W7_Rdp(A,W) RESULT(s)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A(:,:,:,:,:,:,:),W(:,:,:,:,:,:,:)
REAL(KIND_R) :: s
!!--begin--
s = MAXVAL( ABS(A) * W )
!!--end--
END FUNCTION



END MODULE
