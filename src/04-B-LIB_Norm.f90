!!# MODULE <<LIB_Norm>>
MODULE LIB_Norm
!!## PURPOSE
!! Puts the $\ell^1$, $\ell^2$, and $L^\infty$ norms
!! in one place.


!!## USAGE
!! * the $\ell^\infty$ norm,
!
!        s = NormInfty( A [,W] )
!
!! * the $\ell^2$ norm,
!
!        s = NormEll2 ( A [,W] )
!
!! * the $\ell^1$ norm,
!
!        s = NormEll1 ( A [,W] )
!
!! * the squared $\ell^2$ norm,
!
!        s = NormEll2Sqr ( A [,W] )
!

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!#### EXTERNAL PROCEDURES
USE FUN_NormInfty                              !!((03-A-FUN_NormInfty.f90))
USE FUN_NormEll2                               !!((03-A-FUN_NormEll2.f90))
USE FUN_NormEll1                               !!((03-A-FUN_NormEll1.f90))
USE FUN_NormEll2Sqr                            !!((03-A-FUN_NormEll2Sqr.f90))


!!#### DEFAULT IMPLICIT
IMPLICIT NONE


!!#### DEFAULT ACCESS
PRIVATE


!!#### PUBLIC ACCESS LIST
PUBLIC :: NormInfty,NormEll2,NormEll1,NormEll2Sqr


END MODULE
