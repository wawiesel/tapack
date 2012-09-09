!!# MODULE <<FUN_CoefficientBinomial>>
MODULE FUN_CoefficientBinomial

!!## PURPOSE
!! Defines function <CoefficientBinomial(n,k)> which produces the
!! binomial coefficient,
!
!!             / n \         n!
!!            |     | = -----------
!!             \ k /     (n-k)! k!
!
!! or in \LaTeX, the formula is,
!! $$
!!    \left( {\begin{array}{*{20}c} n \\ k \\ \end{array}} \right) = \frac{{n!}}{{k!\left( {n - k} \right)!}}
!! $$
!

!!## USAGE
!
!!    y = CoefficientBinomial(n,k)
!!
!! where <y> is <REAL(KIND_Rdp)>, <n> and <k> are integers,
!! <INTEGER(KIND_I1)>, <INTEGER(KIND_I2)>, <INTEGER(KIND_I4)>,
!! or <INTEGER(KIND_I8).>


!!## EXTERNAL KINDS
!! * kind of the real function return value
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                                  !!((01-A-KND_IntrinsicTypes.f90))
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,KIND_I1,KIND_I2,KIND_I4,KIND_I8 !!((01-A-KND_IntrinsicTypes.f90))


!!## EXTERNAL PROCEDURES
USE FUN_FactorialLn,ONLY: FactorialLn                                          !!((08-B-FUN_FactorialLn.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
!! * debug with PUBLIC default access, run with PRIVATE default access
PRIVATE

!!## FUNCTION OVERLOADING
INTERFACE CoefficientBinomial
 MODULE PROCEDURE CoefficientBinomial_I1
 MODULE PROCEDURE CoefficientBinomial_I2
 MODULE PROCEDURE CoefficientBinomial_I4
 MODULE PROCEDURE CoefficientBinomial_I8
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: CoefficientBinomial

!!## CONTAINED PROCEDURES
CONTAINS

!!### CoefficientBinomial_I1
PURE ELEMENTAL FUNCTION CoefficientBinomial_I1( n , k ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1                                   !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "10-B-FUN_CoefficientBinomial.f90.hdr"
!!--begin--
INCLUDE "10-B-FUN_CoefficientBinomial.f90.bdy"
!!--end--
END FUNCTION

!!### CoefficientBinomial_I2
PURE ELEMENTAL FUNCTION CoefficientBinomial_I2( n , k ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2                                   !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "10-B-FUN_CoefficientBinomial.f90.hdr"
!!--begin--
INCLUDE "10-B-FUN_CoefficientBinomial.f90.bdy"
!!--end--
END FUNCTION

!!### CoefficientBinomial_I4
PURE ELEMENTAL FUNCTION CoefficientBinomial_I4( n , k ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4                                   !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "10-B-FUN_CoefficientBinomial.f90.hdr"
!!--begin--
INCLUDE "10-B-FUN_CoefficientBinomial.f90.bdy"
!!--end--
END FUNCTION

!!### CoefficientBinomial_I8
PURE ELEMENTAL FUNCTION CoefficientBinomial_I8( n , k ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8                                   !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "10-B-FUN_CoefficientBinomial.f90.hdr"
!!--begin--
INCLUDE "10-B-FUN_CoefficientBinomial.f90.bdy"
!!--end--
END FUNCTION

END MODULE
