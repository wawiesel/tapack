!!## MODULE: FUNCTION  SmartOption
MODULE FUN_SmartOption
!!### PURPOSE
!! An <OPTION_ARRAY> contains a set of options (of any intrinsic
!! type-kind) to use for various options in your code.  It could
!! be a list of tolerances to use for an iterative system solution,
!! it could be the method number to use for each time step, etc.


!!### EXAMPLE
!! You are solving an ODE and would like to use one of three
!! solvers based on some <METRIC>.  You have some ranges
!!  @ use solver1 for m0 .LE. m .LT. m1
!!  @ use solver2 for m1 .LE. m .LT. m2
!!  @ use solver3 for m2 .LE. m .LT. m3
!!
!! so now if you pass <METRIC_ARRAY = [m01,m12,m23]>, where
!! <m01=(m0+m1)/2>, <m12=(m1+m2)/2>, and <m23=(m2+m3)/2>, and the
!! value of the metric, <METRIC = m>, then the returned option
!! will be the correct solver, with the added functionality
!! of returning the first option if <m.LT.m0> or last option
!! if <m.GT.m3>.

!!### USAGE
!! The return value of <OPTION> is dependent on the optional
!! variables present.  The optional variables which appear
!! first take presidence.
!!
!! @ the <INDEX> optional indicates the index of <OPTION_ARRAY>,
!!   with the added intelligence of selecting the lower bound
!!   if the <INDEX> is less than the lower bound of <OPTION_ARRAY>,
!!   or the uppper bound of <OPTION_ARRAY> is if the <INDEX>
!!   is greater than the upper bound.  The lower bound is 1, the
!!   upper bound is the <SIZE> of the array.
!!
!! @ the <METRIC_ARRAY> is an array of reals which specify
!!   a discrete set of conditions.  In this case, the index
!!   is selected as
!
!           INDEX = NearestScalar( METRIC_ARRAY , METRIC , DEFAULT )
!
!!   or
!
!           INDEX = MINLOC( METRIC_ARRAY )
!
!!   if <METRIC> is not present.

!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,KIND_Csp,KIND_Cdp,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
              KIND_L1,KIND_L2,KIND_L4

!!#### PROCEDURES
USE FUN_NEARLOC                                                    !!((06-A-FUN_NEARLOC.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE


!!### DEFAULT ACCESS
PRIVATE


!!### PROCEDURE OVERLOADING
INTERFACE SmartOption
 MODULE PROCEDURE SmartOption_Rsp
 MODULE PROCEDURE SmartOption_Rdp
 MODULE PROCEDURE SmartOption_Csp
 MODULE PROCEDURE SmartOption_Cdp
 MODULE PROCEDURE SmartOption_I1
 MODULE PROCEDURE SmartOption_I2
 MODULE PROCEDURE SmartOption_I4
 MODULE PROCEDURE SmartOption_I8
 MODULE PROCEDURE SmartOption_L1
 MODULE PROCEDURE SmartOption_L2
 MODULE PROCEDURE SmartOption_L4
 MODULE PROCEDURE SmartOption_default_Rsp
 MODULE PROCEDURE SmartOption_default_Rdp
 MODULE PROCEDURE SmartOption_default_Csp
 MODULE PROCEDURE SmartOption_default_Cdp
 MODULE PROCEDURE SmartOption_default_I1
 MODULE PROCEDURE SmartOption_default_I2
 MODULE PROCEDURE SmartOption_default_I4
 MODULE PROCEDURE SmartOption_default_I8
 MODULE PROCEDURE SmartOption_default_L1
 MODULE PROCEDURE SmartOption_default_L2
 MODULE PROCEDURE SmartOption_default_L4
END INTERFACE


!!### PUBLIC ACCESS LIST
PUBLIC :: SmartOption


!!## PROCEDURES
CONTAINS


!!### PURE FUNCTION: SmartOption_Rsp
PURE FUNCTION SmartOption_Rsp( OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC  ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_R.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_Rdp
PURE FUNCTION SmartOption_Rdp( OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC  ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_R.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_Csp
PURE FUNCTION SmartOption_Csp( OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC  ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_C.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_Cdp
PURE FUNCTION SmartOption_Cdp( OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC  ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_C.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_I1
PURE FUNCTION SmartOption_I1( OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC  ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_I.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_I2
PURE FUNCTION SmartOption_I2( OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC  ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_I.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_I4
PURE FUNCTION SmartOption_I4( OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC  ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_I.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_I8
PURE FUNCTION SmartOption_I8( OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC  ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_I.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: SmartOption_L1
PURE FUNCTION SmartOption_L1( OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC  ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_L.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_L2
PURE FUNCTION SmartOption_L2( OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC  ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_L.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_L4
PURE FUNCTION SmartOption_L4( OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC  ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_L.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_default_Rsp
PURE FUNCTION SmartOption_default_Rsp( DEFAULT , OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_default_R.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption_default.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_default_Rdp
PURE FUNCTION SmartOption_default_Rdp( DEFAULT , OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_default_R.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption_default.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_default_Csp
PURE FUNCTION SmartOption_default_Csp( DEFAULT , OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_default_C.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption_default.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_default_Cdp
PURE FUNCTION SmartOption_default_Cdp( DEFAULT , OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_default_C.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption_default.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_default_I1
PURE FUNCTION SmartOption_default_I1( DEFAULT , OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_default_I.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption_default.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_default_I2
PURE FUNCTION SmartOption_default_I2( DEFAULT , OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_default_I.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption_default.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_default_I4
PURE FUNCTION SmartOption_default_I4( DEFAULT , OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_default_I.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption_default.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_default_I8
PURE FUNCTION SmartOption_default_I8( DEFAULT , OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_default_I.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption_default.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: SmartOption_default_L1
PURE FUNCTION SmartOption_default_L1( DEFAULT , OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_default_L.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption_default.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_default_L2
PURE FUNCTION SmartOption_default_L2( DEFAULT , OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_default_L.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption_default.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SmartOption_default_L4
PURE FUNCTION SmartOption_default_L4( DEFAULT , OPTION_ARRAY , &
  INDEX , METRIC_ARRAY , METRIC ) RESULT(Option)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4                       !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-FUN_SmartOption_default_L.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_SmartOption_default.f90.bdy"
!!--end--
END FUNCTION


END MODULE
