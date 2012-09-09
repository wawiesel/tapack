!!## COMMAND CARD: simple
MODULE CC1_simple
!!### PURPOSE
!! Defines the simple command \simple{val}.

!!#### MODULES
!! @ input/output toolbox
!! @ feeback object
!! @ case insensitive check for string equality
USE ISO_varying_string                        !!((03-A-ISO_varying_string.f90))
USE TBX_SIO                                   !!((10-A-TBX_SIO.f90))
USE USR_fdbk                                  !!((08-C-USR_fdbk.f90))
USE BOP_sEQ                                   !!((03-A-BOP_sEQ.f90))


!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE


!!### INTERFACES
INTERFACE simple
 MODULE PROCEDURE simple_L1
 MODULE PROCEDURE simple_L2
 MODULE PROCEDURE simple_L4
 MODULE PROCEDURE simple_I1
 MODULE PROCEDURE simple_I2
 MODULE PROCEDURE simple_I4
 MODULE PROCEDURE simple_I8
 MODULE PROCEDURE simple_Rsp
 MODULE PROCEDURE simple_Rdp
 MODULE PROCEDURE simple_S
 MODULE PROCEDURE simple_VS
END INTERFACE


!!### PUBLIC ACCESS LIST
PUBLIC :: simple


!!## MODULE PROCEDURES
CONTAINS

SUBROUTINE simple_L1( SIO , switch , fdbk )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "11-A-CC1_simple_L.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE simple_L2( SIO , switch , fdbk )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "11-A-CC1_simple_L.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE simple_L4( SIO , switch , fdbk )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "11-A-CC1_simple_L.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE simple_I1( SIO , val , fdbk , KEYS )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "11-A-CC1_simple_I.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE simple_I2( SIO , val , fdbk , KEYS )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "11-A-CC1_simple_I.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE simple_I4( SIO , val , fdbk , KEYS )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "11-A-CC1_simple_I.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE simple_I8( SIO , val , fdbk , KEYS )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "11-A-CC1_simple_I.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE simple_Rsp( SIO , val , fdbk )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "11-A-CC1_simple_R.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE simple_Rdp( SIO , val , fdbk )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "11-A-CC1_simple_R.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE simple_S( SIO , val , fdbk )
INCLUDE "11-A-CC1_simple_S.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE simple_VS( SIO , val , fdbk )
INCLUDE "11-A-CC1_simple_VS.f90.bdy"
!!--end--
END SUBROUTINE

ENDMODULE
