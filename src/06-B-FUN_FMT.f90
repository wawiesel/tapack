!!# MODULE: FUNCTION FMT
MODULE FUN_FMT

!!## PURPOSE
!! The formatting function <FMT> returns <VS> (a varying string)
!! for input/output of a single numeric entity, which could be used
!! in any FORMAT specificer (like that in a <READ> or <WRITE> statement).

!!## DETAILS
!! The format spec is of the form  <Pre><F><y><.z><Post> where
!!  @ <F> is a format string (f,e,a,i,g, etc.),
!!  @ <y> is the field specifier,
!!  @ <z> is the number of significant digits,
!!  @ <Post> is a post-append string, and
!!  @ <Pre> is a pre-append string.


!!## OWNER
! [waw]: william.wieselquist@gmail.com


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes   ,ONLY: KIND_L1,KIND_L2,KIND_L4,& !!((01-A-KND_IntrinsicTypes.f90))
                  KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
                  KIND_Rsp,KIND_Rdp,&
                  KIND_Csp,KIND_Cdp,&
                  KIND_S,KIND_Sfmt


!!## EXTERNAL PARAMETERS
USE PAR_IntrinsicLengths,ONLY: LEN_L1,LEN_L2,LEN_L4,&     !!((02-A-PAR_IntrinsicLengths.f90))
                  LEN_I1,LEN_I2,LEN_I4,LEN_I8,&
                  LEN_Rsp,LEN_Rdp,&
                  LEN_Csp,LEN_Cdp


!!#### FORTRAN STANDARDS MODULES
USE ISO_varying_string                                    !!((03-A-ISO_varying_string.f90))


!!## GLOBAL BINARY OPERATORS
USE BOP_sEQ                                               !!((03-A-BOP_sEQ.f90))


!!## EXTERNAL PROCEDURES
USE FUN_STR                                               !!((05-B-FUN_STR.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE OVERLOADING
INTERFACE FMT
 MODULE PROCEDURE FMT_Sen
 MODULE PROCEDURE FMT_Rsp
 MODULE PROCEDURE FMT_Rdp
 MODULE PROCEDURE FMT_Csp
 MODULE PROCEDURE FMT_Cdp
 MODULE PROCEDURE FMT_I1
 MODULE PROCEDURE FMT_I2
 MODULE PROCEDURE FMT_I4
 MODULE PROCEDURE FMT_I8
 MODULE PROCEDURE FMT_L1
 MODULE PROCEDURE FMT_L2
 MODULE PROCEDURE FMT_L4
END INTERFACE

INTERFACE FMT_
 MODULE PROCEDURE FMT_null
END INTERFACE



!!## PUBLIC ACCESS LIST
PUBLIC :: FMT,FMT_
PUBLIC :: FMT_Sen
PUBLIC :: FMT_Rsp
PUBLIC :: FMT_Rdp
PUBLIC :: FMT_Csp
PUBLIC :: FMT_Cdp
PUBLIC :: FMT_I1
PUBLIC :: FMT_I2
PUBLIC :: FMT_I4
PUBLIC :: FMT_I8
PUBLIC :: FMT_L1
PUBLIC :: FMT_L2
PUBLIC :: FMT_L4

!!## MODULE PROCEDURES
CONTAINS


!!### FUNCTION: FMT_NULL
FUNCTION FMT_null( F , y , z , Post , Pre ) RESULT(VS)
INCLUDE "06-B-FUN_FMT_null.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_null.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_Sen
FUNCTION FMT_Sen( X , y , Post , Pre ) RESULT(VS)
INCLUDE "06-B-FUN_FMT_S.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_S.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_Rsp
FUNCTION FMT_Rsp( X , F , y , z , Post , Pre ) RESULT(VS)
!!#### LOCAL KINDS/LENGTHS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp             !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_R=LEN_Rsp

INCLUDE "06-B-FUN_FMT_R.f90.hdr"
INCLUDE "06-B-FUN_FMT.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_R.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_Rdp
FUNCTION FMT_Rdp( X , F , y , z , Post , Pre ) RESULT(VS)
!!#### LOCAL KINDS/LENGTHS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp             !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_R=LEN_Rdp

INCLUDE "06-B-FUN_FMT_R.f90.hdr"
INCLUDE "06-B-FUN_FMT.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_R.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_Csp
FUNCTION FMT_Csp( X , F , y , z , Post , Pre ) RESULT(VS)
!!#### LOCAL KINDS/LENGTHS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp             !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_C=LEN_Csp

INCLUDE "06-B-FUN_FMT_C.f90.hdr"
INCLUDE "06-B-FUN_FMT.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_C.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_Cdp
FUNCTION FMT_Cdp( X , F , y , z , Post , Pre ) RESULT(VS)
!!#### LOCAL KINDS/LENGTHS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp             !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_C=LEN_Cdp

INCLUDE "06-B-FUN_FMT_C.f90.hdr"
INCLUDE "06-B-FUN_FMT.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_C.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_I1
FUNCTION FMT_I1( X , F , y , z , Post , Pre ) RESULT(VS)
!!#### LOCAL KINDS/LENGTHS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1              !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_I=LEN_I1

INCLUDE "06-B-FUN_FMT_I.f90.hdr"
INCLUDE "06-B-FUN_FMT.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_I.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_I2
FUNCTION FMT_I2( X , F , y , z , Post , Pre ) RESULT(VS)
!!#### LOCAL KINDS/LENGTHS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2              !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_I=LEN_I2

INCLUDE "06-B-FUN_FMT_I.f90.hdr"
INCLUDE "06-B-FUN_FMT.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_I.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_I4
FUNCTION FMT_I4( X , F , y , z , Post , Pre ) RESULT(VS)
!!#### LOCAL KINDS/LENGTHS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4              !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_I=LEN_I4

INCLUDE "06-B-FUN_FMT_I.f90.hdr"
INCLUDE "06-B-FUN_FMT.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_I.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_I8
FUNCTION FMT_I8( X , F , y , z , Post , Pre ) RESULT(VS)
!!#### LOCAL KINDS/LENGTHS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8              !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_I=LEN_I8

INCLUDE "06-B-FUN_FMT_I.f90.hdr"
INCLUDE "06-B-FUN_FMT.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_I.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_L1
FUNCTION FMT_L1( X , F , y , z , Post , Pre ) RESULT(VS)
!!#### LOCAL KINDS/LENGTHS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1              !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_L=LEN_L1
INCLUDE "06-B-FUN_FMT_L.f90.hdr"
INCLUDE "06-B-FUN_FMT.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_L.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_L2
FUNCTION FMT_L2( X , F , y , z , Post , Pre ) RESULT(VS)
!!#### LOCAL KINDS/LENGTHS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2              !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_L=LEN_L2
INCLUDE "06-B-FUN_FMT_L.f90.hdr"
INCLUDE "06-B-FUN_FMT.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_L.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: FMT_L4
FUNCTION FMT_L4( X , F , y , z , Post , Pre ) RESULT(VS)
!!#### LOCAL KINDS/LENGTHS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4              !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_L=LEN_L4

INCLUDE "06-B-FUN_FMT_L.f90.hdr"
INCLUDE "06-B-FUN_FMT.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_FMT_L.f90.bdy"
INCLUDE "06-B-FUN_FMT.f90.bdy"
!!--end--
END FUNCTION


END MODULE
