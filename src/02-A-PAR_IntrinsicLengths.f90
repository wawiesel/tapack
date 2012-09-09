!!## MODULE: PARAMETERS for  IntrinsicLengths
MODULE PAR_IntrinsicLengths

!!### PURPOSE
!! Defines the lengths of intrinsic type-kinds string
!! representation in scientific format, <+0.123456E+07>, for reals,
!! <(+0.1234567E+08,+0.1234567E+08)> for complex, <+012345678>, for integer
!! and <___F>,<___T> for <.FALSE.>,<.TRUE.> where <"_"> is a space.



!!### DETAILS
!! All other numeric formats should use the same or less characters to
!! represent the given value and the minimum length necessary for internal
!! READ/WRITE statements to work.



!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: & !!((01-A-KND_IntrinsicTypes.f90))
              KIND_Rsp,KIND_Rdp,&
              KIND_Csp,KIND_Cdp,&
              KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
              KIND_L1,KIND_L2,KIND_L4,&
              KIND_I,KIND_L,KIND_R,KIND_C



!!### HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 154.2006
!         Contact  = william.wieselquist AT gmail.com



!!### DEFAULT IMPLICIT
IMPLICIT NONE



!!### DEFAULT ACCESS
PUBLIC



!!### PARAMETERS DECLARATIONS 1
!! @ lengths of all intrinsics for internal file
!!   string writing/reading to work
INTEGER,PARAMETER :: LEN_L1  = 4
INTEGER,PARAMETER :: LEN_L2  = 4
INTEGER,PARAMETER :: LEN_L4  = 4
INTEGER,PARAMETER :: LEN_I1  = 6
INTEGER,PARAMETER :: LEN_I2  = 8
INTEGER,PARAMETER :: LEN_I4  = 13
INTEGER,PARAMETER :: LEN_I8  = 22
INTEGER,PARAMETER :: LEN_Rsp = PRECISION(1._KIND_Rsp)   + 10
INTEGER,PARAMETER :: LEN_Rdp = PRECISION(1._KIND_Rdp)   + 11
INTEGER,PARAMETER :: LEN_Csp = PRECISION(1._KIND_Csp)*2 + 23
INTEGER,PARAMETER :: LEN_Cdp = PRECISION(1._KIND_Cdp)*2 + 24


!!### PARAMETERS DECLARATIONS 2
!! @ lengths of default intrinsics
INTEGER,PARAMETER :: LEN_L   = 4
INTEGER,PARAMETER :: LEN_I   = 13
INTEGER,PARAMETER :: LEN_R   = PRECISION(1._KIND_R  )   + 10
INTEGER,PARAMETER :: LEN_C   = PRECISION(1._KIND_C  )*2 + 23


!!### PARAMETERS DECLARATIONS 3
!! @ length of a formatting string for a single number
INTEGER,PARAMETER :: LEN_Sfmt= 10



END MODULE
