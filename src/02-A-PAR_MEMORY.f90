!!# MODULE <<PAR_MEMORY>>
MODULE PAR_MEMORY

!!## PURPOSE
!! Define the number of bytes per element for
!! the intrinsic types and overhead for pointers and
!! arrays.



!!## DETAILS
!! Used for the <MEMORY> and <MEMORYn> functions.



!!## HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 104.2006
!         Contact  = william.wieselquist AT gmail.com



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PUBLIC



!!## EXTERNAL PARAMETERS
!! * bytes per element of intrinsic types
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_R   = 4
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_Rsp = 4
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_Rdp = 8
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_C   = 2*BYTES_PER_ELEMENT_R
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_Csp = 2*BYTES_PER_ELEMENT_Rsp
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_Cdp = 2*BYTES_PER_ELEMENT_Rdp
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_I1  = 1
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_I2  = 2
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_I   = 4
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_I4  = 4
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_I8  = 8
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_L   = 4
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_L1  = 1
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_L2  = 2
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_L4  = 4
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_S   = 4
INTEGER,PARAMETER :: BYTES_PER_ELEMENT_VS  = 4
!!
!! * overhead for containers
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_A0 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_A1 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_A2 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_A3 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_A4 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_A5 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_A6 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_A7 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_P0 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_P1 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_P2 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_P3 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_P4 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_P5 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_P6 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_P7 = 0
INTEGER,PARAMETER :: BYTES_FOR_OVERHEAD_VS = BYTES_PER_ELEMENT_I



END MODULE
