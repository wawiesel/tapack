!!# KINDS MODULE: <<KND_Currents>>
MODULE KND_Currents


!!## PURPOSE
!! Define the real kind for the current, <KIND_Current>.



!!## MODULE DEPENDENCIES
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))



!!## OWNER(S)
!! [waw] William A. Wieselquist, william.wieselquist AT gmail.com



!!## HISTORY
!!
!! + (01/01/2007) created [waw]



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE



!!## KIND DECLARATIONS
!! The real kind for the current is <<KIND_Current>>.
INTEGER,PARAMETER :: KIND_Current = KIND_Rdp



!!## PUBLIC ACCESS LIST
PUBLIC :: KIND_Current



END MODULE
