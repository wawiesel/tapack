!!# KINDS MODULE: <<KND_EddingtonFactors>>
MODULE KND_EddingtonFactors


!!## PURPOSE
!! Define the real kind for the Eddington factors, <KIND_Eddington>.



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
!! The real kind for the scalar flux is <<KIND_Eddington>>.
INTEGER,PARAMETER :: KIND_Eddington  = KIND_Rdp



!!## PUBLIC ACCESS LIST
PUBLIC :: KIND_Eddington



END MODULE
