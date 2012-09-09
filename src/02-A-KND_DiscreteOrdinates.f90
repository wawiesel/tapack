!!# KINDS MODULE: <<KND_DiscreteOrdinates>>
MODULE KND_DiscreteOrdinates


!!## PURPOSE
!! Define the real kind for the discrete ordinates package
!! (DOR) angular integration variables, <KIND_DOR>.



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
!! The real kind for the discrete ordinates package is <<KIND_DOR>>.
INTEGER,PARAMETER :: KIND_DOR = KIND_Rdp



!!## PUBLIC ACCESS LIST
PUBLIC :: KIND_DOR



END MODULE
