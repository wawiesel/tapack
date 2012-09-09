!!# KINDS MODULE: <<KND_AngularFluxes>>
MODULE KND_AngularFluxes


!!## PURPOSE
!! Define the real kind for the angular flux, <KIND_AngularFlux>.



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
!! The real kind for the angular flux is <<KIND_AngularFlux>>.
INTEGER,PARAMETER :: KIND_AngularFlux = KIND_Rdp



!!## PUBLIC ACCESS LIST
PUBLIC :: KIND_AngularFlux



END MODULE
