!!# KINDS MODULE: <<KND_QuasiDiffusion>>
MODULE KND_QuasiDiffusion


!!## PURPOSE
!! Define the kind <KIND_QDF> for the Quasidiffusion package
!! (with acronym QDF).



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
!! The general purpose kind <<KIND_QDF>> for this package.
INTEGER,PARAMETER :: KIND_QDF = KIND_Rdp



!!## PUBLIC ACCESS LIST
PUBLIC :: KIND_QDF



END MODULE
