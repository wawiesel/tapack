!!# KINDS MODULE: <<KND_MoCshort>>
MODULE KND_MoCshort


!!## PURPOSE
!! Define the kind <KIND_MCS> for the Method of Characteristics
!! with ``short'' (intra-cell) interpolation transport method
!! (with acronym MCS) and make accessible some kinds defined elsewhere.



!!## MODULE DEPENDENCIES
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
USE KND_AngularFluxes  !!((02-A-KND_AngularFluxes.f90))
USE KND_ScalarFluxes   !!((02-A-KND_ScalarFluxes.f90))
USE KND_Currents       !!((02-A-KND_Currents.f90))
USE KND_Source         !!((02-A-KND_Source.f90))



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
!! The general purpose kind <<KIND_MCS>> for this package.
INTEGER,PARAMETER :: KIND_MCS = KIND_Rdp



!!## PUBLIC ACCESS LIST
PUBLIC :: KIND_AngularFlux
PUBLIC :: KIND_Current
PUBLIC :: KIND_MCS
PUBLIC :: KIND_ExtSource
PUBLIC :: KIND_ScalarFlux



END MODULE
