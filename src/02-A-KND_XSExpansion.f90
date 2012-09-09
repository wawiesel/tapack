!!# KINDS MODULE: <<KND_XSExpansion>>
MODULE KND_XSExpansion


!!## PURPOSE
!! Provides the kinds needed by modules which manipulate
!! cross sections (XS) via the Cross Section Expansion
!! (XSe) package.



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
!! * <<KIND_Expos>> is the kind for Exposures.
!! * <<KIND_DNuc>>  is the kind for Nuclide densities.
!! * <<KIND_Mic>>   is the kind for Microscopic cross sections.
!! * <<KIND_Mac>>   is the kind for Macroscopic cross sections.
!! * <<KIND_Adf>>   is the kind for Assembly Discontinuity Factors (ADFs).
!! * <<KIND_Power>> is the kind for the power to which
!!                  differences are raised.
!! * <<KIND_dTerm>> is the kind for cross section term differences.
!! * <<KIND_Frac>>  is the kind for set fractions.
INTEGER,PARAMETER :: KIND_Expos = KIND_Rdp
INTEGER,PARAMETER :: KIND_DNuc  = KIND_Rdp
INTEGER,PARAMETER :: KIND_Mic   = KIND_Rdp
INTEGER,PARAMETER :: KIND_Mac   = KIND_Rdp
INTEGER,PARAMETER :: KIND_Adf   = KIND_Rdp
INTEGER,PARAMETER :: KIND_Power = KIND_Rdp
INTEGER,PARAMETER :: KIND_dTerm = KIND_Rdp
INTEGER,PARAMETER :: KIND_Frac  = KIND_Rdp



!!## PUBLIC ACCESS LIST
PUBLIC :: KIND_Expos
PUBLIC :: KIND_DNuc
PUBLIC :: KIND_Mic
PUBLIC :: KIND_Mac
PUBLIC :: KIND_Adf
PUBLIC :: KIND_Power
PUBLIC :: KIND_dTerm
PUBLIC :: KIND_Frac



END MODULE
