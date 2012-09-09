!!# MODULE <<VAR_XSMonkey>>
MODULE VAR_XSMonkey

!!## PURPOSE
!! Provide cross section storage.

!!## DETAILS
!! Their form is:
! X(g,i)
!   g = energy group
!   i = cell index
!! where X is a cross section

USE KND_XSExpansion !!((02-A-KND_XSExpansion.f90))

!12. Macroscopic cross sections (nuclear data components of
!!  coefficients of transport and acceleration equations)
REAL(KIND_Mac),POINTER :: MacT ( : , : ) => NULL()
REAL(KIND_Mac),POINTER :: MacS ( : , : ) => NULL()
REAL(KIND_Mac),POINTER :: MacNu( : , : ) => NULL()
REAL(KIND_Mac),POINTER :: MacF ( : , : ) => NULL()

ENDMODULE
