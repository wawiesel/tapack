!!# KINDS MODULE: <<KND_Source>>
MODULE KND_Source


!!## PURPOSE
!! Define the real kind for the source, <KIND_Source>, and
!! the external source <KIND_ExtSource>.



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
!! The real kind for the combined source is <<KIND_Source>> and
!! just the external source is <<KIND_ExtSource>>.
INTEGER,PARAMETER :: KIND_Source    = KIND_Rdp
INTEGER,PARAMETER :: KIND_ExtSource = KIND_Rdp



!!## PUBLIC ACCESS LIST
PUBLIC :: KIND_Source
PUBLIC :: KIND_ExtSource



END MODULE
