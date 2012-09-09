!!# PARAMETERS MODULE: <<PAR_Colors_RGBA_sp>>
MODULE PAR_Colors_RGBA_sp


!!## PURPOSE
!! Define colors with ``RGBA" values as arrays <RGBA=(/R,G,B,A/)>,
!! in single precision (sp), where <R> is the red-value, <G> is
!! the green-value, and <B> is the blue-value, and <A> is the
!! intensity, all from 0 to 1.



!!## MODULE DEPENDENCIES
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))



!!## OWNER(S)
!! [waw] William A. Wieselquist, william.wieselquist AT gmail.com



!!## HISTORY
!!
!! + (01/01/2007) created [waw]



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PUBLIC



!!## EXTERNAL PARAMETERS
INCLUDE "02-A-PAR_Colors_RGBA.f90.hdr"



!!## PRIVATE ACCESS LIST
PRIVATE :: KIND_R



END MODULE
