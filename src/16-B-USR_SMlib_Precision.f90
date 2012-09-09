!
!!  Copyright (C) 1996 Ernst A. Meese
!!    Refer to the file copyright.doc for details and important disclaimer.
!
!!  Created: January 1996
!!  Development version
!
MODULE USR_SMlib_Precision
  !
  !!  This module is part of SMLIB v. 1.1.  It defines the real precision USEd
  !!  throughout the library.  Redefine prec in this file to change working
  !!  precision.
  !
  !! ------------------------------------------------------------------------
  !
  !!  Copyright (C) 1996 Ernst A. Meese
  !!    Refer to the file copyright.doc for details and important disclaimer.
  !
  !!  Created: January 1996
  !!  Development version
  !
  !! ------------------------------------------------------------------------
  !!  Module contents
  !
  !!   Parameters:
  !!      single_prec - Kind value for single precision.
  !!      double_prec - Kind value for double precision.
  !!      prec        - Working precision of SMLIB.
  !
  !! -----------------------------------------------------------------------
  !
  IMPLICIT NONE

  INTEGER, PARAMETER :: single_prec = KIND(1.0E0)
  INTEGER, PARAMETER :: double_prec = KIND(1.0D0)
  INTEGER, PARAMETER ::        prec = double_prec

END MODULE USR_SMlib_Precision

