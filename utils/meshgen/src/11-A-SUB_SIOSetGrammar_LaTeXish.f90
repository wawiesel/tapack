!!# SUBROUTINE MODULE <<SUB_SIOSetGrammar_LaTeXish>>
MODULE SUB_SIOSetGrammar_LaTeXish


!!## PURPOSE
!! Set <SIO> streaming input/output data structure grammar to
!! a <LaTeX>-like grammar.


!!## AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.com | June 2009


!!## EXTERNAL
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
USE USR_SIO            !!((09-A-USR_SIO.f90))
USE TBX_SIO            !!((10-A-TBX_SIO.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## IDENTIFICATION
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: file_ = "11-A-SUB_SIOSetGrammar_LaTeXish.f90"
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: mod_  = "SUB_SIOSetGrammar_LaTeXish"

!!## DEFAULT ACCESS
PRIVATE


!!## PUBLIC ROUTINES
PUBLIC :: SIOSetGrammar_LaTeXish


!!## LOCAL
CONTAINS

!!### SUBROUTINE >>SIOSetGrammar_LaTeXish<<
SUBROUTINE SIOSetGrammar_LaTeXish(SIO)

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_SIO),INTENT(INOUT) :: SIO

!!--begin--

SIO%Comment      = "!"
SIO%Continue     = "&"
SIO%Stop         = "#"
SIO%Ignore       = " "
SIO%CmdBeg       = "\"
SIO%ArgBeg       = "{"
SIO%ArgEnd       = "}"
SIO%ArgDelim     = "}{"
SIO%SubArgDelim  = ","
SIO%ArgDefine    = "="
SIO%Argdefault   = "*"
SIO%StrTag       = '"'
SIO%LEN_ArgDelim = 2

!!--end--
END SUBROUTINE SIOSetGrammar_LaTeXish

END MODULE
