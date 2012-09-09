!!# KINDS MODULE: >>KND_IntrinsicTypes<<
MODULE KND_IntrinsicTypes


!!## PURPOSE
!! Provides the definition of the kinds for all intrinsic
!! types: <REAL>, <INTEGER>, <COMPLEX>, <LOGICAL>, and <CHARACTER>.



!!## DETAILS
!! In order to ease the porting of code from 32-bit to 64-bit systems
!! and vice-versa, these kind declarations are provided.  If possible,
!! modules should try and use these kinds instead of local kinds,
!! that way changes made here can propagate through the entire hierarchy.



!!## OWNER(S)
!! [waw] William A. Wieselquist, william.wieselquist AT gmail.com



!!## HISTORY
!!
!! + (12/16/2007) updated to include links and anchors [waw]
!! + (07/01/2006) updated with character kinds [waw]
!! + (01/01/2005) created [waw]



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE



!!## KIND DECLARATIONS 1: Basic Numeric Types
!!
!! * >>LOGICAL<< kinds
INTEGER,PARAMETER :: KIND_L1 = 1
INTEGER,PARAMETER :: KIND_L2 = 2
INTEGER,PARAMETER :: KIND_L4 = 4
!!
!! * >>INTEGER<< kinds
INTEGER,PARAMETER :: KIND_I1 = SELECTED_INT_KIND(2)
INTEGER,PARAMETER :: KIND_I2 = SELECTED_INT_KIND(4)
INTEGER,PARAMETER :: KIND_I4 = SELECTED_INT_KIND(9)
INTEGER,PARAMETER :: KIND_I8 = SELECTED_INT_KIND(18)
!!
!! * >>REAL<< kinds
INTEGER,PARAMETER :: KIND_Rsp = KIND(1.0)
INTEGER,PARAMETER :: KIND_Rdp = KIND(1.0D0)
!!
!! * >>COMPLEX<< kinds (same as real kinds)
INTEGER,PARAMETER :: KIND_Csp = KIND_Rsp
INTEGER,PARAMETER :: KIND_Cdp = KIND_Rdp



!!## KIND DECLARATIONS 2: Character String Kinds
!!
!! Below all the >>CHARACTER<< kinds are defined, just in case
!! they are different.
!!
!! * character string kind for english alphabet <KIND_Sen>
!! * character string kind for integers <KIND_Sint>
!! * character string kind for reals <KIND_Sreal>
!! * character string kind for cmplx <KIND_Scmplx>
!! * character string kind for files <KIND_Sfile>
!! * character string kind for formatting statements <KIND_Sfmt>
INTEGER,PARAMETER :: KIND_Sen    = KIND("abcdefghijklmnop&
                                        &qrstuvwxyzABCDEF&
                                        &GHIJKLMNOPQRSTUV&
                                        &WXYZ()~!@#$%^&*(&
                                        &)-_=+[]{}\|;:',<&
                                        &>./?`")
INTEGER,PARAMETER :: KIND_Sint   = KIND("0123456789+-")
INTEGER,PARAMETER :: KIND_Sreal  = KIND("0123456789.&
                                        &eEdD+-")
INTEGER,PARAMETER :: KIND_Scmplx = KIND("0123456789.&
                                        &eEdD+-(),")
INTEGER,PARAMETER :: KIND_Sfile  = KIND("abcdefghijklmnop&
                                        &qrstuvwxyzABCDEF&
                                        &GHIJKLMNOPQRSTUV&
                                        &WXYZ_0123456789.")
INTEGER,PARAMETER :: KIND_Sfmt   = KIND_Sen



!!## KIND DECLARATIONS 3: Default Kinds
!!
!! Below the default kinds are set for various intrinsic types.
!!
!! * default <<LOGICAL>> kind
!! * default <<INTEGER>> kind
!! * default <<REAL>> kind
!! * default <<COMPLEX>> kind
!! * default <<CHARACTER>> or string kind
INTEGER,PARAMETER :: KIND_L  = KIND_L4
INTEGER,PARAMETER :: KIND_I  = KIND_I4
INTEGER,PARAMETER :: KIND_R  = KIND_Rsp
INTEGER,PARAMETER :: KIND_C  = KIND_Csp
INTEGER,PARAMETER :: KIND_S  = KIND_Sen


!!## PUBLIC ACCESS LIST
!! * numeric kinds
PUBLIC :: KIND_L1
PUBLIC :: KIND_L2
PUBLIC :: KIND_L4
PUBLIC :: KIND_I1
PUBLIC :: KIND_I2
PUBLIC :: KIND_I4
PUBLIC :: KIND_I8
PUBLIC :: KIND_Rsp
PUBLIC :: KIND_Rdp
PUBLIC :: KIND_Csp
PUBLIC :: KIND_Cdp
!! * string kinds
PUBLIC :: KIND_Sen
PUBLIC :: KIND_Sint
PUBLIC :: KIND_Sreal
PUBLIC :: KIND_Scmplx
PUBLIC :: KIND_Sfile
PUBLIC :: KIND_Sfmt
!! * default kinds
PUBLIC :: KIND_L
PUBLIC :: KIND_I
PUBLIC :: KIND_R
PUBLIC :: KIND_C
PUBLIC :: KIND_S


END MODULE
