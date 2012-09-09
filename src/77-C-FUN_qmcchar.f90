!!# FUNCTION MODULE: <qmcchar>
MODULE FUN_qmcchar

!!## PURPOSE
!! Return the total source as a function of $s$ along a
!! characteristic.

!!## MODULES
USE KND_Characteristics                       !!((03-C-KND_Characteristics.f90))
USE VAR_Characteristics,ONLY: Nd,r0,Omega,s01 !!((75-C-VAR_Characteristics.f90))
USE FUN_qmcfull                               !!((76-C-FUN_qmcfull.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE

PRIVATE

!!## PUBLIC ACCESS LIST
PUBLIC :: Qchar,sigmachar
PUBLIC :: Qchar1
PUBLIC :: Cached_Qchar,Cached_sigmachar
PUBLIC :: Cached_Qchar1

CONTAINS


!!### FUNCTION <Qchar>
FUNCTION Qchar( s )

!!#### PURPOSE
!! Return source along a characteristic $s$, the slow
!! way.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: s

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: Qchar

!!--begin--

Qchar   = Qfull( r0(1:Nd) + Omega(1:Nd)*s )

!!--end--
END FUNCTION



!!### FUNCTION <Qchar1>
FUNCTION Qchar1( s )

!!#### PURPOSE
!! Return source along a characteristic $s$, the slow
!! way.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: s

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: Qchar1

!!--begin--

Qchar1   = Qfull1( r0(1:Nd) + Omega(1:Nd)*s )

!!--end--
END FUNCTION



!!### FUNCTION <Cached_Qchar>
FUNCTION Cached_Qchar(  sQ , iQ  ) RESULT(Qchar)

!!#### PURPOSE
!! Return source along a characteristic $s$ with knowledge
!! of what cells the characteristic traverses.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: sQ(:)
INTEGER       ,INTENT(IN) :: iQ(SIZE(sQ))

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: Qchar(SIZE(sQ))

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: s,sl,sn,s1,s2,ds
INTEGER        :: n

!!--begin--

sl = 0.0_KIND_qmc
s1 = 0.0_KIND_qmc
DO n=1,SIZE(sQ)

 !get new length
 sn = sQ(n)

 !get length
 ds = sn - sl

 !get ending s for interval
 s2 = s1 + ds

 !get midpoint s inside interval
 s = 0.5_KIND_qmc*( s1 + s2 )

 !get the source
 Qchar(n) = Qfull( r0(:) + Omega(:)*s , i=iQ(n) )

 !update s values
 sl = sn
 s1 = s2
END DO

!!--end--
END FUNCTION




!!### FUNCTION <Cached_Qchar1>
FUNCTION Cached_Qchar1(  sQ , iQ  , Q0 ) RESULT(Qchar1)

!!#### PURPOSE
!! Return <a,b> from <a+b*s> representation of the
!! the source along a characteristic $s$ with knowledge
!! of what cells the characteristic traverses.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: sQ(:)
INTEGER       ,INTENT(IN) :: iQ(SIZE(sQ))

!!#### OPTIONAL OUTPUT
REAL(KIND_qmc),INTENT(OUT),OPTIONAL :: Q0(SIZE(sQ))

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: Qchar1(SIZE(sQ))

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: s,sl,sn,s1,s2,ds,sa,sb
INTEGER        :: n

!!--begin--

sl = 0.0_KIND_qmc
s1 = 0.0_KIND_qmc
DO n=1,SIZE(sQ)

 !get new length
 sn = sQ(n)

 !get length
 ds = sn - sl

 !get ending s for interval
 s2 = s1 + ds

 !get midpoint s inside interval
 s = 0.5_KIND_qmc*( s1 + s2 )

 !get the flat component
 IF( PRESENT(Q0) )THEN
  Q0(n) = Qfull( r0(:) + Omega(:)*s  , i=iQ(n) )
 END IF

 !get the linear source
 Qchar1(n) = Qfull1_mesh( r0(:) + Omega(:)*s , i=iQ(n) )

 !update s values
 sl = sn
 s1 = s2
END DO

!!--end--
END FUNCTION



!!### FUNCTION <sigmachar>
FUNCTION sigmachar( s )

!!#### PURPOSE
!! Return total cross section along a characteristic $s$,
!! the slow way.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: s

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: sigmachar

!!--begin--

sigmachar = sigmafull( r0(1:Nd) + Omega(1:Nd)*s )

!!--end--
END FUNCTION





!!### FUNCTION <Cached_sigmachar>
FUNCTION Cached_sigmachar(  ssigma , isigma  ) RESULT(sigmachar)

!!#### PURPOSE
!! Return total cross section along a characteristic $s$
!! with knowledge of what cells the characteristic traverses.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: ssigma(:)
INTEGER       ,INTENT(IN) :: isigma(SIZE(ssigma))

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: sigmachar(SIZE(ssigma))

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: s,sl,sn,ds,s1,s2
INTEGER        :: n

!!--begin--

sl = 0.0_KIND_qmc
s1 = 0.0_KIND_qmc
DO n=1,SIZE(ssigma)
 sn = ssigma(n)

 !get length
 ds = sn - sl

 !get ending s for interval
 s2 = s1 + ds

 !get midpoint s inside interval
 s = 0.5_KIND_qmc*( s1 + s2 )

 !get the source
 sigmachar(n) = sigmafull( r0(:) + Omega(:)*s , i=isigma(n) )

 !update s values
 sl = sn
 s1 = s2
END DO

!!--end--
END FUNCTION



END MODULE
