!!# USER MODULE: <<USR_Characteristics>>
MODULE USR_Characteristics

!!## PURPOSE
!! Provides user routines for the method of characteristics
!! solution of the linearized Boltzmann transport equation.

!!## NOTES
!!


!!## USAGE



!!### EXTERNAL KINDS
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))


!!### EXTERNAL PROCEDURES
USE FUN_IsError                                                   !!((05-A-FUN_IsError.f90))
USE FUN_Error                                                     !!((04-A-FUN_Error.f90))
USE SUB_Stop                                                      !!((04-B-SUB_Stop.f90))
USE FUN_Integrate1_aq                                             !!((06-B-FUN_Integrate1_aq.f90))
USE USR_NEGEXP                                                    !!((10-C-USR_NEGEXP.f90))
USE VAR_NEGEXP                                                    !!((09-C-VAR_NEGEXP.f90))
USE FUN_STR                                                       !!((05-B-FUN_STR.f90))
USE FUN_Error                                                     !!((04-A-FUN_Error.f90))
USE FUN_Interp1S_Linear                                           !!((05-B-FUN_Interp1S_Linear.f90))
USE FUN_Default                                                   !!((04-A-FUN_Default.f90))
USE FUN_SIZEa                                                     !!((06-B-FUN_SIZEa.f90))
USE FUN_SIZEn                                                     !!((07-B-FUN_SIZEn.f90))
USE FUN_NewUnit                                                   !!((04-B-FUN_NewUnit.f90))
USE FUN_IsApprox                                                  !!((03-A-FUN_IsApprox.f90))
USE SUB_Reallocate                                                !!((04-B-SUB_Reallocate.f90))
USE FUN_qmcchar                                                   !!((77-C-FUN_qmcchar.f90))
USE FUN_qmcfull                                                   !!((76-C-FUN_qmcfull.f90))
USE FUN_Random                                                    !!((03-A-FUN_Random.f90))
USE SUB_Sort_quick                                                !!((03-A-SUB_Sort_quick.f90))
USE SUB_CLEARn                                                    !!((04-A-SUB_CLEARn.f90))
USE FUN_NewFile                                                   !!((05-B-FUN_NewFile.f90))
USE VAR_Characteristics,ONLY: c,Nd,r0,Omega,s01,psi0,NaiveAlloc,& !!((75-C-VAR_Characteristics.f90))
  Cached_Omega,Cached_r0,Cached_s01,Cached_psi0,Cached_sQ,&
  Cached_iQ,Cached_ssigma,Cached_isigma,NQ,Ns,sQ,Q,ssigma,sigma,&
  DEFAULT_tol,DEFAULT_NSamples,DEFAULT_NCoarse,DEFAULT_NCollapse,&
  using_refinement,g,h,Nc,Using_1DCachedArrays,CharacteristicSourceOrder,Q1,&
  Cached_1sQ,Cached_1Q,Cached_1iQ,&
  Cached_1ssigma,Cached_1sigma,Cached_1isigma,&
  iQ,isigma

!!### DEFAULT IMPLICIT
IMPLICIT NONE


!!### DEFAULT ACCESS
PRIVATE

INTERFACE qmc_Relocate
 MODULE PROCEDURE qmc_Relocate0
 MODULE PROCEDURE qmc_Relocate1
END INTERFACE

PUBLIC :: KIND_qmc

PUBLIC :: qmc_Init

PUBLIC :: qmc_SetOptions

PUBLIC :: qmc_Relocate

PUBLIC :: qmc_cache
PUBLIC :: qmc_Discover_Q
PUBLIC :: qmc_Discover_sigma

PUBLIC :: qmc_Q
PUBLIC :: qmc_sigma
PUBLIC :: qmc_tau

!PUBLIC :: qmc_BwdPsi
PUBLIC :: qmc_FwdPsi
PUBLIC :: qmc_FwdPsi_linsrc

PUBLIC :: qmc_Kill
PUBLIC :: qmc_Print_xySlices
PUBLIC :: qmc_Nc

PUBLIC :: GNUPLOT_f1,GNUPLOT_f2
PUBLIC :: NQ,NS


!!## PROCEDURES
CONTAINS

!!### FUNCITON: qmc_Nc()
FUNCTION qmc_Nc()
INTEGER :: qmc_Nc
!!--begin--
qmc_Nc = Nc !from VAR_Characteristics
!!--end--
END FUNCTION

!!### FUNCTION: FwdPsi
FUNCTION qmc_FwdPsi() RESULT(psi)

!!#### PURPOSE
!! Evaluate a forward characteristic:
!! << psi(s) = psi(0)*exp(-tau(s)) + int_0^s Q(s') exp(-tau(s)+tau(s')) ds' >>
REAL(KIND_qmc) :: psi

!!--begin--

!WRITE(34,"(i,3e12.4)")c,psi0,qmc_atten(),qmc_srcint()

psi = psi0 * qmc_atten() + qmc_srcint()

!!--end--
END FUNCTION



!!### FUNCTION: FwdPsi_linsrc
FUNCTION qmc_FwdPsi_linsrc() RESULT(psi)

!!#### PURPOSE
!! Evaluate a forward characteristic:
!! << psi(s) = psi(0)*exp(-tau(s)) + int_0^s Q(s') exp(-tau(s)+tau(s')) ds' >>
REAL(KIND_qmc) :: psi

!!--begin--

!WRITE(34,"(i,3e12.4)")c,psi0,qmc_atten(),qmc_srcint_linsrc()
psi = psi0 * qmc_atten() + qmc_srcint_linsrc()

!!--end--
END FUNCTION



FUNCTION qmc_atten( ) RESULT(val)
REAL(KIND_qmc) :: val
REAL(KIND_qmc) :: opacity

!!--begin

!calculate opacity
opacity = qmc_tau( s01 )

!evaluate exponential
val = exp(-opacity)

!!--end--
END FUNCTION



FUNCTION qmc_srcint( ) RESULT(val)
REAL(KIND_qmc) :: val
INTEGER        :: n
REAL(KIND_qmc) :: ds,a
!!--begin--

val = 0._KIND_qmc

IF( s01==0._KIND_qmc )RETURN

!get first ds
ds = sQ(1)
a  = 0.d0

!loop through values
DO n=1,NQ-1

 !new calculation procedure
 val = val + NewSourceIntegral0( Q(n) , ds , a , s01 , sigma(n) )

 !get new ds
 a  = sQ(n)
 ds = sQ(n+1) - a

END DO
!new calculation procedure
val = val + NewSourceIntegral0( Q(n) , ds , a , s01 , sigma(n) )

!!--end--
END FUNCTION



FUNCTION qmc_srcint_linsrc( ) RESULT(val)
REAL(KIND_qmc) :: val
INTEGER        :: n
REAL(KIND_qmc) :: ds,a
!!--begin--

val = 0._KIND_qmc

IF( s01==0._KIND_qmc )RETURN

!get first ds
ds = sQ(1)
a  = 0.d0

!loop through values
DO n=1,NQ-1

 !new calculation procedure
 val = val + NewSourceIntegral1( Q(n) , Q1(n) , ds , a , s01 , sigma(n) )

 !get new ds
 a  = sQ(n)
 ds = sQ(n+1) - a

END DO
!new calculation procedure
val = val + NewSourceIntegral1( Q(n) , Q1(n) , ds , a , s01 , sigma(n) )

!!--end--
END FUNCTION



PURE ELEMENTAL FUNCTION qmc_srcfun( s_ ) RESULT(f)
REAL(KIND_qmc),INTENT(IN) :: s_
REAL(KIND_qmc) :: f
REAL(KIND_qmc) :: opacity
!!--begin--

!get opacity travelled through
opacity = qmc_tau(s01) - qmc_tau(s_)

!get exponentially attenuated source
f = exp(-opacity) * qmc_Q(s_)

!!--end--
END FUNCTION



SUBROUTINE ALLOCATER( g , N )
REAL(KIND_qmc),POINTER :: g(:)
INTEGER :: N

IF( NaiveAlloc )THEN
 ALLOCATE( g(N) )
ELSE
 IF( SIZEn(g)<N )THEN
  CALL REALLOCATE( g , N+SIZEn(g) )
 END IF
END IF

END SUBROUTINE

SUBROUTINE ALLOCATER_I( g , N )
INTEGER,POINTER :: g(:)
INTEGER :: N

IF( NaiveAlloc )THEN
 ALLOCATE( g(N) )
ELSE
 IF( SIZEn(g)<N )THEN
  CALL REALLOCATE( g , N+SIZEn(g) )
 END IF
END IF

END SUBROUTINE


SUBROUTINE DEALLOCATER( g )
REAL(KIND_qmc),POINTER :: g(:)

IF( NaiveAlloc )THEN
 CALL CLEARn( g )
END IF

END SUBROUTINE



SUBROUTINE ALLOCATER_L( g , N )
LOGICAL,POINTER :: g(:)
INTEGER :: N

IF( NaiveAlloc )THEN
 ALLOCATE( g(N) )
ELSE
 IF( SIZE(g)<N )THEN
  CALL REALLOCATE( g , N+SIZE(g) )
 END IF
END IF

END SUBROUTINE

SUBROUTINE DEALLOCATER_L( g )
LOGICAL,POINTER :: g(:)

IF( NaiveAlloc )THEN
 CALL CLEARn( g )
END IF

END SUBROUTINE


SUBROUTINE qmc_Cache( c_ , r0_ , Omega_ , s01_ , psi0_ , &
  sQ , iQ , ssigma , isigma )

USE USR_SparseArray                                               !!((45-C-USR_SparseArray.f90))
REAL(KIND_qmc),INTENT(IN) :: r0_(Nd),Omega_(Nd),s01_,psi0_

REAL(KIND_qmc),INTENT(IN) :: sQ(:),ssigma(:)
INTEGER       ,INTENT(IN) :: iQ(SIZE(sQ)),isigma(SIZE(ssigma))
INTEGER       ,INTENT(IN) :: c_
!!--begin--

!characteristic index
c = c_

!phase space and bc
r0    = r0_
Omega = Omega_
s01   = s01_
psi0  = psi0_

!set cache values
Cached_Omega(:,c) = Omega_
Cached_r0(:,c)    = r0_
Cached_s01(c)     = s01_
Cached_psi0(c)    = psi0_

!cache source segmentation
!NQ = SIZE(sQ)
![waw] test new sparse array procedures
!IF( NQ>SIZE(Cached_sQ,1) )THEN
! WRITE(*,*)"reallocating cached sources by ",NQ-SIZE(Cached_sQ,1)
! CALL REALLOCATE( Cached_sQ , (/NQ-SIZE(Cached_sQ,1),0/) , fill=Error(1._KIND_qmc) )
! CALL REALLOCATE( Cached_iQ , (/NQ-SIZE(Cached_iQ,1),0/) , fill=Error(1) )
!END IF
!Cached_sQ(1:NQ,c) = sQ
!Cached_iQ(1:NQ,c) = iQ
IF( Using_1DCachedArrays )THEN
 CALL ASSIGN_SPARSEARRAY(Cached_1sQ,Cached_1Q,c,sQ)
 CALL ASSIGN_SPARSEARRAY(Cached_1iQ,Cached_1Q,c,iQ)
ELSE
 CALL ASSIGN_DENSEARRAY(Cached_sQ,c,sQ)
 CALL ASSIGN_DENSEARRAY(Cached_iQ,c,iQ)
END IF
!WRITE(45,*)"c=",c
!WRITE(45,*)"sQ=",sQ
!WRITE(45,*)"iQ=",iQ

!cache sigma segmentation
!Ns = SIZE(ssigma)
![waw] test new sparse arrays
!IF( Ns>SIZE(Cached_ssigma,1) )THEN
! WRITE(*,*)"reallocating cached total xs by ",Ns-SIZE(Cached_ssigma,1)
! CALL REALLOCATE( Cached_ssigma , (/Ns-SIZE(Cached_ssigma,1),0/) , fill=Error(1._KIND_qmc) )
! CALL REALLOCATE( Cached_isigma , (/Ns-SIZE(Cached_isigma,1),0/) , fill=Error(1) )
!END IF
!Cached_ssigma(1:Ns,c) = ssigma
!Cached_isigma(1:Ns,c) = isigma
IF( Using_1DCachedArrays )THEN
 CALL ASSIGN_SPARSEARRAY(Cached_1ssigma,Cached_1sigma,c,ssigma)
 CALL ASSIGN_SPARSEARRAY(Cached_1isigma,Cached_1sigma,c,isigma)
ELSE
 CALL ASSIGN_DENSEARRAY(Cached_ssigma,c,ssigma)
 CALL ASSIGN_DENSEARRAY(Cached_isigma,c,isigma)
END IF
!WRITE(45,*)"ssigma=",ssigma
!WRITE(45,*)"isigma=",isigma


!!--end--
END SUBROUTINE



SUBROUTINE qmc_Discover_Q( tol , Nsamples , Ncollapse , Ncoarse , sA , UseCached )
USE USR_SparseArray                                               !!((45-C-USR_SparseArray.f90))

!!#### OPTIONAL INPUT
REAL(KIND_qmc),INTENT(IN),OPTIONAL :: tol
INTEGER       ,INTENT(IN),OPTIONAL :: Nsamples
INTEGER       ,INTENT(IN),OPTIONAL :: Ncollapse
INTEGER       ,INTENT(IN),OPTIONAL :: Ncoarse
REAL(KIND_qmc),INTENT(IN),OPTIONAL :: sA(:)
LOGICAL       ,INTENT(IN),OPTIONAL :: UseCached

!!#### LOCAL VARIABLES
INTEGER                :: n,ell
REAL(KIND_qmc)         :: tol_,s,sl,sn
INTEGER                :: NCoarse_,NCollapse_,NSamples_
LOGICAL                :: UseCached_

!!--begin--

!get local tolerance
tol_       = DEFAULT(DEFAULT_tol,tol)
NCoarse_   = DEFAULT(DEFAULT_NCoarse,NCoarse)
NCollapse_ = DEFAULT(DEFAULT_NCollapse,NCollapse)
NSamples_  = DEFAULT(DEFAULT_NSamples,NSamples)
UseCached_ = DEFAULT(.FALSE.,UseCached)

!initialize
CALL Kill_Q()

!through cached characteristic
IF( UseCached_ )THEN

 IF( Using_1DCachedArrays )THEN
  NQ = SIZE_Entry_SPARSEARRAY(Cached_1sQ,Cached_1Q,c)
  CALL ALLOCATER_I( iQ , NQ )
  CALL ALLOCATER( sQ , NQ )
  CALL ALLOCATER(  Q , NQ )
  CALL ALLOCATER( Q1 , NQ )

  CALL ACCESS_SPARSEARRAY( Cached_1sQ , Cached_1Q , c , sQ(1:NQ) )
  CALL ACCESS_SPARSEARRAY( Cached_1iQ , Cached_1Q , c , iQ(1:NQ) )

 ELSE
  NQ = SIZE_Entry_DENSEARRAY(Cached_sQ,c)
  CALL ALLOCATER_I( iQ , NQ )
  CALL ALLOCATER( sQ , NQ )
  CALL ALLOCATER(  Q , NQ )
  CALL ALLOCATER( Q1 , NQ )

  CALL ACCESS_DENSEARRAY( Cached_sQ , c , sQ(1:NQ) )
  CALL ACCESS_DENSEARRAY( Cached_iQ , c , iQ(1:NQ) )

 END IF

 SELECT CASE( CharacteristicSourceOrder )
  CASE(2)      ; Q1(1:NQ) = Cached_Qchar1( sQ(1:NQ) , iQ(1:NQ) , Q0=Q(1:NQ) )
  CASE DEFAULT ; Q (1:NQ) = Cached_Qchar ( sQ(1:NQ) , iQ(1:NQ) )
 END SELECT

ELSE

 IF( .NOT.PRESENT(sA) )THEN

  !get number of samples
  NQ = MAX(2,NSamples_)
  CALL ALLOCATER( sQ , NQ )
  CALL ALLOCATER(  Q , NQ )
  CALL ALLOCATER( Q1 , NQ )

  CALL uniformsequence( NQ , sQ , s01 )

 !provided sequence
 ELSE

  NQ = SIZE(SA)
  CALL ALLOCATER( sQ , NQ )
  CALL ALLOCATER(  Q , NQ )
  CALL ALLOCATER( Q1 , NQ )
  sQ(1:NQ) = SA

 END IF

 !get function values
 sl = 0.0_KIND_qmc
 DO n=1,NQ
  sn   = sQ(n)
  s    = 0.5_KIND_qmc*( sl + sn )
  Q(n) = Qchar ( s )
  Q1(n)= Qchar1( s )
  sl   = sn
 END DO

 !refine sequence
 IF( using_refinement )THEN
  CALL refine_sequence()
  CALL collapse_sequence()
 END IF

END IF

CONTAINS


SUBROUTINE refine_sequence()
LOGICAL,POINTER,SAVE :: mask(:)=>NULL()
INTEGER :: COUNT_mask,l
REAL(KIND_qmc) :: resid

!!--begin--
DO
 CALL ALLOCATER( g , NQ-1 )
 CALL ALLOCATER( h , NQ-1 )

 !get partial integrals of discrete values
 CALL partial_integrals( NQ , Q , sQ , g )

 !send real function in for better integrals
 CALL better_integrals( Qchar , NQ , Q , sQ , h )


 !**multisplit**
 !get mask and cleanup
 !CALL ALLOCATER_L( mask , NQ-1 )
 !mask(1:NQ-1) = ABS(g(1:NQ-1)-h(1:NQ-1))<tol_
 !absolute tolerance check
 !CALL split_intervals( mask , Qchar , Q , sQ , NQ , COUNT_mask )
 !CALL DEALLOCATER( g )
 !CALL DEALLOCATER( h )
 !IF( COUNT_mask==0 )EXIT


 !**single split**
 l = MAXLOC( ABS(g(1:NQ-1)-h(1:NQ-1)) , 1 )
 resid = ABS(g(l)-h(l))
 CALL DEALLOCATER( g )
 CALL DEALLOCATER( h )
 IF( resid<tol_ )EXIT
 CALL split_interval( l , Qchar , Q , sQ , NQ )

END DO

!!--end--
END SUBROUTINE


SUBROUTINE collapse_sequence()
!***print stuff out to check it
!CALL GNUPLOT_f1( "before" , Q , sQ )

!collapse intervals that are too fine
!CALL CPU_TIME(t1)
DO n=1,NCollapse_
 CALL collapse_interval( Q , sQ , NQ , tol_ , COUNT_mask=ell )
 !CALL GNUPLOT_f1( "after"//TRIM(STR(ell)) , Q(1:NQ) , sQ(1:NQ) )
 IF( ell<=NCoarse_ )EXIT
END DO
!CALL CPU_TIME(t2)
!dt4 = dt4 + MAX(t2-t1,0.)
!WRITE(*,*)"dt_collapse_Q=",dt4
!WRITE(*,*)"SIZE(Q)=",SIZE(Q)

END SUBROUTINE


!!--end--
END SUBROUTINE




SUBROUTINE uniformsequence(N,s,s01)
INTEGER       ,INTENT(IN)    :: N
REAL(KIND_qmc),INTENT(INOUT) :: s(N)
REAL(KIND_qmc),INTENT(IN)    :: s01
INTEGER :: i

REAL(KIND_qmc) :: ds

!!--begin--

!get delta
ds = s01/REAL(N,KIND_qmc)

!get way points for uniform distribution
s(1) = ds
DO i=2,N
 s(i) = s(i-1) + ds
END DO

!!--end--
END SUBROUTINE



SUBROUTINE Kill_Q()
!!--begin--
IF( NaiveAlloc )THEN
 CALL CLEARn(sQ)
 CALL CLEARn(Q)
ELSE
 IF( ASSOCIATED(sQ) )THEN
  sQ = 0._KIND_qmc
 END IF
 IF( ASSOCIATED(Q) )THEN
  Q = 0._KIND_qmc
 END IF
 NQ = 0
END IF
!!--end--
END SUBROUTINE


PURE FUNCTION qmc_Q( s ) RESULT(val)
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: s

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: val

!!#### LOCAL VARIABLES
INTEGER :: i,LB,UB
REAL(KIND_qmc) :: f1,f2,d

!!--begin--

!LB is lower bound
LB = 1

!UB is upper bound
UB = NQ

!i is current division (starts in center)
!       <-----LB----i-----UB---->
i = (LB+UB)/2

!start search
DO WHILE( i/=LB )
 IF( s<sQ(i) )THEN
  UB = i
 ELSE
  LB = i
 ENDIF
 i = (LB+UB)/2
END DO

val = Q(LB)

!d = (sQ(UB)-sQ(LB))
!linear approx
!IF( ABS(d)>1.d-10 )THEN
! f2  = (s-sQ(LB))/d
! f1  = 1._KIND_R - f2
! val = f1*Q(LB) + f2*Q(UB)

!simple fixed approx
!ELSE
! val = Q(LB)
!END IF

!!--end--
END FUNCTION




SUBROUTINE qmc_Discover_sigma( tol , Nsamples , Ncollapse , Ncoarse , sA , UseCached )

USE USR_SparseArray                                               !!((45-C-USR_SparseArray.f90))

!!#### OPTIONAL INPUT
REAL(KIND_qmc),INTENT(IN),OPTIONAL :: tol
INTEGER       ,INTENT(IN),OPTIONAL :: Nsamples
INTEGER       ,INTENT(IN),OPTIONAL :: Ncollapse
INTEGER       ,INTENT(IN),OPTIONAL :: Ncoarse
REAL(KIND_qmc),INTENT(IN),OPTIONAL :: sA(:)
LOGICAL       ,INTENT(IN),OPTIONAL :: UseCached

!!#### LOCAL VARIABLES
INTEGER                :: n,ell
REAL(KIND_qmc)         :: tol_,s,sl,sn
INTEGER                :: NCoarse_,NCollapse_,NSamples_
LOGICAL                :: UseCached_

!!--begin--

!get local tolerance
tol_       = DEFAULT(DEFAULT_tol,tol)
NCoarse_   = DEFAULT(DEFAULT_NCoarse,NCoarse)
NCollapse_ = DEFAULT(DEFAULT_NCollapse,NCollapse)
NSamples_  = DEFAULT(DEFAULT_NSamples,NSamples)
UseCached_ = DEFAULT(.FALSE.,UseCached)

!initialize
CALL Kill_sigma()

!through cached characteristic
IF( UseCached_ )THEN

 IF( Using_1DCachedArrays )THEN
  Ns = SIZE_Entry_SPARSEARRAY(Cached_1ssigma,Cached_1sigma,c)
  CALL ALLOCATER_I( isigma , Ns )
  CALL ALLOCATER( ssigma , Ns )
  CALL ALLOCATER(  sigma , Ns )

  CALL ACCESS_SPARSEARRAY( Cached_1ssigma , Cached_1sigma , c , ssigma(1:Ns) )
  CALL ACCESS_SPARSEARRAY( Cached_1isigma , Cached_1sigma , c , isigma(1:Ns) )

 ELSE
  Ns = SIZE_Entry_DENSEARRAY(Cached_ssigma,c)
  CALL ALLOCATER_I( isigma , Ns )
  CALL ALLOCATER( ssigma , Ns )
  CALL ALLOCATER(  sigma , Ns )

  CALL ACCESS_DENSEARRAY( Cached_ssigma , c , ssigma(1:Ns) )
  CALL ACCESS_DENSEARRAY( Cached_isigma , c , isigma(1:Ns) )

 END IF

 sigma(1:Ns) = Cached_sigmachar ( ssigma(1:Ns) , isigma(1:Ns) )

 !Ns = SIZEa(Cached_ssigma(:,c))
 !CALL ALLOCATER( ssigma , Ns )
 !CALL ALLOCATER(  sigma , Ns )
 !ssigma(1:Ns) = Cached_ssigma(1:Ns,c)
 !sigma (1:Ns) = Cached_sigmachar( ssigma(1:Ns) , Cached_isigma(1:Ns,c) )

ELSE

 IF( .NOT.PRESENT(sA) )THEN

  !get number of samples
  Ns = MAX(2,NSamples_)
  CALL ALLOCATER( ssigma , Ns )
  CALL ALLOCATER(  sigma , Ns )

  CALL uniformsequence( Ns , ssigma , s01 )

 !provided sequence
 ELSE

  Ns = SIZE(SA)
  CALL ALLOCATER( ssigma , Ns )
  CALL ALLOCATER(  sigma , Ns )
  ssigma(1:Ns) = sA

 END IF

 !get function values
 sl = 0.0_KIND_qmc
 DO n=1,Ns
  sn   = ssigma(n)
  s    = 0.5_KIND_qmc*( sl + sn )
  sigma(n) = sigmachar( s )
  sl   = sn
 END DO

 !refine sequence
 IF( using_refinement )THEN
  CALL refine_sequence()
  CALL collapse_sequence()
 END IF

END IF


CONTAINS


SUBROUTINE refine_sequence()
LOGICAL,POINTER,SAVE :: mask(:)=>NULL()
INTEGER :: COUNT_mask,l
REAL(KIND_qmc) :: resid

!!--begin--
DO
 CALL ALLOCATER( g , Ns-1 )
 CALL ALLOCATER( h , Ns-1 )

 !get partial integrals of discrete values
 CALL partial_integrals( Ns , sigma , ssigma , g )

 !send real function in for better integrals
 CALL better_integrals( Qchar , Ns , sigma , ssigma , h )


 !**multisplit**
 !get mask and cleanup
 !CALL ALLOCATER_L( mask , Ns-1 )
 !mask(1:Ns-1) = ABS(g(1:Ns-1)-h(1:Ns-1))<tol_
 !absolute tolerance check
 !CALL split_intervals( mask , Qchar , sigma , ssigma , Ns , COUNT_mask )
 !CALL DEALLOCATER( g )
 !CALL DEALLOCATER( h )
 !IF( COUNT_mask==0 )EXIT


 !**single split**
 l = MAXLOC( ABS(g(1:Ns-1)-h(1:Ns-1)) , 1 )
 resid = ABS(g(l)-h(l))
 CALL DEALLOCATER( g )
 CALL DEALLOCATER( h )
 IF( resid<tol_ )EXIT
 CALL split_interval( l , Qchar , sigma , ssigma , Ns )

END DO

!!--end--
END SUBROUTINE

SUBROUTINE collapse_sequence()
!***print stuff out to check it
!CALL GNUPLOT_f1( "before" , sigma , ssigma )

!collapse intervals that are too fine
!CALL CPU_TIME(t1)
DO n=1,NCollapse_
 CALL collapse_interval( sigma , ssigma , Ns , tol_ , COUNT_mask=ell )
 !CALL GNUPLOT_f1( "after"//TRIM(STR(ell)) , sigma(1:Ns) , ssigma(1:Ns) )
 IF( ell<=NCoarse_ )EXIT
END DO
!CALL CPU_TIME(t2)
!dt4 = dt4 + MAX(t2-t1,0.)
!WRITE(*,*)"dt_collapse_sigma=",dt4
!WRITE(*,*)"SIZE(sigma)=",SIZE(sigma)

END SUBROUTINE


!!--end--
END SUBROUTINE




SUBROUTINE Kill_sigma()
!!--begin--
IF( NaiveAlloc )THEN
 CALL CLEARn(ssigma)
 CALL CLEARn(sigma)
ELSE
 Ns = 0
END IF
!!--end--
END SUBROUTINE


PURE ELEMENTAL FUNCTION qmc_sigma( s ) RESULT(val)
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: s

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: val

!!#### LOCAL VARIABLES
INTEGER :: i,LB,UB
REAL(KIND_qmc) :: f1,f2

!!--begin--

!LB is lower bound
LB = 1

!UB is upper bound
UB = Ns

!i is current division (starts in center)
!       <-----LB----i-----UB---->
i = (LB+UB)/2

!start search
DO WHILE( i/=LB )
 IF( s<ssigma(i) )THEN
  UB = i
 ELSE
  LB = i
 ENDIF
 i = (LB+UB)/2
END DO

val = sigma(LB)

!f2  = (s-ssigma(LB))/(ssigma(UB)-ssigma(LB))
!f1  = 1._KIND_R - f2
!val = f1*sigma(LB) + f2*sigma(UB)

!!--end--
END FUNCTION




SUBROUTINE qmc_Init( Nd_ , Nc_ )
USE USR_SparseArray                                               !!((45-C-USR_SparseArray.f90))
INTEGER,INTENT(IN) :: Nd_
INTEGER,INTENT(IN),OPTIONAL :: Nc_
REAL(KIND_qmc) :: empty_real
INTEGER :: empty_int

!!--begin--

!initialize number of dimensions
Nd = Nd_

!initialize position
CALL CLEARn( r0 )
ALLOCATE( r0(Nd) )
r0    = 0._KIND_qmc

!initialize omega
CALL CLEARn( Omega )
ALLOCATE( Omega(Nd)  )
Omega = 1._KIND_qmc
Omega = Omega/SQRT(DOT_PRODUCT(Omega,Omega))

IF( PRESENT(Nc_) )THEN
 Nc = Nc_
 empty_real = Error(1._KIND_qmc)
 empty_int  = Error(1)
 CALL CLEARn( Cached_r0 )
 CALL CLEARn( Cached_Omega )
 CALL CLEARn( Cached_s01 )
 CALL CLEARn( Cached_psi0 )
 CALL CLEARn( Cached_sQ )
 CALL CLEARn( Cached_iQ )
 CALL CLEARn( Cached_ssigma )
 CALL CLEARn( Cached_isigma )
 ALLOCATE( Cached_r0(Nd,Nc) )
 ALLOCATE( Cached_Omega(Nd,Nc) )
 ALLOCATE( Cached_s01(Nc) )
 ALLOCATE( Cached_psi0(Nc) )
 Cached_r0    = empty_real
 Cached_Omega = empty_real
 Cached_s01   = empty_real
 Cached_psi0  = empty_real
 IF( Using_1DCachedArrays )THEN
  CALL INIT_SPARSEARRAY( Cached_1sQ , Cached_1Q , (/DEFAULT_Nsamples,Nc/) )
  CALL INIT_SPARSEARRAY( Cached_1iQ , Cached_1Q , (/DEFAULT_Nsamples,Nc/) )
  CALL INIT_SPARSEARRAY( Cached_1ssigma , Cached_1sigma , (/DEFAULT_Nsamples,Nc/) )
  CALL INIT_SPARSEARRAY( Cached_1isigma , Cached_1sigma , (/DEFAULT_Nsamples,Nc/) )
 ELSE
  ALLOCATE( Cached_sQ(DEFAULT_Nsamples,Nc) )
  ALLOCATE( Cached_iQ(DEFAULT_Nsamples,Nc) )
  ALLOCATE( Cached_ssigma(DEFAULT_Nsamples,Nc) )
  ALLOCATE( Cached_isigma(DEFAULT_Nsamples,Nc) )
  Cached_sQ    = empty_real
  Cached_iQ    = empty_int
  Cached_ssigma= empty_real
  Cached_isigma= empty_int
 END IF
END IF

!!--end--
END SUBROUTINE



SUBROUTINE qmc_Kill()
!!--begin--
CALL CLEARn( r0 )
CALL CLEARn( Omega )
CALL CLEARn( sQ )
CALL CLEARn( Q )
CALL CLEARn( ssigma )
CALL CLEARn( sigma )
!!--end--
END SUBROUTINE



SUBROUTINE qmc_Relocate0( r0_ , Omega_ , s01_ , psi0_ )
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))
REAL(KIND_qmc),INTENT(IN) :: r0_(Nd)
REAL(KIND_qmc),INTENT(IN) :: Omega_(Nd)
REAL(KIND_qmc),INTENT(IN) :: s01_
REAL(KIND_qmc),INTENT(IN) :: psi0_

!!--begin--

r0     = r0_
Omega  = Omega_
s01    = s01_
psi0   = psi0_

!!--end--
END SUBROUTINE


SUBROUTINE qmc_Relocate1( c_ )
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))
INTEGER,INTENT(IN) :: c_

!!--begin--

c     = c_
Omega = Cached_Omega(:,c)
r0    = Cached_r0(:,c)
s01   = Cached_s01(c)
psi0  = Cached_psi0(c)

!!--end--
END SUBROUTINE


PURE ELEMENTAL FUNCTION qmc_tau( s ) RESULT(tau)
REAL(KIND_qmc),INTENT(IN) :: s
REAL(KIND_qmc) :: tau
INTEGER :: i,LB,UB
!!--begin--

!interval bisection

!LB is lower bound
LB = 1

!UB is upper bound
UB = NS

!i is current division (starts in center)
!       <-----LB----i-----UB---->
i = (LB+UB)/2

!start search
DO WHILE( i/=LB )
 IF( s<ssigma(i) )THEN
  UB = i
 ELSE
  LB = i
 ENDIF
 i = (LB+UB)/2
END DO

!up to lower bound
tau = ssigma(1)*sigma(1)
DO i=1,LB-1
 tau = tau + (ssigma(i+1)-ssigma(i))*sigma(i+1)
END DO
IF( LB+1<=Ns )THEN
 tau = tau + sigma(LB+1)*(s-ssigma(LB))
END IF
!!--end--
END FUNCTION




SUBROUTINE split_interval( j , realf , f , x , N )
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))
INTERFACE
 FUNCTION realf(s)
 USE KND_Characteristics                                          !!((03-C-KND_Characteristics.f90))
 REAL(KIND_qmc),INTENT(IN) :: s
 REAL(KIND_qmc) :: realf
 END FUNCTION
END INTERFACE
REAL(KIND_qmc),POINTER :: f(:),x(:)
INTEGER,INTENT(INOUT) :: N
INTEGER,INTENT(IN) :: j
!!--begin--

CALL ALLOCATER( f , N+1 )
CALL ALLOCATER( x , N+1 )

f(j+2:n+1) = f(j+1:n)
x(j+2:n+1) = x(j+1:n)
x(j+1) = (x(j+2)+x(j))/2._KIND_R
f(j+1) = realf( x(j+1) )

N = N + 1

!!--end--
END SUBROUTINE


SUBROUTINE split_intervals( mask , realf , f , x , N , COUNT )
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))
INTERFACE
 FUNCTION realf(s)
 USE KND_Characteristics                                          !!((03-C-KND_Characteristics.f90))
 REAL(KIND_qmc),INTENT(IN) :: s
 REAL(KIND_qmc) :: realf
 END FUNCTION
END INTERFACE
LOGICAL,POINTER :: mask(:)
REAL(KIND_qmc),POINTER :: f(:),x(:)
INTEGER,INTENT(INOUT) :: N,COUNT
INTEGER :: j,i
!!--begin--

COUNT = 0
j = 1
i = 0
DO
 i = i + 1
 IF( i>=N-1 )EXIT
 IF( .NOT.mask(i) )THEN
  COUNT = COUNT + 1
  CALL split_interval( j , realf , f , x , N )
  j = j + 2
 END IF
END DO

!!--end--
END SUBROUTINE


SUBROUTINE partial_integrals( N , f , x , g )
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))
INTEGER,INTENT(IN) :: N
REAL(KIND_qmc),INTENT(IN)  :: f(N),x(N)
REAL(KIND_qmc),INTENT(OUT) :: g(N-1)
INTEGER :: i
DO i=2,N
 g(i-1) = (f(i-1)+f(i))*(x(i)-x(i-1))/2._KIND_R
END DO
END SUBROUTINE


SUBROUTINE better_integrals( realf , N , f , x , h )
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))
INTERFACE
 FUNCTION realf(s)
 USE KND_Characteristics                                          !!((03-C-KND_Characteristics.f90))
 REAL(KIND_qmc),INTENT(IN) :: s
 REAL(KIND_qmc) :: realf
 END FUNCTION
END INTERFACE
INTEGER,INTENT(IN) :: N
REAL(KIND_qmc),INTENT(IN)  :: x(N)
REAL(KIND_qmc),INTENT(IN)  :: f(N)
REAL(KIND_qmc),INTENT(OUT) :: h(N-1)
REAL(KIND_qmc) :: fl,fm,fh,xm
INTEGER :: i1,i2
INTEGER :: i
i1=-1
i2=0
DO i=2,n

 i1=i1+2
 i2=i2+2

 xm = (x(i)+x(i-1))/2._KIND_R

 fl = f(i-1)
 fm = realf(xm)
 fh = f(i  )

 h(i-1) = (fl+fm)*(xm-x(i-1))/2._KIND_R + &
          (fm+fh)*(x(i  )-xm)/2._KIND_R

END DO

END SUBROUTINE



SUBROUTINE collapse_interval( f , x , N , tol_ , COUNT_mask )
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))
REAL(KIND_qmc),POINTER :: f(:),x(:)
INTEGER :: N
INTEGER :: j,i,nn,k
INTEGER,OPTIONAL :: COUNT_mask
LOGICAL       ,POINTER,SAVE :: mask(:) =>NULL()
REAL(KIND_qmc),POINTER,SAVE :: fn(:)   =>NULL(),xn(:)=>NULL()
REAL(KIND_qmc),POINTER,SAVE :: g(:)    =>NULL()
REAL(KIND_qmc),POINTER,SAVE :: swap1(:)=>NULL(),swap2(:)=>NULL()
REAL(KIND_qmc) :: h0,h1,tol_
REAL(KIND_qmc),SAVE :: dt=0.d0
!!--begin--

!NULLIFY( fn , xn , g , sQ , fs , mask )
!get old n
nn = n

!ALLOCATE( xn(N) , fn(N) )
CALL ALLOCATER( xn , N )
CALL ALLOCATER( fn , N )

!for each pass
!ALLOCATE( g(N-1) , mask(N-2))
CALL ALLOCATER( g , N-1 )
CALL ALLOCATER_L( mask , N-2 )
CALL partial_integrals( N , f , x , g )

!evaluate mask
DO i=1,N-2
 h0 = g(i)+g(i+1)
 h1 = (f(i)+f(i+2))*(x(i+2)-x(i))/2._KIND_Rdp
 mask(i) = (ABS(h0-h1))<tol_
END DO
IF( PRESENT(COUNT_mask) )THEN
 COUNT_mask = COUNT(mask(1:N-2))
END IF
IF( .NOT.ANY(mask(1:N-2)) )THEN
 CALL DEALLOCATER( g )
 CALL DEALLOCATER_L( mask )
 RETURN
END IF

i = 1
k = 1
xn(1) = x(1)
fn(1) = f(1)
DO

 IF( i==N-1 )THEN
  xn(k+1)=x(i+1)
  fn(k+1)=f(i+1)
  N = k+1
  EXIT
 END IF
 IF( i==N )THEN
  N = k
  EXIT
 END IF

 !k is index of new array
 IF( mask(i) )THEN
  k = k + 1
  i = i + 2
  xn(k) = x(i)
  fn(k) = f(i)
 ELSE
  k = k + 1
  i = i + 1
  xn(k) = x(i)
  fn(k) = f(i)
 END IF
END DO
CALL ALLOCATER( xn , n )
CALL ALLOCATER( fn , n )


!for each intervals
!i = 1
!DO k=2,N
!
! IF( i==N-1 )THEN
!  x(k)=x(i+1)
!  f(k)=f(i+1)
!  N = i+1
!  EXIT
! END IF
! IF( i==N )THEN
!  x(k)=x(i)
!  f(k)=f(i)
!  N = i
!  EXIT
! END IF
!
! !collapse or move on
! IF( mask(i) )THEN
!  x(k) = x(i+2)
!  f(k) = f(i+2)
!  i = i + 2
! ELSE
!  x(k) = x(i+1)
!  f(k) = f(i+1)
!  i = i + 1
! END IF
!
!END DO


!CALL CPU_TIME(t1)
swap1 => NULL(); swap2 => NULL()
swap1 => x     ; swap2 => f
x     => xn    ; f => fn
xn    => swap1 ; fn => swap2
swap1 => NULL(); swap2 => NULL()
!
!CALL CPU_TIME(t2)
!dt = dt + MAX(t2-t1,0.)
!WRITE(*,*)"dt_swap=",dt

!DEALLOCATE( sQ , fs , g , mask )
!NULLIFY( g , mask , sQ , fs , xn , fn )

!!--end--
END SUBROUTINE


SUBROUTINE qmc_SetOptions( tol , Nsamples , Ncollapse , Ncoarse , &
  ArrayOpt )

!!#### OPTIONAL INPUT
REAL(KIND_qmc),INTENT(IN),OPTIONAL :: tol
INTEGER       ,INTENT(IN),OPTIONAL :: Nsamples
INTEGER       ,INTENT(IN),OPTIONAL :: Ncollapse
INTEGER       ,INTENT(IN),OPTIONAL :: Ncoarse
CHARACTER(2)  ,INTENT(IN),OPTIONAL :: ArrayOpt

!!--begin--

!get local tolerance
IF( PRESENT(Ncollapse) )THEN
 DEFAULT_Ncollapse = Ncollapse
END IF
IF( PRESENT(Ncoarse) )THEN
 DEFAULT_Ncoarse = Ncoarse
END IF
IF( PRESENT(Nsamples) )THEN
 DEFAULT_Nsamples = Nsamples
END IF
IF( PRESENT(tol) )THEN
 DEFAULT_tol = tol
END IF
IF( PRESENT(ArrayOpt) )THEN
 IF( ArrayOpt=="1D" )THEN
  Using_1DCachedArrays = .TRUE.
 ELSE
  Using_1DCachedArrays = .FALSE.
 END IF
END IF

!!--end--
END SUBROUTINE


SUBROUTINE qmc_Print_xySlices( rmin , rmax , Ni,Nj, qfile , sfile )
USE ISO_varying_string                                            !!((03-A-ISO_varying_string.f90))
REAL(KIND_qmc),INTENT(IN) :: rmin(2),rmax(2)
CHARACTER(*),INTENT(IN) :: qfile,sfile
INTEGER :: sunit,qunit
INTEGER,INTENT(IN) :: Nj,Ni
INTEGER :: i,j
REAL(KIND_qmc) :: x, y,dr(2)
TYPE(varying_string) :: VSq,VSs

!!--begin--
qunit = NewFile(qfile)
sunit = Newfile(sfile)

x = rmin(1)
y = rmin(2)
dr(2) = (rmax(2)-rmin(2))/REAL(Nj,KIND_qmc)
dr(1) = (rmax(1)-rmin(1))/REAL(Ni,KIND_qmc)

DO j=0,Nj

 vSq = ""
 VSs = ""
 x = rmin(1)
 DO i=0,Ni
  VSq = VSq//TRIM(STR(    Qfull2D(x,y),"(e16.5)"))//" "
  VSs = VSs//TRIM(STR(sigmafull2D(x,y),"(e16.5)"))//" "
  x = x + dr(1)
 END DO

 CALL put_line(qunit,VSq)
 CALL put_line(sunit,VSs)

 y = y + dr(2)

END DO

CLOSE( qunit )
CLOSE( sunit )

!!--end--
END SUBROUTINE


SUBROUTINE GNUPLOT_f1( file , f , x )
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))
USE FUN_NewFile                                                   !!((05-B-FUN_NewFile.f90))
CHARACTER(*),INTENT(IN) :: file
REAL(KIND_qmc),INTENT(IN) :: f(:),x(:)
INTEGER :: i,unit
!!--begin--
unit = NewFile(file)
DO i=1,SIZE(f)
 WRITE(unit,"(e21.9,1x,e21.9)")x(i),f(i)
END DO
CLOSE(unit)
!!--end--
END SUBROUTINE

SUBROUTINE GNUPLOT_f2( file , f , x , y )
USE KND_Characteristics                                           !!((03-C-KND_Characteristics.f90))
USE FUN_NewFile                                                   !!((05-B-FUN_NewFile.f90))
CHARACTER(*),INTENT(IN) :: file
REAL(KIND_qmc),INTENT(IN) :: f(:),x(:),y(:)
INTEGER :: i,unit
!!--begin--
unit = NewFile(file)
DO i=1,SIZE(f)
 WRITE(unit,"(e21.9,1x,e21.9)")x(i),y(i),f(i)
END DO
CLOSE(unit)
!!--end--
END SUBROUTINE



FUNCTION NewSourceIntegral0( Q0 , ds , s , s01 , sigt ) RESULT(psi_src)
!!#### PURPOSE
!! Integrate a flat isotropic source.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: Q0,ds,s,s01,sigt

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: psi_src

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: ea,eb,a,b,m0

!!--begin--

!void case - expand exponential  in a series
IF( sigt<1.d-12 )THEN

 psi_src = Q0*ds

!non-void case - use the full characteristic
ELSE
 a = s
 b = s+ds

 ea = exp( -sigt*(s01-a) )
 eb = exp( -sigt*(s01-b) )

 m0 =   (eb-ea)/sigt

 psi_src = Q0*m0

END IF

!!--end--
END FUNCTION



FUNCTION NewSourceIntegral1( Q0 , Q1 , ds , s , s01 , sigt ) RESULT(psi_src)
!!#### PURPOSE
!! Integrate a linear isotropic source.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: Q0,Q1,ds,s,s01,sigt

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: psi_src

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: ea,eb,a,b,m0,m1,c

!!--begin--

!void case - expand exponential  in a series
IF( sigt<1.d-12 )THEN

 psi_src = Q0*ds

!non-void case - use the full characteristic
ELSE

 a = s
 b = s+ds
 c = 0.5d0*(b-a)

 ea = exp( -sigt*(s01-a) )
 eb = exp( -sigt*(s01-b) )


 m0 =   (eb-ea)/sigt
 m1 = c*(eb+ea)/sigt - m0/sigt

 psi_src = Q0*m0 + Q1*m1

END IF

!!--end--
END FUNCTION


END MODULE
