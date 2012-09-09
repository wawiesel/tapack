!!# USER MODULE: <<LIB_SubcellBalances>>
MODULE LIB_SubcellBalances

!!## PURPOSE
!! A library of some subcell balance solutions.


!!## NOTES



!!## USAGE

!!## MODULES
USE KND_IntrinsicTypes                !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_Default                       !!((04-A-FUN_Default.f90))
USE USR_PolyExpoMoment                !!((11-C-USR_PolyExpoMoment.f90))
USE FUN_FactorialLn                   !!((08-B-FUN_FactorialLn.f90))
USE FUN_CoefficientBinomial           !!((10-B-FUN_CoefficientBinomial.f90))
USE FUN_IsApprox                      !!((03-A-FUN_IsApprox.f90))
USE FUN_STR                           !!((05-B-FUN_STR.f90))
USE SUB_ExponentialMoment             !!((10-C-SUB_ExponentialMoment.f90))
USE FUN_NewUnit                       !!((04-B-FUN_NewUnit.f90))
USE FUN_NewFile                       !!((05-B-FUN_NewFile.f90))
USE FUN_STR                           !!((05-B-FUN_STR.f90))
USE FUN_Random                        !!((03-A-FUN_Random.f90))
USE LIB_RandomSubcellData             !!((07-C-LIB_RandomSubcellData.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PUBLIC ACCESS LISTING
!! * basic real kind for library functions
!! * The E-component of the angular flux on the
!!   outgoing edge assuming parabolic incoming, <PsiEOut_Para>
!! * The E-component of the angular flux on the
!!   cell interior assuming parabolic incoming, <PsiEInterior_Para>
!! * The S-component of the angular flux on the
!!   outgoing edge assuming linear source, <PsiSOut_Lin>
!! * The S-component of the angular flux on the
!!   cell interior assuming linear source, <PsiSOut_Lin>
PUBLIC :: PsiE_Out_Para
PUBLIC :: PsiS_Out_Lin
PUBLIC :: PsiE_Interior_Para
PUBLIC :: PsiS_Interior_Lin
PUBLIC :: Q_Interior_Lin

PUBLIC :: TEST_PsiS_Out_Lin
PUBLIC :: TEST_PsiE_Out_Para
PUBLIC :: TEST_PsiS_Interior_Lin
PUBLIC :: TEST_PsiE_Interior_Para
PUBLIC :: CHECK1_PsiS_Out_Lin
PUBLIC :: CHECK1_PsiE_Out_Para
PUBLIC :: CHECK1_PsiS_Interior_Lin
PUBLIC :: CHECK1_PsiE_Interior_Para
PUBLIC :: MCOMPARE_PsiS_Interior_Lin

!!## PROCEDURES
CONTAINS


!!### FUNCTION <<CHECK1_PsiS_Out_Lin>>
FUNCTION CHECK1_PsiS_Out_Lin(Qa,Qb,&
  t1,t2,smin1,smin2,smax1,smax2,sigma,q1,qs,qt,sintheta,&
  reldiff,tol,testtol) RESULT(Pass)

!!#### PURPOSE
!! Checks symmetry and positivity of the expressions.

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: Qa,Qb
LOGICAL                   :: Pass

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2,sigma,q1,qs,qt,sintheta

!!#### OPTIONAL INPUT
REAL(KIND_PM),OPTIONAL,INTENT(IN)  :: tol,testtol

!!#### OPTIONAL OUTPUT
REAL(KIND_PM),OPTIONAL,INTENT(OUT) :: reldiff

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: Qaavg,Qbavg,reldiff_,testtol_
LOGICAL        :: SymCheck,PosCheck

!!--begin--
testtol_ = DEFAULT(PM_DEFAULT_TESTTOL,testtol)

Qa=PsiS_Out_Lin(Qaavg,t1,t2,smin1,smin2,smax1,smax2,sigma,q1,qs,qt,sintheta,tol=tol)
Qb=PsiS_Out_Lin(Qbavg,t1,t2,smin2,smin1,smax2,smax1,sigma,q1+qt*t1+qt*t2,qs,-qt,sintheta,tol=tol)

reldiff_ = ( ABS(Qa-Qb)/((Qa+Qb)/2._KIND_PM) )

IF( PRESENT(reldiff) )THEN
 reldiff=reldiff_
END IF

SymCheck = reldiff_<=testtol_
PosCheck = (Qa>=0._KIND_PM) .AND. (Qb>=0._KIND_PM)

Pass = SymCheck .AND. PosCheck

IF( .NOT.Pass )THEN
 IF( .NOT.SymCheck )THEN
  WRITE(*,*)"FAIL: Symmetry violated!"
 END IF
 IF( .NOT.PosCheck )THEN
  WRITE(*,*)"FAIL: Positivity violated!"
 END IF
 WRITE(*,*)"sigma=",sigma
 WRITE(*,*)"sintheta=",sintheta
 WRITE(*,*)"q1=",q1
 WRITE(*,*)"qs=",qs
 WRITE(*,*)"qt=",qt
 WRITE(*,*)"t1=",t1
 WRITE(*,*)"t2=",t2
 WRITE(*,*)"smin1=",smin1
 WRITE(*,*)"smin2=",smin2
 WRITE(*,*)"smax1=",smax1
 WRITE(*,*)"smax2=",smax2
 WRITE(*,*)"Qa=",Qa
 WRITE(*,*)"Qb=",Qb
 WRITe(*,*)
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<CHECK1_PsiE_Out_Para>>
FUNCTION CHECK1_PsiE_Out_Para(Psia,Psib,&
  t1,t2,smin1,smin2,smax1,smax2,sigma,c,b,a,sintheta,&
  reldiff,tol,testtol) RESULT(Pass)

!!#### PURPOSE
!! Check the symmetry and positivity of the <PsiE_Out_Para>.

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: Psia,Psib
LOGICAL                   :: Pass

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2,sigma,c,b,a,sintheta

!!#### OPTIONAL INPUT
REAL(KIND_PM),OPTIONAL,INTENT(IN)  :: tol
REAL(KIND_PM),OPTIONAL,INTENT(IN)  :: testtol

!!#### OPTIONAL OTUPUT
REAL(KIND_PM),OPTIONAL,INTENT(OUT) :: reldiff

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: c2,b2,a2,reldiff_,testtol_,tol_
LOGICAL       :: SymCheck,PosCheck
REAL(KIND_PM) :: Psiaavg,Psibavg

!!--begin--

testtol_  = DEFAULT(PM_DEFAULT_TESTTOL,testtol)

!original parabola
Psia=PsiE_Out_Para(Psiaavg,t1,t2,smin1,smin2,smax1,smax2,sigma,&
  c,b,a,sintheta,tol=tol)



!reverse incoming parabola and side to test
c2 = c + b*t1 + a*t1**2 + b*t2 + 2*a*t1*t2 + a*t2**2
b2 = -b - 2*a*t1 - 2*a*t2
a2 = a
Psib=PsiE_Out_Para(Psibavg,t1,t2,smin2,smin1,smax2,smax1,sigma,&
  c2,b2,a2,sintheta,tol=tol)

IF( Psia==0._KIND_PM .AND. Psib==0._KIND_PM )THEN
 reldiff_ = 0._KIND_PM
ELSE
 reldiff_ = ( ABS(Psia-Psib)/((Psia+Psib)/2._KIND_PM) )
END IF

IF( PRESENT(reldiff) )THEN
 reldiff=reldiff_
END IF

SymCheck = reldiff_<=testtol_
PosCheck = ((Psia>=0._KIND_PM) .AND. (Psib>=0._KIND_PM))

Pass = SymCheck .AND. PosCheck

IF( .NOT.Pass )THEN
 IF( .NOT.SymCheck )THEN
  WRITE(*,*)"FAIL: Symmetry violated!"
 END IF
 IF( .NOT.PosCheck )THEN
  WRITE(*,*)"FAIL: Positivity violated!"
 END IF
 WRITE(*,*)"sigma=",sigma
 WRITE(*,*)"sintheta=",sintheta
 WRITE(*,*)"c=",c
 WRITE(*,*)"b=",b
 WRITE(*,*)"a=",a
 WRITE(*,*)"t1=",t1
 WRITE(*,*)"t2=",t2
 WRITE(*,*)"smin1=",smin1
 WRITE(*,*)"smin2=",smin2
 WRITE(*,*)"smax1=",smax1
 WRITE(*,*)"smax2=",smax2
 WRITE(*,*)"Psia=",Psia
 WRITE(*,*)"Psib=",Psib
 WRITE(*,*)
END IF

!!--end--
END FUNCTION



!!### FUNCTION <<PsiS_Out_Lin>>
FUNCTION PsiS_Out_Lin(Qavg,t1,t2,smin1,smin2,smax1,smax2,&
  sigma,q1,qs,qt,sintheta,tol) RESULT(Q)

!!#### PURPOSE
!! Calculate the outgoing contribution from the source within
!! a cell.

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM),INTENT(IN) :: sigma,q1,qs,qt,sintheta

!!#### OPTIONAL INPUT
REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: Qavg
REAL(KIND_PM) :: Q

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: x,x0,coeff,dt,ds2,ds1,c00,c01,c10,PM00,PM01,PM10
REAL(KIND_PM) :: bt,b1,PM(0:1),tol_

!!--begin--

tol_ = Default( PM_DEFAULT_TOL , tol )

dt=t2-t1
ds2=smax2-smin2
ds1=smax1-smin1

!calculate for case 1)
IF( ds2>=ds1 )THEN
 b1=ds1/ds2
 bt=1._KIND_PM-b1
 IF( sintheta==0._KIND_PM )THEN
  x=SQRT(HUGE(x))
  coeff=x !also huge
 ELSE
  x=sigma*ds2/sintheta
  coeff=1._KIND_PM/sintheta
 END IF
 x0=sigma*ds2
 c00 = q1 + qs*smax1+qt*t1
 c10 = qs*(smax2-smax1)+qt*dt
 c01 = -qs*ds2
 CALL PolyExpoMoment2(i=0,MaxN=1,x=x,bt=bt,b1=b1,x0=x0,c=coeff,PM=PM(0:1),tol=tol_)
 PM00 = PM(0)
 PM01 = PM(1)
 CALL PolyExpoMoment2(i=1,MaxN=0,x=x,bt=bt,b1=b1,x0=x0,c=coeff,PM=PM(0:0),tol=tol_)
 PM10 = PM(0)
 !old code forgot sintheta
 Qavg=ds2*( c00*PM00 + c10*PM10 + c01*PM01 )
 !Qavg=sintheta*ds2*( c00*PM00 + c10*PM10 + c01*PM01 )
 Q=dt*Qavg

!calculate for case 2
ELSE
 b1=ds2/ds1
 bt=1._KIND_PM-b1
 IF( sintheta==0._KIND_PM )THEN
  x=SQRT(HUGE(x))
  coeff=x !also huge
 ELSE
  x=sigma*ds1/sintheta
  coeff=1._KIND_PM/sintheta
 END IF
 x0=sigma*ds1
 c00 = q1 + qs*smax2+qt*t2
 c10 = qs*(smax1-smax2)-qt*dt
 c01 = -qs*ds1
 CALL PolyExpoMoment2(i=0,MaxN=1,x=x,bt=bt,b1=b1,x0=x0,c=coeff,PM=PM(0:1),tol=tol_)
 PM00 = PM(0)
 PM01 = PM(1)
 CALL PolyExpoMoment2(i=1,MaxN=0,x=x,bt=bt,b1=b1,x0=x0,c=coeff,PM=PM(0:0),tol=tol_)
 PM10 = PM(0)
 !old code forgot sintheta
 Qavg=ds1*( c00*PM00 + c10*PM10 + c01*PM01 )
 !Qavg=sintheta*ds1*( c00*PM00 + c10*PM10 + c01*PM01 )
 Q=dt*Qavg
END IF

!!--end--
END FUNCTION




!!### FUNCTION <<PsiE_Out_Para>>
FUNCTION PsiE_Out_Para(Psiavg,t1,t2,smin1,smin2,smax1,smax2,sigma,&
  c,b,a,sintheta,tol) RESULT(Psi)

!!#### PURPOSE
!! Calculate the outgoing angular flux according to a parabolic incoming
!! flux distribution.

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM),INTENT(IN) :: sigma,c,b,a,sintheta

!!#### OPTIONAL INPUT
REAL(KIND_PM),INTENT(IN) :: tol

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: Psiavg
REAL(KIND_PM) :: Psi

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: x,dt,ds2,ds1,c0,c1,c2,tol_
REAL(KIND_PM) :: PM(0:2)

!!--begin--

tol_ = Default( PM_DEFAULT_TOL , tol )

dt  = t2-t1
ds2 = smax2-smin2
ds1 = smax1-smin1

!calculate for case 1)
IF( ds2>=ds1 )THEN

 IF( sintheta==0._KIND_PM )THEN
  x=SQRT(HUGE(x))
!  coeff=x !also huge
 ELSE
  x=sigma*(ds2-ds1)/sintheta
!  coeff=1._KIND_PM/sintheta
 END IF
! x0=sigma*(ds2-ds1)

 c0 = a*t1**2 + b*t1 + c
 c1 = 2*a*dt*t1 + b*dt
 c2 = a*dt**2
 CALL PolyExpoMoment(MaxN=2,x=x,x0=x,c=1._KIND_PM,PM=PM(0:2),tol=tol_)
 !old code that didn't have sintheta
 Psiavg = EXP(-sigma*ds1/sintheta)*( c0*PM(0) + c1*PM(1) + c2*PM(2) )
 !Psiavg = sintheta*EXP(-sigma*ds1/sintheta)*( c0*PM(0) + c1*PM(1) + c2*PM(2) )
 Psi    = dt*PsiAvg

!calculate for case 2
ELSE

 IF( sintheta==0._KIND_PM )THEN
  x=SQRT(HUGE(x))
!  coeff=x !also huge
 ELSE
  x=sigma*(ds1-ds2)/sintheta !reldifferent than above
  !coeff=1._KIND_PM/sintheta
 END IF
 !x0=sigma*(ds1-ds2) !reldifferent than above

 c0 =  a*dt**2 + 2*a*t1*dt + b*dt + a*t1**2 + b*t1 + c
 c1 = -b*dt    - 2*a*t1*dt - 2*a*dt**2
 c2 =  a*dt**2
 CALL PolyExpoMoment(MaxN=2,x=x,x0=x,c=1._KIND_PM,PM=PM(0:2),tol=tol_)
 !old code that didn't have sintheta
 Psiavg = EXP(-sigma*ds2/sintheta)*( c0*PM(0) + c1*PM(1) + c2*PM(2) )
 !Psiavg = sintheta*EXP(-sigma*ds2/sintheta)*( c0*PM(0) + c1*PM(1) + c2*PM(2) )
 Psi    = dt*PsiAvg

END IF

!!--end--
END FUNCTION


!!### FUNCTION <<Q_Interior_Lin>>
FUNCTION Q_Interior_Lin(Qavg,t1,t2,smin1,smin2,smax1,smax2,&
  q1,qs,qt,tol,Noisy) RESULT(Q)

!!#### PURPOSE
!! Return the integrated interior source.

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM),INTENT(IN) :: q1,qs,qt

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: Qavg
REAL(KIND_PM) :: Q

!!#### OPTIONAL INPUT
REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol
LOGICAL,INTENT(IN),OPTIONAL :: Noisy

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: dt,ds2,ds1
REAL(KIND_PM) :: bt,b1,tol_
REAL(KIND_PM) :: smaxbar,sminbar,Dsbar
REAL(KIND_PM) :: dsfrac,smint,smaxt,smin0,smax0
REAL(KIND_PM) :: P00,P01,P10,c00,c01,c10
LOGICAL       :: Noisy_

!!--begin--

tol_ = Default( PM_DEFAULT_TOL , tol )
Noisy_    = Default( .FALSE. , Noisy )

dt=t2-t1
ds2=smax2-smin2
ds1=smax1-smin1

smaxbar = 0.5_KIND_PM*(smax1+smax2)
sminbar = 0.5_KIND_PM*(smin1+smin2)
Dsbar = smaxbar-sminbar

!calculate for case 1)
IF( ds2>=ds1 )THEN

 b1=ds1/ds2
 bt=1._KIND_PM-b1
 smin0 = smin1
 smax0 = smax1
 smaxt = smax2-smax1
 smint = smin2-smin1

 dsfrac = 2._KIND_PM/( 1._KIND_PM + ds1/ds2 )

 P00=EVAL_Pij(0,0,b1,bt)
 P10=EVAL_Pij(1,0,b1,bt)
 P01=EVAL_Pij(0,1,b1,bt)
 c00=dsfrac*( qs*smin0 + qt*t1 + q1 )
 c10=dsfrac*( qt*dt + qs*smint )
 c01=dsfrac*( qs*ds2 )

 !produce average first
 Qavg = c00*P00 + c10*P10 + c01*P01

 !produce integral second
 Q = Qavg*dt*dsbar

!calculate for case 2
ELSE

 b1=ds2/ds1
 bt=1._KIND_PM-b1
 smin0 = smin2
 smax0 = smax2
 smaxt = smax1-smax2
 smint = smin1-smin2

 dsfrac = 2._KIND_PM/( 1._KIND_PM + ds2/ds1 )

 P00=EVAL_Pij(0,0,b1,bt)
 P10=EVAL_Pij(1,0,b1,bt)
 P01=EVAL_Pij(0,1,b1,bt)
 c00=dsfrac*( qs*smin0 + qt*t2 + q1 )
 c10=dsfrac*( -qt*dt + qs*smint )
 c01=dsfrac*( qs*ds1 )

 !produce average first
 Qavg = c00*P00 + c10*P10 + c01*P01
 !produce integral second
 Q = Qavg*dt*dsbar

END IF

!!--end--
END FUNCTION



!!### FUNCTION <<PsiS_Interior_Lin>>
FUNCTION PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,smax1,smax2,&
  sigma,q1,qs,qt,sintheta,tol,ForceSTD,ForceNZE,Noisy) RESULT(Q)

!!#### PURPOSE
!! Return the contribution to the interior from the source.

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM),INTENT(IN) :: sigma,q1,qs,qt,sintheta

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: Qavg
REAL(KIND_PM) :: Q

!!#### OPTIONAL INPUT
REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol
LOGICAL,INTENT(IN),OPTIONAL :: ForceNZE,ForceSTD,Noisy

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: x,x0,coeff,dt,ds2,ds1,c00,c01,c10,c01a,c01b
REAL(KIND_PM) :: bt,b1,tol_
REAL(KIND_PM) :: smaxbar,sminbar,Dsbar

REAL(KIND_PM) :: PM2(0:1,0:1)
REAL(KIND_PM) :: dsfrac,smint,smaxt,smin0,smax0
REAL(KIND_PM) :: PM00,PM10,PM01,P00,P01,P10
LOGICAL        :: ForceNZE_,ForceSTD_,Noisy_

!!--begin--

tol_ = Default( PM_DEFAULT_TOL , tol )

!get whether we force near-zero expansion or use
!the standard solution
ForceNZE_ = Default( .FALSE. , ForceNZE )
ForceSTD_ = Default( .FALSE. , ForceSTD )
Noisy_    = Default( .FALSE. , Noisy )

dt=t2-t1
ds2=smax2-smin2
ds1=smax1-smin1

smaxbar = 0.5_KIND_PM*(smax1+smax2)
sminbar = 0.5_KIND_PM*(smin1+smin2)
Dsbar = smaxbar-sminbar

!calculate for case 1)
IF( ds2>=ds1 )THEN

 b1=ds1/ds2
 bt=1._KIND_PM-b1
 smin0 = smin1
 smax0 = smax1
 smaxt = smax2-smax1
 smint = smin2-smin1
 IF( sintheta==0._KIND_PM )THEN
  x=SQRT(HUGE(x))
  !coeff=x !also huge
  !xa = x
 ELSE
  x=sigma*ds2/sintheta
  !coeff=1._KIND_PM/sintheta
  !xa = sigma*ds2*bt/sintheta
 END IF

 !x0=sigma*ds2
 !xa0=sigma*ds2*bt

 dsfrac = 2._KIND_PM/( 1._KIND_PM + ds1/ds2 )

 IF( .NOT. ForceSTD_ )THEN
  IF( x<=SMALL_X .OR. ForceNZE_ )THEN
   Q = QInterior_Lin_NZE(Qavg,t1,t2,dt,bt,dsfrac,ds2,dsbar,&
                         smin1,smin2,&
                         sigma,q1,qs,qt,sintheta,tol=tol_)
   IF( Noisy_ )THEN
	WRITE(*,*)"x=",x
	WRITE(*,*)"using case 1 NZE expansion for PsiS_Interior_Lin"
    WRITE(*,*)" "
   END IF
   RETURN
  END IF
 END IF
 IF( Noisy_ )THEN
  WRITE(*,*)"using case 1 STD expansion for PsiS_Interior_Lin"
  WRITE(*,*)" "
 END IF

 !0) constant q1 part
 !----
 c00 = q1/(sigma)
 CALL PolyExpoMoment2(i=0,MaxN=0,x=x,bt=bt,b1=b1,x0=x,&
   c=1._KIND_PM,PM=PM2(0,0:0),tol=tol_)
 PM00 = ( 1._KIND_PM - PM2(0,0)*dsfrac )
 !----

 !1) linear qt part
 c10 = qt*dsfrac/sigma
 CALL PolyExpoMoment2(i=1,MaxN=0,x=x,bt=bt,b1=b1,x0=x,&
   c=1._KIND_PM,PM=PM2(1,0:0),tol=tol_)
 PM10 = b1*t1 + b1*dt/2._KIND_PM + bt*t1/2._KIND_PM + dt*bt/3._KIND_PM - &
        t1*PM2(0,0) - &
        dt*PM2(1,0)

 !2) linear qs part
 c01  = qs*dsfrac/sigma
 P00=EVAL_Pij(0,0,b1,bt)
 P10=EVAL_Pij(1,0,b1,bt)
 P01=EVAL_Pij(0,1,b1,bt)
 PM01 = smin0*(P00-PM2(0,0)) + &
        smint*(P10-PM2(1,0)) + &
		ds2*P01 - &
        (sintheta/sigma)*(P00-PM2(0,0))

 !produce average first
 Qavg = c00*PM00 + c10*PM10 + c01*PM01
 !produce integral second
 Q = Qavg*dt*dsbar

!calculate for case 2
ELSE

 b1=ds2/ds1
 bt=1._KIND_PM-b1
 smin0 = smin2
 smax0 = smax2
 smaxt = smax1-smax2
 smint = smin1-smin2
 IF( sintheta==0._KIND_PM )THEN
  x=SQRT(HUGE(x))
  !coeff=x !also huge
  !xa = x
 ELSE
  x=sigma*ds1/sintheta
  !coeff=1._KIND_PM/sintheta
  !xa = sigma*ds1*bt/sintheta
 END IF

 !x0=sigma*ds1
 !xa0=sigma*ds1*bt

 dsfrac = 2._KIND_PM/( 1._KIND_PM + ds2/ds1 )

 IF( .NOT. ForceSTD_ )THEN
  IF( x<=SMALL_X .OR. ForceNZE_ )THEN
   Q = QInterior_Lin_NZE(Qavg,t2,t1,dt,bt,dsfrac,ds1,dsbar,&
                         smin2,smin1,&
                         sigma,q1,qs,qt,sintheta,tol=tol_)
   IF( Noisy_ )THEN
    WRITE(*,*)"x=",x
	WRITE(*,*)"using case 2 NZE expansion for PsiS_Interior_Lin"
	WRITE(*,*)" "
   END IF
   RETURN
  END IF
 END IF
 IF( Noisy_ )THEN
  WRITE(*,*)"using STD case 2 expansion for PsiS_Interior_Lin"
  WRITE(*,*)" "
 END IF

 !0) constant q1 part
 !----
 c00 = q1/(sigma)
 CALL PolyExpoMoment2(i=0,MaxN=0,x=x,bt=bt,b1=b1,x0=x,&
   c=1._KIND_PM,PM=PM2(0,0:0),tol=tol_)
 PM00 = ( 1._KIND_PM - PM2(0,0)*dsfrac )
 !----

 !1) linear qt part
 c10 = qt*dsfrac/sigma
 CALL PolyExpoMoment2(i=1,MaxN=0,x=x,bt=bt,b1=b1,x0=x,&
   c=1._KIND_PM,PM=PM2(1,0:0),tol=tol_)
 PM10 = b1*t2 - b1*dt/2._KIND_PM + bt*t2/2._KIND_PM - dt*bt/3._KIND_PM - &
        t2*PM2(0,0) + &
        dt*PM2(1,0)

 !2) linear qs part
 c01  = qs*dsfrac/sigma
 P00=EVAL_Pij(0,0,b1,bt)
 P10=EVAL_Pij(1,0,b1,bt)
 P01=EVAL_Pij(0,1,b1,bt)
 PM01 = smin0*(P00-PM2(0,0)) + &
        smint*(P10-PM2(1,0)) + &
		ds1*P01 - &
        (sintheta/sigma)*(P00-PM2(0,0))

 !produce average first
 Qavg = c00*PM00 + c10*PM10 + c01*PM01
 !produce integral second
 Q = Qavg*dt*dsbar

END IF

!!--end--
END FUNCTION



!!### FUNCTION <<QInterior_Lin_NZE>>
FUNCTION QInterior_Lin_NZE(Qavg,t1,t2,dt,bt,dsfrac,ds,dsbar,&
  smin1,smin2,&
  sigma,q1,qs,qt,sintheta,tol) RESULT(Q)

!!#### PURPOSE
!! Return the near-zero expansion (NZE) for the case 1 condition,
!! just pass different terms for the case 2 condition.

USE FUN_Default                       !!((04-A-FUN_Default.f90))
USE FUN_Factorial                     !!((06-B-FUN_Factorial.f90))

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,dt,bt,dsfrac,ds,dsbar
REAL(KIND_PM),INTENT(IN) :: smin1,smin2
REAL(KIND_PM),INTENT(IN) :: sigma,q1,qs,qt,sintheta

!!#### OPTIONAL INPUT
REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: Qavg
REAL(KIND_PM) :: Q

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: x,sum1,sumt,sums,sum
REAL(KIND_PM) :: NZE1,NZE12,NZEell12,NZEN1
REAL(KIND_PM) :: tol_

!!--begin--

 tol_ = Default(PM_DEFAULT_TOL,tol)

 x=sigma*ds/sintheta

 IF( q1/=0._KIND_PM )THEN
  NZE1     = NZESeries( x , C1     , offset=2 , tol=tol_ )
 ELSE
  NZE1     = 0._KIND_PM
 END IF
 IF( qt/=0._KIND_PM .OR. qs/=0._KIND_PM )THEN
  NZE12    = NZESeries( x , C12    , offset=2 , tol=tol_ )
  NZEell12 = NZESeries( x , Cell12 , offset=2 , tol=tol_ )
 ELSE
  NZE12    = 0._KIND_PM
  NZEell12 = 0._KIND_PM
 END IF
 IF( qs/=0._KIND_PM )THEN
  NZEN1    = NZESeries( x , CN1    , offset=2 , tol=tol_ )
 ELSE
  NZEN1    = 0._KIND_PM
 END IF

 sum1  = NZE1
 sumt  = (   t1+   t2)*NZE12 +    t1*NZEell12
 sums  = (smin1+smin2)*NZE12 + smin1*NZEell12 + ds*NZEN1

 Qavg = (sum1*q1+sumt*qt+sums*qs)*ds*dsfrac/sintheta
 Q    = Qavg*dt*dsbar

!!--end--
CONTAINS

FUNCTION C1(N)
USE FUN_FactorialLn                   !!((08-B-FUN_FactorialLn.f90))
INTEGER,INTENT(IN) :: N
REAL(KIND_PM) :: C1,coeff
INTEGER :: ell
!!--begin--
C1 = 0._KIND_PM
DO ell=0,N
 coeff = EXP( - FactorialLn( ell ) - FactorialLn( N-ell ) )
 C1 = C1 + ( (-Bt)**ell ) * coeff /REAL(ell+1,KIND_PM)
END DO
!!--end--
END FUNCTION

FUNCTION CN1(N)
USE FUN_FactorialLn                   !!((08-B-FUN_FactorialLn.f90))
INTEGER,INTENT(IN) :: N
REAL(KIND_PM) :: CN1,coeff
INTEGER :: ell
!!--begin--

CN1 = 0._KIND_PM
DO ell=0,N+1
 coeff = EXP( - FactorialLn( ell ) - FactorialLn( N+1-ell ) )
 CN1 = CN1 + ( (-Bt)**ell ) * &
   ( 1._KIND_PM/REAL(ell+1,KIND_PM) )*coeff
END DO

!!--end--
END FUNCTION

FUNCTION C12(N)
USE FUN_FactorialLn                   !!((08-B-FUN_FactorialLn.f90))
INTEGER,INTENT(IN) :: N
REAL(KIND_PM) :: C12,coeff
INTEGER :: ell
!!--begin--

C12 = 0._KIND_PM
DO ell=0,N
 coeff = EXP( - FactorialLn( ell ) - FactorialLn( N-ell ) )
 C12 = C12 + ( (-Bt)**ell ) * &
   ( 1._KIND_PM/(REAL(ell+1,KIND_PM)*REAL(ell+2,KIND_PM)) )*coeff
END DO

!!--end--
END FUNCTION


FUNCTION Cell12(N)
USE FUN_FactorialLn                   !!((08-B-FUN_FactorialLn.f90))
INTEGER,INTENT(IN) :: N
REAL(KIND_PM) :: Cell12,coeff
INTEGER :: ell
!!--begin--

Cell12 = 0._KIND_PM
DO ell=0,N
 coeff = EXP( - FactorialLn( ell ) - FactorialLn( N-ell ) )
 Cell12 = Cell12 + ( (-Bt)**ell ) * &
   ( REAL(ell,KIND_PM)/(REAL(ell+1,KIND_PM)*REAL(ell+2,KIND_PM)) )*coeff
END DO

!!--end--
END FUNCTION


FUNCTION NZESeries( x , f , offset , tol ) RESULT(sum)
REAL(KIND_PM)            :: sum
REAL(KIND_PM),INTENT(IN) :: x,tol
INTEGER,INTENT(IN)        :: offset

!define function f
INTERFACE
 FUNCTION f(N)
 USE USR_PolyExpoMoment,ONLY: KIND_PM !!((11-C-USR_PolyExpoMoment.f90))
 REAL(KIND_PM)     :: f
 INTEGER,INTENT(IN) :: N
 END FUNCTION f
END INTERFACE

!!#### LOCAL VARIABLES
INTEGER        :: k,kk
REAL(KIND_PM) :: terms(0:1000)

!!--begin--
!go forward to find the proper truncation term
k = -1
DO
 k = k + 1
 IF( k>=UBOUND(terms,1) )THEN
  WRITE(*,*)"Warning: the near-zero expansion has exceeded the &
    &upper bound of the terms array: "//TRIM(STR(UBOUND(terms,1)))
  EXIT
 END IF
 terms(k) = ((-x)**k) * ( f(k+offset) )
 !change to relative tolerance condition, the leading term can
 !actually be negative here, so use absolute value
 !change to <= if terms are 0
 IF( ABS(terms(k))<=tol_*ABS(terms(0)) )EXIT
END DO

!backward to accumulate terms (prevents loss of accuracy)
sum = 0._KIND_PM
DO kk=k,0,-1
 sum = sum + terms(kk)
END DO

IF( k>=UBOUND(terms,1) )THEN
 WRITE(*,*)" Warning: the near-zero expansion sum is ",sum
 WRITE(*,*)" Warning: the last 5 terms are: ",terms(995:1000)
END IF

!!--end--
END FUNCTION


END FUNCTION




!!### FUNCTION <<PsiE_Interior_Para>>
FUNCTION PsiE_Interior_Para(Psiavg,t1,t2,smin1,smin2,smax1,smax2,sigma,&
  c,b,a,sintheta,tol) RESULT(Psi)

!!#### PURPOSE
!! Return the contribution to the interior angular flux according
!! to a parabolic incoming flux distribution.

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM),INTENT(IN) :: sigma,c,b,a,sintheta

!!#### OPTIONAL INPUT
REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: Psiavg
REAL(KIND_PM) :: Psi

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: x,xa,x0,xa0
REAL(KIND_PM) :: dt,ds2,ds1,c00,c10,c20
REAL(KIND_PM) :: smaxbar,sminbar,dsbar,dsfrac
REAL(KIND_PM) :: b1,bt,coeff,tol_
REAL(KIND_PM) :: PM2(0:2,0:0)

!!--begin--

tol_ = Default( PM_DEFAULT_TOL , tol )

dt=t2-t1
ds2=smax2-smin2
ds1=smax1-smin1

smaxbar = 0.5_KIND_PM*(smax1+smax2)
sminbar = 0.5_KIND_PM*(smin1+smin2)
Dsbar = smaxbar-sminbar

!calculate for case 1)
IF( ds2>=ds1 )THEN

 b1=ds1/ds2
 bt=1._KIND_PM-b1
 IF( sintheta==0._KIND_PM )THEN
  x=SQRT(HUGE(x))
  coeff=x !also huge
  xa = x
 ELSE
  x=sigma*ds2/sintheta
  coeff=1._KIND_PM/sintheta
  xa = sigma*ds2*bt/sintheta
 END IF

 x0=sigma*ds2
 xa0=sigma*ds2*bt

 dsfrac = ds2/dsbar

 c00 = (   a*t1**2 + b*t1 + c )*dsfrac
 c10 = ( 2*a*dt*t1 + b*dt     )*dsfrac
 c20 = (   a*dt**2            )*dsfrac
 CALL PolyExpoMoment2(i=0,MaxN=0,x=x,bt=bt,b1=b1,x0=x,c=1._KIND_PM,PM=PM2(0,0:0),tol=tol_)
 CALL PolyExpoMoment2(i=1,MaxN=0,x=x,bt=bt,b1=b1,x0=x,c=1._KIND_PM,PM=PM2(1,0:0),tol=tol_)
 CALL PolyExpoMoment2(i=2,MaxN=0,x=x,bt=bt,b1=b1,x0=x,c=1._KIND_PM,PM=PM2(2,0:0),tol=tol_)

 !produce average first
 Psiavg = c00*PM2(0,0) + c10*PM2(1,0) + c20*PM2(2,0)
 !produce integral second
 Psi = Psiavg*dt*dsbar


!calculate for case 2
ELSE
 b1=ds2/ds1
 bt=1._KIND_PM-b1
 IF( sintheta==0._KIND_PM )THEN
  x=SQRT(HUGE(x))
  coeff=x !also huge
  xa = x
 ELSE
  x=sigma*ds1/sintheta
  coeff=1._KIND_PM/sintheta
  xa = sigma*ds1*bt/sintheta
 END IF

 x0=sigma*ds1
 xa0=sigma*ds1*bt

 dsfrac = ds1/dsbar

 c00 = (   a*t2**2 + b*t2 + c )*dsfrac
 c10 = (-2*a*dt*t2 - b*dt     )*dsfrac
 c20 = (   a*dt**2            )*dsfrac
 CALL PolyExpoMoment2(i=0,MaxN=0,x=x,bt=bt,b1=b1,x0=x,c=1._KIND_PM,PM=PM2(0,0:0),tol=tol_)
 CALL PolyExpoMoment2(i=1,MaxN=0,x=x,bt=bt,b1=b1,x0=x,c=1._KIND_PM,PM=PM2(1,0:0),tol=tol_)
 CALL PolyExpoMoment2(i=2,MaxN=0,x=x,bt=bt,b1=b1,x0=x,c=1._KIND_PM,PM=PM2(2,0:0),tol=tol_)

 !produce average first
 Psiavg = c00*PM2(0,0) + c10*PM2(1,0) + c20*PM2(2,0)
 !produce integral second
 Psi = Psiavg*dt*dsbar

END IF

!!--end--
END FUNCTION




!!### FUNCTION <<TEST_PsiS_Interior_Lin>>
FUNCTION TEST_PsiS_Interior_Lin() RESULT(Pass)
!!#### PURPOSE
!! Test the interior integration kernel for sources.

!!#### DETAILS
!! Mathematica was used with extended precision to produce
!! the exact Q values for the following tests, however
!! the Matematica code not valid in all the cases for
!! which the interior integration kernel is.

!!#### REQUIRED OUTPUT
LOGICAL :: Pass

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: t1,t2,Qavg,smin1,smin2,smax1,smax2
REAL(KIND_PM) :: sigma,q1,qs,qt,sintheta,Q
REAL(KIND_PM) :: Q_exact,sigma_range(2),mulsigma
REAL(KIND_PM) :: Qnze,Qstd
INTEGER :: i,U1

!!--begin--


!test 0a
sintheta = 0.900000000000000d0
sigma    = 5.000000000000000D-002
smax2    = 1.30000000000000d0
smin2    = 0.100000000000000d0
smax1    = 1.00000000000000d0
smin1    = 0.000000000000000D+000
t1       = 0.0_KIND_PM
t2       = 1.0_KIND_PM
qs       = 0._KIND_PM
qt       = 0._KIND_PM
q1       = 1._KIND_PM
Pass = COMPARE_PsiS_Interior_Lin(t1,t2,smin1,smin2,smax1,smax2,&
   sigma,q1,qs,qt,sintheta)
IF( .NOT. Pass )RETURN

q1       = 0._KIND_PM
qt       = 0._KIND_PM
qs       = 1._KIND_PM
Q = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Q_exact = 0.27973717114853883944690976300890899852785551937032_KIND_PM
Pass = IsApprox(Q,Q_exact)
IF( .NOT. Pass )RETURN


!Test ZZZ
smin1 =  0.5_KIND_PM
smin2 =  1._KIND_PM
smax1 =  1._KIND_PM
smax2 =  1.4_KIND_PM
t1    = -1._KIND_PM
t2    =  2._KIND_PM
sigma=1._KIND_PM/15._KIND_PM

q1=1._KIND_PM; qt=0._KIND_PM; qs=0._KIND_PM
Q_exact = 0.3351243994350684774169912729274319548563_KIND_PM
Q = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Pass = IsApprox(Q,Q_exact)
IF( .NOT. Pass )RETURN

q1=0._KIND_PM; qt=1._KIND_PM; qs=0._KIND_PM
Q_exact = 0.1306818053779611392025969689762731349473_KIND_PM
Q = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Pass = IsApprox(Q,Q_exact)
IF( .NOT. Pass )RETURN

q1=0._KIND_PM; qt=0._KIND_PM; qs=1._KIND_PM
Q_exact = 0.2960171748129480630157114922606687685019_KIND_PM
Q = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Pass = IsApprox(Q,Q_exact)
IF( .NOT. Pass )RETURN

q1       = 0._KIND_PM
qs       = 0._KIND_PM
qt       = 1._KIND_PM
Pass = COMPARE_PsiS_Interior_Lin(t1,t2,smin1,smin2,smax1,smax2,&
   sigma,q1,qs,qt,sintheta)
IF( .NOT. Pass )RETURN


!test 1a
sintheta = 1.0_KIND_PM
sigma    = 1.0_KIND_PM
t1       = 0.0_KIND_PM
t2       = 1.0_KIND_PM
smax1    = 0.9_KIND_PM
smax2    = 1.0_KIND_PM
smin1    = 0.2_KIND_PM
smin2    = -0.5_KIND_PM
qs = 0._KIND_PM
qt = 1._KIND_PM
q1 = 1._KIND_PM
Pass = COMPARE_PsiS_Interior_Lin(t1,t2,smin1,smin2,smax1,smax2,&
   sigma,q1,qs,qt,sintheta)
IF( .NOT. Pass )RETURN

!test 1a
sintheta = 1.0_KIND_PM
sigma    = 1.0_KIND_PM
t1       = 0.0_KIND_PM
t2       = 1.0_KIND_PM
smax1    = 0.9_KIND_PM
smax2    = 1.0_KIND_PM
smin1    = 0.2_KIND_PM
smin2    = -0.5_KIND_PM
qs = 1._KIND_PM
qt = 1._KIND_PM
q1 = 1._KIND_PM
Pass = COMPARE_PsiS_Interior_Lin(t1,t2,smin1,smin2,smax1,smax2,&
   sigma,q1,qs,qt,sintheta)
IF( .NOT. Pass )RETURN

!test 1b
sintheta = 1.0_KIND_PM
sigma    = 0.05_KIND_PM
t1       = 0.0_KIND_PM
t2       = 1.0_KIND_PM
smax1    = 0.9_KIND_PM
smax2    = 1.0_KIND_PM
smin1    = 0.2_KIND_PM
smin2    = -0.5_KIND_PM
qs = 0._KIND_PM
qt = 0._KIND_PM
q1 = 1._KIND_PM
Pass = COMPARE_PsiS_Interior_Lin(t1,t2,smin1,smin2,smax1,smax2,&
   sigma,q1,qs,qt,sintheta)
IF( .NOT. Pass )RETURN

Pass = MONOCHECK_PsiS_Interior_Lin(t1,t2,smin1,smin2,smax1,smax2,&
   q1,qs,qt,sintheta)
IF( .NOT.Pass )RETURN


!test 1
sintheta = 1.0_KIND_PM
sigma    = 0.1_KIND_PM
t1       = 0.0_KIND_PM
t2       = 1.0_KIND_PM
smax1    = 0.9_KIND_PM
smax2    = 1.0_KIND_PM
smin1    = 0.2_KIND_PM
smin2    = -0.5_KIND_PM
qs = 0._KIND_PM
qt = 1._KIND_PM
q1 = 1._KIND_PM

Pass = COMPARE_PsiS_Interior_Lin(t1,t2,smin1,smin2,smax1,smax2,&
   sigma,q1,qs,qt,sintheta)
IF( .NOT.Pass )RETURN

qs    =  0.0_KIND_PM
qt    = -1.0_KIND_PM
q1    =  2.0_KIND_PM
smax1 =  1.0_KIND_PM
smax2 =  0.9_KIND_PM
smin1 = -0.5_KIND_PM
smin2 =  0.2_KIND_PM
Pass = COMPARE_PsiS_Interior_Lin(t1,t2,smin1,smin2,smax1,smax2,&
   sigma,q1,qs,qt,sintheta)

IF( .NOT.Pass )RETURN

!test 0
sintheta = 0.8_KIND_PM
sigma    = 1.4_KIND_PM
t1       = 0.3_KIND_PM
t2       = 0.9_KIND_PM
smax1    = 1.1_KIND_PM
smax2    = 1.0_KIND_PM
smin1    = 0.0_KIND_PM
smin2    = 0.8_KIND_PM
qs = 0._KIND_PM
qt = 0._KIND_PM
q1 = 1._KIND_PM

Pass = COMPARE_PsiS_Interior_Lin(t1,t2,smin1,smin2,smax1,smax2,&
   sigma,q1,qs,qt,sintheta)
IF( .NOT.Pass )RETURN


!test 2
sintheta = 0.5_KIND_PM
sigma    = 4.0_KIND_PM
t1       = -0.3_KIND_PM
t2       = +0.2_KIND_PM
smax1    = 0.4_KIND_PM
smax2    = 4.0_KIND_PM
smin1    = -0.4_KIND_PM
smin2    = +0.1_KIND_PM
qs = 0._KIND_PM
qt = 0._KIND_PM
q1 = 1._KIND_PM
Q       = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Q_exact = Mathematica_PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Pass = IsApprox(Q,Q_exact)
IF( .NOT.Pass )RETURN

!just change one thing
smax2=3.0_KIND_PM
Q       = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Q_exact = Mathematica_PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Pass = IsApprox(Q,Q_exact)
IF( .NOT.Pass )RETURN


!just change one thing
smax2=2.0_KIND_PM
Q       = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Q_exact = Mathematica_PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Pass = IsApprox(Q,Q_exact)
IF( .NOT.Pass )RETURN


!just change one thing
smax2=1.0_KIND_PM
Q       = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Q_exact = Mathematica_PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Pass = IsApprox(Q,Q_exact)
IF( .NOT.Pass )RETURN

!just change one thing
smax2=0.92_KIND_PM
Q       = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Q_exact = Mathematica_PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Pass = IsApprox(Q,Q_exact)
IF( .NOT.Pass )RETURN


!just change one thing
smax2=0.901_KIND_PM
Q       = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Q_exact = Mathematica_PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)
Pass = IsApprox(Q,Q_exact)
IF( .NOT.Pass )RETURN


!!--end--
END FUNCTION


!!### FUNCTION <<Mathematica_PsiS_Interior_Lin>>
FUNCTION Mathematica_PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
  smax1,smax2,sigma,q1,qs,qt,sintheta) RESULT(Q)

!!#### PURPOSE
!! Use Mathematica to get an exact solution.
!!
!! Only valid when (smax1-smin1)/=(smax2-smax1) and sigt not too small.

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM),INTENT(IN) :: sigma,q1,qs,qt,sintheta

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: Qavg
REAL(KIND_PM) :: Q

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: E,area,c1,cs,ct

!!--begin--

E = EXP(1._KIND_PM)
c1 = ((t1 - t2)*(2*E**(((-smax1 + smin1)*sigma)/sintheta)*sintheta**2 - &
                 2*E**(((-smax2 + smin2)*sigma)/sintheta)*sintheta**2 + &
				 (smax1 - smax2 - smin1 + smin2)*sigma*(2*sintheta + &
				(-smax1 - smax2 + smin1 + smin2)*sigma)))/&
                (2._KIND_PM*(smax1 - smax2 - smin1 + smin2)*sigma**3)

cs = ((sintheta*(smax1 + smax2 - smin1 - smin2)*(t1 - t2))/2. - &
     ((smax1**2 + smax1*smax2 + smax2**2 - smin1**2 - smin1*smin2 - smin2**2)*(t1 - t2)*sigma)/6. - &
	 (sintheta*(-((smin1 - smin2)*t1**2*sigma) + (smin1 - smin2)*t2**2*sigma + &
	 (2*E**(((-smax1 + smin1)*sigma)/sintheta)*sintheta*(t1 - t2)**2*&
	 (sintheta*(smax1 - smax2 - 2*smin1 + 2*smin2) + smin1*(-smax1 + smax2 + smin1 - smin2)*sigma))/&
	 ((smax1 - smax2 - smin1 + smin2)**2*sigma) + &
	 (2*E**(((-smax2 + smin2)*sigma)/sintheta)*sintheta*(t1 - t2)**2*&
	 (sintheta*(-smax1 + smax2 + 2*smin1 - 2*smin2) + smin2*(smax1 - smax2 - smin1 + smin2)*sigma))/&
	 ((smax1 - smax2 - smin1 + smin2)**2*sigma) + 2*t2*(-(sintheta*t1) + sintheta*t2 + smin2*t1*sigma - &
	 smin1*t2*sigma) + &
	 2*t1*(sintheta*(t1 - t2) - smin2*t1*sigma + smin1*t2*sigma)))/(2.*(t1 - &
	 t2)*sigma))/sigma**2

ct = (-2*(smax1 - smax2 - smin1 + smin2)*t1**3*sigma**3 + 2*(smax1 - smax2 - smin1 + smin2)*t2**3*sigma**3 + &
     (6*E**(((-smax1 + smin1)*sigma)/sintheta)*sintheta**2*(t1 - t2)**2*&
	 (sintheta*(t1 - t2) + (smax1 - smax2 - smin1 + smin2)*t1*sigma))/(smax1 - smax2 - smin1 + smin2)**2 - &
	 (6*E**(((-smax2 + smin2)*sigma)/sintheta)*sintheta**2*(t1 - t2)**2*&
	 (sintheta*(t1 - t2) + (smax1 - smax2 - smin1 + smin2)*t2*sigma))/(smax1 - smax2 - smin1 + smin2)**2 - &
	 3*t1**2*sigma**2*(sintheta*(-t1 + t2) + (smax2*t1 - smin2*t1 - smax1*t2 + smin1*t2)*sigma) + &
	 3*t2**2*sigma**2*(sintheta*(-t1 + t2) + (smax2*t1 - smin2*t1 - smax1*t2 + smin1*t2)*sigma))/(6.*(t1 - t2)*sigma**4)

Q = c1*q1 + cs*qs + ct*qt
area = 0.5_KIND_PM*(smax1-smin1 + smax2-smin2)*(-t1 + t2)
Qavg = Q/area

!!--end--
END FUNCTION



!!### FUNCTION <<CHECK1_PsiS_Interior_Lin>>
FUNCTION CHECK1_PsiS_Interior_Lin(tol,testtol,NoisyUnit) RESULT(Pass)

!!#### PURPOSE
!! This analytic test makes sure that the choice of <SMALL_X>
!! is valid and checks the Standard and Near-Zero Expansion (NZE)
!! methods against analytic results.

!!#### REQUIRED OUTPUT
LOGICAL :: Pass

!!#### OPTIONAL INPUT
INTEGER      ,INTENT(IN),OPTIONAL :: NoisyUnit
REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol,testtol

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: t1,t2,Qavg,smin1,smin2,smax1,smax2
REAL(KIND_PM) :: sigma,q1,qs,qt,sintheta,Q,Qstd,Qnze,testtol_
INTEGER       :: NoisyUnit_,i
REAL(KIND_PM) :: Q_exact(0:19),sigmalist(0:19),x,SMALL_X_
LOGICAL       :: Pass_std,Pass_NZE

!!--begin--

NoisyUnit_ = Default( 0 , NoisyUnit )
testtol_   = Default( PM_DEFAULT_TESTTOL , testtol )

!test 0: generated by Mathematica for the expansion
smin1    = 0.0_KIND_PM
smin2    = 0.1_KIND_PM
smax1    = 1.0_KIND_PM
smax2    = 1.3_KIND_PM
t2       = 1._KIND_PM
t1       = 0._KIND_PM
sintheta = 0.9_KIND_PM
sigma    = 0.5_KIND_PM
qs = 0._KIND_PM
qt = 0._KIND_PM
q1 = 1._KIND_PM
!for varying sigma, mathematica result is
!-((81 \[ExponentialE]^(-4 \[Sigma]/3) -
!  81 \[ExponentialE]^(-10 \[Sigma]/9) +
!  2 (9 - 11 \[Sigma]) \[Sigma])/(20 \[Sigma]^3))
!Qmat = -( (81*E**(-4*sigma/3) - 81*E**(-10*sigma/9) + 2*(9 - 11*sigma)*sigma)/(20*sigma**3) )
!don't get too small or won't be accurate

!! Exact to the number of decimal places shown.
sigmalist(00) = 0.99_KIND_PM
Q_exact  (00) = 0.46721671981062352160502288516367568864052290665533_KIND_PM
sigmalist(01) = 0.5_KIND_PM
Q_exact  (01) = 0.55489617523684107153380824975227374358207880203838_KIND_PM
sigmalist(02) = 0.1_KIND_PM
Q_exact  (02) = 0.64729097426039837735868157921671552472902852489231_KIND_PM
sigmalist(03) = 0.01_KIND_PM
Q_exact  (03) = 0.67132124285454689708352798427065894386076962883596_KIND_PM
sigmalist(04) = 0.001_KIND_PM
Q_exact  (04) = 0.67379802743211597524897167361683598195170033986253_KIND_PM
sigmalist(05) = 0.0001_KIND_PM
Q_exact  (05) = 0.67404646175599178285197613824658993755886160931833_KIND_PM
sigmalist(06) = 0.00001_KIND_PM
Q_exact  (06) = 0.67407131276570825515694698766818034130588473157810_KIND_PM
sigmalist(07) = 0.000001_KIND_PM
Q_exact  (07) = 0.67407379794247189755556843056220919983215482544930_KIND_PM
sigmalist(08) = 0.0000001_KIND_PM
Q_exact  (08) = 0.67407404646090620045722635031966039718193573322381_KIND_PM
sigmalist(09) = 0.00000001_KIND_PM
Q_exact  (09) = 0.67407407131275721015272060083591597762331894752933_KIND_PM
sigmalist(10) = 0.000000001_KIND_PM
Q_exact  (10) = 0.67407407379794238691634202101235854968342000603672_KIND_PM
sigmalist(11) = 0.0000000001_KIND_PM
Q_exact  (11) = 0.67407407404646090535064490169179425155379474041548_KIND_PM
sigmalist(12) = 0.00000000001_KIND_PM
Q_exact  (12) = 0.67407407407131275720165459716506627984826161327986_KIND_PM
sigmalist(13) = 0.000000000001_KIND_PM
Q_exact  (13) = 0.67407407407379794238683136078646547780248200646921_KIND_PM
sigmalist(14) = 1.E-13_KIND_PM
Q_exact  (14) = 0.67407407407404646090534979508934613625969548612170_KIND_PM
sigmalist(15) = 1.E-15_KIND_PM
Q_exact  (15) = 0.67407407407407379794238683127580523090992226792594_KIND_PM
sigmalist(16) = 1.E-17_KIND_PM
Q_exact  (16) = 0.67407407407407407131275720164609054348605395518976_KIND_PM
sigmalist(16) = 1.E-20_KIND_PM
Q_exact  (16) = 0.67407407407407407407131275720164609053498793049840_KIND_PM
sigmalist(17) = 1.E-24_KIND_PM
Q_exact  (17) = 0.67407407407407407407407379794238683127572016460914_KIND_PM
sigmalist(18) = 1.E-40_KIND_PM
Q_exact  (18) = 0.67407407407407407407407407407407407407404646090535_KIND_PM
sigmalist(19) = 0._KIND_PM
Q_exact  (19) = 91._KIND_PM/135._KIND_PM



IF( NoisyUnit_/=0 )THEN
 WRITE(NoisyUnit_,"(4(A,1x))")"sigma","Qnze","Qstd","Q_exact"
END IF

DO i=0,19

 sigma = sigmalist(i)

 !use NZE
 Qnze = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
             smax1,smax2,sigma,q1,qs,qt,sintheta,tol=tol,ForceNZE=.TRUE.)

 !use standard
 Qstd = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
             smax1,smax2,sigma,q1,qs,qt,sintheta,tol=tol,ForceSTD=.TRUE.)

 !use best (choice of either based on SMALL_X)
 Q    = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
             smax1,smax2,sigma,q1,qs,qt,sintheta,tol=tol)

 Pass = ABS(Q-Q_exact(i))/Q_exact(i)<=testtol_
 Pass_std  = ABS(Qstd-Q_exact(i))/Q_exact(i)<=testtol_
 Pass_Nze  = ABS(Qnze-Q_exact(i))/Q_exact(i)<=testtol_
 IF( .NOT.Pass )EXIT

 !know this is case 2
 x=(smax2-smin2)*sigma/sintheta

 !if the two methods are in agreement, we can still use this x
 IF( Pass_Nze .AND. Pass_std )THEN
  SMALL_X_ = x
 END IF
 !once they are not in agreement, small_x will be fixed

 IF( NoisyUnit_/=0 )THEN
  WRITE(NoisyUnit_,"(4(Es10.5,1x))")sigma,Qnze,Qstd,Q_exact(i)
 END IF

END DO

!if the small x we found is greater than the one we already use
!then we have a problem
IF( SMALL_X_ > SMALL_X )THEN
 Pass = .FALSE.
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<COMPARE_PsiS_Interior_Lin>>
FUNCTION COMPARE_PsiS_Interior_Lin(t1,t2,smin1,smin2,smax1,smax2,&
   sigma,q1,qs,qt,sintheta) RESULT(Pass)

!!#### PURPOSE
!! Compare two results.

!!#### REQUIRED OUTPUT
LOGICAL :: Pass

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM),INTENT(IN) :: sigma,q1,qs,qt,sintheta

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: Q,Qavg
REAL(KIND_PM) :: Q_exact,Qavg_exact

!!--begin--

Q       = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)

Q_exact = Mathematica_PsiS_Interior_Lin(Qavg_exact,t1,t2,smin1,smin2,&
            smax1,smax2,sigma,q1,qs,qt,sintheta)

Pass    = IsApprox(Q,Q_exact) .AND. IsApprox(Qavg,Qavg_exact)

!!--end--
END FUNCTION



!!### SUBROUTINE <<MCOMPARE_PsiS_Interior_Lin>>
SUBROUTINE MCOMPARE_PsiS_Interior_Lin(Unit,t1,t2,smin1,smin2,smax1,smax2,&
   sigma_range,divsigma,q1,qs,qt,sintheta)

!!#### PURPOSE
!! Test the interior integration kernel for sources.

!!#### REQUIRED OUTPUT
LOGICAL :: Pass

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: Unit
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM),INTENT(IN) :: sigma_range(2),q1,qs,qt,sintheta
REAL(KIND_PM),INTENT(IN) :: divsigma

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: Qnze,Qavgnze
REAL(KIND_PM) :: Qstd,Qavgstd
REAL(KIND_PM) :: Qmat,Qavgmat
REAL(KIND_PM) :: sigma

!!--begin--

WRITE(Unit,"(4(A,1x))")"sigma","Q-nearzeroexpansion","Q-standard","Q-mathematica"

sigma = sigma_range(1)
DO

 Qnze    = PsiS_Interior_Lin(Qavgnze,t1,t2,smin1,smin2,&
             smax1,smax2,sigma,q1,qs,qt,sintheta,ForceNZE=.TRUE.)

 Qstd    = PsiS_Interior_Lin(Qavgstd,t1,t2,smin1,smin2,&
             smax1,smax2,sigma,q1,qs,qt,sintheta,ForceSTD=.TRUE.)

 Qmat = Mathematica_PsiS_Interior_Lin(Qavgmat,t1,t2,smin1,smin2,&
             smax1,smax2,sigma,q1,qs,qt,sintheta)

 WRITE(Unit,"(4(Es19.6,1x))")sigma,Qnze,Qstd,Qmat

 sigma = sigma*divsigma

 IF( sigma>sigma_range(2) )EXIT

END DO
!!--end--
END SUBROUTINE



!!### FUNCTION <<MONOCHECK_PsiS_Interior_Lin>>
FUNCTION MONOCHECK_PsiS_Interior_Lin(t1,t2,smin1,smin2,smax1,smax2,&
   q1,qs,qt,sintheta) RESULT(Pass)

!!#### PURPOSE
!! Check whether results are monotonic for "all" sigma.

!!#### REQUIRED OUTPUT
LOGICAL :: Pass

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM),INTENT(IN) :: q1,qs,qt,sintheta

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: sigma,sigmalast,Q,Qavg,Qlast
REAL(KIND_PM) :: Q_exact,Qavg_exact
REAL(KIND_PM) :: dQ_dsigma,dQ,dsigma

!!--begin--

!large range
sigma = 1.D+20
Q     = 0.d0
DO
 sigmalast = sigma
 sigma     = sigma/5.d0

 Qlast     = Q
 Q         = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
               smax1,smax2,sigma,q1,qs,qt,sintheta)

 dQ=(Q-Qlast)
 dsigma=(sigma-sigmalast)

 dQ_dsigma = dQ/dsigma
 WRITE(98,*)sigma,Q,dQ_dsigma

 Pass = (Q>=Qlast)

 IF( .NOT.Pass )RETURN
 IF( sigma<1.D-20 )EXIT

END DO

!small range
sigma = 1.D+1
Q     = 0.d0
DO
 sigmalast = sigma
 sigma     = sigma/1.1d0

 Qlast   = Q
 Q       = PsiS_Interior_Lin(Qavg,t1,t2,smin1,smin2,&
             smax1,smax2,sigma,q1,qs,qt,sintheta)

 dQ=(Q-Qlast)
 dsigma=(sigma-sigmalast)

 dQ_dsigma = dQ/dsigma

 Pass = (Q>=Qlast)

 IF( .NOT.Pass )RETURN
 IF( sigma<1.D-5 )EXIT

END DO


!!--end--
END FUNCTION



!!### FUNCTION <<TEST_PsiS_Out_Lin>>
FUNCTION TEST_PsiS_Out_Lin(tol,testtol,NoisyUnit,NCount) RESULT(Pass)

!!#### PURPOSE
!! Test the <PsiS_Out_Para> function with random slices.

!!#### REQUIRED OUTPUT
LOGICAL :: Pass

!!#### OPTIONAL INPUT
INTEGER      ,INTENT(IN),OPTIONAL :: NoisyUnit,NCount
REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol,testtol

!!#### LOCAL VARIABLES
INTEGER       :: NoisyUnit_
REAL(KIND_PM) :: reldiff,reldiff0,Qa,Qb
INTEGER       :: count,NCount_
REAL(KIND_PM) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM) :: sigma,q1,qs,qt,sintheta

!!--begin--

NoisyUnit_ = Default( 0 , NoisyUnit )
NCount_    = Default( 1000 , NCount )

!!--test random slices and data for symmetry and non-negativity
reldiff=0._KIND_PM
DO count=1,NCount_
 !code to generate a random slice geometry-wise
 !!---------------------------------------
 CALL RandomSlice(10._KIND_Rdp,t1,t2,smin1,smin2,smax1,smax2)
 !!---------------------------------------

 !code to generate a random data using the geometry
 !!---------------------------------------
 CALL RandomData_PosLin(1.E5_KIND_Rdp,1.E0_KIND_Rdp,t1,t2,smin1,smin2,&
   smax1,smax2,sigma,q1,qs,qt,sintheta)
 !!---------------------------------------


 !!---------------------------------------
 Pass = CHECK1_PsiS_Out_Lin(Qa,Qb,&
   t1,t2,smin1,smin2,smax1,smax2,sigma,q1,qs,qt,sintheta,reldiff=reldiff0,&
     tol=tol,testtol=testtol)
 reldiff=MAX(reldiff0,reldiff)

 IF( .NOT.Pass )THEN
  IF( NoisyUnit_/=0 )THEN
   WRITE(NoisyUnit_,*)"CHECK1_PsiS_Out_Para: FAILED"
  END IF
  EXIT
 END IF

 !!---------------------------------------

END DO

IF( NoisyUnit_/=0 )THEN
 WRITE(NoisyUnit_,*)"***** maximum rel. difference=",reldiff
END IF

!!--end--
END FUNCTION



!!### FUNCTION <<TEST_PsiE_Out_Para>>
FUNCTION TEST_PsiE_Out_Para(tol,testtol,NoisyUnit,NCount) RESULT(Pass)

!!#### PURPOSE
!! Test the <PsiE_Out_Para> function with random slices.

!!#### REQUIRED OUTPUT
LOGICAL :: Pass

!!#### OPTIONAL INPUT
INTEGER      ,INTENT(IN),OPTIONAL :: NoisyUnit,NCount
REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol,testtol

!!#### LOCAL VARIABLES
INTEGER       :: NoisyUnit_
REAL(KIND_PM) :: reldiff,reldiff0,Qa,Qb
INTEGER       :: count,NCount_
REAL(KIND_PM) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM) :: sigma,c,b,a,sintheta

!!--begin--

NoisyUnit_ = Default( 0 , NoisyUnit )
NCount_    = Default( 1000 , NCount )

!!--test random slices and data for symmetry and non-negativity
reldiff=0._KIND_PM
DO count=1,NCount_
 !code to generate a random slice geometry-wise
 !!---------------------------------------
 CALL RandomSlice(10._KIND_Rdp,t1,t2,smin1,smin2,smax1,smax2)
 !!---------------------------------------

 !code to generate a random data using the geometry
 !!---------------------------------------
 CALL RandomData_PosPara(1.E5_KIND_Rdp,1.E0_KIND_Rdp,t1,t2,smin1,smin2,&
   smax1,smax2,sigma,c,b,a,sintheta)
 !!---------------------------------------


 !!---------------------------------------
 Pass = CHECK1_PsiE_Out_Para(Qa,Qb,&
   t1,t2,smin1,smin2,smax1,smax2,sigma,c,b,a,sintheta,reldiff=reldiff0,&
     tol=tol,testtol=testtol)
 reldiff=MAX(reldiff0,reldiff)

 IF( .NOT.Pass )THEN
  IF( NoisyUnit_/=0 )THEN
   WRITE(NoisyUnit_,*)"CHECK1_PsiE_Out_Para: FAILED"
  END IF
  EXIT
 END IF

 !!---------------------------------------

END DO

IF( NoisyUnit_/=0 )THEN
 WRITE(NoisyUnit_,*)"***** maximum rel. difference=",reldiff
END IF

!!--end--
END FUNCTION



!!### FUNCTION <<CHECK1_PsiE_Interior_Para>>
FUNCTION CHECK1_PsiE_Interior_Para(Psia,Psib,&
  t1,t2,smin1,smin2,smax1,smax2,sigma,c,b,a,sintheta,&
  reldiff,tol,testtol) RESULT(Pass)

!!#### PURPOSE
!! Checks symmetry and positivity of the <PsiE_Interior_Para>.

!!#### REQUIRED OUTPUT
REAL(KIND_PM),INTENT(OUT) :: Psia,Psib
LOGICAL                   :: Pass

!!#### REQUIRED INPUT
REAL(KIND_PM),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2,sigma,c,b,a,sintheta

!!#### OPTIONAL INPUT
REAL(KIND_PM),OPTIONAL,INTENT(IN)  :: tol,testtol

!!#### OPTIONAL OUTPUT
REAL(KIND_PM),OPTIONAL,INTENT(OUT) :: reldiff

!!#### LOCAL VARIABLES
REAL(KIND_PM) :: c2,b2,a2,reldiff_,testtol_
LOGICAL       :: SymCheck,PosCheck
REAL(KIND_PM) :: Psiaavg,Psibavg

!!--begin--

testtol_  = DEFAULT(PM_DEFAULT_TESTTOL,testtol)

!original parabola
Psia=PsiE_Interior_Para(Psiaavg,t1,t2,smin1,smin2,smax1,smax2,sigma,&
  c,b,a,sintheta,tol=tol)

!reverse incoming parabola and side to test
c2 = c + b*t1 + a*t1**2 + b*t2 + 2*a*t1*t2 + a*t2**2
b2 = -b - 2*a*t1 - 2*a*t2
a2 = a
Psib=PsiE_Interior_Para(Psibavg,t1,t2,smin2,smin1,smax2,smax1,sigma,&
  c2,b2,a2,sintheta,tol=tol)

reldiff_ = ( ABS(Psia-Psib)/(TINY(1.d0)+(Psia+Psib)/2._KIND_PM) )

IF( PRESENT(reldiff) )THEN
 reldiff=reldiff_
END IF

SymCheck = reldiff_<=testtol_
PosCheck = (Psia>=0._KIND_PM) .AND. (Psib>=0._KIND_PM)

Pass = SymCheck .AND. PosCheck

IF( .NOT.Pass )THEN
 IF( .NOT.SymCheck )THEN
  WRITE(*,*)"FAIL: Symmetry violated!"
 END IF
 IF( .NOT.PosCheck )THEN
  WRITE(*,*)"FAIL: Positivity violated!"
 END IF
 WRITE(*,*)"sigma=",sigma
 WRITE(*,*)"sintheta=",sintheta
 WRITE(*,*)"c=",c
 WRITE(*,*)"b=",b
 WRITE(*,*)"a=",a
 WRITE(*,*)"t1=",t1
 WRITE(*,*)"t2=",t2
 WRITE(*,*)"smin1=",smin1
 WRITE(*,*)"smin2=",smin2
 WRITE(*,*)"smax1=",smax1
 WRITE(*,*)"smax2=",smax2
 WRITE(*,*)"Psia=",Psia
 WRITE(*,*)"Psib=",Psib
 WRITE(*,*)
END IF

!!--end--
END FUNCTION



!!### FUNCTION <<TEST_PsiE_Interior_Para>>
FUNCTION TEST_PsiE_Interior_Para(tol,testtol,NoisyUnit,NCount) RESULT(Pass)

!!#### PURPOSE
!! Test the <PsiE_Interior_Para> function with random slices.

!!#### REQUIRED OUTPUT
LOGICAL :: Pass

!!#### OPTIONAL INPUT
INTEGER      ,INTENT(IN),OPTIONAL :: NoisyUnit,NCount
REAL(KIND_PM),INTENT(IN),OPTIONAL :: tol,testtol

!!#### LOCAL VARIABLES
INTEGER       :: NoisyUnit_
REAL(KIND_PM) :: reldiff,reldiff0,Qa,Qb
INTEGER       :: count,NCount_
REAL(KIND_PM) :: t1,t2,smin1,smin2,smax1,smax2
REAL(KIND_PM) :: sigma,c,b,a,sintheta

!!--begin--

NoisyUnit_ = Default( 0 , NoisyUnit )
NCount_    = Default( 1000 , NCount )

!!--test random slices and data for symmetry and non-negativity
reldiff=0._KIND_PM
DO count=1,NCount_
 !code to generate a random slice geometry-wise
 !!---------------------------------------
 CALL RandomSlice(10._KIND_Rdp,t1,t2,smin1,smin2,smax1,smax2)
 !!---------------------------------------

 !code to generate a random data using the geometry
 !!---------------------------------------
 CALL RandomData_PosPara(1.E5_KIND_Rdp,1.E0_KIND_Rdp,t1,t2,smin1,smin2,&
   smax1,smax2,sigma,c,b,a,sintheta)
 !!---------------------------------------


 !!---------------------------------------
 Pass = CHECK1_PsiE_Interior_Para(Qa,Qb,&
   t1,t2,smin1,smin2,smax1,smax2,sigma,c,b,a,sintheta,reldiff=reldiff0,&
     tol=tol,testtol=testtol)
 reldiff=MAX(reldiff0,reldiff)

 IF( .NOT.Pass )THEN
  IF( NoisyUnit_/=0 )THEN
   WRITE(NoisyUnit_,*)"CHECK1_PsiE_Interior_Para: FAILED"
  END IF
  EXIT
 END IF

 !!---------------------------------------

END DO

IF( NoisyUnit_/=0 )THEN
 WRITE(NoisyUnit_,*)"***** maximum rel. difference=",reldiff
END IF

!!--end--
END FUNCTION


END MODULE
