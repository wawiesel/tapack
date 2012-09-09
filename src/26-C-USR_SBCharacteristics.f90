!!# USER MODULE: <<USR_SBCharacteristics>>
MODULE USR_SBCharacteristics

!!## PURPOSE
!! Provides user routines for the subcell balance characteristic
!! method of Karpov [ref] for solution of the linearized Boltzmann
!! transport equation.


!!## NOTES
!!

!!## VERSION 0.03

!!## CHANGES
!! * v0.03 (2011-09-01) - Problem with parabolic piecewise interpolator exploding
!!                        for thin sub-slices!  Current solution: do not allow tstar
!!                        to be closer to a side than 1.2*minimum edge thickness.
!! * v0.02 (2011-07-10) - Changed qmc_DiscontinuousFix to base its placement
!!                        of the outgoing verts on the incoming verts, so
!!                        now we need to pass InVerts as an argument.  Corrected
!!                        qmc_Ds to have the correct formula.
!! * v0.01 (2011-07-09) - Original version.




!!## EXTERNAL KINDS
USE KND_Characteristics !!((03-C-KND_Characteristics.f90))
USE FUN_Error           !!((04-A-FUN_Error.f90))
USE FUN_IsError         !!((05-A-FUN_IsError.f90))
USE USR_PolyExpoMoment  !!((11-C-USR_PolyExpoMoment.f90))
USE FUN_Default         !!((04-A-FUN_Default.f90))
USE FUN_STR             !!((05-B-FUN_STR.f90))
USE LIB_SubcellBalances !!((12-C-LIB_SubcellBalances.f90))
USE FUN_xyPERPCCW       !!((03-A-FUN_xyPERPCCW.f90))
USE FUN_xyPERPCW        !!((04-A-FUN_xyPERPCW.f90))
USE FUN_xyDIRECTION     !!((04-B-FUN_xyDIRECTION.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE

!!## PUBLIC ACCESS LISTING
!! * basic real kind
PUBLIC :: KIND_qmc
!! * initializer and printer
PUBLIC :: qmc_INIT
PUBLIC :: qmc_PRINT
!! * geometry bounds
PUBLIC :: qmc_t1                    !qmc_t1()
PUBLIC :: qmc_t2                    !qmc_t2()
PUBLIC :: qmc_Dt                    !qmc_Dt()
PUBLIC :: qmc_smin                  !qmc_smin(t)
PUBLIC :: qmc_smax                  !qmc_smax(t)
PUBLIC :: qmc_Ds                    !qmc_Ds(t)
PUBLIC :: qmc_AreaSli               !qmc_AreaSli
PUBLIC :: qmc_LenSliIn              !qmc_LenSliIn
PUBLIC :: qmc_LenSliOut             !qmc_LenSliOut
!! * functions of (s,t) within the slice (normal and simple interface)
PUBLIC :: qmc_PsiSB                 !qmc_PsiSB(s,t)
PUBLIC :: qmc_PsiSB0                !qmc_PsiSB0(s,t)
PUBLIC :: qmc_Q                     !qmc_Q(s,t)
!! * functions of (t) within the slice (incoming and outgoing)
PUBLIC :: qmc_PsiSB_in              !qmc_PsiSB_in(t)
PUBLIC :: qmc_PsiSB_out             !qmc_PsiSB_out(t)
!! * functions with no arguments
PUBLIC :: qmc_PsiSB_int      !integrated within slice
PUBLIC :: qmc_PsiSB_intin    !integrated incoming
PUBLIC :: qmc_PsiSB_intout   !integrated outgoing
PUBLIC :: qmc_PsiSB_out1     !outgoing left point
PUBLIC :: qmc_PsiSB_out2     !outgoing right point
!! * utility functions
PUBLIC :: qmc_PsiEdge_from_2Pt1Avg  !one parabola
PUBLIC :: qmc_PsiEdge2_from_2Pt1Avg !piecewise parabola
PUBLIC :: qmc_PsiInOut_from_PsiEdge

PUBLIC :: qmc_Q_from_QXY
PUBLIC :: qmc_CalcBalance

PUBLIC :: qmc_ThinEdgeCount
PUBLIC :: qmc_ThinEdgeCheck
PUBLIC :: qmc_DiscontinuousFix

PUBLIC :: PParaInterpolant_Karpov
PUBLIC :: CWENO_Parabola_2Pt1Avg

PUBLIC :: TEST_Karpov

PUBLIC :: PRINT_PPara
PUBLIC :: GNUPLOT_PPara

!!## LOCAL PARAMETERS
!! * keys for interpolation
CHARACTER(*),PARAMETER :: KEYS_EdgeInterpolator(6)=(/"Parabolic      ",&
                                                     "LinearEdges    ",&
                                                     "LinearAverage  ",&
                                                     "Flat           ",&
                                                     "CWENO          ",&
                                                     "ParabolicKarpov"/)
!! * indices corresponding to above edgeinterpolator keys
INTEGER,PARAMETER :: QMC_PARABOLIC=1
INTEGER,PARAMETER :: QMC_LINEAREDGES=2
INTEGER,PARAMETER :: QMC_LINEARAVERAGE=3
INTEGER,PARAMETER :: QMC_FLAT=4
INTEGER,PARAMETER :: QMC_CWENO=5
INTEGER,PARAMETER :: QMC_PARABOLICKARPOV=6


!!## LOCAL VARIABLES
!! * variables
REAL(KIND_qmc) :: a,b,c
REAL(KIND_qmc) :: q1,qs,qt
REAL(KIND_qmc) :: t1,smin1,smax1
REAL(KIND_qmc) :: t2,smin2,smax2
REAL(KIND_qmc) :: LenSliIn,LenSliOut
REAL(KIND_qmc) :: AreaSli
INTEGER,SAVE   :: slice_id
REAL(KIND_qmc) :: sigma,sintheta
INTEGER        :: COUNT_ThinEdges = 0

!! * options
REAL(KIND_qmc) :: MIN_EDGE_THICKNESS = 1.2e-8_KIND_qmc
REAL(KIND_qmc) :: QMC_DEFAULT_TOL     = 10._KIND_qmc*EPSILON(1._KIND_qmc)
REAL(KIND_qmc) :: CWENO_cwt           = 1._KIND_qmc
REAL(KIND_qmc) :: CWENO_eps           = 1.E-4_KIND_qmc
INTEGER        :: CWENO_order         = 2
INTEGER        :: EdgeInterpolator    = QMC_PARABOLIC

!(make options public)
PUBLIC :: MIN_EDGE_THICKNESS
PUBLIC :: EdgeInterpolator,KEYS_EdgeInterpolator
PUBLIC :: CWENO_Cwt,CWENO_eps,CWENO_order
PUBLIC :: QMC_PARABOLICKARPOV

!!## PROCEDURES
CONTAINS


!!### FUNCTION <<qmc_ThinEdgeCount>>
FUNCTION qmc_ThinEdgeCount(init)
INTEGER :: qmc_ThinEdgeCount
LOGICAL,INTENT(IN),OPTIONAL :: init
!!--begin--
IF( PRESENT(init) )THEN
 IF( init )THEN
  COUNT_ThinEdges = 0
 END IF
END IF
qmc_ThinEdgeCount = COUNT_ThinEdges
!!--end--
END FUNCTION


!!### FUNCTION <<qmc_PsiSB0>>
FUNCTION qmc_PsiSB0( s , t )

!!#### PURPOSE
!! Provide a basic interface to the <qmc_PsiSB>
!! function.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: s,t

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_PsiSB0

!!--begin--
qmc_PsiSB0 = qmc_PsiSB(slice_id,s,t)
!!--end--
END FUNCTION



!!### FUNCTION <<qmc_Q>>
FUNCTION qmc_Q( id , s , t )

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: id
REAL(KIND_qmc),INTENT(IN) :: s,t

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_Q

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

qmc_Q = q1 + qs*s + qt*t

!!--end--
END FUNCTION



!!### FUNCTION <<qmc_PsiSB>>
FUNCTION qmc_PsiSB( id , s , t )

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: id
REAL(KIND_qmc),INTENT(IN) :: s,t

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_PsiSB

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: ds,smin0,b0,coeff
REAL(KIND_qmc) :: x,x0,PM(0:1),PsiE,PsiS

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

ds    = MAX(smax2-smin2,smax1-smin1)
smin0 = qmc_smin(id,t)
b0    = (s-smin0)/ds

IF( sintheta==0._KIND_qmc )THEN
 x     = SQRT(HUGE(1._KIND_qmc))
 coeff = x
 x0    = ds*sigma*b0
ELSE
 x     = ds*sigma*b0/sintheta
 coeff = 1._KIND_qmc/sintheta
 x0    = ds*sigma*b0
END IF

!get the polymoment
CALL PolyExpoMoment( MaxN=1 , x=x , x0=x0 , c=coeff , PM=PM(0:1) , &
  tol=QMC_DEFAULT_TOL )

!get the easy E-component
PsiE = qmc_PsiSB_in(id,t)*EXP(-sigma*(s-smin0)/sintheta)

!get the constant, s and t parts of the S-component
PsiS = ds*b0*( (q1 + qs*b0*ds + qs*smin0 + qt*t)*PM(0) + &
               (   - qs*b0*ds                  )*PM(1) )

qmc_PsiSB = PsiS + PsiE

!!--end--
END FUNCTION


!!### FUNCTION <<qmc_PsiSB_in>>
FUNCTION qmc_PsiSB_in(id,t)

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: id
REAL(KIND_qmc),INTENT(IN) :: t

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_PsiSB_in

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

qmc_PsiSB_in = a*(t**2) + b*t + c

!!--end--
END FUNCTION



!!### FUNCTION <<qmc_PsiSB_out>>
FUNCTION qmc_PsiSB_out(id,t)

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: id
REAL(KIND_qmc),INTENT(IN) :: t

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_PsiSB_out

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

!![waw] this could be inlined later (instead of just
!! calling the <PsiSB(s,t)> function.)
qmc_PsiSB_out = qmc_PsiSB( id,qmc_smax(id,t) , t )

!!--end--
END FUNCTION



FUNCTION qmc_LenSliOut(id)
INTEGER,INTENT(IN) :: id
REAL(KIND_qmc) :: qmc_LenSliOut
INCLUDE "26-C-USR_SBCharacteristics.f90.sup"
qmc_LenSliOut = LenSliOut
END FUNCTION

FUNCTION qmc_LenSliIn(id)
INTEGER,INTENT(IN) :: id
REAL(KIND_qmc) :: qmc_LenSliIn
INCLUDE "26-C-USR_SBCharacteristics.f90.sup"
qmc_LenSliIn = LenSliIn
END FUNCTION

FUNCTION qmc_AreaSli(id)
INTEGER,INTENT(IN) :: id
REAL(KIND_qmc) :: qmc_AreaSli
INCLUDE "26-C-USR_SBCharacteristics.f90.sup"
qmc_AreaSli = AreaSli
END FUNCTIOn


!!### SUBROUTINE <<qmc_PRINT>>
SUBROUTINE qmc_PRINT(id,Unit)
USE VAR_Units           !!((03-A-VAR_Units.f90))
USE FUN_Default         !!((04-A-FUN_Default.f90))

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: id

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: Unit

!!#### LOCAL VARIABLES
INTEGER :: Unit_
REAL(KIND_qmc) :: tm

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

Unit_ = Default( DEFAULT_OUTPUT_UNIT , Unit )
WRITE(Unit_,*)"--begin----------qmc_PRINT------------------------"
WRITE(Unit_,*)"Slice ID = ",slice_id
WRITE(Unit_,*)" "
WRITE(Unit_,*)"slice incoming flux expansion psiin(t) = a t**2 + b t + c"
WRITE(Unit_,*)"  a = ",a
WRITE(Unit_,*)"  b = ",b
WRITE(Unit_,*)"  c = ",c
WRITE(Unit_,*)" "
WRITE(Unit_,*)"Source function q(s,t) = q1 + qs s + qt t"
WRITE(Unit_,*)"  q1 = ",q1
WRITE(Unit_,*)"  qs = ",qs
WRITE(Unit_,*)"  qt = ",qt
WRITE(Unit_,*)" "
WRITE(Unit_,*)"Geometry Info"
WRITE(Unit_,*)"    t1,t2    = ",t1,t2
WRITE(Unit_,*)" smin1,smin2 = ",smin1,smin2
WRITE(Unit_,*)" smax1,smax2 = ",smax1,smax2
WRITE(Unit_,*)"  LenSliIn   = ",LenSliIn
WRITE(Unit_,*)"  LenSliOut  = ",LenSliOut
WRITE(Unit_,*)"   AreaSli   = ",AreaSli
WRITE(Unit_,*)" "
tm = (t1+t2)*0.5_KIND_qmc
WRITE(Unit_,*)"Consistency Checks"
WRITE(Unit_,*)" PsiIn diff t1",qmc_PsiSB_in(id,t1)-qmc_PsiSB(id,qmc_smin(id,t1),t1)
WRITE(Unit_,*)" PsiIn diff tm",qmc_PsiSB_in(id,tm)-qmc_PsiSB(id,qmc_smin(id,tm),tm)
WRITE(Unit_,*)" PsiIn diff t2",qmc_PsiSB_in(id,t2)-qmc_PsiSB(id,qmc_smin(id,t2),t2)
WRITE(Unit_,*)
WRITE(Unit_,*)"--end------------qmc_PRINT------------------------"

!!--end--
END SUBROUTINE



!!### FUNCTION <<qmc_CalcBalance>>
FUNCTION qmc_CalcBalance(id,tol,relerr)

!!#### PURPOSE
!! Return the integrated angular flux inside the slice.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: id

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_CalcBalance

!!#### OPTIONAL INPUT
REAL(KIND_qmc),OPTIONAL,INTENT(IN)  :: tol
REAL(KIND_qmc),OPTIONAL,INTENT(OUT) :: relerr

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: PsiE_out,PsiS_out,Psi_in,PsiE_int,PsiS_int
REAL(KIND_qmc) :: PsiEavg_out,PsiSavg_out,Psiavg_in,PsiEavg_int,PsiSavg_int
REAL(KIND_qmc) :: Psi_out,Psi_int,nout(2),nin(2)
REAL(KIND_qmc) :: Q_int,Qavg_int,Bal1,Bal2,PsiAvg_int,Psiavg_out
REAL(KIND_qmc) :: maxavg
!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

PsiE_int = PsiE_Interior_Para(PsiEavg_int,t1,t2,smin1,smin2,smax1,smax2,sigma,c ,b ,a ,sintheta,tol)
PsiS_int = PsiS_Interior_Lin (PsiSavg_int,t1,t2,smin1,smin2,smax1,smax2,sigma,q1,qs,qt,sintheta,tol)
Psi_int  = PsiE_int + PsiS_int
Psiavg_int  = PsiEavg_int + PsiSavg_int

PsiE_out = PsiE_Out_Para(PsiEavg_out,t1,t2,smin1,smin2,smax1,smax2,sigma,c ,b ,a ,sintheta,tol)
PsiS_out = PsiS_Out_Lin (PsiSavg_out,t1,t2,smin1,smin2,smax1,smax2,sigma,q1,qs,qt,sintheta,tol)
Psi_out  = PsiE_out + PsiS_out
Psiavg_out  = PsiEavg_out + PsiSavg_out

Psi_in    = qmc_PsiSB_IntIn(id,Psiavg_in)
Q_Int     = Q_Interior_Lin(Qavg_int,t1,t2,smin1,smin2,smax1,smax2,q1,qs,qt,tol)

Bal1 = Psi_out*sintheta  - Psi_in*sintheta + sigma*Psi_int - Q_Int
!get outedge and inedge normals
!nout = xyPERPCCW_U( xyDIRECTION_PP( (/t1,smax1/) , (/t2,smax2/) ) )
!nin  = xyPERPCW_U ( xyDIRECTION_PP( (/t1,smin1/) , (/t2,smin2/) ) )

!Bal2 = Psiavg_out*LenSliOut*nout(2)*sintheta + &
!       Psiavg_in *LenSliIn *nin (2)*sintheta + &
!      sigma*Psiavg_int*AreaSli - Qavg_int*AreaSli

qmc_CalcBalance=Bal1

IF( PRESENT(relerr) )THEN
 maxavg = MAXVAL(ABS( (/Psiavg_out,Psiavg_in,Qavg_int,Psiavg_int/)) )
 maxavg = MERGE(TINY(0._KIND_qmc),maxavg,maxavg==0._KIND_qmc)
 relerr = Bal1/maxavg
END IF

!!--end--
END FUNCTION



!!### FUNCTION <<qmc_PsiSB_int>>
FUNCTION qmc_PsiSB_int(id,Avg,tol)

!!#### PURPOSE
!! Return the integrated angular flux inside the slice.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: id

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_PsiSB_int
REAL(KIND_qmc),INTENT(OUT) :: Avg

!!#### OPTIONAL INPUT
REAL(KIND_qmc),OPTIONAL,INTENT(IN) :: tol

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: PsiE,PsiS,PsiEavg,PsiSavg
!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

!get E- and S- component
PsiE = PsiE_Interior_Para(PsiEavg,t1,t2,smin1,smin2,smax1,smax2,sigma,c ,b ,a ,sintheta,tol)
PsiS = PsiS_Interior_Lin (PsiSavg,t1,t2,smin1,smin2,smax1,smax2,sigma,q1,qs,qt,sintheta,tol)

!combine
qmc_PsiSB_int = PsiE + PsiS
Avg           = PsiEavg+PsiSavg

!!--end--
END FUNCTION


!!### FUNCTION <<qmc_PsiSB_intout>>
FUNCTION qmc_PsiSB_intout(id,Avg,tol)

!!#### PURPOSE
!! Return the integrated angular flux out of the slice.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: id

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_PsiSB_intout
REAL(KIND_qmc),INTENT(OUT) :: Avg

!!#### OPTIONAL INPUT
REAL(KIND_qmc),INTENT(IN),OPTIONAL :: tol

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: PsiS,PsiE,PsiEavg,PsiSavg

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

!get E-component and S-component
PsiE = PsiE_Out_Para(PsiEavg,t1,t2,smin1,smin2,smax1,smax2,sigma,c ,b ,a ,sintheta,tol)
PsiS = PsiS_Out_Lin (PsiSavg,t1,t2,smin1,smin2,smax1,smax2,sigma,q1,qs,qt,sintheta,tol)

!combine
qmc_PsiSB_intout = PsiE + PsiS
Avg              = PsiEavg+PsiSavg

!!--end--
END FUNCTION



!!### FUNCTION <<qmc_PsiSB_intin>>
FUNCTION qmc_PsiSB_intin(id,Avg)

!!#### PURPOSE
!! Return the integrated angular flux into the slice.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: id

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_PsiSB_intin
REAL(KIND_qmc) :: Avg

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

qmc_PsiSB_intin = &
  a*(t2**3-t1**3)/3._KIND_qmc + &
  b*(t2**2-t1**2)/2._KIND_qmc + &
  c*(t2   -t1   )

Avg = qmc_PsiSB_intin/(t2-t1)

!!--end--
END FUNCTION



!!### FUNCTION <<qmc_PsiSB_out1>>
FUNCTION qmc_PsiSB_out1(id)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: id

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_PsiSB_out1

!!#### PURPOSE
!! Return the pointwise angular flux value out of the slice
!! at the left-most t-value, $t_1$.

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

qmc_PsiSB_out1 = qmc_PsiSB_out( id,t1 )

!!--end--
END FUNCTION



!!### FUNCTION <<qmc_PsiSB_out2>>
FUNCTION qmc_PsiSB_out2(id)

!!#### PURPOSE
!! Return the pointwise angular flux value out of the slice
!! at the right-most t-value, $t_2$.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: id

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_PsiSB_out2

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

qmc_PsiSB_out2 = qmc_PsiSB_out( id,t2 )

!!--end--
END FUNCTION



!!### SUBROUTINE <<qmc_INIT>>
SUBROUTINE qmc_INIT( id , Sli , psiin , q  , MacT , RecipSin )

!!#### PURPOSE
!! Initialize the variables needed for this subcell balance method
!! which operates on slices.

!!#### DETAILS
!! The incoming angular flux <psiin>, must already be converted to
!! $(s,t)$ coordinates---i.e. a function of $t$ only.  The
!! source must also be a function of $(s,t)$ coordinates.
!!
!! The <id> must be unique for each slice.  It is how we decide to use
!! cached data or to actually recalculate some value.

!!#### LOCAL MODULES
USE FUN_xyCENTROID      !!((05-B-FUN_xyCENTROID.f90))
USE FUN_xySAREA         !!((03-A-FUN_xySAREA.f90))

!!#### REQUIRED INPUT
!! * the id number for this slice <id>
!! * the slice <Sli>
!! * the incoming quadratic function <psiin>
!! * the total source within the slice <q>
!! * the macroscopic total cross section <MacT>
!! * the reciprocal of the $\sin(\theta)$
INTEGER       ,INTENT(IN) :: id
REAL(KIND_qmc),INTENT(IN) :: Sli(3,2)
REAL(KIND_qmc),INTENT(IN) :: psiin(3)
REAL(KIND_qmc),INTENT(IN) :: q(3)
REAL(KIND_qmc),INTENT(IN) :: MacT
REAL(KIND_qmc),INTENT(IN) :: RecipSin

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: SliQg(2,4),CSli(2),ASli

!!--begin--

!! Get the slice geometry.
t1    = Sli(1,1)
smin1 = Sli(2,1)
smax1 = Sli(3,1)
t2    = Sli(1,2)
smin2 = Sli(2,2)
smax2 = Sli(3,2)

!! length of incoming slice edge
LenSliIn  = SQRT( (t2-t1)**2 + (smin2-smin1)**2 )

!! length of outgoing slice edge
LenSliOut = SQRT( (t2-t1)**2 + (smax2-smax1)**2 )

!area of slice
AreaSli = 0.5_KIND_qmc*(smax1-smin1+smax2-smin2)*(t2-t1)

!! Get incoming angular flux variables <a,b,c>.
!! $$ \psi(t) = a t**2 + b t + c $$
!! Note the ordering of psiin (starts with constant term).
a     = psiin(3)
b     = psiin(2)
c     = psiin(1)

!! Get source variables <q1,qs,qt>.
!! $$ q(s,t) = q1 + q_s s + q_t t $$
q1    = q(1)
qs    = q(2)
qt    = q(3)

!translate to slice center
SliQg(:,1) = (/t1,smin1/)
SliQg(:,2) = (/t2,smin2/)
SliQg(:,3) = (/t2,smax2/)
SliQg(:,4) = (/t1,smax1/)
ASli = xySAREA_Pg( 4 , SliQg )
CSli = xyCENTROID_Pg( 4 , SliQg , AREA=ASli)
!q1    = q(1)+qs*CSli(2)+qt*CSli(1)

!set id
slice_id = id

!total cross section
sigma=mact

!sin(theta)
sintheta=1._KIND_qmc/RecipSin

!!--end--
END SUBROUTINE


FUNCTION qmc_Ds( id , t )

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: id
REAL(KIND_qmc),INTENT(IN) :: t

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_Ds

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

qmc_Ds = ((smax2-smin2)-(smax1-smin1))*((t-t1)/(t2-t1)) + smax1-smin1

!!--end--
END FUNCTION


FUNCTION qmc_Dt(id)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: id

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_Dt

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

qmc_Dt = t2-t1

!!--end--
END FUNCTION



!!### FUNCTION <<qmc_smax>>
FUNCTION qmc_smax(id,t)

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: id
REAL(KIND_qmc),INTENT(IN) :: t

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_smax

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

IF( IsThinEdge(id,LenSliOut,(/t1,t2/)) )THEN
 qmc_smax = (smax1+smax2)/2._KIND_qmc
ELSE
 qmc_smax = smax1 + (t-t1)*(smax2-smax1)/(t2-t1)
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<qmc_smin>>
FUNCTION qmc_smin(id,t)

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: id
REAL(KIND_qmc),INTENT(IN) :: t

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_smin

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

IF( IsThinEdge(id,LenSliIn,(/t1,t2/)) )THEN
 qmc_smin = (smin1+smin2)/2._KIND_qmc
ELSE
 qmc_smin = smin1 + (t-t1)*(smin2-smin1)/(t2-t1)
END IF

!!--end--
END FUNCTION


!!### FUNCTION <<qmc_t1>>
FUNCTION qmc_t1(id)

!!#### PURPOSE
!! Return left side <t1>.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: id

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_t1

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

qmc_t1 = t1

!!--end--
END FUNCTION



!!### FUNCTION <<qmc_t2>>
FUNCTION qmc_t2(id)

!!#### PURPOSE
!! Return right side <t2>.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: id

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: qmc_t2

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

qmc_t2 = t2

!!--end--
END FUNCTION



!!### FUNCTION <<IsThinEdge>>
FUNCTION IsThinEdge(id,LenEdge,T)

!!#### PURPOSE
!! Check if the edge is thin.

!!#### REQUIRED INPUT
INTEGER       ,INTENT(IN) :: id
REAL(KIND_qmc),INTENT(IN) :: LenEdge,T(2)

!!#### REQUIRED OUTPUT
LOGICAL :: IsThinEdge

!!--begin--

INCLUDE "26-C-USR_SBCharacteristics.f90.sup"

IsThinEdge = ((T(2)-T(1)) <= MIN_EDGE_THICKNESS*LenEdge)

IF( IsThinEdge )THEN
 COUNT_ThinEdges = COUNT_ThinEdges + 1
END IF

!!--end--
END FUNCTION


!!### SUBROUTINE <<qmc_DiscontinuousFix>>
SUBROUTINE qmc_DiscontinuousFix( Sli , OutVert )
REAL(KIND_qmc),INTENT(IN) :: Sli(:,:)
INTEGER       ,INTENT(INOUT) :: OutVert(:)
REAL(KIND_qmc) :: DT,t1,t2
INTEGER :: n
!!--begin--
DT=Sli(1,SIZE(Sli,2))-Sli(1,1)

DO n=1,SIZE(OutVert)-1
 t1=Sli(1,n)
 t2=Sli(1,n+1)
 !vertex ''sharing'' for thin edges
 IF( qmc_ThinEdgeCheck(DT,(/t1,t2/)) )THEN
  IF( OutVert(n)==0 )THEN
   OutVert(n) = OutVert(n+1)
  ELSE IF( OutVert(n+1)==0 )THEN
   OutVert(n+1) = OutVert(n)
  END IF
 END IF
END DO
!!--end--
END SUBROUTINE


!!### FUNCTION <<qmc_ThinEdgeCheck>>
FUNCTION qmc_ThinEdgeCheck(LenEdge,T) RESULT(IsThinEdge)

!!#### PURPOSE
!! Check if the edge is thin, without updating
!! the thin edge count or requiring a passed <id>.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: LenEdge,T(2)

!!#### REQUIRED OUTPUT
LOGICAL :: IsThinEdge

!!--begin--

IsThinEdge = ((T(2)-T(1)) <= MIN_EDGE_THICKNESS*LenEdge)

!!--end--
END FUNCTION


!!### FUNCTION <<CWENO_Parabola_2Pt1Avg>>
FUNCTION CWENO_Parabola_2Pt1Avg(y1,y2,yavg,L,cwt,order,eps) RESULT(Para)

!!#### PURPOSE
!! Reconstruct a parabolic function according to
!! CWENO methodology.

!!#### MODULES
USE FUN_Default         !!((04-A-FUN_Default.f90))

!!#### REQUIRED INPUT
REAL(KIND_Rdp),INTENT(IN) :: y1,y2,yavg,L

!!#### OPTIONAL INPUT
REAL(KIND_Rdp),INTENT(IN),OPTIONAL :: cwt
INTEGER       ,INTENT(IN),OPTIONAL :: order
REAL(KIND_Rdp),INTENT(IN),OPTIONAL :: eps

!!#### REQUIRED OUTPUT
REAL(KIND_Rdp) :: Para(3)

!!#### LOCAL VARIABLES
REAL(KIND_Rdp) :: eps_,cwt_
INTEGER        :: p
REAL(KIND_Rdp) :: Ccenter,Cleft,Cright
REAL(KIND_Rdp) :: ISleft,ISright,IScenter
REAL(KIND_Rdp) :: acenter,aleft,aright,atot
REAL(KIND_Rdp) :: wcenter,wleft,wright
REAL(KIND_Rdp) :: fcenter(3),fright(3),fleft(3)

!!--begin--

eps_ = Default(1.E-4_KIND_qmc,eps)
p    = Default(2,order)
cwt_ = Default(0.5_KIND_qmc,cwt)

!must be between 0 and 1
CCenter = MIN(MAX(cwt_,0._KIND_qmc),1._KIND_qmc)

!the left side and right side weights are created as equal
!parts of (1-Ccenter).
Cleft  = (1._KIND_qmc-Ccenter)/2._KIND_qmc
Cright = Cleft

!the smoothness indicators follow from [ref]
ISleft   = 4._KIND_qmc*((y1 - yavg)**2)
ISright  = 4._KIND_qmc*((y2 - yavg)**2)
IScenter = ((3._KIND_qmc + Ccenter**2)*(y1**2) + &
            (3._KIND_qmc + Ccenter**2)*(y2**2) - &
             12._KIND_qmc*y2*yavg + &
             12._KIND_qmc*(yavg**2) - &
              2._KIND_qmc*y1*( &
                (-3._KIND_qmc + Ccenter**2)*y2 + &
                  6._KIND_qmc*yavg) )/(Ccenter**2)

!**in the above code, the substitution has already been made that
!Cleft=Cright=(1-Ccenter)/2 as it leads to much shorter code

!the unnormalized weights are
acenter = Ccenter/((eps_ + IScenter )**p)
aleft   = Cleft  /((eps_ +   ISleft )**p)
aright  = Cright /((eps_ +  ISright )**p)
atot    = (acenter+aleft+aright)

!the normalized weights are
wcenter = acenter/atot
wleft   = aleft/atot
wright  = aright/atot

!the parabolic polynomial functions are (1,x,x**2)
fleft   = (/ y1                         ,&
            (2._KIND_qmc/L)*(yavg - y1) ,&
             0._KIND_qmc                /)
fright  = (/ y2-2._KIND_qmc*(y2-yavg)   ,&
            (2._KIND_qmc/L)*(y2-yavg)   ,&
             0._KIND_qmc      /)
fcenter = (/ (L**2)*( (1 + Ccenter)*y1 - (-1._KIND_qmc + Ccenter)*(y2 - 2*yavg) ),&
            -2*L*((3 + Ccenter)*y1 + 3*y2 - Ccenter*y2 - 6*yavg)     ,&
             6*(y1 + y2 - 2*yavg)                                    /)
fcenter = fcenter/(2._KIND_qmc*Ccenter*(L**2))

!non-inlined version
!fopt = (/ y1                         , &
!         (-2*y2 - 4*y1 + 6*yavg)/(L) , &
!        ( 3*y1 + 3*y2 - 6*yavg)/(L**2) /)
!fcenter = (fopt-Cleft*fleft-Cright*fright)/Ccenter

Para = wleft*fleft + wright*fright + wcenter*fcenter

!!--end--
END FUNCTION



FUNCTION TEST_Karpov(Noisy) RESULT(Pass)
USE FUN_IsApprox        !!((03-A-FUN_IsApprox.f90))
USE FUN_Default         !!((04-A-FUN_Default.f90))
LOGICAL,INTENT(IN),OPTIONAL :: Noisy
REAL(KIND_Rdp) :: y1,y2,yavg,L,xbar,PPara(3,2),PPara_exact(3,2)
REAL(KIND_Rdp) :: xbar_exact
LOGICAL :: Pass
LOGICAL :: Noisy_

!!--begin--

Noisy_=Default(.TRUE.,Noisy)

!trial
y1    = 0.0_KIND_qmc
y2    = 1.0_KIND_qmc
yavg  = 0.999_KIND_qmc
L     = 1.0_KIND_qmc
PPara = PParaInterpolant_Karpov(y1,y2,yavg,L,xbar)
IF( Noisy_ )THEN
 CALL PRINT_PPara(xbar,PPara,L)
END IF

!test 0
y1    = 3.0_KIND_qmc
y2    = 4.0_KIND_qmc
yavg  = 3.02_KIND_qmc
L     = 3.0_KIND_qmc
PPara = PParaInterpolant_Karpov(y1,y2,yavg,L,xbar)
IF( Noisy_ )THEN
 CALL PRINT_PPara(xbar,PPara,L)
END IF
PPara_exact(:,2) = (/248.4444444444444_KIND_qmc,&
                     -174.0740740740741_KIND_qmc,&
                     30.86419753086420_KIND_qmc/)
PPara_exact(:,1) = (/3.0_KIND_qmc,0._KIND_qmc,0._KIND_qmc/)
xbar_exact=2.82_KIND_qmc
Pass = IsApprox((/xbar,PPara/),(/xbar_exact,PPara_exact/))
IF( .NOT.Pass )GOTO 666


!test 1
y1    = 0.0_KIND_qmc
y2    = 1.0_KIND_qmc
yavg  = 0.5_KIND_qmc
L     = 2.0_KIND_qmc
PPara = PParaInterpolant_Karpov(y1,y2,yavg,L,xbar)
IF( Noisy_ )THEN
 CALL PRINT_PPara(xbar,PPara,L)
END IF
PPara_exact(:,1) = (/0.0_KIND_qmc,0.5_KIND_qmc,0.0_KIND_qmc/)
xbar_exact = ERROR(xbar)
PPara_exact(:,2) = ERROR(xbar)
Pass = IsApprox((/xbar,PPara/),(/xbar_exact,PPara_exact/))
IF( .NOT.Pass )GOTO 666

!test 2
y1    = 1.0_KIND_qmc
y2    = 0.0_KIND_qmc
yavg  = 0.5_KIND_qmc
L     = 2.0_KIND_qmc
PPara = PParaInterpolant_Karpov(y1,y2,yavg,L,xbar)
IF( Noisy_ )THEN
 CALL PRINT_PPara(xbar,PPara,L)
END IF
PPara_exact(:,1) = (/1.0_KIND_qmc,-0.5_KIND_qmc,0.0_KIND_qmc/)
PPara_exact(:,2) = ERROR(xbar)
xbar_exact       = ERROR(xbar)
Pass = IsApprox((/xbar,PPara/),(/xbar_exact,PPara_exact/))
IF( .NOT.Pass )GOTO 666

!test 3
y1    = 0.0_KIND_qmc
y2    = 1.0_KIND_qmc
yavg  = 0.5_KIND_qmc
L     = 10.0_KIND_qmc
PPara = PParaInterpolant_Karpov(y1,y2,yavg,L,xbar)
IF( Noisy_ )THEN
 CALL PRINT_PPara(xbar,PPara,L)
END IF
PPara_exact(:,1) = (/0.0_KIND_qmc,0.1_KIND_qmc,0.0_KIND_qmc/)
xbar_exact=ERROR(xbar)
PPara_exact(:,2) = ERROR(xbar)
Pass = IsApprox((/xbar,PPara/),(/xbar_exact,PPara_exact/))
IF( .NOT.Pass )GOTO 666


!test 4
y1    = 3.0_KIND_qmc
y2    = 4.0_KIND_qmc
yavg  = 3.98_KIND_qmc
L     = 3.0_KIND_qmc
PPara = PParaInterpolant_Karpov(y1,y2,yavg,L,xbar)
IF( Noisy_ )THEN
 CALL PRINT_PPara(xbar,PPara,L)
END IF
PPara_exact(:,1) = (/3.000000000000000_KIND_qmc,&
                     11.11111111111111_KIND_qmc,&
                     -30.86419753086420_KIND_qmc/)
PPara_exact(:,2) = (/4.0_KIND_qmc,0._KIND_qmc,0._KIND_qmc/)
xbar_exact=0.18_KIND_qmc
Pass = IsApprox((/xbar,PPara/),(/xbar_exact,PPara_exact/))
IF( .NOT.Pass )GOTO 666


!test 5
y1    = 4.0_KIND_qmc
y2    = 3.0_KIND_qmc
yavg  = 3.98_KIND_qmc
L     = 3.0_KIND_qmc
PPara = PParaInterpolant_Karpov(y1,y2,yavg,L,xbar)
IF( Noisy_ )THEN
 CALL PRINT_PPara(xbar,PPara,L)
END IF
PPara_exact(:,1) = (/4.0_KIND_qmc,0._KIND_qmc,0._KIND_qmc/)
PPara_exact(:,2) = (/-241.4444444444444_KIND_qmc,&
                      174.0740740740741_KIND_qmc,&
                     -30.86419753086420_KIND_qmc/)
xbar_exact=2.82_KIND_qmc
Pass = IsApprox((/xbar,PPara/),(/xbar_exact,PPara_exact/))
IF( .NOT.Pass )GOTO 666


!test6
L=8.5984014931059094233079566_KIND_qmc
y1=0.85428740709975122300311276_KIND_qmc
y2=8.3652615099894423023770088_KIND_qmc
yavg=2.6025922877436917132595828_KIND_qmc
PPara = PParaInterpolant_Karpov(y1,y2,yavg,L,xbar)
IF( Noisy_ )THEN
 CALL PRINT_PPara(xbar,PPara,L)
END IF
PPara_exact(:,1) = (/0.85428740709975122300311276_KIND_qmc,0._KIND_qmc,0._KIND_qmc/)
PPara_exact(:,2) = (/2.256332617604223130637754_KIND_qmc,&
                     -1.080934197326799193858458_KIND_qmc,&
                     0.2083418441496121988928199_KIND_qmc/)
xbar_exact=2.594136098148796220973967_KIND_qmc
Pass = IsApprox((/xbar,PPara/),(/xbar_exact,PPara_exact/))
IF( .NOT.Pass )GOTO 666


666 CONTINUE

!!--end--
END FUNCTION


!!### SUBROUTINE <<GNUPLOT_PPara>>
SUBROUTINE GNUPLOT_PPara( xbar,PPara,L,fname)
REAL(KIND_Rdp),INTENT(IN) :: xbar,PPara(3,2),L
CHARACTER(*),INTENT(IN),OPTIONAL :: fname
CHARACTER(4) :: fname_
IF( PRESENT(fname) )THEN
  fname_=fname
ELSE
  fname_='f'
END IF
!!--begin--
IF( IsError(xbar) )THEN
    WRITE(*,'(a,3(a,e10.3))')TRIM(fname_),"(x) = ",PPara(3,1),"*x**2 + ",PPara(2,1),"*x + ",PPara(1,1)
ELSE
    WRITE(*,'(a,1(a,es12.5),a)')TRIM(fname_),"(x) = x < ",xbar," ? \"
    WRITE(*,'(3(a,es12.5),a)')"      ",PPara(3,1),"*x**2 + ",PPara(2,1),"*x + ",PPara(1,1)," : \"
    WRITE(*,'(3(a,es12.5),a)')"      ",PPara(3,2),"*x**2 + ",PPara(2,2),"*x + ",PPara(1,2),";"
END IF

!!--end--
END SUBROUTINE

!!### SUBROUTINE <<PRINT_PPara>>
SUBROUTINE PRINT_PPara( xbar,PPara,L)
REAL(KIND_Rdp),INTENT(IN) :: xbar,PPara(3,2),L
!!--begin--
WRITE(*,*)" xbar=",xbar
WRITE(*,*)" ax**2+bx+c, x<xbar"
WRITE(*,*)" a=",PPara(3,1)
WRITE(*,*)" b=",PPara(2,1)
WRITE(*,*)" c=",PPara(1,1)
WRITE(*,*)" ax**2+bx+c, x>=xbar"
WRITE(*,*)" a=",PPara(3,2)
WRITE(*,*)" b=",PPara(2,2)
WRITE(*,*)" c=",PPara(1,2)

!!--end--
END SUBROUTINE

!!### FUNCTION <<PParaInterpolant_Karpov>>
FUNCTION PParaInterpolant_Karpov(y1,y2,yavg,L,xbar) RESULT(PPara)
!!#### PURPOSE
!! Construct a piecewise parabolic polynomial interpolant of
!! two endpoint values and an average value.

!!#### REQUIRED INPUT
REAL(KIND_Rdp),INTENT(IN) :: y1,y2,yavg,L

!!#### REQUIRED OUTPUT
REAL(KIND_Rdp),INTENT(OUT) :: xbar
REAL(KIND_Rdp) :: PPara(3,2)

!!#### LOCAL VARIABLES
REAL(KIND_Rdp) :: ymin,ymax,r,dy,lstarl,lstarr
LOGICAL :: Noisy_=.FALSE.

!!--begin--

xbar = ERROR(xbar)
PPara = ERROR(xbar)


ymin = MIN(y1,y2)
ymax = MAX(y1,y2)

IF( yavg>=ymax )THEN
 !possibly non-monotonic (parabola based on ymax only)
 PPara(1,1) = ymax
 PPara(2,1) = (-6*ymax + 6*yavg)/(L)
 PPara(3,1) = ( 6*ymax - 6*yavg)/(L**2)
 IF( Noisy_ )THEN
    WRITE(*,*)"WARNING: discontinuous yavg>=ymax interpolant used."
 END IF

ELSE IF( yavg<=ymin )THEN
 !possibly non-monotonic (parabola based on ymin only)
 PPara(1,1) = ymin
 PPara(2,1) = (-6*ymin + 6*yavg)/(L)
 PPara(3,1) = ( 6*ymin - 6*yavg)/(L**2)
 IF( Noisy_ )THEN
    WRITE(*,*)"WARNING: discontinuous yavg<=ymin interpolant used."
 END IF

ELSE
 !monotonic

 !if we have a dy that vanishes
 dy = y2-y1

 IF( dy==0._KIND_qmc )THEN
  PPara(1,1) = y1
  PPara(2,1) = 0._KIND_qmc
  PPara(3,1) = 0._KIND_qmc

 !dy is normal
 ELSE

  !get the left lstar
  lstarl =     3*L*(y2-yavg)/(y2-y1)
  !get the right lstar
  lstarr = L - 3*L*(y1-yavg)/(y1-y2)
!   WRITE(*,*)'lstarl=',lstarl
!   WRITE(*,*)'lstarr=',lstarr
!   WRITE(*,*)'L=',L
!   WRITE(*,*)'y1=',y1
!   WRITE(*,*)'y2=',y2
!   WRITE(*,*)'yavg=',yavg
!   WRITE(*,*)'dy=',dy
  IF( 0._KIND_qmc<lstarl .AND. lstarl<L )THEN
   !use left piecewise parabolic
   r = (y2 - y1)/(lstarl**2)     !evaluate at l=lstarl
   PPara(1,1) = y1               !y1
   PPara(2,1) = 2*r*lstarl       !2*y2-2*y1
   PPara(3,1) = -r               !y1-y2
   PPara(1,2) = y2               !=y2 as it should
   PPara(2,2) = 0._KIND_qmc
   PPara(3,2) = 0._KIND_qmc
   xbar = lstarl

  ELSE IF( 0._KIND_qmc<lstarr .AND. lstarr<L )THEN
   !use right piecewise parabolic
   !y1 + 2*(y2-y1)*l/lstarl + (y1-y2)*(l/lstarl)**2
   !
   !l->(L-l)
   !lstarl->(L-lstarr)
   !y1 + 2*(y2-y1)*(L-l)/(L-lstarr) + (y1-y2)*((L-l)/(L-lstarr))**2
   !y1 + 2*(y2-y1)*L/(L-lstarr) + (y1-y2)*L**2/(L-lstarr))**2 + = y1 + (y2-y1)*2*L*(L-lstarr)/(L-lstarr)**2 - (y2-y1)*L**2/(L-lstarr)**2 = y1+(y1-y2)*(2*lstarr*L-L**2)/(L-lstarr)**2
   !2*(y1-y2)*l*(L-lstarr)/(L-lstarr)**2 - 2*(y1-y2)*l*L/(L-lstarr)**2 + = -2*(y1-y2)*lstarr*L/(L-lstarr)**2
   !(y1-y2)*l**2/(L-lstarr)**2 
   r = (y1 - y2)/((L-lstarr)**2)
   PPara(1,1) = y1
   PPara(2,1) = 0._KIND_qmc
   PPara(3,1) = 0._KIND_qmc              !evaluate at l=lstarr
   PPara(1,2) = y1 - r*lstarr**2         !y1 - r*lstarr**2 
   PPara(2,2) = 2*r*lstarr               !2*r*lstarr**2
   PPara(3,2) = -r                       !-r*lstarr**2 ---> y1
   xbar = lstarr                         !at L
                                         !y1 - r*(L**2-2*lstarr*L+lstarr**2)=y1-r*(L-lstarr)**2 = y2
  ELSE
   !use center parabolic
   PPara(1,1) = y1
   PPara(2,1) = (-2*y2 - 4*y1 + 6*yavg)/(L)
   PPara(3,1) = ( 3*y1 + 3*y2 - 6*yavg)/(L**2)

  END IF

 END IF

END IF

!!--end--
END FUNCTION



!!### FUNCTION <<PParaInterpolant_Karpov2>>
FUNCTION PParaInterpolant_Karpov2(y1,y2,yavg,L,xbar,ccenter,p,eps) RESULT(PPara)
!!#### PURPOSE
!! Construct a piecewise parabolic polynomial interpolant of
!! two endpoint values and an average value.

!!#### REQUIRED INPUT
REAL(KIND_Rdp),INTENT(IN) :: y1,y2,yavg,L

!!#### REQUIRED OUTPUT
REAL(KIND_Rdp),INTENT(OUT) :: xbar
REAL(KIND_Rdp) :: PPara(3,2)

!!#### OPTIONAL INPUT
REAL(KIND_Rdp) :: ccenter,eps
INTEGER        :: p

!!#### LOCAL VARIABLES
REAL(KIND_Rdp) :: ymin,ymax,r,dy,lstarl,lstarr
REAL(KIND_Rdp) :: PParaC(3,2),Cleft,Cright
REAL(KIND_Rdp) :: aleft,aright,acenter
REAL(KIND_Rdp) :: ISleft,ISright,IScenter
REAL(KIND_Rdp) :: wleft,wright,wcenter

!!--begin--

xbar = ERROR(xbar)
PPara = ERROR(xbar)


ymin = MIN(y1,y2)
ymax = MAX(y1,y2)

IF( yavg>=ymax )THEN
 !possibly non-monotonic (parabola based on ymax only)
 WRITE(*,*)"non-monotonic with ymax"
 PPara(1,1) = ymax
 PPara(2,1) = (-6*ymax + 6*yavg)/(L)
 PPara(3,1) = ( 6*ymax - 6*yavg)/(L**2)

ELSE IF( yavg<=ymin )THEN
 !possibly non-monotonic (parabola based on ymin only)
 WRITE(*,*)"non-monotonic with ymin"
 PPara(1,1) = ymin
 PPara(2,1) = (-6*ymin + 6*yavg)/(L)
 PPara(3,1) = ( 6*ymin - 6*yavg)/(L**2)

ELSE
 !monotonic

 !if we have a dy that vanishes
 dy = y2-y1

 IF( dy==0._KIND_qmc )THEN
  PPara(1,1) = y1
  PPara(2,1) = 0._KIND_qmc
  PPara(3,1) = 0._KIND_qmc

 !dy is normal
 ELSE

  !get the left lstar
  lstarl = L*(3*y2      - 3*yavg)/dy

  !get the right lstar
  lstarr = L*(2*y1 + y2 - 3*yavg)/dy

  !**in the above code, the substitution has already been made that
  Cright=(1-Ccenter)/2._KIND_Rdp
  Cleft=Cright
  ISleft  = (lstarl)**2
  ISright = (L-lstarr)**2
  IScenter = ((3._KIND_qmc + Ccenter**2)*(y1**2) + &
              (3._KIND_qmc + Ccenter**2)*(y2**2) - &
             12._KIND_qmc*y2*yavg + &
             12._KIND_qmc*(yavg**2) - &
              2._KIND_qmc*y1*( &
                (-3._KIND_qmc + Ccenter**2)*y2 + &
                  6._KIND_qmc*yavg) )/(Ccenter**2)

  !the unnormalized weights are
  acenter = Ccenter/((eps + IScenter )**p)
  aleft   = Cleft  /((eps +   ISleft )**p)
  aright  = Cright /((eps +  ISright )**p)

  IF( aleft>aright )THEN
   !use left piecewise parabolic
   r = (y2 - y1)/(lstarl**2)
   PPara(1,1) = y2 - r*lstarl**2
   PPara(2,1) = 2*r*lstarl
   PPara(3,1) = -r
   PPara(1,2) = y2
   PPara(2,2) = 0._KIND_qmc
   PPara(3,2) = 0._KIND_qmc
   xbar = lstarl

   !center parabolic
   PParaC(1,1) = y1
   PParaC(2,1) = (-2*y2 - 4*y1 + 6*yavg)/(L)
   PParaC(3,1) = ( 3*y1 + 3*y2 - 6*yavg)/(L**2)
   PParaC(1,2) = PParaC(1,1) -   PParaC(2,1)*lstarl + PParaC(3,1)*lstarl**2
   PParaC(2,2) = PParaC(2,1) - 2*PParaC(3,1)*lstarl
   PParaC(3,2) = PParaC(3,1)

   !the normalized weights are
   wcenter = acenter/(acenter+aleft)
   wleft   = aleft  /(acenter+aleft)
   PPara = PPara*wleft + PParaC*wcenter

  ELSE
   !use right piecewise parabolic
   r = (y1 - y2)/((L-lstarr)**2)
   PPara(1,1) = y1
   PPara(2,1) = 0._KIND_qmc
   PPara(3,1) = 0._KIND_qmc
   PPara(1,2) = y1 - r*lstarr**2
   PPara(2,2) = 2*r*lstarr
   PPara(3,2) = -r
   xbar = lstarr

   !center parabolic
   PParaC(1,1) = y1
   PParaC(2,1) = (-2*y2 - 4*y1 + 6*yavg)/(L)
   PParaC(3,1) = ( 3*y1 + 3*y2 - 6*yavg)/(L**2)
   PParaC(1,2) = PParaC(1,1) -   PParaC(2,1)*lstarr + PParaC(3,1)*lstarr**2
   PParaC(2,2) = PParaC(2,1) - 2*PParaC(3,1)*lstarr
   PParaC(3,2) = PParaC(3,1)

   !the normalized weights are
   wcenter = acenter/(acenter+aright)
   wright  = aright /(acenter+aright)
   PPara = PPara*wright + PParaC*wcenter

  END IF

 END IF

END IF

!!--end--
END FUNCTION



!!!### FUNCTION <<PParaInterpolant_Karpov3>>
!FUNCTION PParaInterpolant_Karpov3(y1,y2,yavg,L,xbar) RESULT(PPara)
!!!#### PURPOSE
!!! Construct a piecewise parabolic polynomial interpolant of
!!! two endpoint values and an average value, using a more
!!! robust algorithm than PParaInterpolant_Karpov.
!
!!!#### REQUIRED INPUT
!REAL(KIND_Rdp),INTENT(IN) :: y1,y2,yavg,L
!
!!!#### REQUIRED OUTPUT
!REAL(KIND_Rdp),INTENT(OUT) :: xbar
!REAL(KIND_Rdp) :: PPara(3,2)
!
!!!#### LOCAL VARIABLES
!REAL(KIND_Rdp) :: ymin,ymax,dy,lstarl,lstarr
!REAL(KIND_Rdp) :: y1_,y2_,yavg_,dy_,dyL_,dyR_
!
!!!--begin--
!
!xbar = ERROR(xbar)
!PPara = ERROR(xbar)
!
!
!ymin = MIN(y1,y2)
!ymax = MAX(y1,y2)
!
!IF( yavg>=ymax )THEN
! !possibly non-monotonic (parabola based on ymax only)
! PPara(1,1) = ymax
! PPara(2,1) = (-6*ymax + 6*yavg)/(L)
! PPara(3,1) = ( 6*ymax - 6*yavg)/(L**2)
!
!ELSE IF( yavg<=ymin )THEN
! !possibly non-monotonic (parabola based on ymin only)
! PPara(1,1) = ymin
! PPara(2,1) = (-6*ymin + 6*yavg)/(L)
! PPara(3,1) = ( 6*ymin - 6*yavg)/(L**2)
!
!ELSE
! !monotonic
!
! !if we have a dy that vanishes
! dy = y2-y1
!
! IF( dy==0._KIND_qmc )THEN
!  PPara(1,1) = y1
!  PPara(2,1) = 0._KIND_qmc
!  PPara(3,1) = 0._KIND_qmc
!
! !dy is normal
! ELSE
!
!  !change to normalized space
!  y1_ = y1/ymax
!  y2_ = y2/ymax
!  yavg_ = yavg/ymax
!  dy_ = y2_ - y1_
!
!  !get the left lstar
!  lstarl = L*(3*y2_      - 3*yavg_)/dy_
!  !get the right lstar
!  lstarr = L*(2*y1_ + y2_ - 3*yavg_)/dy_
!
!  IF( 0._KIND_qmc<lstarl .AND. lstarl<L )THEN
!   dyL_ = y2_-yavg_
!   !use left piecewise parabolic
!   PPara(1,1) = y1_
!   PPara(2,1) =  (2.d0/3.d0) * (dy_)**2 / ((L*dyL_)   )
!   PPara(3,1) = -(1.d0/9.d0) * (dy_)**3 / ((L*dyL_)**2)
!   PPara(1,2) = y2_
!   PPara(2,2) = 0._KIND_qmc
!   PPara(3,2) = 0._KIND_qmc
!   xbar = lstarl
!   PPara = PPara*ymax
!  ELSE IF( 0._KIND_qmc<lstarr .AND. lstarr<L )THEN
!   dyR_ = y1_-yavg_
!   !use right piecewise parabolic
!   PPara(1,1) = y1_
!   PPara(2,1) = 0._KIND_qmc
!   PPara(3,1) = 0._KIND_qmc
!   PPara(1,2) = y2_ + (1.d0/9.d0)*(dy_**2)*(6*dyR_-5*dy_)/((dy_-dyR_)**2)
!   PPara(2,2) = (2.d0/9.d0)*(dy_**2)*(2*dy_-3*dyR_)/(L*(dy_-dyR_)**2)
!   PPara(3,2) = (1.d0/9.d0)*(dy_**3)/(L**2*(dy_-dyR_)**2)
!   xbar = lstarr
!   PPara = PPara*ymax
!  ELSE
!   !use center parabolic
!   PPara(1,1) = y1_
!   PPara(2,1) = (-2*y2_ - 4*y1_ + 6*yavg_)/(L)
!   PPara(3,1) = ( 3*y1_ + 3*y2_ - 6*yavg_)/(L**2)
!   PPara(:,1) = PPara(:,1)*ymax
!  END IF
!
!
! END IF
!
!END IF
!
!!!--end--
!END FUNCTION


!!### FUNCTION <<qmc_Q_FROM_QXY>>
FUNCTION qmc_Q_FROM_QXY( es , QXY , corig , crot ) RESULT(Q)

!!#### PURPOSE
!! Transform a source in (x,y) coordinates to (s,t) coordinates.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: es(2),QXY(3)
REAL(KIND_qmc),INTENT(IN),OPTIONAL :: corig(3),crot(3) !additional mapping params

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: q(3)

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: et(2)
REAL(KIND_qmc) :: dx_ds,dx_dt,dy_ds,dy_dt,alpha

!!--begin--

 !constant
 q(1) = QXY(1)

IF( .NOT.(PRESENT(crot).AND.PRESENT(corig)) )THEN
 !s-direction (travel direction--projected to xy-plane)
 q(2) = es(1)*QXY(2) + es(2)*QXY(3)

 !t-direction (perpendicular to s)
 q(3) = es(2)*QXY(2) - es(1)*QXY(3)
ELSE

 !s-direction (travel direction--projected to xy-plane)
 q(2) = es(1)*QXY(2) + es(2)*QXY(3)

 !t-direction (perpendicular to s)
 q(3) = es(2)*QXY(2) - es(1)*QXY(3)

 dx_ds = corig(1)/crot(2)
 dy_ds = corig(2)/crot(2)
 dx_dt = corig(1)/crot(1)
 dy_dt = corig(2)/crot(1)
 alpha = corig(3)

 !s-direction (travel direction--projected to xy-plane)
 q(2) = ( ( es(1)*dx_ds - alpha*es(2)*dx_dt )*QXY(2) + &
          ( es(2)*dy_ds + alpha*es(1)*dy_dt )*QXY(3) )/(1.d0-alpha**2)

 !t-direction (perpendicular to s)
 q(3) =  ( ( es(2)*dx_dt - alpha*es(1)*dx_ds )*QXY(2) + &
           (-es(1)*dy_dt - alpha*es(2)*dy_ds )*QXY(3) )/(1.d0-alpha**2)

 !s-direction (travel direction--projected to xy-plane)
 q(2) = es(1)*QXY(2) + es(2)*QXY(3)

 !t-direction (perpendicular to s)
 q(3) = es(2)*QXY(2) - es(1)*QXY(3)

END IF
!!--end--
END FUNCTION



!!### FUNCTION <<qmc_PsiInOut_from_PsiEdge>>
FUNCTION qmc_PsiInOut_from_PsiEdge( PsiEdge , LenEdge , T , &
  PsiAvg , tvals ) RESULT(PsiInOut)

!!#### PURPOSE
!! Convert a parabolic function representation on an edge ($\ell$
!! coordinates) to the slice edge ($t$ coordinates).

!!#### DETAILS
!! The conversion centers around the following expression
!!
!! $$ \ell(t) = \frac{t-T_1}{T_2-T_1} L      \quad \ell \in [0,L] $$
!!
!! and the inverse
!!
!! $$ t(\ell) = T_1 + \frac{\ell}{L} (T_2-T_1) \quad t \in [T_1,T_2]. $$
!!
!! This $T_1$ and $T_2$ are not the same as the $t_1$ and $t_2$
!! that form the bounds for a slice.  These are for a whole edge.



!!#### REQUIRED INPUT
!! * the 1D parabolic edge function for $\ell \in [0,L]$
!      PsiEdge(1) + PsiEdge(2) * l + PsiEdge(3)* l**2
!! * the length of the edge $L=$<LenEdge>
!! * the t-bounds of the EDGE (not slice)
REAL(KIND_qmc),INTENT(IN) :: PsiEdge(3)
REAL(KIND_qmc),INTENT(IN) :: LenEdge
REAL(KIND_qmc),INTENT(IN) :: T(2)
REAL(KIND_qmc),INTENT(IN) :: PsiAvg

!!#### OPTIONAL INPUT/OUTPUT
!! * input $\ell$ values to map out to $t$ values
REAL(KIND_qmc),INTENT(INOUT),OPTIONAL :: tvals(:)

!!#### REQUIRED OUTPUT
!! * the 1D parabolic edge function for $t \in [t1,t2]$
!      PsiInOut(1) + PsiInOut(2) * t + PsiInOut(3) * t**2
REAL(KIND_qmc) :: PsiInOut(3)

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: Dt,LdivDt1,LdivDt2,Lm
INTEGER :: i

!!--begin--

!get some convenience variables
Dt = T(2) - T(1)

!set the output
IF( Dt<=MIN_EDGE_THICKNESS*LenEdge )THEN
 !we have some options for slices which are almost parallel
 !to direction of travel

 !0. assume constant and equal to average
 PsiInOut(1) = PsiAvg
 !1. assume constant and equal to midpoint value
 !Lm = LenEdge*0.5_KIND_qmc
 !PsiInOut(1) = PsiEdge(1) + PsiEdge(2)*Lm + PsiEdge(3)*(Lm**2)
 !2. assume constant and equal to arithmetic average of left and right
 !PsiInOut(1) = 0.5_KIND_qmc*( 2*PsiEdge(1)              + &
 !                               PsiEdge(2)*LenEdge      + &
 !                               PsiEdge(3)*(LenEdge**2) )
 !3. assume constant and equal to left endpoint
 !PsiInOut(1) = PsiEdge(1)
 !4. assume constant and equal to right endpoint
 !PsiInOut(1) = PsiEdge(1) + PsiEdge(2)*LenEdge + PsiEdge(3)*(LenEdge**2)
 PsiInOut(2) = 0._KIND_qmc
 PsiInOut(3) = 0._KIND_qmc
ELSE
 LdivDt1 = LenEdge/Dt
 LdivDt2 = LdivDt1**2
 PsiInOut(1) = PsiEdge(1) - (T(1))*PsiEdge(2)*LdivDt1 +   (T(1)**2)*PsiEdge(3)*LdivDt2
 PsiInOut(2) =                     PsiEdge(2)*LdivDt1 - 2*(T(1)   )*PsiEdge(3)*LdivDt2
 PsiInOut(3) =                                                      PsiEdge(3)*LdivDt2
END IF

IF( PRESENT(tvals) )THEN
 DO i=1,SIZE(tvals)
  IF( .NOT.IsError(tvals(i)) )THEN
   tvals(i) = T(1) + (tvals(i)/LenEdge) * (Dt)
  END IF
 END DO
END IF

!!--end--
END FUNCTION






!!### FUNCTION <<qmc_PsiEdge_from_2Pt1Avg>>
FUNCTION qmc_PsiEdge_from_2Pt1Avg( PsiAvg , LenEdge , &
  Psi1 , Psi2 , HasExtrema , Extrema ) RESULT(PsiEdge)

!!#### PURPOSE
!! Convert 3 values of the outgoing edge to a parabolic function.


!!#### REQUIRED INPUT
!! * the average over an outgoing edge <PsiAvg>
!! * the length of the edge $L=$<LenEdge>
!! * the point values at the left and right <Psi1> and <Psi2>
REAL(KIND_qmc),INTENT(IN) :: PsiAvg
REAL(KIND_qmc),INTENT(IN) :: LenEdge
REAL(KIND_qmc),INTENT(IN) :: Psi1
REAL(KIND_qmc),INTENT(IN) :: Psi2

!!#### REQUIRED OUTPUT
!! * the 1D parabolic edge function for $\ell \in [0,L]$
!      PsiEdge(1) + PsiEdge(2) * \ell + PsiEdge(3) * \ell**2
REAL(KIND_qmc) :: PsiEdge(3)

!!#### OPTIONAL OUTPUT
!! * whether or not there is an extrema in the region
LOGICAL       ,INTENT(OUT),OPTIONAL :: HasExtrema
REAL(KIND_qmc),INTENT(OUT),OPTIONAL :: Extrema

!!#### LOCAL VARIABLES
REAL(KIND_qmc) :: ellstar


!!--begin--

IF( PRESENT(Extrema) )THEN
 Extrema = Error(Extrema)
END IF

SELECT CASE(EdgeInterpolator)
 CASE(QMC_PARABOLIC)
  !parabolic interpolant
  PsiEdge(3) = ( 3*Psi1 - 6*PsiAvg + 3*Psi2 )/LenEdge**2
  PsiEdge(2) = (-4*Psi1 + 6*PsiAvg - 2*Psi2 )/LenEdge
  PsiEdge(1) = Psi1

 CASE(QMC_LINEAREDGES)
  !linear interpolant variant which satisfies edge values
  PsiEdge(3) = 0._KIND_qmc
  PsiEdge(2) = (Psi2-Psi1)/LenEdge
  PsiEdge(1) = Psi1

 CASE(QMC_LINEARAVERAGE)
  !linear interpolant variant which satisfies average value
  PsiEdge(3) = 0._KIND_qmc
  PsiEdge(2) = (Psi2-Psi1)/LenEdge
  PsiEdge(1) = PsiAvg - 0.5_KIND_qmc*(Psi2-Psi1)

 CASE(QMC_FLAT)
  !flat interpolant
  PsiEdge(3) = 0._KIND_qmc
  PsiEdge(2) = 0._KIND_qmc
  PsiEdge(1) = PsiAvg

 CASE(QMC_CWENO)
  !centered WENO-based interpolant
  PsiEdge = CWENO_Parabola_2Pt1Avg(Psi1,Psi2,PsiAvg,LenEdge,&
            CWENO_cwt,CWENO_order,CWENO_eps)

 CASE DEFAULT
  WRITE(*,*)"Fatal Error: NO EDGE INTERPOLATOR SET!"
  STOP

END SELECT

IF( PRESENT(HasExtrema) )THEN
 IF( PsiEdge(3)==0._KIND_qmc )THEN
  ellstar = -LenEdge
 ELSE
  ellstar = -PsiEdge(2)/(2*PsiEdge(3))
 END IF
 HasExtrema = (0._KIND_qmc<=ellstar .AND. ellstar<=LenEdge)
 IF( PRESENT(Extrema) )THEN
  IF( HasExtrema )THEN
   Extrema = ellstar
  END IF
 END IF
END IF

!!--end--
END FUNCTION



!!### FUNCTION <<qmc_PsiEdge2_from_2Pt1Avg>>
FUNCTION qmc_PsiEdge2_from_2Pt1Avg( PsiAvg , LenEdge , &
  Psi1 , Psi2 , lstar , HasExtrema , Extrema ) RESULT(PsiEdge2)

!!#### PURPOSE
!! Convert 3 values of the outgoing edge to a
!! piecewise parabolic interpolator function.
!
! the function is piecewise continuous in C_0 and C_1
!
! for 0     <= l <= lstar : PsiEdge2(:,1)
! for lstar <  l <= L     : PsiEdge2(:,2)
!
! where c=PsiEdge2(1,i)
!       b=PsiEdge2(2,i)
!       a=PsiEdge2(3,i)
! in the function y(l) = al**2 + bl + c
!
USE FUN_IsError         !!((05-A-FUN_IsError.f90))

!!#### CHANGES
!! v0.03 - removed part of the piecewise if lstar is too small.

!!#### REQUIRED INPUT
!! * the average over an outgoing edge <PsiAvg>
!! * the length of the edge $L=$<LenEdge>
!! * the point values at the left and right <Psi1> and <Psi2>
REAL(KIND_qmc),INTENT(IN) :: PsiAvg
REAL(KIND_qmc),INTENT(IN) :: LenEdge
REAL(KIND_qmc),INTENT(IN) :: Psi1
REAL(KIND_qmc),INTENT(IN) :: Psi2

!!#### REQUIRED OUTPUT
!! * the 1D parabolic edge functions for $\ell \in [0,L]$
!      PsiEdge(1,i) + PsiEdge(2,i) * \ell + PsiEdge(3,i) * \ell**2
!! * the $\ell$ value at which we switch from i=1 to i=2
REAL(KIND_qmc)             :: PsiEdge2(3,2)
REAL(KIND_qmc),INTENT(OUT) :: lstar

!!#### OPTIONAL OUTPUT
!! * whether or not there is an extrema in the region
LOGICAL       ,INTENT(OUT),OPTIONAL :: HasExtrema
REAL(KIND_qmc),INTENT(OUT),OPTIONAL :: Extrema

!!--begin--

IF( EdgeInterpolator==QMC_PARABOLICKARPOV )THEN
 !CAUTION: this routine sometimes returns parabolics dominated by roundoff errors
 !hopefully v0.03 is fixing it
 PsiEdge2 = PParaInterpolant_Karpov(Psi1,Psi2,PsiAvg,LenEdge,lstar)
 IF( ABS(lstar/LenEdge)<=MIN_EDGE_THICKNESS )THEN
    PsiEdge2(1  ,1)=Psi1
    PsiEdge2(2:3,1)=0._KIND_qmc
 ELSEIF( ABS(lstar/LenEdge-1.d0)<=MIN_EDGE_THICKNESS )THEN
    PsiEdge2(1  ,2)=Psi2
    PsiEdge2(2:3,2)=0._KIND_qmc
 END IF

ELSE
 lstar = ERROR(lstar)
 PsiEdge2(:,1) = ERROR(lstar)
 PsiEdge2(:,2) = qmc_PsiEdge_from_2Pt1Avg( PsiAvg , LenEdge , &
  Psi1 , Psi2 , HasExtrema , Extrema )
END IF

!!--end--
END FUNCTION


END MODULE
