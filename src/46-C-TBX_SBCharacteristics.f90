!!# TOOLBOX MODULE: <<TBX_SBCharacteristics>>
MODULE TBX_SBCharacteristics

!!## PURPOSE
!! Provides toolbox routines for the subcell balance characteristic
!! method of Karpov [ref] for solution of the linearized Boltzmann
!! transport equation.


!!## NOTES
!! Depends on the user module, <USR_SBCharacteristics>.


!!## USAGE



!!## EXTERNAL KINDS
USE KND_Characteristics   !!((03-C-KND_Characteristics.f90))
USE USR_SBCharacteristics !!((26-C-USR_SBCharacteristics.f90))
USE FUN_IsApprox          !!((03-A-FUN_IsApprox.f90))
USE FUN_Default           !!((04-A-FUN_Default.f90))
USE LIB_SubcellBalances   !!((12-C-LIB_SubcellBalances.f90))
USE FUN_NewFile           !!((05-B-FUN_NewFile.f90))
USE FUN_NewUnit           !!((04-B-FUN_NewUnit.f90))
USE USR_PolyExpoMoment    !!((11-C-USR_PolyExpoMoment.f90))
USE SUB_ExponentialMoment !!((10-C-SUB_ExponentialMoment.f90))
USE VAR_Units             !!((03-A-VAR_Units.f90))
USE FUN_STR               !!((05-B-FUN_STR.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PUBLIC ACCESS

!!### Elements from <USR_SBCharacteristics>
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
PUBLIC :: qmc_PsiInOut_from_PsiEdge
PUBLIC :: qmc_PsiEdge_from_2Pt1Avg
PUBLIC :: qmc_Q_from_QXY
PUBLIC :: qmc_ThinEdgeCount
!!
!!### Elements from <TBX_SBCharacteristics>
!! * testing routine
!PUBLIC :: TEST_PsiSB
PUBLIC :: GNUPLOT_PsiSB
PUBLIC :: TEST_CWENO_Parabola_2Pt1Avg

!!## PROCEDURES
CONTAINS



! SUBROUTINE TEST_PsiSB( )
! USE LIB_xy                !!((10-B-LIB_xy.f90))
! USE FUN_xySLICE           !!((09-A-FUN_xySLICE.f90))
! USE FUN_NewFile           !!((05-B-FUN_NewFile.f90))
! USE FUN_STR               !!((05-B-FUN_STR.f90))
! USE FUN_xySAREA           !!((03-A-FUN_xySAREA.f90))
! USE FUN_xyCENTROID        !!((05-B-FUN_xyCENTROID.f90))
! USE FUN_xyTRANSLATE       !!((05-B-FUN_xyTRANSLATE.f90))
! USE SUB_Pause             !!((04-B-SUB_Pause.f90))
! USE FUN_xyDIRECTION       !!((04-B-FUN_xyDIRECTION.f90))
! USE FUN_getArgFile        !!((11-C-FUN_getArgFile.f90))
! USE FUN_xyDIST            !!((04-B-FUN_xyDIST.f90))
! USE FUN_xyANGLE           !!((05-B-FUN_xyANGLE.f90))
! USE FUN_xyROTATE          !!((05-B-FUN_xyROTATE.f90))
! USE USR_SparseArray       !!((45-C-USR_SparseArray.f90))
! 
! REAL(KIND_qmc) :: q(3),MacT,RecipSin
! REAL(KIND_qmc) :: U(2),t,s,z,z2,Pc(2),Area,z3
! REAL(KIND_qmc) :: PsiSB_avg,PsiSB_avgout,PsiSB_avgin
! REAL(KIND_qmc) :: AreaCell
! REAL(KIND_qmc) :: Psi_int
! INTEGER        :: N,i,j,CellCase,k,I1,I2
! INTEGER        :: InpUnit,OutUnit,InpFile
! INTEGER        :: Unit_Sli,Unit_Pg
! INTEGER,ALLOCATABLE :: INSIDE(:),OUTSIDE(:)
! LOGICAL,ALLOCATABLE :: OUTFACES(:),OUTVERTS(:)
! REAL(KIND_qmc),ALLOCATABLE :: Psi_intin(:),Psi_intout(:)
! REAL(KIND_qmc),ALLOCATABLE :: psiedge(:,:),Pg(:,:),Sli(:,:),LenEdge(:)
! REAL(KIND_qmc),ALLOCATABLE :: AreaSli(:),LenSliIn(:),LenSliOut(:)
! REAL(KIND_qmc),ALLOCATABLE :: PsiSB_intin(:),PsiSB_intout(:),PsiSB_int(:)
! REAL(KIND_qmc),ALLOCATABLE :: PsiVert(:),PsiEdgeAvg(:),LenEdge0(:)
! REAL(KIND_qmc),ALLOCATABLE :: Pgr(:,:)
! LOGICAL :: HasExtrema
! REAL(KIND_qmc) :: Extrema,Psi1,Psi2,PsiAvg,PsiSB_out1,PsiSB_out2
! REAL(KIND_qmc) :: theta,Tl(2),Avg
! 
! 
! !!--begin--
! 
! InpUnit = getArgFile( I=1 , DefaultToDefaultInput =.TRUE. )
! OutUnit = getArgFile( I=2 , DefaultToDefaultOutput=.TRUE. )
! 
! WRITE(OutUnit,"(a)")"Choose one of the following cells/angle combos."
! WRITE(OutUnit,"(a)")"1) Unit Triangle"
! WRITE(OutUnit,"(a)")"2) Unit Square"
! WRITE(OutUnit,"(a)")"3) Custom Polygon and Angle"
! READ(InpUnit,*)CellCase
! 
! SELECT CASE(CellCase)
!  CASE(1) ; N = 3 ; ALLOCATE( Pg(2,N) )
!                    Pg(:,1) = (/0.d0,0.d0/)
!                    Pg(:,2) = (/1.d0,0.d0/)
!                    Pg(:,3) = (/1.d0,1.d0/)
! 
!  CASE(2) ; N = 4 ; ALLOCATE( Pg(2,N) )
!                    Pg(:,1) = (/0.d0,0.d0/)
!                    Pg(:,2) = (/1.d0,0.d0/)
!                    Pg(:,3) = (/1.d0,1.d0/)
!                    Pg(:,4) = (/0.d0,1.d0/)
! 
!  CASE DEFAULT
!   WRITE(OutUnit,"(a)")"Enter number of sides to polygon."
!   READ(InpUnit,*)N
!   ALLOCATE( Pg(2,N) )
! 
!   WRITE(OutUnit,"(a)")"Input points in counter clockwise order."
!   DO i=1,N
!    READ(InpUnit,*)Pg(:,i)
!   END DO
! 
! END SELECT
! 
! WRITE(OutUnit,"(a)")"Enter direction of travel."
! READ(InpUnit,*)U
! U = xyDIRECTION_V( U )
! 
! ALLOCATE( OUTFACES(N) , OUTVERTS(N) )
! ALLOCATE( INSIDE(N-1) , OUTSIDE(N-1) )
! ALLOCATE( psiedge(3,N) , Sli(3,N) )
! ALLOCATE( Psi_intout(N) , Psi_intin(N) , LenEdge(N) )
! ALLOCATE( AreaSli(N-1) , LenSliIn(N-1) , LenSliOut(N-1) )
! ALLOCATE( PsiSB_intin(N-1) , PsiSB_int(N-1) , PsiSB_intout(N-1) )
! ALLOCATE( PsiVert(N) , PsiEdgeAvg(N) , LenEdge0(N) )
! ALLOCATE( Pgr(2,N) )
! 
! WRITE(OutUnit,"(a)")"The input vertices were: "
! DO i=1,N
!  WRITE(OutUnit,"(i5,1x,2e12.5)")i,Pg(:,i)
! END DO
! WRITE(OutUnit,*)
! IF( InpUnit==DEFAULT_INPUT_UNIT )THEN
!  CALL Pause(s="Press <ENTER>")
! END IF
! 
! 
! WRITE(OutUnit,"(a)")"The input direction of travel is: "
! WRITE(OutUnit,"(2e12.5)")U
! WRITE(OutUnit,*)
! IF( InpUnit==DEFAULT_INPUT_UNIT )THEN
!  CALL Pause(s="Press <ENTER>")
! END IF
! 
! 
! WRITE(OutUnit,"(a)")"The area is:"
! Area = xySAREA_Pg( N , Pg )
! WRITE(OutUnit,"(a)")TRIM(STR(Area))
! IF( InpUnit==DEFAULT_INPUT_UNIT )THEN
!  CALL Pause(s="Press <ENTER>")
! END IF
! 
! 
! WRITE(OutUnit,"(a)")"The centroid is:"
! Pc   = xyCENTROID_Pg( N , Pg , Area )
! WRITE(OutUnit,"(a)")TRIM(STR(Pc(1)))//" "//TRIM(STR(Pc(2)))
! IF( InpUnit==DEFAULT_INPUT_UNIT )THEN
!  CALL Pause(s="Press <ENTER>")
! END IF
! 
! 
! WRITE(OutUnit,"(a)")"The polygon translated to local coordinates is:"
! Pg   = xyTRANSLATE_Px( N , Pg , -Pc )
! DO i=1,N
!  WRITE(OutUnit,"(i5,1x,2e12.5)")i,Pg(:,i)
! END DO
! WRITE(OutUnit,*)
! IF( InpUnit==DEFAULT_INPUT_UNIT )THEN
!  CALL Pause(s="Press <ENTER>")
! END IF
! 
! 
! WRITE(OutUnit,"(a)")"The polygon rotated to align with direction U:"
! theta = xyANGLE_UU( U , (/0._KIND_qmc,1._KIND_qmc/) )
! Pgr   = xyROTATE_Px( N , Pg , theta )
! DO i=1,N
!  WRITE(OutUnit,"(i5,1x,2e12.5)")i,Pgr(:,i)
! END DO
! WRITE(OutUnit,*)
! IF( InpUnit==DEFAULT_INPUT_UNIT )THEN
!  CALL Pause(s="Press <ENTER>")
! END IF
! 
! 
! !get slices and outgoing faces
! Sli = xySLICE_Rdp( N , Pg , U , OUTSIDE=OUTSIDE , INSIDE=INSIDE )
! OUTFACES = xyOUTFACES_PgU( N , Pg , U , OUTVERTS )
! 
! WRITE(OutUnit,"(a)")"The division of this polygon into slices is given by"
! WRITE(OutUnit,*)
! WRITE(OutUnit,"(a8,3a17)")"i","ti","smin(ti)","smax(ti)"
! DO i=1,N
!  WRITE(OutUnit,"(i8,3e17.9)")i,Sli(:,i)
! END DO
! WRITE(OutUnit,*)
! Unit_Sli = NewFile("Sli")
! CALL GNUPLOT_Sli( Unit_Sli , N , Sli )
! CLOSE(Unit_Sli)
! 
! Unit_Pg = NewFile("Pg")
! CALL GNUPLOT_Pg( Unit_Pg , N , Pg )
! CLOSE(Unit_Pg)
! WRITE(OutUnit,"(a)")"The slices and polygon have been written to files <Sli> and <Pg>."
! WRITE(OutUnit,*)
! IF( InpUnit==DEFAULT_INPUT_UNIT )THEN
!  CALL Pause(s="Press <ENTER>")
! END IF
! 
! 
! 
! WRITE(OutUnit,"(a)")"For each subcell the incoming/outgoing edges are"
! WRITE(OutUnit,*)
! WRITE(OutUnit,"(3a8)")"k","inedge","outedge"
! DO k=1,N-1
!  WRITE(OutUnit,"(3i8)")k,INSIDE(k),OUTSIDE(k)
! END DO
! IF( InpUnit==DEFAULT_INPUT_UNIT )THEN
!  CALL Pause(s="Press <ENTER>")
! END IF
! 
! 
! WRITE(OutUnit,"(a)")"Enter the pointwise angular flux values for all verts"
! DO i=1,N
!  WRITE(OutUnit,*)"vert",i
!  READ(InpUnit,*)PsiVert(i)
! END DO
! WRITE(OutUnit,*)
! 
! 
! WRITE(OutUnit,"(a)")"Enter the average angular flux values for all edges"
! DO i=1,N
!  WRITE(OutUnit,*)"edge",i
!  READ(InpUnit,*)PsiEdgeAvg(i)
! END DO
! WRITE(OutUnit,*)
! 
! 
! WRITE(OutUnit,"(a)")"Enter the total cross section"
! READ(InpUnit,*)MacT
! 
! 
! WRITE(OutUnit,"(a)")"Enter the Reciprocal of the Sin(theta)"
! READ(InpUnit,*)RecipSin
! 
! 
! WRITE(OutUnit,"(a)")"Enter the total source in subcell coordinates"
! WRITE(OutUnit,"(a)")" q(s,t) = q0 + qs s + qt t"
! WRITE(OutUnit,"(a)")" in the form <q0 qs qt>"
! READ(InpUnit,*)q
! 
! 
! 
! !determine incoming flux functions
! DO i=1,N
! 
!  i1 = i
!  i2 = MERGE(1,i+1,i==N)
! 
!  LenEdge0(i) = xyDIST_PP( Pg(:,i1),Pg(:,i2) )
! 
!  !first in local <l> coordinates
!  PsiEdge(:,i) = qmc_PsiEdge_from_2Pt1Avg( PsiEdgeAvg(i) , LenEdge0(i) , &
!     PsiVert(i1) , PsiVert(i2) , HasExtrema , Extrema )
! 
!  WRITE(OutUnit,"(a,i)")"Function al^2 + bl + c generated for edge ",i
!  WRITE(OutUnit,"(a,Es24.13)")" Length = ",LenEdge0(i)
!  WRITE(OutUnit,"(a,Es24.13)")" a = ",PsiEdge(3,i)
!  WRITE(OutUnit,"(a,Es24.13)")" b = ",PsiEdge(2,i)
!  WRITE(OutUnit,"(a,Es24.13)")" c = ",PsiEdge(1,i)
! 
!  IF( HasExtrema )THEN
!   WRITE(OutUnit,"(a,Es24.13)")" Extrema located at l=",Extrema
!  ELSE
!   WRITE(OutUnit,"(a  )")" No Extrema "
!  END IF
! 
!  !get <t> coordinates of this edges endpoints
!  Tl(1) = Pgr(1,i1)
!  Tl(2) = Pgr(1,i2)
! 
!  !now in <t> coordinates
!  PsiEdge(:,i) = qmc_PsiInOut_from_PsiEdge( PsiEdge(:,i) , LenEdge0(i) , Tl , &
!    PsiEdgeAvg(i) )
! 
!  WRITE(OutUnit,"(a,i)")"Function at^2 + bt + c converted for edge ",i
!  WRITE(OutUnit,"(a,Es24.13)")" Length = ",LenEdge0(i)
!  WRITE(OutUnit,"(a,Es24.13)")" a = ",PsiEdge(3,i)
!  WRITE(OutUnit,"(a,Es24.13)")" b = ",PsiEdge(2,i)
!  WRITE(OutUnit,"(a,Es24.13)")" c = ",PsiEdge(1,i)
! 
! END DO
! 
! 
! Psi_int    = 0
! Psi_intout = 0
! Psi_intin  = 0
! AreaCell   = 0
! LenEdge    = 0
! 
! DO k=1,N-1
! 
!  WRITE(OutUnit,"(a)")"==== "
!  WRITE(OutUnit,"(a,i)")"==== Results for subcell         k       = ",k
! 
!  !get edge and vert quantities
!  i = INSIDE(k)
!  i1 = i
!  i2 = MERGE(1,i+1,i==N)
!  Psi1 = PsiVert(i1)
!  Psi2 = PsiVert(i2)
!  PsiAvg = PsiEdgeAvg(i)
!  WRITE(OutUnit,"(a,i)")"  Incoming edge                     = ",i
!  WRITE(OutUnit,"(a,Es24.13)")"  Average flux value                = ",PsiAvg
!  WRITE(OutUnit,"(a,i)")"  Left vertex index                 = ",i1
!  WRITE(OutUnit,"(a,Es24.13)")"  Flux value at left vertex         = ",Psi1
!  WRITE(OutUnit,"(a,i)")"  Right vertex index                = ",i2
!  WRITE(OutUnit,"(a,Es24.13)")"  Flux value at right vertex        = ",Psi2
! 
! 
!  CALL qmc_INIT( id       = k                    , &
!                 Sli      = Sli(:,k:k+1)         , &
!                 psiin    = psiedge(:,i) , &
!                 q        = q                    , &
!                 MacT     = MacT                 , &
!                 RecipSin = RecipSin               )
!  CALL qmc_PRINT(k,Unit=OutUnit)
! 
!  !get geometric quantities
!  AreaSli     (k) = qmc_AreaSli(k)
!  LenSliOut   (k) = qmc_LenSliOut(k)
!  LenSliIn    (k) = qmc_LenSliIn(k)
! 
!  WRITE(OutUnit,"(a,Es24.13)")"  Area of the slice                 = ",AreaSli(k)
!  WRITE(OutUnit,"(a,Es24.13)")"  Length of incoming slice edge     = ",LenSliIn(k)
!  WRITE(OutUnit,"(a,Es24.13)")"  Length of outgoing slice edge     = ",LenSliOut(k)
! 
!  !printing of visualization files
!  CALL GNUPLOT_PsiSB(k)
! 
!  PsiSB_int   (k) = qmc_PsiSB_int(k,Avg)
!  PsiSB_intin (k) = qmc_PsiSB_intin(k,Avg)
!  PsiSB_intout(k) = qmc_PsiSB_intout(k,Avg)
! 
!  PsiSB_avg    = PsiSB_int   (k)/AreaSli(k)
!  PsiSB_avgin  = PsiSB_intin (k)/LenSliIn(k)
!  PsiSB_avgout = PsiSB_intout(k)/LenSliOut(k)
!  PsiSB_out1   = qmc_PsiSB_out1(k)
!  PsiSB_out2   = qmc_PsiSB_out2(k)
! 
!  WRITE(OutUnit,"(a,Es24.13)")"  Subcell integral of psi(s,t)      = ",PsiSB_int(k)
!  WRITE(OutUnit,"(a,Es24.13)")"  Subcell integral of psiin(t)      = ",PsiSB_intin(k)
!  WRITE(OutUnit,"(a,Es24.13)")"  Subcell integral of psiout(t)     = ",PsiSB_intout(k)
!  WRITE(OutUnit,"(a,Es24.13)")"  Subcell average of psi(s,t)       = ",PsiSB_avg
!  WRITE(OutUnit,"(a,Es24.13)")"  Subcell average of psiin(t)       = ",PsiSB_avgin
!  WRITE(OutUnit,"(a,Es24.13)")"  Subcell average of psiout(t)      = ",PsiSB_avgout
!  WRITE(OutUnit,"(a,Es24.13)")"  Left  pointwise value, psiout(t1) = ",PsiSB_out1
!  WRITE(OutUnit,"(a,Es24.13)")"  Right pointwise value, pisout(t2) = ",PsiSB_out2
!  WRITE(OutUnit,"(a)")"==== "
!  WRITE(OutUnit,"(a)")"==== "
!  Psi_int       = Psi_int      + PsiSB_int(k)
!  AreaCell      = AreaCell     + AreaSli(k)
! 
! END DO
! 
! !accumulate edge values from slice values
! !outgoing
! DO k=1,N-1
!  i = OUTSIDE(k)
!  LenEdge(i)    = LenEdge(i)    + LenSliOut(k)
!  Psi_intout(i) = Psi_intout(i) + PsiSB_intout(k)
! END DO
! 
! !incoming
! DO k=1,N-1
!  i = INSIDE(k)
!  LenEdge(i)   = LenEdge(i)   + LenSliIn(k)
!  Psi_intin(i) = Psi_intin(i) + PsiSB_intin(k)
! END DO
! 
! WRITE(OutUnit,"(a)")"  "
! WRITE(OutUnit,"(a)")" *** CELL BALANCE SUMMARY *** "
! WRITE(OutUnit,"(a)")"  "
! WRITE(OutUnit,"(a,Es24.13)")"  area of the cell                = ",AreaCell
! WRITE(OutUnit,"(a)")"  "
! WRITE(OutUnit,"(a,Es24.13)")"  cell integral of psi(s,t)       = ",Psi_int
! WRITE(OutUnit,"(a)")"  "
! DO i=1,N
!  WRITE(OutUnit,"(a,i)")"            edge",i
!  WRITE(OutUnit,"(a,Es24.13)")"  length of edge                  = ",LenEdge(i)
!  IF( OUTFACES(i) )THEN
!   WRITE(OutUnit,"(a,Es24.13)")"  outgoing edge integral          = ",Psi_intout(i)
!  ELSE
!   WRITE(OutUnit,"(a,Es24.13)")"  incoming edge integral          = ",Psi_intin(i)
!  END IF
!  WRITE(OutUnit,"(a)")" "
! END DO
! WRITE(OutUnit,"(a)")"  "
! 
! 
! InpFile = NewFile("psisbtest.inp")
! WRITE(InpFile,*)3
! WRITE(InpFile,*)N
! DO i=1,N
!  WRITE(InpFile,*)Pg(:,i)
! END DO
! WRITE(InpFile,*)U
! WRITE(InpFile,*)
! DO i=1,N
!  WRITE(InpFile,*)PsiVert(i)
! END DO
! DO i=1,N
!  WRITE(InpFile,*)PsiEdgeAvg(i)
! END DO
! WRITE(InpFile,*)MacT
! WRITE(InpFile,*)RecipSin
! WRITE(InpFile,*)q
! CLOSE( InpFile )
! 
! STOP
! 
! !!--end--
! END SUBROUTINE


!!### GNUPLOTTING SUBROUTINE <<GNUPLOT_PsiSB>>
SUBROUTINE GNUPLOT_PsiSB(k)
USE FUN_STR               !!((05-B-FUN_STR.f90))
USE FUN_NewFile           !!((05-B-FUN_NewFile.f90))
INTEGER :: Unit_Psi,Unit_Q,Unit_Sli
INTEGER :: Unit_PsiIn,Unit_PsiOut
INTEGER :: i,j,k
REAL(KIND_qmc) :: s,t,z,z2,z3

!!--begin--
Unit_Psi    = NewFile("Psi"//TRIM(STR(k)))
Unit_PsiIn  = NewFile("PsiIn"//TRIM(STR(k)))
Unit_PsiOut = NewFile("PsiOut"//TRIM(STR(k)))
Unit_Q      = NewFile("Q"//TRIM(STR(k)))
Unit_Sli    = NewFile("SliLS"//TRIM(STR(k)))
DO i=0,20
 t = qmc_t1(i) + (REAL(i,KIND_Rdp)/20d0)*qmc_Dt(i)
 DO j=0,20
  s = qmc_smin(i,t) + (REAL(j,KIND_Rdp)/20.d0)*qmc_Ds(i,t)
  z  = qmc_PsiSB( i , s , t )
  z2 = qmc_Q( i , s , t )

  WRITE(Unit_Psi,"(3(a,1x))")TRIM(STR(t)),TRIM(STR(s)),TRIM(STR(z))
  WRITE(Unit_Psi,*)
  WRITE(Unit_Q  ,"(3(a,1x))")TRIM(STR(t)),TRIM(STR(s)),TRIM(STR(z2))
  WRITE(Unit_Q  ,*)
 END DO
 WRITE(Unit_PsiIn ,"(3(a,1x))")TRIM(STR(t)),TRIM(STR(qmc_smin(i,t))),TRIM(STR(qmc_PsiSB_in (i,t)))
 WRITE(Unit_PsiOut,"(3(a,1x))")TRIM(STR(t)),TRIM(STR(qmc_smax(i,t))),TRIM(STR(qmc_PsiSB_out(i,t)))
 WRITE(Unit_Sli,"(2(a,1x))")TRIM(STR(t)),TRIM(STR(qmc_smin(i,t)))
 WRITE(Unit_Sli,"(2(a,1x))")TRIM(STR(t)),TRIM(STR(qmc_smax(i,t)))
END DO
CLOSE(Unit_Sli)
CLOSE(Unit_Psi)
CLOSE(Unit_Q)
CLOSE(Unit_PsiIn)
CLOSE(Unit_PsiOut)
!!--end--
END SUBROUTINE





!!### FUNCTION <<TEST_CWENO_Parabola_2Pt1Avg>>
FUNCTION TEST_CWENO_Parabola_2Pt1Avg(Noisy) RESULT(Pass)

!!#### PURPOSE
!! Test the <CWENO_Parabola_2Pt1Avg> function
!! for generating non-oscillatory parabolas from
!! 2 pieces of pointwise data and 1 piece of average
!! data.

!!#### MODULES
USE LIB_Norm              !!((04-B-LIB_Norm.f90))
USE FUN_Default           !!((04-A-FUN_Default.f90))

!!#### OPTIONAL INPUT
LOGICAL,INTENT(IN),OPTIONAL :: Noisy

!!#### REQUIRED OUTPUT
LOGICAL        :: Pass

!!#### LOCAL VARIABLES
REAL(KIND_Rdp) :: y1,y2,yavg,L,cwt,eps,Para(3)
INTEGER        :: order
REAL(KIND_Rdp) :: Exact_Para(3),diff_Para(3)
LOGICAL        :: Noisy_

!!--begin--

Noisy_ = Default( .FALSE. , Noisy )

y1    = 1._KIND_Rdp
y2    = 0._KIND_Rdp
L     = 1._KIND_Rdp
yavg  = 0.65_KIND_Rdp*(y1 + y2)/2._KIND_Rdp
cwt   = 15._KIND_Rdp/16._KIND_Rdp
order = 2
eps   = 1.E-4_KIND_Rdp

Para = CWENO_Parabola_2Pt1Avg(y1,y2,yavg,L,cwt,order,eps)
Exact_Para = (/0.9142092753843537733_KIND_Rdp,&
              -1.7134328598334230965_KIND_Rdp,&
               0.8025214635970733251_KIND_Rdp/)
diff_Para = Para-Exact_Para
Pass = (NormInfty(diff_Para)<10._KIND_Rdp*EPSILON(1._KIND_Rdp))
IF( .NOT.Pass )RETURN
IF( Noisy_ )THEN
 WRITE(*,*)" CWENO_Parabola_2Pt1Avg test 1: "//MERGE("PASS","FAIL",Pass)
END IF


y1    = 1._KIND_Rdp
y2    = 0._KIND_Rdp
L     = 1._KIND_Rdp
yavg  = 1.1_KIND_Rdp*(y1 + y2)/2._KIND_Rdp
cwt   = 10._KIND_Rdp/16._KIND_Rdp
order = 2
eps   = 1.E-4_KIND_Rdp

Para = CWENO_Parabola_2Pt1Avg(y1,y2,yavg,L,cwt,order,eps)
Exact_Para = (/0.99646953621825003448_KIND_Rdp,&
              -0.71191654819297553608_KIND_Rdp,&
              -0.271533786365286799318_KIND_Rdp/)
diff_Para = Para-Exact_Para
Pass = (NormInfty(diff_Para)<10._KIND_Rdp*EPSILON(1._KIND_Rdp))
IF( .NOT.Pass )RETURN
IF( Noisy_ )THEN
 WRITE(*,*)" CWENO_Parabola_2Pt1Avg test 2: "//MERGE("PASS","FAIL",Pass)
END IF

y1    = 0._KIND_Rdp
y2    = 1._KIND_Rdp
L     = 1._KIND_Rdp
yavg  = 0.9_KIND_Rdp*(y1 + y2)/2._KIND_Rdp
cwt   = 10._KIND_Rdp/16._KIND_Rdp
order = 2
eps   = 1.E-4_KIND_Rdp

Para = CWENO_Parabola_2Pt1Avg(y1,y2,yavg,L,cwt,order,eps)
Exact_Para = (/0.003530463781749965519_KIND_Rdp,&
               0.7119165481929755361_KIND_Rdp,&
               0.27153378636528679932_KIND_Rdp/)
diff_Para = Para-Exact_Para
Pass = (NormInfty(diff_Para)<10._KIND_Rdp*EPSILON(1._KIND_Rdp))
IF( .NOT.Pass )RETURN
IF( Noisy_ )THEN
 WRITE(*,*)" CWENO_Parabola_2Pt1Avg test 3: "//MERGE("PASS","FAIL",Pass)
END IF


y1=2.71828182845904523536029_KIND_Rdp
y2=3.14159265358979323846264_KIND_Rdp
L =7.0_KIND_Rdp
yavg=0.9_KIND_Rdp*(y1 + y2)/2._KIND_Rdp
cwt=8._KIND_Rdp/16._KIND_Rdp
order=2
eps=1.E-4_KIND_Rdp

Para = CWENO_Parabola_2Pt1Avg(y1,y2,yavg,L,cwt,order,eps)
Exact_Para = (/2.7179060931816069714_KIND_Rdp,&
              -0.023157704500677814741_KIND_Rdp,&
               5.4728260862873539090E-6_KIND_Rdp/)
diff_Para = Para-Exact_Para
Pass = (NormInfty(diff_Para)<10._KIND_Rdp*EPSILON(1._KIND_Rdp))
IF( .NOT.Pass )RETURN
IF( Noisy_ )THEN
 WRITE(*,*)" CWENO_Parabola_2Pt1Avg test 4: "//MERGE("PASS","FAIL",Pass)
END IF

!!--end--
END FUNCTION


!!### SUBROUTINE <<PROMPT_MCOMPARE>>
SUBROUTINE PROMPT_MCOMPARE(t1,t2,smin1,smin2,smax1,smax2,&
    q1,qs,qt,sintheta)
!!#### PURPOSE
!! Prompt to multi-compare some data.

!!#### MODULES
USE FUN_NewFile           !!((05-B-FUN_NewFile.f90))

!!#### REQUIRED INPUT
REAL(KIND_Rdp),INTENT(IN) :: t1,t2,smin1,smin2,smax1,smax2,&
    q1,qs,qt,sintheta

!!#### LOCAL VARIABLES
REAL(KIND_Rdp) :: sigma_range(2),mulsigma
CHARACTER(128) :: file
INTEGER        :: Unit
!!--begin--
WRITE(*,*)"Enter the sigma range for the kernel to check"
READ(*,*)sigma_range
WRITE(*,*)"Enter the sigma multiplier to traverse the range"
READ(*,*)mulsigma
WRITE(*,*)"Enter the file to write the info to"
READ(*,*)file
Unit = NewFile(TRIM(ADJUSTL(file)))
CALL MCOMPARE_PsiS_Interior_Lin(Unit,t1,t2,smin1,smin2,smax1,smax2,&
   sigma_range,mulsigma,q1,qs,qt,sintheta)
CLOSE(Unit)
!!--end--
END SUBROUTINE



!!### SUBROUTINE <<PRINT_OverlapTest>>
SUBROUTINE PRINT_OverlapTest()
!!#### PURPOSE
!! Print some files to test the components of the
!! <PsiS_Interior_Lin> routine.

!!#### LOCAL VARIABLES
INTEGER :: U1
LOGICAL :: Pass

!!--begin--

U1 = NewFile("antstlinsrc-6.txt")
Pass=CHECK1_PsiS_Interior_Lin(tol=1.d-6 ,NoisyUnit=U1); CLOSE(U1)

U1 = Newfile("antstlinsrc-9.txt")
Pass=CHECK1_PsiS_Interior_Lin(tol=1.d-9 ,NoisyUnit=U1); CLOSE(U1)

U1 = NewFile("antstlinsrc-12.txt")
Pass=CHECK1_PsiS_Interior_Lin(tol=1.d-12,NoisyUnit=U1); CLOSE(U1)

U1 = NewFile("antstlinsrc-15.txt")
Pass=CHECK1_PsiS_Interior_Lin(tol=1.d-15,NoisyUnit=U1); CLOSE(U1)

U1 = NewFile("antstlinsrc-18.txt")
Pass=CHECK1_PsiS_Interior_Lin(tol=1.d-18,NoisyUnit=U1); CLOSE(U1)

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<PRINT_ComparePMandM>>
SUBROUTINE PRINT_ComparePMandM()

!!#### PURPOSE
!! Compare the polynomial exponential moments to the
!! exponential moments $M$>

!!#### LOCAL VARIABLES
INTEGER :: u1,u2,u3,u4

!!--begin--
u1 = NewFile("PM")
u2 = NewFile("M")
CALL PRINT_PolyExpoMoment(7,Unit=u1,xstart=1.d2,xdiv=2.d0,ndiv=30)
CALL PRINT_ExponentialMoment(7,Unit=u2,xstart=1.d2,xdiv=1.1d0,ndiv=200)
u3 = NewFile("PMa")
u4 = NewFile("Ma")
CALL PRINT_PolyExpoMoment(7,Unit=u3,xstart=1.d14,xdiv=10.d0,ndiv=30)
CALL PRINT_ExponentialMoment(7,Unit=u4,xstart=1.d14,xdiv=10.d0,ndiv=30)
CLOSE(u1)
CLOSE(u2)
CLOSE(u3)
CLOSE(u4)
!!--end--
END SUBROUTINE


!!### SUBROUTINE <<PRINT_SelectedPM>>
SUBROUTINE PRINT_SelectedPM(NoisyUnit)

!!#### PURPOSE
!! Print selected polynomial exponential moments, $PM$.

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: NoisyUnit

!!#### LOCAL VARIABLES
INTEGER :: u1,u2,u3,u4,i,MaxI,MaxN
INTEGER :: NoisyUnit_
REAL(KIND_qmc) :: bt,b1

!!--begin--

NoisyUnit_ = Default(DEFAULT_OUTPUT_UNIT,NoisyUnit)

IF( NoisyUnit/=0 )THEN
WRITE(NoisyUnit_,*)"This prints results for 2D polynomial exponential moments PM(i,n)."
WRITE(NoisyUnit_,*)""
WRITE(NoisyUnit_,*)"  PM(i,n) = Integrate[ t^i * "
WRITE(NoisyUnit_,*)"               Integrate[ s^n Exp[-x s] , {s,0,bt*t+b1} ], "
WRITE(NoisyUnit_,*)"            {t,0,1}]"
WRITE(NoisyUnit_,*)" Note that the s-integral upper bound is a linear function of t."
WRITE(NoisyUnit_,*)""
WRITE(NoisyUnit_,*)""
WRITE(NoisyUnit_,*)"1. We report results for 4 tests of values of bt and b1, "
WRITE(NoisyUnit_,*)"    named t1, t2, t3, and t4."
WRITE(NoisyUnit_,*)""
WRITE(NoisyUnit_,*)"2. We generate files with 6 columns, "
WRITE(NoisyUnit_,*)"    x first, then PM(i,0) for i=0...4"
WRITE(NoisyUnit_,*)""
WRITE(NoisyUnit_,*)"3. File names t[*]-PM[i] contain an optical thickness range where the "
WRITE(NoisyUnit_,*)"    functions are interesting, from about x=100 to 1e-8."
WRITE(NoisyUnit_,*)""
WRITE(NoisyUnit_,*)"4. File names t[*]-PMa[i] contain 'all' optical thicknesses, "
WRITE(NoisyUnit_,*)"    x=10^15 to 10^-15"
WRITE(NoisyUnit_,*)""
WRITE(NoisyUnit_,*)""
WRITE(NoisyUnit_,*)""
END IF

!test 1
IF( NoisyUnit_/=0 )THEN
 WRITE(NoisyUnit_,*)" test1:   bt=0.1 b1=0.9"
END IF
bt=0.1_KIND_qmc
b1=0.9_KIND_qmc
MaxN=4
MaxI=4
DO i=0,MaxI
 u3 = NewFile("t1-PM"//TRIM(STR(i)))
 CALL PRINT_PolyExpoMoment2(i,MaxN,bt,b1,Unit=u3,xstart=1.d2,xdiv=2.d0,ndiv=30)
 CLOSE(u3)
 u3 = NewFile("t1-PMa"//TRIM(STR(i)))
 CALL PRINT_PolyExpoMoment2(i,MaxN,bt,b1,Unit=u3,xstart=1.d14,xdiv=10.d0,ndiv=30)
 CLOSE(u3)
END DO

!test 2
IF( NoisyUnit_/=0 )THEN
 WRITE(NoisyUnit_,*)" test2:   bt=1.0 b1=0.0"
END IF
bt=1.0_KIND_qmc
b1=0.0_KIND_qmc
MaxN=4
MaxI=4
DO i=0,MaxI
 u3 = NewFile("t2-PM"//TRIM(STR(i)))
 CALL PRINT_PolyExpoMoment2(i,MaxN,bt,b1,Unit=u3,xstart=1.d2,xdiv=2.d0,ndiv=30)
 CLOSE(u3)
 u3 = NewFile("t2-PMa"//TRIM(STR(i)))
 CALL PRINT_PolyExpoMoment2(i,MaxN,bt,b1,Unit=u3,xstart=1.d14,xdiv=10.d0,ndiv=30)
 CLOSE(u3)
END DO

!test 3
IF( NoisyUnit_/=0 )THEN
 WRITE(NoisyUnit_,*)" test3:   bt=0.5 b1=0.5"
END IF
bt=0.5_KIND_qmc
b1=0.5_KIND_qmc
MaxN=4
MaxI=4
DO i=0,MaxI
 u3 = NewFile("t3-PM"//TRIM(STR(i)))
 CALL PRINT_PolyExpoMoment2(i,MaxN,bt,b1,Unit=u3,xstart=1.d2,xdiv=2.d0,ndiv=30)
 CLOSE(u3)
 u3 = NewFile("t3-PMa"//TRIM(STR(i)))
 CALL PRINT_PolyExpoMoment2(i,MaxN,bt,b1,Unit=u3,xstart=1.d14,xdiv=10.d0,ndiv=30)
 CLOSE(u3)
END DO

!test 4
IF( NoisyUnit_/=0 )THEN
 WRITE(NoisyUnit_,*)" test4:   bt=0.0 b1=1.0"
END IF
bt=0.0_KIND_qmc
b1=1.0_KIND_qmc
MaxN=4
MaxI=4
DO i=0,MaxI
 u3 = NewFile("t4-PM"//TRIM(STR(i)))
 CALL PRINT_PolyExpoMoment2(i,MaxN,bt,b1,Unit=u3,xstart=1.d2,xdiv=2.d0,ndiv=30)
 CLOSE(u3)
 u3 = NewFile("t4-PMa"//TRIM(STR(i)))
 CALL PRINT_PolyExpoMoment2(i,MaxN,bt,b1,Unit=u3,xstart=1.d14,xdiv=10.d0,ndiv=30)
 CLOSE(u3)
END DO

!!--end--
END SUBROUTINE



END MODULE
