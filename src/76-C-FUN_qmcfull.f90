!!# FUNCTION MODULE: <qmcfull>
MODULE FUN_qmcfull
!!## PURPOSE
!! Return a source as a function of $x,y,z$.

USE KND_Characteristics                                !!((03-C-KND_Characteristics.f90))
USE FUN_Heaviside                                      !!((03-A-FUN_Heaviside.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

PRIVATE

!!## PUBLIC ACCESS LIST
PUBLIC :: Qfull,Qfull1,sigmafull
PUBLIC :: Qfull2D,Qfull3D
PUBLIC :: sigmafull2D,sigmafull3D
PUBLIC :: Qfull1_Mesh

!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: SourceVariant = 5
LOGICAL,PARAMETER :: Debug = .FALSE.

CONTAINS


!!### FUNCTION <Qfull3D>
FUNCTION Qfull3D(x,y,z)
!!#### PURPOSE
!! Wrapper function for a specific type of 3D interface.
REAL(KIND_qmc) :: Qfull3D
REAL(KIND_qmc),INTENT(IN) :: x,y,z
!!--begin--
Qfull3D = Qfull((/x,y,z/))
!!--end--
END FUNCTION


!!### FUNCTION <Qfull2D>
FUNCTION Qfull2D(x,y)
!!#### PURPOSE
!! Wrapper function for a specific type of 2D interface.
REAL(KIND_qmc) :: Qfull2D
REAL(KIND_qmc),INTENT(IN) :: x,y
!!--begin--
Qfull2D = Qfull((/x,y/))
!!--end--
END FUNCTION



!!### FUNCTION <sigmafull3D>
FUNCTION sigmafull3D(x,y,z)
!!#### PURPOSE
!! Wrapper function for a specific type of 3D interface.
REAL(KIND_qmc) :: sigmafull3D
REAL(KIND_qmc),INTENT(IN) :: x,y,z
!!--begin--
sigmafull3D = sigmafull((/x,y,z/))
!!--end--
END FUNCTION


!!### FUNCTION <sigmafull2D>
FUNCTION sigmafull2D(x,y)
!!#### PURPOSE
!! Wrapper function for a specific type of 2D interface.
REAL(KIND_qmc) :: sigmafull2D
REAL(KIND_qmc),INTENT(IN) :: x,y
!!--begin--
sigmafull2D = sigmafull((/x,y/))
!!--end--
END FUNCTION



!!### FUNCTION <Qfull>
FUNCTION Qfull(r,i)

!!#### PURPOSE
!! Return the source as a function of <r=(/x,y,z/)>,
!! and possibly the cell index i.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: r(:)

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: i

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: Qfull

!!--begin--

SELECT CASE( SourceVariant )
 CASE( 1 )    ; Qfull = 0._KIND_qmc
 CASE( 2 )    ; Qfull = 1._KIND_qmc
 CASE( 3 )    ; Qfull = Qfull_bonkers (r(1),r(2))
 CASE( 4 )    ; Qfull = Qfull_randcube(r(1),r(2),r(3))
 CASE DEFAULT ; Qfull = Qfull_mesh    (r,i)
END SELECT

!!--end--
END FUNCTION


!!### FUNCTION <Qfull1>
FUNCTION Qfull1(r,i)

!!#### PURPOSE
!! Return the first derivative of the source as a function of <r=(/x,y,z/)>,
!! and possibly the cell index i.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: r(:)

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: i

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: Qfull1

!!--begin--

SELECT CASE( SourceVariant )
 CASE( 1 )    ; Qfull1 = 0._KIND_qmc
 CASE( 2 )    ; Qfull1 = 0._KIND_qmc
 CASE( 3 )    ; Qfull1 = 0._KIND_qmc
 CASE( 4 )    ; Qfull1 = 0._KIND_qmc
 CASE DEFAULT ; Qfull1 = Qfull1_mesh(r,i)
END SELECT

!!--end--
END FUNCTION



!!### FUNCTION <sigmafull>
FUNCTION sigmafull(r,i) RESULT(sigma)

!!#### PURPOSE
!! Return the total cross section as a function of
!! <r=(/x,y,z/)>, and possibly the cell index i.

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: r(:)

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: i

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: sigma

!!--begin--

SELECT CASE( SourceVariant )
 CASE( 1 )    ; sigma = 0._KIND_qmc
 CASE( 2 )    ; sigma = 5._KIND_qmc
 CASE( 3 )    ; sigma = sigmafull_bonkers (r(1),r(2))
 CASE( 4 )    ; sigma = sigmafull_randcube(r(1),r(2),r(3))
 CASE DEFAULT ; sigma = sigmafull_mesh    (r,i)
END SELECT

!!--end--
END FUNCTION




!!### FUNCTION <Qfull1_mesh>
FUNCTION Qfull1_mesh(r,i) RESULT(Qfull1)

!!#### PURPOSE
!! Provide a source function capable of looking up the derivative
!! in the total source with respect to <s>.

!!#### MODULES
USE TBX_Mesh        ,ONLY: InteriorCell,CellCentroid   !!((15-B-TBX_Mesh.f90))
USE VAR_Mesh        ,ONLY: Mesh                        !!((46-B-VAR_Mesh.f90))
USE VAR_ScalarFluxes,ONLY: ScalarFluxCellFunction      !!((04-C-VAR_ScalarFluxes.f90))
USE VAR_Materials   ,ONLY: l_,CoeffScalarFluxM         !!((48-B-VAR_Materials.f90))
USE VAR_Source      ,ONLY: ExtSourceCellFunction       !!((47-B-VAR_Source.f90))
USE VAR_Characteristics,ONLY: Omega                    !!((75-C-VAR_Characteristics.f90))

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: r(Mesh%Ndim)

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: i

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: Qfull1

!!#### LOCAL VARIABLES
INTEGER                             :: i0,l,g
REAL(KIND_qmc),DIMENSION(Mesh%Ndim) :: linear

!!--begin--

IF( PRESENT(i) )THEN
 i0 = i
ELSE
 i0 = InteriorCell(Mesh,r)
END IF

IF( i0==0 )THEN
 !WRITE(*,*)"[[qmcfull: Qfull1_mesh]] warning: in a region where the cell is not defined."
 Qfull1 = 0.d0
 RETURN
END IF

g  = 1

!!get the values
IF( Debug )THEN
 Qfull1  = REAL(i,KIND_qmc)
ELSE
 l      = l_(i0)
 linear = CoeffScalarFluxM(g,l) * ScalarFluxCellFunction(2:3,g,i0) + &
          ExtSourceCellFunction(2:3,g,i0)
 Qfull1 = DOT_PRODUCT(linear,Omega)
END IF

!!--end--
END FUNCTION



!!### FUNCTION <Qfull_mesh>
FUNCTION Qfull_mesh(r,i) RESULT(Qfull)

!!#### PURPOSE
!! Provide a source function capable of looking up the total
!! source assuming cell-based quantities.

!!#### MODULES
USE TBX_Mesh        ,ONLY: InteriorCell,CellCentroid   !!((15-B-TBX_Mesh.f90))
USE VAR_Mesh        ,ONLY: Mesh                        !!((46-B-VAR_Mesh.f90))
USE VAR_ScalarFluxes,ONLY: ScalarFluxCellFunction      !!((04-C-VAR_ScalarFluxes.f90))
USE VAR_Materials   ,ONLY: l_,CoeffScalarFluxM         !!((48-B-VAR_Materials.f90))
USE VAR_Source      ,ONLY: ExtSourceCellFunction       !!((47-B-VAR_Source.f90))

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: r(Mesh%Ndim)

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: i

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: Qfull

!!#### LOCAL VARIABLES
INTEGER                             :: i0,l,g
REAL(KIND_qmc),DIMENSION(Mesh%Ndim) :: CC,linear,u
REAL(KIND_qmc)                      :: const

!!--begin--

IF( PRESENT(i) )THEN
 i0 = i
ELSE
 i0 = InteriorCell(Mesh,r)
END IF

IF( i0==0 )THEN
 !WRITE(*,*)"[[qmcfull: Qfull_mesh]] warning: in a region where the cell is not defined."
 Qfull = 0.d0
 RETURN
END IF

CC = CellCentroid(Mesh,i0)
g  = 1

!!get the values
IF( Debug )THEN
 Qfull  = REAL(i,KIND_qmc)
ELSE
 u      = r - CC
 l      = l_(i0)
 const  = CoeffScalarFluxM(g,l) * ScalarFluxCellFunction(1  ,g,i0) + &
                                  ExtSourceCellFunction (1  ,g,i0)
 linear = CoeffScalarFluxM(g,l) * ScalarFluxCellFunction(2:3,g,i0) + &
                                  ExtSourceCellFunction (2:3,g,i0)
 Qfull  = const + DOT_PRODUCT(linear,u)
END IF

!!--end--
END FUNCTION



!!### FUNCTION: <sigmafull_mesh>
FUNCTION sigmafull_mesh(r,i)  RESULT(sigmafull)

!!#### PURPOSE
!! Provide a sigma (total cross section) function capable of looking up the total
!! source assuming cell-based quantities.

!!#### MODULES
USE TBX_Mesh          ,ONLY: InteriorCell,CellCentroid !!((15-B-TBX_Mesh.f90))
USE VAR_Mesh          ,ONLY: Mesh                      !!((46-B-VAR_Mesh.f90))
USE VAR_Materials     ,ONLY: l_                        !!((48-B-VAR_Materials.f90))
USE VAR_XSMonkey      ,ONLY: MaCT                      !!((47-B-VAR_XSMonkey.f90))

!!#### REQUIRED INPUT
REAL(KIND_qmc),INTENT(IN) :: r(Mesh%Ndim)

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: i

!!#### REQUIRED OUTPUT
REAL(KIND_qmc) :: sigmafull

!!#### LOCAL VARIABLES
INTEGER :: i0,l,g

!!--begin--

IF( PRESENT(i) )THEN
 i0 = i
ELSE
 i0 = InteriorCell(Mesh,r)
END IF
IF( i0==0 )THEN
 !WRITE(*,*)"[[qmcfull: sigmafull_mesh]] warning: in a region where the cell is not defined."
 sigmafull = 0.d0
 RETURN
END IF
g  = 1

!!get the values
IF( Debug )THEN
 sigmafull  = REAL(1,KIND_qmc)/REAL(i,KIND_qmc)
ELSE
 l          = l_(i0)
 sigmafull  = MacT(g,l)
END IF

!!--end--
END FUNCTION




!!--------------------various special test functions----------------------------------

FUNCTION Qfull_bonkers(x,y) RESULT(Qfull)
REAL(KIND_qmc),INTENT(IN) :: x
REAL(KIND_qmc),INTENT(IN),OPTIONAL :: y
REAL(KIND_qmc) :: Qfull
!!--begin--
Qfull = 1._KIND_qmc/(0.1_KIND_qmc+ABS(x-0.5_KIND_qmc)+ABS(y-0.5_KIND_qmc))
!!--end--
END FUNCTION



FUNCTION sigmafull_bonkers(x,y) RESULT(sigma)
REAL(KIND_qmc),INTENT(IN) :: x
REAL(KIND_qmc),INTENT(IN),OPTIONAL :: y
REAL(KIND_qmc) :: sigma
!!--begin--
sigma = 1._KIND_qmc
IF( x>=.25_KIND_qmc .AND. x<=.75_KIND_qmc )THEN
 IF( y>=.25_KIND_qmc .AND. y<=.75_KIND_qmc )THEN
  sigma = 2._KIND_qmc
 END IF
END IF
!!--end--
END FUNCTION


FUNCTION Qfull_randcube(x,y,z) RESULT(Qfull)
REAL(KIND_qmc),INTENT(IN) :: x
REAL(KIND_qmc),INTENT(IN),OPTIONAL :: y,z
REAL(KIND_qmc) :: Qfull
INTEGER :: i,j,k

!!begin--

!get i,j,k in between 1 and 5 from x,y,z in between 0 and 1
IF( x==1._KIND_qmc )THEN
 i = 5
ELSE
 i = CEILING( 5._KIND_qmc * x )
END IF

IF( PRESENT(y) )THEN
 IF( y==1._KIND_qmc )THEN
  j = 5
 ELSE
  j = CEILING( 5._KIND_qmc * y )
 END IF
ELSE
 j = 1
END IF

IF( PRESENT(z) )THEN
 IF( z==1._KIND_qmc )THEN
  k = 5
 ELSE
  k = CEILING( 5._KIND_qmc * z )
 END IF
ELSE
 k = 1
END IF


SELECT CASE(i*100+j*10+k*1)
  CASE(111); Qfull=0.451E+01_KIND_qmc
  CASE(112); Qfull=0.530E+01_KIND_qmc
  CASE(113); Qfull=0.586E+01_KIND_qmc
  CASE(114); Qfull=0.608E+01_KIND_qmc
  CASE(115); Qfull=0.477E+01_KIND_qmc
  CASE(121); Qfull=0.306E+01_KIND_qmc
  CASE(122); Qfull=0.801E+01_KIND_qmc
  CASE(123); Qfull=0.629E+01_KIND_qmc
  CASE(124); Qfull=0.932E+00_KIND_qmc
  CASE(125); Qfull=0.448E+01_KIND_qmc
  CASE(131); Qfull=0.164E+01_KIND_qmc
  CASE(132); Qfull=0.303E+01_KIND_qmc
  CASE(133); Qfull=0.207E+01_KIND_qmc
  CASE(134); Qfull=0.414E+01_KIND_qmc
  CASE(135); Qfull=0.141E+01_KIND_qmc
  CASE(141); Qfull=0.272E+01_KIND_qmc
  CASE(142); Qfull=0.237E+01_KIND_qmc
  CASE(143); Qfull=0.539E+01_KIND_qmc
  CASE(144); Qfull=0.214E+01_KIND_qmc
  CASE(145); Qfull=0.830E+01_KIND_qmc
  CASE(151); Qfull=0.192E+01_KIND_qmc
  CASE(152); Qfull=0.855E+01_KIND_qmc
  CASE(153); Qfull=0.299E+01_KIND_qmc
  CASE(154); Qfull=0.639E+01_KIND_qmc
  CASE(155); Qfull=0.534E+01_KIND_qmc
  CASE(211); Qfull=0.432E+01_KIND_qmc
  CASE(212); Qfull=0.109E+01_KIND_qmc
  CASE(213); Qfull=0.756E+01_KIND_qmc
  CASE(214); Qfull=0.688E+01_KIND_qmc
  CASE(215); Qfull=0.991E-01_KIND_qmc
  CASE(221); Qfull=0.505E+01_KIND_qmc
  CASE(222); Qfull=0.953E+01_KIND_qmc
  CASE(223); Qfull=0.375E+01_KIND_qmc
  CASE(224); Qfull=0.878E+01_KIND_qmc
  CASE(225); Qfull=0.871E+01_KIND_qmc
  CASE(231); Qfull=0.462E+01_KIND_qmc
  CASE(232); Qfull=0.854E+01_KIND_qmc
  CASE(233); Qfull=0.302E+00_KIND_qmc
  CASE(234); Qfull=0.111E+01_KIND_qmc
  CASE(235); Qfull=0.725E+01_KIND_qmc
  CASE(241); Qfull=0.751E+00_KIND_qmc
  CASE(242); Qfull=0.641E+01_KIND_qmc
  CASE(243); Qfull=0.977E+01_KIND_qmc
  CASE(244); Qfull=0.339E+01_KIND_qmc
  CASE(245); Qfull=0.340E+00_KIND_qmc
  CASE(251); Qfull=0.406E+01_KIND_qmc
  CASE(252); Qfull=0.238E+01_KIND_qmc
  CASE(253); Qfull=0.976E+01_KIND_qmc
  CASE(254); Qfull=0.713E+01_KIND_qmc
  CASE(255); Qfull=0.695E+01_KIND_qmc
  CASE(311); Qfull=0.183E+01_KIND_qmc
  CASE(312); Qfull=0.295E+01_KIND_qmc
  CASE(313); Qfull=0.284E+01_KIND_qmc
  CASE(314); Qfull=0.931E+01_KIND_qmc
  CASE(315); Qfull=0.827E+01_KIND_qmc
  CASE(321); Qfull=0.197E+01_KIND_qmc
  CASE(322); Qfull=0.769E+01_KIND_qmc
  CASE(323); Qfull=0.325E+01_KIND_qmc
  CASE(324); Qfull=0.507E+01_KIND_qmc
  CASE(325); Qfull=0.838E+01_KIND_qmc
  CASE(331); Qfull=0.830E+01_KIND_qmc
  CASE(332); Qfull=0.628E-01_KIND_qmc
  CASE(333); Qfull=0.786E+01_KIND_qmc
  CASE(334); Qfull=0.991E+01_KIND_qmc
  CASE(335); Qfull=0.861E+00_KIND_qmc
  CASE(341); Qfull=0.574E+01_KIND_qmc
  CASE(342); Qfull=0.806E+00_KIND_qmc
  CASE(343); Qfull=0.200E+01_KIND_qmc
  CASE(344); Qfull=0.460E+01_KIND_qmc
  CASE(345); Qfull=0.901E+01_KIND_qmc
  CASE(351); Qfull=0.849E+01_KIND_qmc
  CASE(352); Qfull=0.721E+01_KIND_qmc
  CASE(353); Qfull=0.782E+01_KIND_qmc
  CASE(354); Qfull=0.479E+01_KIND_qmc
  CASE(355); Qfull=0.751E+01_KIND_qmc
  CASE(411); Qfull=0.186E+01_KIND_qmc
  CASE(412); Qfull=0.583E+01_KIND_qmc
  CASE(413); Qfull=0.606E+01_KIND_qmc
  CASE(414); Qfull=0.402E+01_KIND_qmc
  CASE(415); Qfull=0.833E+01_KIND_qmc
  CASE(421); Qfull=0.898E+01_KIND_qmc
  CASE(422); Qfull=0.969E+01_KIND_qmc
  CASE(423); Qfull=0.264E+01_KIND_qmc
  CASE(424); Qfull=0.801E+01_KIND_qmc
  CASE(425); Qfull=0.355E+01_KIND_qmc
  CASE(431); Qfull=0.176E+01_KIND_qmc
  CASE(432); Qfull=0.152E+01_KIND_qmc
  CASE(433); Qfull=0.461E+01_KIND_qmc
  CASE(434); Qfull=0.765E+01_KIND_qmc
  CASE(435); Qfull=0.535E+01_KIND_qmc
  CASE(441); Qfull=0.715E+01_KIND_qmc
  CASE(442); Qfull=0.574E+01_KIND_qmc
  CASE(443); Qfull=0.806E+01_KIND_qmc
  CASE(444); Qfull=0.874E+01_KIND_qmc
  CASE(445); Qfull=0.761E+01_KIND_qmc
  CASE(451); Qfull=0.680E+01_KIND_qmc
  CASE(452); Qfull=0.779E+01_KIND_qmc
  CASE(453); Qfull=0.835E+01_KIND_qmc
  CASE(454); Qfull=0.315E+01_KIND_qmc
  CASE(455); Qfull=0.453E+01_KIND_qmc
  CASE(511); Qfull=0.833E+01_KIND_qmc
  CASE(512); Qfull=0.368E+01_KIND_qmc
  CASE(513); Qfull=0.674E+01_KIND_qmc
  CASE(514); Qfull=0.112E+01_KIND_qmc
  CASE(515); Qfull=0.440E+01_KIND_qmc
  CASE(521); Qfull=0.172E+01_KIND_qmc
  CASE(522); Qfull=0.819E+01_KIND_qmc
  CASE(523); Qfull=0.456E+01_KIND_qmc
  CASE(524); Qfull=0.145E+01_KIND_qmc
  CASE(525); Qfull=0.792E-01_KIND_qmc
  CASE(531); Qfull=0.564E+01_KIND_qmc
  CASE(532); Qfull=0.597E+00_KIND_qmc
  CASE(533); Qfull=0.806E+01_KIND_qmc
  CASE(534); Qfull=0.995E+01_KIND_qmc
  CASE(535); Qfull=0.840E+01_KIND_qmc
  CASE(541); Qfull=0.253E+01_KIND_qmc
  CASE(542); Qfull=0.362E+01_KIND_qmc
  CASE(543); Qfull=0.187E+01_KIND_qmc
  CASE(544); Qfull=0.462E+01_KIND_qmc
  CASE(545); Qfull=0.817E+01_KIND_qmc
  CASE(551); Qfull=0.258E+01_KIND_qmc
  CASE(552); Qfull=0.958E+01_KIND_qmc
  CASE(553); Qfull=0.544E+01_KIND_qmc
  CASE(554); Qfull=0.827E+01_KIND_qmc
  CASE(555); Qfull=0.331E+01_KIND_qmc
  CASE DEFAULT; Qfull=0.000E+01_KIND_qmc
END SELECT


!!--end--
END FUNCTION


FUNCTION sigmafull_randcube(x,y,z) RESULT(sigmafull)
REAL(KIND_qmc),INTENT(IN) :: x
REAL(KIND_qmc),INTENT(IN),OPTIONAL :: y,z
REAL(KIND_qmc) :: sigmafull
INTEGER :: i,j,k

!!begin--

!get i,j,k in between 1 and 5 from x,y,z in between 0 and 1
IF( x==5._KIND_qmc )THEN
 i = 5
ELSE
 i = CEILING( 5._KIND_qmc * x )
END IF

IF( PRESENT(y) )THEN
 IF( y==5._KIND_qmc )THEN
  j = 5
 ELSE
  j = CEILING( 5._KIND_qmc * y )
 END IF
ELSE
 j = 1
END IF

IF( PRESENT(z) )THEN
 IF( z==5._KIND_qmc )THEN
  k = 5
 ELSE
  k = CEILING( 5._KIND_qmc * z )
 END IF
ELSE
 k = 1
END IF


SELECT CASE(i*100+j*10+k*1)
  CASE(111); sigmafull=0.451E+01_KIND_qmc
  CASE(112); sigmafull=0.530E+01_KIND_qmc
  CASE(113); sigmafull=0.586E+01_KIND_qmc
  CASE(114); sigmafull=0.608E+01_KIND_qmc
  CASE(115); sigmafull=0.477E+01_KIND_qmc
  CASE(121); sigmafull=0.306E+01_KIND_qmc
  CASE(122); sigmafull=0.801E+01_KIND_qmc
  CASE(123); sigmafull=0.629E+01_KIND_qmc
  CASE(124); sigmafull=0.932E+00_KIND_qmc
  CASE(125); sigmafull=0.448E+01_KIND_qmc
  CASE(131); sigmafull=0.164E+01_KIND_qmc
  CASE(132); sigmafull=0.303E+01_KIND_qmc
  CASE(133); sigmafull=0.207E+01_KIND_qmc
  CASE(134); sigmafull=0.414E+01_KIND_qmc
  CASE(135); sigmafull=0.141E+01_KIND_qmc
  CASE(141); sigmafull=0.272E+01_KIND_qmc
  CASE(142); sigmafull=0.237E+01_KIND_qmc
  CASE(143); sigmafull=0.539E+01_KIND_qmc
  CASE(144); sigmafull=0.214E+01_KIND_qmc
  CASE(145); sigmafull=0.830E+01_KIND_qmc
  CASE(151); sigmafull=0.192E+01_KIND_qmc
  CASE(152); sigmafull=0.855E+01_KIND_qmc
  CASE(153); sigmafull=0.299E+01_KIND_qmc
  CASE(154); sigmafull=0.639E+01_KIND_qmc
  CASE(155); sigmafull=0.534E+01_KIND_qmc
  CASE(211); sigmafull=0.432E+01_KIND_qmc
  CASE(212); sigmafull=0.109E+01_KIND_qmc
  CASE(213); sigmafull=0.756E+01_KIND_qmc
  CASE(214); sigmafull=0.688E+01_KIND_qmc
  CASE(215); sigmafull=0.991E-01_KIND_qmc
  CASE(221); sigmafull=0.505E+01_KIND_qmc
  CASE(222); sigmafull=0.953E+01_KIND_qmc
  CASE(223); sigmafull=0.375E+01_KIND_qmc
  CASE(224); sigmafull=0.878E+01_KIND_qmc
  CASE(225); sigmafull=0.871E+01_KIND_qmc
  CASE(231); sigmafull=0.462E+01_KIND_qmc
  CASE(232); sigmafull=0.854E+01_KIND_qmc
  CASE(233); sigmafull=0.302E+00_KIND_qmc
  CASE(234); sigmafull=0.111E+01_KIND_qmc
  CASE(235); sigmafull=0.725E+01_KIND_qmc
  CASE(241); sigmafull=0.751E+00_KIND_qmc
  CASE(242); sigmafull=0.641E+01_KIND_qmc
  CASE(243); sigmafull=0.977E+01_KIND_qmc
  CASE(244); sigmafull=0.339E+01_KIND_qmc
  CASE(245); sigmafull=0.340E+00_KIND_qmc
  CASE(251); sigmafull=0.406E+01_KIND_qmc
  CASE(252); sigmafull=0.238E+01_KIND_qmc
  CASE(253); sigmafull=0.976E+01_KIND_qmc
  CASE(254); sigmafull=0.713E+01_KIND_qmc
  CASE(255); sigmafull=0.695E+01_KIND_qmc
  CASE(311); sigmafull=0.183E+01_KIND_qmc
  CASE(312); sigmafull=0.295E+01_KIND_qmc
  CASE(313); sigmafull=0.284E+01_KIND_qmc
  CASE(314); sigmafull=0.931E+01_KIND_qmc
  CASE(315); sigmafull=0.827E+01_KIND_qmc
  CASE(321); sigmafull=0.197E+01_KIND_qmc
  CASE(322); sigmafull=0.769E+01_KIND_qmc
  CASE(323); sigmafull=0.325E+01_KIND_qmc
  CASE(324); sigmafull=0.507E+01_KIND_qmc
  CASE(325); sigmafull=0.838E+01_KIND_qmc
  CASE(331); sigmafull=0.830E+01_KIND_qmc
  CASE(332); sigmafull=0.628E-01_KIND_qmc
  CASE(333); sigmafull=0.786E+01_KIND_qmc
  CASE(334); sigmafull=0.991E+01_KIND_qmc
  CASE(335); sigmafull=0.861E+00_KIND_qmc
  CASE(341); sigmafull=0.574E+01_KIND_qmc
  CASE(342); sigmafull=0.806E+00_KIND_qmc
  CASE(343); sigmafull=0.200E+01_KIND_qmc
  CASE(344); sigmafull=0.460E+01_KIND_qmc
  CASE(345); sigmafull=0.901E+01_KIND_qmc
  CASE(351); sigmafull=0.849E+01_KIND_qmc
  CASE(352); sigmafull=0.721E+01_KIND_qmc
  CASE(353); sigmafull=0.782E+01_KIND_qmc
  CASE(354); sigmafull=0.479E+01_KIND_qmc
  CASE(355); sigmafull=0.751E+01_KIND_qmc
  CASE(411); sigmafull=0.186E+01_KIND_qmc
  CASE(412); sigmafull=0.583E+01_KIND_qmc
  CASE(413); sigmafull=0.606E+01_KIND_qmc
  CASE(414); sigmafull=0.402E+01_KIND_qmc
  CASE(415); sigmafull=0.833E+01_KIND_qmc
  CASE(421); sigmafull=0.898E+01_KIND_qmc
  CASE(422); sigmafull=0.969E+01_KIND_qmc
  CASE(423); sigmafull=0.264E+01_KIND_qmc
  CASE(424); sigmafull=0.801E+01_KIND_qmc
  CASE(425); sigmafull=0.355E+01_KIND_qmc
  CASE(431); sigmafull=0.176E+01_KIND_qmc
  CASE(432); sigmafull=0.152E+01_KIND_qmc
  CASE(433); sigmafull=0.461E+01_KIND_qmc
  CASE(434); sigmafull=0.765E+01_KIND_qmc
  CASE(435); sigmafull=0.535E+01_KIND_qmc
  CASE(441); sigmafull=0.715E+01_KIND_qmc
  CASE(442); sigmafull=0.574E+01_KIND_qmc
  CASE(443); sigmafull=0.806E+01_KIND_qmc
  CASE(444); sigmafull=0.874E+01_KIND_qmc
  CASE(445); sigmafull=0.761E+01_KIND_qmc
  CASE(451); sigmafull=0.680E+01_KIND_qmc
  CASE(452); sigmafull=0.779E+01_KIND_qmc
  CASE(453); sigmafull=0.835E+01_KIND_qmc
  CASE(454); sigmafull=0.315E+01_KIND_qmc
  CASE(455); sigmafull=0.453E+01_KIND_qmc
  CASE(511); sigmafull=0.833E+01_KIND_qmc
  CASE(512); sigmafull=0.368E+01_KIND_qmc
  CASE(513); sigmafull=0.674E+01_KIND_qmc
  CASE(514); sigmafull=0.112E+01_KIND_qmc
  CASE(515); sigmafull=0.440E+01_KIND_qmc
  CASE(521); sigmafull=0.172E+01_KIND_qmc
  CASE(522); sigmafull=0.819E+01_KIND_qmc
  CASE(523); sigmafull=0.456E+01_KIND_qmc
  CASE(524); sigmafull=0.145E+01_KIND_qmc
  CASE(525); sigmafull=0.792E-01_KIND_qmc
  CASE(531); sigmafull=0.564E+01_KIND_qmc
  CASE(532); sigmafull=0.597E+00_KIND_qmc
  CASE(533); sigmafull=0.806E+01_KIND_qmc
  CASE(534); sigmafull=0.995E+01_KIND_qmc
  CASE(535); sigmafull=0.840E+01_KIND_qmc
  CASE(541); sigmafull=0.253E+01_KIND_qmc
  CASE(542); sigmafull=0.362E+01_KIND_qmc
  CASE(543); sigmafull=0.187E+01_KIND_qmc
  CASE(544); sigmafull=0.462E+01_KIND_qmc
  CASE(545); sigmafull=0.817E+01_KIND_qmc
  CASE(551); sigmafull=0.258E+01_KIND_qmc
  CASE(552); sigmafull=0.958E+01_KIND_qmc
  CASE(553); sigmafull=0.544E+01_KIND_qmc
  CASE(554); sigmafull=0.827E+01_KIND_qmc
  CASE(555); sigmafull=0.331E+01_KIND_qmc
  CASE DEFAULT; sigmafull=0.000E+01_KIND_qmc
END SELECT


!!--end--
END FUNCTION




END MODULE
