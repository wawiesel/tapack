!!# MODULE <<TBX_ComputationalGeometry>>
MODULE TBX_ComputationalGeometry

!!## PURPOSE
!! The geometry toolkit defines many geometrical operations such as dot products,
!! cross products, intersections, mappings, and distance, area , and volume calculations
!! for various common shapes such as triangles, quadrilaterals, polygons, circles,
!! tetrahedrons, boxes, and spheres.
!
!! The world is assumed to be described by cartesian x-y-z geometry, but objects in the
!! world may be described with cartesian geometry, spherical r-p-a
!! (radius-polar angle-azimuthal angle) geometry, or cylindrical r-h-p (radius-height-polar angle)
!! geometry.


!!## NOMENCLATURE
!! This section attempts to describe the various nomenclature you will encounter
!! in this geometry toolkit.

!! * SCALARS
!!   These variable names indicate scalars with various properties.
!!   s,d   : scalar real
!!   x     : x-component value
!!   y     : y-component value
!!   z     : z-component value
!!   r     : radius value
!!   rsqr  : radius squared value
!!   p     : polar angle value
!!   a     : azimuthal angle value
!!   h     : height value
!!   N     : number (integer)
!!   DIST  : distance
!!   AREA  : area
!!   VOLU  : volume
!!   sDIST : signed distance
!!   sAREA : signed area

!! * ALGORITHM SPECIFICATION I: operator
!!   The following operators exist in the geometry toolkit.
!!   DOT_       : dot product of two vectors
!!   CROSS_     : cross product of two vectors
!!   NORM_      : normalization of two vectors
!!   AREA_      : area calculation for a surface
!!  sAREA_      : signed area calculation for a surface
!!   DIST_      : distance calculation
!!  sDIST_      : signed distance calculation
!!   VOLU_      : volume calculation
!!   CENTROID_  : centroid calculation
!!   INTERSECT_ : intersection calculation

!!## EXTERNAL PARAMETERS
USE PAR_ComputationalGeometry !!((02-A-PAR_ComputationalGeometry.f90))

!!## BASIC ROUTINES
!! * number of dimensions for rectangular geometry variables in 1D, 2D, and 3D.
USE FUN_NDIM                  !!((08-B-FUN_NDIM.f90))

!!### 2-d calculational routines
USE FUN_xyANGLE               !!((05-B-FUN_xyANGLE.f90))
USE FUN_xyCENTROID            !!((05-B-FUN_xyCENTROID.f90))
USE FUN_xyCOLINEAR            !!((05-B-FUN_xyCOLINEAR.f90))
USE FUN_xyCONTIGUOUS          !!((06-B-FUN_xyCONTIGUOUS.f90))
USE FUN_xyCONVEXHULL          !!((07-B-FUN_xyCONVEXHULL.f90))
USE FUN_xyCOPLANAR            !!((06-B-FUN_xyCOPLANAR.f90))
USE FUN_xyDIRECTION           !!((04-B-FUN_xyDIRECTION.f90))
USE FUN_xyDIST                !!((04-B-FUN_xyDIST.f90))
USE FUN_rpDOT                 !!((03-A-FUN_rpDOT.f90))
USE FUN_xyDOT                 !!((03-A-FUN_xyDOT.f90))
USE FUN_xyINTERIOR            !!((07-B-FUN_xyINTERIOR.f90))
USE FUN_xyINTERSECT           !!((06-A-FUN_xyINTERSECT.f90))
USE FUN_xyLINE                !!((05-B-FUN_xyLINE.f90))
USE FUN_xyLINESEGMENT         !!((05-B-FUN_xyLINESEGMENT.f90))
USE FUN_xyNORM                !!((03-A-FUN_xyNORM.f90))
USE FUN_xyPERPCW              !!((04-A-FUN_xyPERPCW.f90))
USE FUN_xyPERPCCW             !!((03-A-FUN_xyPERPCCW.f90))
USE FUN_xyPLANE               !!((05-B-FUN_xyPLANE.f90))
USE FUN_xyPOINT               !!((05-B-FUN_xyPOINT.f90))
USE FUN_xyPOLYGON             !!((08-A-FUN_xyPOLYGON.f90))
USE FUN_xyQUADRILATERAL       !!((08-A-FUN_xyQUADRILATERAL.f90))
USE FUN_xyRAY                 !!((05-B-FUN_xyRAY.f90))
USE FUN_xyREFLECT             !!((05-B-FUN_xyREFLECT.f90))
USE FUN_xyROTATE              !!((05-B-FUN_xyROTATE.f90))
USE FUN_xySAREA               !!((03-A-FUN_xySAREA.f90))
USE FUN_xySDIST               !!((03-A-FUN_xySDIST.f90))
USE FUN_xyTRIANGULATE         !!((07-B-FUN_xyTRIANGULATE.f90))
USE FUN_xyUNITVECTOR          !!((04-A-FUN_xyUNITVECTOR.f90))
USE FUN_xyVECTOR              !!((03-A-FUN_xyVECTOR.f90))
USE FUN_xyINTEGRAL1           !!((06-A-FUN_xyINTEGRAL1.f90))
USE FUN_xyINTEGRALX           !!((07-A-FUN_xyINTEGRALX.f90))
USE FUN_xyINTEGRALY           !!((07-A-FUN_xyINTEGRALY.f90))
USE FUN_xyINTEGRALF           !!((08-A-FUN_xyINTEGRALF.f90))
USE FUN_xyNORMSQRD            !!((03-A-FUN_xyNORMSQRD.f90))
USE FUN_xyINTEGRALF           !!((08-A-FUN_xyINTEGRALF.f90))

!!### 3-d calculational routines
USE FUN_xyzSAREA              !!((06-B-FUN_xyzSAREA.f90))
USE FUN_xyzPLANE              !!((06-B-FUN_xyzPLANE.f90))
USE FUN_xyzINTERSECT          !!((07-B-FUN_xyzINTERSECT.f90))
USE FUN_xyzDOT                !!((05-B-FUN_xyzDOT.f90))
USE FUN_xyzCROSS              !!((05-B-FUN_xyzCROSS.f90))
USE FUN_xyzCENTROID           !!((07-B-FUN_xyzCENTROID.f90))
USE FUN_xyzPLANE              !!((06-B-FUN_xyzPLANE.f90))
USE FUN_xyzCOPLANAR           !!((07-B-FUN_xyzCOPLANAR.f90))
USE FUN_xyzSDIST              !!((06-B-FUN_xyzSDIST.f90))

!!### allocation routines
USE LIB_ALLOCATE_Shape        !!((03-A-LIB_ALLOCATE_Shape.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PUBLIC

END MODULE
