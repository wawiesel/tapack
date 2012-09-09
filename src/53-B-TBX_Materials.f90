!!# TOOLBOX MODULE <<TBX_Materials>>
MODULE TBX_Materials

!!## PURPOSE
!! Provide routines to manipulate materials even more
!! than provided in <USR_Materials>.

USE TBX_Mesh                             !!((15-B-TBX_Mesh.f90))
USE TBX_ComputationalGeometry            !!((09-A-TBX_ComputationalGeometry.f90))
USE FUN_NewFile                          !!((05-B-FUN_NewFile.f90))
USE VAR_EnergyGroups                     !!((47-B-VAR_EnergyGroups.f90))
USE PAR_Constants_Rdp,ONLY: c_4_times_PI !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_SelectColor                      !!((05-A-FUN_SelectColor.f90))
USE USR_Materials                        !!((47-B-USR_Materials.f90))
USE VAR_Materials                        !!((48-B-VAR_Materials.f90))
USE PAR_Colors_RGBA_sp                   !!((02-A-PAR_Colors_RGBA_sp.f90))
USE KND_XSExpansion                      !!((02-A-KND_XSExpansion.f90))
USE USR_fdbk                             !!((08-C-USR_fdbk.f90))
USE FUN_DEFAULT                          !!(())
USE SUB_REALLOCATE                       !!(())
USE FUN_STR                              !!((05-B-FUN_STR.f90))
USE FUN_SIZEa                            !!((06-B-FUN_SIZEa.f90))
USE FUN_IsError                          !!((05-A-FUN_IsError.f90))

!!## DEFAULT SETTINGS
IMPLICIT NONE
PRIVATE

!!## PUBLIC ACCESS LIST
PUBLIC :: OUTPUT_ColorMap
PUBLIC :: SETUP_MaterialFills
PUBLIC :: SETUP_Materials

!!## MODULE PROCEDURES
CONTAINS


!!### SUBROUTINE <<OUTPUT_Colormap>>
SUBROUTINE OUTPUT_ColorMap(Mesh)
!!#### PURPOSE
!! Output a file "ColorMapC.dat" which contains
!! RGBA values for each cell.

!!#### DETAILS
!! The file is a standard <*.dat> file, with a
!! header line:
!
!     4
!     NUM_Cells(Mesh)
!
!! and a body listing the red, green, blue, and
!! intensity components (RGBA standard) for each cell.
!! Each color component is on a line by itself for
!! a total of <1+4*NUM_Cells(Mesh)> lines in the file.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh

!!#### LOCAL VARIABLES
INTEGER :: Unit_colormap,l,i
REAL    :: color(4)

!!--begin--

 Unit_colormap = NewFile("ColorMapC.dat")
 WRITE(Unit_colormap,*)SIZE(color)
 WRITE(Unit_colormap,*)NUM_Cells(Mesh)
 DO i=1,NUM_Cells(Mesh)
  IF( .NOT.ASSOCIATED(l_) )THEN
   l = 0
  ELSE
   l = l_(i)
  END IF
  IF( l==0 )THEN
   color = RGBA_black
  ELSE
   color = SelectColor( LABEL_materials(l) )
  END IF
  DO l=1,SIZE(color)
   WRITE(Unit_colormap,"(f12.5)")color(l)
  END DO
 END DO
 CLOSE(unit_colormap)

END SUBROUTINE


!!### SUBROUTINE <<SETUP_MaterialFills>>
SUBROUTINE SETUP_MaterialFills( Mesh , MaterialFill , Split )

!!#### PURPOSE
!! Set up the material fills mapping array <l_>.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh)     ,INTENT(INOUT) :: Mesh
TYPE(TYPE_MaterialFill),POINTER    :: MaterialFill(:)

!!#### OPTIONAL INPUT
LOGICAL,OPTIONAL,INTENT(IN) :: Split

!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: proc_= "SETUP_MaterialFill"
LOGICAL     ,PARAMETER :: Noisy_=.FALSE.

!!#### LOCAL VARIABLES
INTEGER        :: i,a,npts_Pg,l
REAL(KIND_Rsp) :: color(1:4)
INTEGER        :: Unit_colormap
REAL(KIND_MSH) :: P_centroid(Mesh%NDim)
REAL(KIND_MSH) :: SAREA
LOGICAL        :: Split_

!!--begin--

!initialize
Split_ = DEFAULT( .FALSE. , Split )

!kick out
IF( .NOT.ASSOCIATED(MaterialFill) )RETURN

![hack] assume polygon verts are organized properly
![hack] assume material fills are polygons

IF( Split_ )THEN
 !split the cells loop
 DO a=1,SIZE(MaterialFill)

  !split the mesh across each polygon
  !calculate number of points
  npts_Pg = SIZE(MaterialFill(a)%coeff)/Mesh%NDim
  !send actual polygon shape into routine
  CALL Split_Mesh_Pg( Mesh , npts_Pg , xyPOLYGON_(MaterialFill(a)%coeff) )

 END DO

END IF

!determine l_(i) to include material fills
IF( .NOT.ASSOCIATED(l_) )THEN
 ALLOCATE( l_(NUM_Cells(Mesh)) )
 l_ = 0
ELSE
 CALL REALLOCATE( l_ , NUM_Cells(Mesh)-SIZE(l_) )
END IF

!put the fills into the materials->cells mapping
DO a=1,SIZE(MaterialFill)
 npts_Pg = SIZE(MaterialFill(a)%coeff)/Mesh%NDim
 SAREA      = xySAREA_Pg   ( npts_Pg , xyPOLYGON_(MaterialFill(a)%coeff) )
 P_centroid = xyCENTROID_Pg( npts_Pg , xyPOLYGON_(MaterialFill(a)%coeff) , SAREA )
 DO i=1,NUM_Cells(Mesh)
  !WRITE(*,*)"cell i=",i
  !WRITE(*,*)"centroid = ",CellCentroid(Mesh,i)
  IF( xyINTERIOR_PgP( npts_Pg , xyPOLYGON_(MaterialFill(a)%coeff) , CellCentroid(Mesh,i) , P_centroid ) )THEN
   !WRITE(*,*)"found material fill!"
   l_(i) = MaterialFill(a) % l
  END IF
 END DO
END DO

!write out materials colors file
CALL OUTPUT_ColorMap(Mesh)

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<SETUP_Materials>>
SUBROUTINE SETUP_Materials( Mesh , MAX_l,Nl,MacT,MacS,MacNu,MacF, &
  CoeffScalarFluxM , fdbk )

!!#### PURPOSE
!! Get materials ready.

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_Mesh),INTENT(INOUT) :: Mesh
INTEGER,INTENT(INOUT) :: MAX_l,Nl
REAL(KIND_Mac),POINTER :: MacT(:,:),MacS(:,:),MacNu(:,:),MacF(:,:)
REAL(KIND_Mac),POINTER :: CoeffScalarFluxM(:,:)

!!#### OPTIONAL INPUT/OUTPUT
!! * feedback variable [fdbk]
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
INTEGER :: g,MAX_i

!!--begin--

MAX_i = NUM_Cells(Mesh)

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MAT]] Determining material distribution...")

CALL SETUP_MaterialFills( Mesh , MaterialFill )

!! Print errors if the material mappings are not set up properly.
IF( .NOT.ASSOCIATED(l_) )THEN
 CALL UpdateAndDump(fdbk_error,fdbk,s="[[MAT]] The cell index to material &
   &mapping array [l_] has not been set.  The computations cannot continue.")
ELSE IF( ANY(l_(1:MAX_i)==0) )THEN
 CALL UpdateAndDump(fdbk_error,fdbk,s="[[MAT]] Some of the materials for cells are&
   & not set.  The computations cannot continue.")
END IF

MAX_l = SIZE(MacT,2)

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MAT]] Maximum number of materials is &
  &[MAX_l="//TRIM(STR(MAX_l))//"]")

!determine actual number of materials
Nl = 0
DO g=1,Ng
 Nl = MAX(SIZEa(MacT(g,:)),Nl)
END DO
CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MAT]] Actual number of materials is &
  &[Nl="//TRIM(STR(Nl))//"]")

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MAT]] Allocating material properties ...")

!scalar flux source coefficients
ALLOCATE( CoeffScalarFluxM(1:NG,1:Nl) )

!reallocate stuff to the right size
CALL REALLOCATE( MacT , (/0,Nl-SIZE(MacT )/) )
WHERE( IsError(MacT) )MacT=0._KIND_MAC

!reallocate stuff to the right size
CALL REALLOCATE( MacS , (/0,Nl-SIZE(MacS )/) )
WHERE( IsError(MacS) )MacS=0._KIND_MAC

CALL REALLOCATE( MacNu, (/0,Nl-SIZE(MacNu)/) )
WHERE( IsError(MacNu) )MacNu=0._KIND_MAC

CALL REALLOCATE( MacF , (/0,Nl-SIZE(MacF )/) )
WHERE( IsError(MacF) )MacF=0._KIND_MAC

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[MAT]] Setting isotropic scalar flux coefficients.")
CoeffScalarFluxM = (MacS + MacNu*MacF)/c_4_times_PI

CALL UpdateAndDump(fdbk_comment,fdbk,s=&
  "[[MAT]] finished setting up material data...")

!!--end--
END SUBROUTINE



END MODULE

