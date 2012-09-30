!!# PROGRAM >>meshgen<<
PROGRAM meshgen

!!## PURPOSE
!! Generate a mesh from a TAPACK input file with only MSH commands.

!!## MODULES
USE KND_IntrinsicTypes         !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp          !!((02-A-PAR_Constants_Rdp.f90))
USE USR_SIO                    !!((09-A-USR_SIO.f90))
USE TBX_SIO                    !!((10-A-TBX_SIO.f90))
USE USR_Mesh                   !!((14-B-USR_Mesh.f90))
USE TBX_Mesh                   !!((15-B-TBX_Mesh.f90))
USE CCS_Mesh                   !!((47-B-CCS_Mesh.f90))
USE SUB_InputLoop              !!((80-C-SUB_InputLoop.f90))
USE SUB_SIOSetGrammar_LaTeXish !!((11-A-SUB_SIOSetGrammar_LaTeXish.f90))
USE FUN_getArgFile             !!((11-C-FUN_getArgFile.f90))
USE USR_fdbk                   !!((08-C-USR_fdbk.f90))
USE FUN_STR                    !!((05-B-FUN_STR.f90))
use VAR_Mesh                   !!((46-B-VAR_Mesh.f90))
use PRN_Mesh                   !!((16-C-PRN_Mesh.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## INTERFACES
INTERFACE
 SUBROUTINE SWITCHBOARD(SIO,fdbk)
 USE USR_fdbk                                  !!((08-C-USR_fdbk.f90))
 USE USR_SIO                                   !!((09-A-USR_SIO.f90))
 TYPE(TYPE_sio),POINTER :: SIO
 TYPE(TYPE_fdbk),INTENT(INOUT),OPTIONAL :: fdbk
 END SUBROUTINE
END INTERFACE


!!## LOCAL VARIABLES
!TYPE(TYPE_Mesh) :: Mesh
TYPE(TYPE_SIO),pointer  :: SIO
TYPE(TYPE_fdbk) :: fdbk
TYPE(varying_string) :: file
INTEGER :: unit
INTEGER :: Nc
INTEGER,ALLOCATABLE :: l_(:)
CHARACTER(32),ALLOCATABLE :: LABEL_Materials(:)
INTEGER :: i,Ni
CHARACTER(120) :: outfile


!!--begin--
!Ni=80000000 !maximum we can have here
!ALLOCATE(LABEL_Materials(Ni))
!DO i=1,Ni
! LABEL_MATERIALS(i)='ABCDEFG'
!END DO
!READ(*,*)

SIO=>null()

!get the file name (but close it because input loop automatically opens)
unit = getArgFile(FILE=file)
CLOSE(unit)

!! * Connect to the control file.
CALL CONNECT(SIO,ACTION  = "Read"      ,&
                 FORM    = "Formatted" ,&
                 FILE    = TRIM(STR(file))  ,&
                 ACCESS  = "Sequential",&
                 fdbk    = fdbk)
CALL SIOSetGrammar_LaTeXish(SIO)
CALL DUMP(fdbk)

CALL InputLoop(SIO,TRIM(STR(file)),SWITCHBOARD_Mesh,fdbk)
WRITE(*,*)'done with input loop'

!allocate the cell material and labels array
Nc=NUM_Cells(Mesh)
ALLOCATE(l_(Nc),LABEL_Materials(1))
l_=1
LABEL_Materials(1)='default'

!gmv file output
outfile=trim(str(file))//'.gmv';

open(unit=unit,file=trim(outfile))
CALL gmvInitialize(Unit,Mesh,l_,LABEL_Materials)
CALL gmvFinalize(Unit,Mesh)
close(unit)
write(*,*)'after gmv output=',trim(outfile)

!text file output
!cannot print more than 64x64 mesh
if( Mesh%NCells<64*64 )then
open(unit=unit,file=trim(str(file))//'.out')
CALL PRINT_Mesh(Mesh,unit)
close(unit)
end if
!write(*,*)'after print_mesh'

WRITE(*,*)"finished."
!!--end--
END PROGRAM
