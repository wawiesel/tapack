!!## COMMAND CARD SWITCHBOARD: Source
MODULE CCS_Source
!!### PURPOSE
!! The command card switchboard (CCS) for the Source
!! (SRC) package.

!!### EXTERNAL PARAMETERS
USE PAR_MoCshort                                  !!((03-A-PAR_MoCshort.f90))

!!### FORTRAN STANDARDS MODULES
USE ISO_varying_string                            !!((03-A-ISO_varying_string.f90))

!!### EXTERNAL PROCEDURES
USE SUB_Pause                                     !!((04-B-SUB_Pause.f90))
USE FUN_LEN_TRIMa                                 !!((03-A-FUN_LEN_TRIMa.f90))
USE FUN_STR                                       !!((05-B-FUN_STR.f90))

!!### GLOBAL USER MODULES
!! @ simple pointer list USEr-module <SPList>
USE USR_fdbk                                      !!((08-C-USR_fdbk.f90))
USE USR_Source                                    !!((35-B-USR_Source.f90))
USE USR_SimpleList                                !!((09-B-USR_SimpleList.f90))
USE CC1_simple,ONLY: SRCanalytic=>simple          !!((11-A-CC1_simple.f90))


!!### GLOBAL LIBRARIES
USE LIB_Prompts                                   !!((06-B-LIB_Prompts.f90))

!!### GLOBAL TOOLBOXES
!! @ input/output toolbox
!! @ mesh toolbox
USE TBX_SIO                                       !!((10-A-TBX_SIO.f90))
USE TBX_Mesh                                      !!((15-B-TBX_Mesh.f90))

!!### GLOBAL VARIABLES
USE VAR_Source                                    !!((47-B-VAR_Source.f90))
USE VAR_EnergyGroups,ONLY: NG                     !!((47-B-VAR_EnergyGroups.f90))
USE VAR_Mesh        ,ONLY: Mesh                   !!((46-B-VAR_Mesh.f90))
USE VAR_QDAnalyticTest,ONLY: Using_AnalyticSource !!((33-C-VAR_QDAnalyticTest.f90))


!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE

!!### PRE-CALCULATED COMMAND HASH VALUES
INTEGER,PARAMETER :: polygon_  = 0000092670
INTEGER,PARAMETER :: analytic_ = 0000090642

!!### ACCESS
PUBLIC :: SWITCHBOARD_Source


!!## MODULE PROCEDURES
CONTAINS


!!### SUBROUTINE: SWITCHBOARD_Source
SUBROUTINE SWITCHBOARD_Source( sio , FdBk )
!!#### PURPOSE
!! The command switchboard for the <EnergyGroups>
!! package.

!!#### REQUIRED INPUT/OUTPUT
!! @ input/output object [sio]
TYPE(TYPE_sio),POINTER :: sio

!!#### OPTIONAL INPUT/OUTPUT
!! @ feedback object [FdBk]
TYPE(TYPE_FdBk),OPTIONAL,INTENT(INOUT) :: FdBk

!!--begin--
!pick routine to execute which loads some data
SELECT CASE(sio%HASH(2))

 CASE(polygon_)   ; CALL SRCpolygon( sio , LABEL_sources , Source , NG , NUM_Dimensions(Mesh) , FdBk )
 CASE(analytic_)  ; CALL SRCanalytic( sio , Using_AnalyticSource , FdBk )
                    IF( Using_AnalyticSource )THEN
                     Using_AnalyticExtSourceOnly = .TRUE.
                    END IF
 CASE DEFAULT    ; WRITE(*,"(a)")WarningPrompt//"SRC (source) command: \"//TRIM(STR(sio%cmd))&
                      //", NOT RECOGNIZED ON LINE=",TRIM(STR(sio%LINE_NUM))//" of FILE="//TRIM(STR(sio%FILE))
END SELECT

!!--end--
END SUBROUTINE


SUBROUTINE SRCpolygon( sio , LABEL_sources , Source , NG , ndimensions , fdbk )
!!#### PURPOSE
!! Defines a polygon of source strength source.

!!#### REQUIRED INPUT/OUTPUT
!! @ input/output object [sio]
!! @ list of source labels [LABEL_sources]
TYPE(TYPE_sio)           ,POINTER :: sio
CHARACTER(*)            ,POINTER :: LABEL_sources(:)
TYPE(TYPE_Source)        ,POINTER :: Source(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: NG
INTEGER,INTENT(IN) :: Ndimensions

!!#### OPTIONAL INPUT/OUTPUT
!! @ feedback object [fdbk]
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### LOCAL VARIABLES
CHARACTER(LEN(LABEL_sources)) :: string
INTEGER :: n
LOGICAL :: SourceHasGradient

!!--begin--
!if writing
IF( Writing(sio) )THEN

 !set label string
 string = LABEL_sources(LEN_TRIMa(LABEL_sources))

 !take the source away from the list of source names
 CALL REMOVE_FROM_LIST( LABEL_sources , string )

 !source has gradient
 SourceHasGradient = ANY( Source(SIZE(Source))%strength1/=0.d0 )

!if reading
ELSE IF( Reading(sio) )THEN

 !reallocate
 CALL Reallocate_Source( Source , 1 , NG , NDimensions )

END IF



!A. ARGUMENTS: read/write the source label, strength,
!!  number of points
CALL BEGIN_ARGUMENTS( sio ,(/"source-label   ",&
                             "source-strength",&
                             "num-points     ",&
                             "source-gradient" /), FdBk , &
                             nsubarg=(/0,NG,0,0/) , &
                             optional=(/.FALSE.,.FALSE.,.FALSE.,.TRUE./) )
CALL ARGUMENT( sio , string , FdBk )
CALL ARGUMENT( sio , Source(SIZE(Source))%strength0(1:NG) , FdBk )
CALL ARGUMENT( sio , n , FdBk )
CALL ARGUMENT( sio , SourceHasGradient , FdBk , Default=.FALSE.)
CALL END_ARGUMENTS( sio , FdBk )

!B. DATABLOCK: read/write the coefficients of the polygon
!! (just points of the polygon)
CALL DATABLOCK( sio , (/1,Ndimensions/) , (/1,n/) , Source(SIZE(source))%coeff , FdBk )
IF( SourceHasGradient )THEN
 CALL DATABLOCK( sio , (/1,Ndimensions/) , (/1,NG/) , Source(SIZE(source))%strength1 , FdBk )
END IF

IF( Reading(sio) )THEN
 !add the source name to the list of source names
 CALL ADD_TO_LIST( LABEL_sources , string )
END IF

!!--end--
END SUBROUTINE


END MODULE
