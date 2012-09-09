!!# PRINTING MODULE <<PRN_QuasiDiffusion>>
MODULE PRN_QuasiDiffusion

!!## PURPOSE
!! The printing-module for the quasidiffusion routines.

!!## EXTERNAL KINDS
USE KND_QuasiDiffusion   ,ONLY: KIND_QDF                 !!((02-A-KND_QuasiDiffusion.f90))
USE KND_DiscreteOrdinates,ONLY: KIND_DOR                 !!((02-A-KND_DiscreteOrdinates.f90))

!!## EXTERNAL PARAMETERS
USE PAR_QuasiDiffusion                                   !!((05-C-PAR_QuasiDiffusion.f90))

!!## GLOBAL ASSIGNMENTS
USE ASN_IntrinsicS                                       !!((05-B-ASN_IntrinsicS.f90))
USE VAR_Units                                            !!((03-A-VAR_Units.f90))

!!## EXTERNAL PROCEDURES
USE SUB_CLEAR                                            !!((04-A-SUB_CLEAR.f90))
USE SUB_CLEARn                                           !!((04-A-SUB_CLEARn.f90))
USE SUB_Pause                                            !!((04-B-SUB_Pause.f90))
USE SUB_Reallocate                                       !!((04-B-SUB_Reallocate.f90))
USE SUB_SAVE                                             !!((06-B-SUB_SAVE.f90))
USE SUB_Stop                                             !!((04-B-SUB_Stop.f90))
USE FUN_Default                                          !!((04-A-FUN_Default.f90))
USE FUN_EQUILOC                                          !!((03-A-FUN_EQUILOC.f90))
USE FUN_Error                                            !!((04-A-FUN_Error.f90))
USE FUN_STR                                              !!((05-B-FUN_STR.f90))
USE FUN_NewFile                                          !!((05-B-FUN_NewFile.f90))
USE FUN_VSTR                                             !!((05-B-FUN_VSTR.f90))
USE FUN_Substitute                                       !!((06-C-FUN_Substitute.f90))

!!## GLOBAL PRINTING SUBROUTINES
USE PRN_Text                                             !!((07-B-PRN_Text.f90))
USE PRN_Matrix_dmat                                      !!((09-B-PRN_Matrix_dmat.f90))
USE PRN_MatrixSymbolic_dmat                              !!((03-A-PRN_MatrixSymbolic_dmat.f90))
USE PRN_Grid2                                            !!((09-B-PRN_Grid2.f90))
USE PRN_Table                                            !!((11-B-PRN_Table.f90))

!!## FORTRAN STANDARDS
USE ISO_varying_string                                   !!((03-A-ISO_varying_string.f90))

!!## USER MODULES
!! * feedback user module
USE USR_fdbk                                             !!((08-C-USR_fdbk.f90))

!!## GLOBAL TOOLBOXES
!! * SMLib sparse matrix computation
USE USR_SMlib_Matrix_Arithmetic                          !!((19-B-USR_SMlib_Matrix_Arithmetic.f90))

!!## GLOBAL USER MODULES
USE USR_DiscreteOrdinates                                !!((34-B-USR_DiscreteOrdinates.f90))
USE USR_QDAnalyticTest                                   !!((47-C-USR_QDAnalyticTest.f90))
USE USR_QuasiDiffusion                                   !!((67-B-USR_QuasiDiffusion.f90))
USE USR_TAPACK,ONLY: RECONSTRUCT_CurrentC                !!((48-C-USR_TAPACK.f90))

!!## GLOBAL LIBRARIES
USE LIB_Prompts                                          !!((06-B-LIB_Prompts.f90))
USE LIB_GenericPhrases                                   !!((07-B-LIB_GenericPhrases.f90))
USE LIB_Norm                                             !!((04-B-LIB_Norm.f90))

!!## GLOBAL TOOLBOXES
USE USR_Mesh                                             !!((14-B-USR_Mesh.f90))
USE TBX_Mesh                                             !!((15-B-TBX_Mesh.f90))
![novis]!USE PLT_Mesh                                    !!((15-C-PLT_Mesh.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## LOCAL KINDS
INTEGER,PARAMETER :: KIND_AngularFlux = KIND_QDF
INTEGER,PARAMETER :: KIND_ScalarFlux  = KIND_QDF
INTEGER,PARAMETER :: KIND_Quadrature  = KIND_QDF

!!## DEFAULT ACCESS
PRIVATE

!!## PUBLIC ACCESS LIST
PUBLIC :: PRINT_LO_SystemInfo
PUBLIC :: PRINT_IterTable
PUBLIC :: PRINT_QuasiDiffusion_Adap3
PUBLIC :: PRINT_TestFunctionals

PUBLIC :: GNUPLOT_LO_ScalarFluxF
PUBLIC :: GNUPLOT_LO_ScalarFluxC
PUBLIC :: GNUPLOT_LO_CurrentFN
PUBLIC :: GNUPLOT_LO_CurrentC
PUBLIC :: GNUPLOT_LO_ScalarFluxTri

!!## MODULE PROCEDURES
CONTAINS


SUBROUTINE PRINT_MatrixAnalysis_CSR( ao , jao , iao , Unit , &
  Title , Key , Type )
!!#### PURPOSE
!! Print info about a sparse matrix using
!! SPARSKIT info subroutine.

USE SUB_SPARSKIT_dinfo                                   !!((06-A-SUB_SPARSKIT_dinfo.f))
USE SUB_SPARSKIT_texplt                                  !!((08-A-SUB_SPARSKIT_texplt.f))
USE LIB_SPARSKIT_ccn                                     !!((05-A-LIB_SPARSKIT_ccn.f))

!!#### REQUIRED INPUT
REAL(KIND_QDF),POINTER :: ao(:)
INTEGER       ,POINTER :: iao(:),jao(:)

!!#### OPTIONAL INPUT
INTEGER,INTENT(IN),OPTIONAL :: Unit
CHARACTER(72),INTENT(IN),OPTIONAL :: title
CHARACTER(8),INTENT(IN),OPTIONAl :: key
CHARACTER(3),INTENT(IN),OPTIONAL :: type


!!#### LOCAL VARIABLES
REAL(KIND_QDF),POINTER :: aw(:)
INTEGER       ,POINTER :: iaw(:),jaw(:)
LOGICAL :: valued
INTEGER :: Unit_,n
CHARACTER(72) :: title_
CHARACTER(8) :: key_
CHARACTER(3) :: type_
CHARACTER(72),PARAMETER :: DEFAULT_TITLE = " SPARSKIT Matrix Analysis "
CHARACTER(8),PARAMETER :: DEFAULT_KEY = "       "
CHARACTER(3),PARAMETER :: DEFAULT_TYPE = "UNK"
!!--begin--
!set defaults
Unit_  = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )
Title_ = DEFAULT( DEFAULT_TITLE , Title )
Key_   = DEFAULT( DEFAULT_KEY , Key )
Type_  = DEFAULT( DEFAULT_TYPE , Type )

!allocate
n = SIZE(iao)-1
ALLOCATE( aw(SIZE(ao)) , jaw(SIZE(jao))  , iaw(SIZE(iao)))

!set valued
valued = ASSOCIATED(ao)

!call info
call dinfo(n,Unit_,ao,jao,iao,valued,title_,key_,type_,aw,jaw,iaw)

!wrapup
DEALLOCATE( aw , jaw , iaw )

!!--end--
END SUBROUTINE



SUBROUTINE PRINT_QuasiDiffusion_Adap3(&
  ScalarFluxF,&
  CurrentF,&
  ScalarFluxC,&
  EVxx,EVyy,EVxy,&
  FExx,FEyy,FExy,&
  CExx,CEyy,CExy,fdbk,Unit)
!!#### PURPOSE
!! Print the 2D Unstructured3 mesh (3 is for three cells make up the mesh)
!! output in an easy to understand format.

!!#### REQUIRED INPUT
REAL(KIND_QDF) ,POINTER :: ScalarFluxF(:,:)
REAL(KIND_QDF) ,POINTER :: ScalarFluxC(:,:)
REAL(KIND_QDF) ,POINTER :: CurrentF(:,:)
REAL(KIND_QDF) ,POINTER :: EVxx(:,:),EVyy(:,:),EVxy(:,:)
REAL(KIND_QDF) ,POINTER :: FExx(:,:),FEyy(:,:),FExy(:,:)
REAL(KIND_QDF) ,POINTER :: CExx(:,:),CEyy(:,:),CExy(:,:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: g,Ng,Unit_
REAL(KIND_QDF) :: EV(1:8)

!!--begin--
!! Get unit.
Unit_ = DEFAULT( DEFAULT_OUTPUT_UNIT , Unit )

!! Print the mesh.
CALL PRINT_Mesh_Adap3( fdbk , Unit=Unit )

!! Print each energy group.
Ng = SIZE(ScalarFluxC,1)
DO g=1,Ng
 WRITE(Unit_,"(a,i3)")"  * Energy Group, g=",g
 IF( ASSOCIATED(EVxx) )THEN
  CALL PRINT_QuasiDiffusion_Adap3_1(&
   ScalarFluxF(g,:),&
   CurrentF(g,:),&
   ScalarFluxC(g,:),&
   EVxx(g,:),EVyy(g,:),EVxy(g,:),&
   FExx(g,:),FEyy(g,:),FExy(g,:),&
   CExx(g,:),CEyy(g,:),CExy(g,:),fdbk,Unit=Unit_)
 ELSE
  EV = ERROR(EV)
  CALL PRINT_QuasiDiffusion_Adap3_1(&
   ScalarFluxF(g,:),&
   CurrentF(g,:),&
   ScalarFluxC(g,:),&
   EV,EV,EV,&
   FExx(g,:),FEyy(g,:),FExy(g,:),&
   CExx(g,:),CEyy(g,:),CExy(g,:),fdbk,Unit=Unit_)
 END IF
END DO

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_QuasiDiffusion_Adap3_1(&
  ScalarFluxF,&
  CurrentF,&
  ScalarFluxC,&
  EVxx,EVyy,EVxy,&
  FExx,FEyy,FExy,&
  CExx,CEyy,CExy,fdbk,Unit)
!!#### PURPOSE
!! Print the 2D Unstructured3 mesh (3 is for three cells make up the mesh)
!! output in an easy to understand format.

!!#### REQUIRED INPUT
REAL(KIND_QDF) ,INTENT(IN) :: ScalarFluxF(:)
REAL(KIND_QDF) ,INTENT(IN) :: ScalarFluxC(:)
REAL(KIND_QDF) ,INTENT(IN) :: CurrentF(:)
REAL(KIND_QDF) ,INTENT(IN) :: EVxx(:),EVyy(:),EVxy(:)
REAL(KIND_QDF) ,INTENT(IN) :: FExx(:),FEyy(:),FExy(:)
REAL(KIND_QDF) ,INTENT(IN) :: CExx(:),CEyy(:),CExy(:)

!!#### OPTIONAL INPUT/OUTPUT
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
CHARACTER(30) :: Table(11,11)


!!--begin--
!start with clear table
CALL CLEAR(table)

!! Eddington xx Factors.
!!
!! * Verts
Table(11,01) = EVxx(001)
Table(11,05) = EVxx(006)
Table(11,06) = EVxx(006)
Table(11,10) = EVxx(002)
Table(01,01) = EVxx(004)
Table(01,05) = EVxx(007)
Table(01,06) = EVxx(007)
Table(01,10) = EVxx(003)
Table(05,01) = EVxx(008)
Table(07,01) = EVxx(008)
Table(05,05) = EVxx(005)
Table(07,05) = EVxx(005)
!!
!! * Print and clear the table.
WRITE(Unit,"(a)")"    * Exx vertex factors"
CALL PRINT_Table(Table,Unit=Unit)
CALL CLEAR(Table)

!! * Cell 1
Table(11,03) = FExx(001)
Table(09,05) = FExx(008)
Table(07,03) = FExx(012)
Table(09,01) = FExx(007)
Table(09,03) = CExx(001)
!!
!! * Cell 3
Table(05,03) = FExx(011)
Table(03,05) = FExx(010)
Table(01,03) = FExx(005)
Table(03,01) = FExx(006)
Table(03,03) = CExx(003)
!!
!! * Cell 2
Table(11,08) = FExx(002)
Table(06,10) = FExx(003)
Table(01,08) = FExx(004)
Table(06,06) = FExx(009)
Table(06,08) = CExx(002)
!!
!! * Print and clear the table.
WRITE(Unit,"(a)")"    * Exx face/cell factors"
CALL PRINT_Table(Table,Unit=Unit)
CALL CLEAR(Table)



!! Eddington yy Factors.
!!
!! * Verts
Table(11,01) = EVyy(001)
Table(11,05) = EVyy(006)
Table(11,06) = EVyy(006)
Table(11,10) = EVyy(002)
Table(01,01) = EVyy(004)
Table(01,05) = EVyy(007)
Table(01,06) = EVyy(007)
Table(01,10) = EVyy(003)
Table(05,01) = EVyy(008)
Table(07,01) = EVyy(008)
Table(05,05) = EVyy(005)
Table(07,05) = EVyy(005)
!!
!! * Print and clear the table.
WRITE(Unit,"(a)")"    * Eyy vertex factors"
CALL PRINT_Table(Table,Unit=Unit)
CALL CLEAR(Table)

!! * Cell 1
Table(11,03) = FEyy(001)
Table(09,05) = FEyy(008)
Table(07,03) = FEyy(012)
Table(09,01) = FEyy(007)
Table(09,03) = CEyy(001)
!!
!! * Cell 3
Table(05,03) = FEyy(011)
Table(03,05) = FEyy(010)
Table(01,03) = FEyy(005)
Table(03,01) = FEyy(006)
Table(03,03) = CEyy(003)
!!
!! * Cell 2
Table(11,08) = FEyy(002)
Table(06,10) = FEyy(003)
Table(01,08) = FEyy(004)
Table(06,06) = FEyy(009)
Table(06,08) = CEyy(002)
!!
!! * Print and clear the table.
WRITE(Unit,"(a)")"    * Eyy face/cell factors"
CALL PRINT_Table(Table,Unit=Unit)
CALL CLEAR(Table)





!! Eddington xy Factors.
!!
!! * Verts
Table(11,01) = EVxy(001)
Table(11,05) = EVxy(006)
Table(11,06) = EVxy(006)
Table(11,10) = EVxy(002)
Table(01,01) = EVxy(004)
Table(01,05) = EVxy(007)
Table(01,06) = EVxy(007)
Table(01,10) = EVxy(003)
Table(05,01) = EVxy(008)
Table(07,01) = EVxy(008)
Table(05,05) = EVxy(005)
Table(07,05) = EVxy(005)
!!
!! * Print and clear the table.
WRITE(Unit,"(a)")"    * Exy vertex factors"
CALL PRINT_Table(Table,Unit=Unit)
CALL CLEAR(Table)


!! * Cell 1
Table(11,03) = FExy(001)
Table(09,05) = FExy(008)
Table(07,03) = FExy(012)
Table(09,01) = FExy(007)
Table(09,03) = CExy(001)
!!
!! * Cell 3
Table(05,03) = FExy(011)
Table(03,05) = FExy(010)
Table(01,03) = FExy(005)
Table(03,01) = FExy(006)
Table(03,03) = CExy(003)
!!
!! * Cell 2
Table(11,08) = FExy(002)
Table(06,10) = FExy(003)
Table(01,08) = FExy(004)
Table(06,06) = FExy(009)
Table(06,08) = CExy(002)
!!
!! * Print and clear the table.
WRITE(Unit,"(a)")"    * Exy face/cell factors"
CALL PRINT_Table(Table,Unit=Unit)
CALL CLEAR(Table)



!! Face/Cell Scalar fluxes.
!!
!! * Cell 1
Table(11,03) = ScalarFluxF(001)
Table(09,05) = ScalarFluxF(008)
Table(07,03) = ScalarFluxF(012)
Table(09,01) = ScalarFluxF(007)
Table(09,03) = ScalarFluxC(001)
!!
!! * Cell 3
Table(05,03) = ScalarFluxF(011)
Table(03,05) = ScalarFluxF(010)
Table(01,03) = ScalarFluxF(005)
Table(03,01) = ScalarFluxF(006)
Table(03,03) = ScalarFluxC(003)
!!
!! * Cell 2
Table(11,08) = ScalarFluxF(002)
Table(06,10) = ScalarFluxF(003)
Table(01,08) = ScalarFluxF(004)
Table(06,06) = ScalarFluxF(009)
Table(06,08) = ScalarFluxC(002)

!! Print the table.
WRITE(Unit,"(a)")"    * ScalarFluxF/ScalarFluxC"
CALL Print_Table( Table , Unit=Unit )
CALL CLEAR(Table)


!! Face Projected Currents.
!!
!! * Cell 1
Table(11,03) = CurrentF(001)
Table(09,05) = CurrentF(008)
Table(07,03) = CurrentF(012)
Table(09,01) = CurrentF(007)
!!
!! * Cell 3
Table(05,03) = CurrentF(011)
Table(03,05) = CurrentF(010)
Table(01,03) = CurrentF(005)
Table(03,01) = CurrentF(006)
!!
!! * Cell 2
Table(11,08) = CurrentF(002)
Table(06,10) = CurrentF(003)
Table(01,08) = CurrentF(004)
Table(06,06) = CurrentF(009)

!! Print the table.
WRITE(Unit,"(a)")"    * CurrentF"
CALL Print_Table( Table , Unit=Unit )
CALL CLEAR(Table)

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_TestFunctionals(Mesh,ScalarFluxF,&
  ScalarFluxC,EddingtonF,EddingtonC,CurrentF,iter)
!!#### PURPOSE
!! Print out some test functionals.

!!#### REQUIRED INPUT
TYPE(TYPE_Mesh),INTENT(IN) :: Mesh
REAL(KIND_QDF) ,INTENT(IN) :: ScalarFluxF(:,:)
REAL(KIND_QDF) ,INTENT(IN) :: ScalarFluxC(:,:)
REAL(KIND_QDF) ,INTENT(IN) :: EddingtonF(:,:)
REAL(KIND_QDF) ,INTENT(IN) :: EddingtonC(:,:)
REAL(KIND_QDF) ,INTENT(IN) :: CurrentF(:,:)
INTEGER        ,INTENT(IN) :: iter

!!#### LOCAL VARIABLES
INTEGER :: Unit
CHARACTER(32),POINTER :: D(:,:)
INTEGER :: n,j,i

!!--begin--
!!Cell-average normalized fractions
Unit = NewFile("D_"//TRIM(STR(iter)) )
CALL CLEARn(D)
ALLOCATE( D(0:NUM_Faces(Mesh)*NUM_Cells(Mesh),5) )
CALL CLEAR(D)
D(0,1:5) = (/"j              ",&
             "i              ",&
             "EF(j)/EC(i)    ",&
             "PhiF(j)/PhiC(i)",&
             "JF(j)/PhiC(i)  "/)
n = 0
DO i=1,NUM_Cells(Mesh)
 DO j=1,NUM_Faces(Mesh)
  n = n + 1
  D(n,1) = TRIM(STR(j))
  D(n,2) = TRIM(STR(i))
  D(n,4) = STR(EddingtonF(1,j)/EddingtonC(1,i))
  D(n,3) = STR(ScalarFluxF(1,j)/ScalarFluxC(1,i))
  D(n,5) = STR(CurrentF(1,j)/ScalarFluxC(1,i))
 END DO
END DO
CALL Print_Table(D,Unit=Unit)
CLOSE( Unit )

!!Boundary-like normalized fractions
Unit = NewFile("C_"//TRIM(STR(iter)) )
CALL CLEARn(D)
ALLOCATE( D(0:NUM_Faces(Mesh),2) )
CALL CLEAR(D)
D(0,1:2) = (/"j            ",&
             "JF(j)/PhiF(j)"/)
n = 0
DO j=1,NUM_Faces(Mesh)
 n = n + 1
 D(n,1) = TRIM(STR(j))
 D(n,2) = STR(CurrentF(1,j)/ScalarFluxF(1,j))
END DO
CALL Print_Table(D,Unit=Unit)
CLOSE( Unit )
CALL CLEARn(D)

!!--end--
END SUBROUTINE




SUBROUTINE PRINT_LO_SystemInfo(A,System,Unit)
!!#### PURPOSE
!! Print some information from the low-order matrix.

!!#### REQUIRED INPUT
TYPE(MSR),INTENT(IN) :: A
INTEGER  ,INTENT(IN) :: System

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
REAL(KIND_QDF),POINTER :: ao(:)
INTEGER       ,POINTER :: iao(:),jao(:)
INTEGER :: Unit_
!!--begin--
Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)

!initialize
NULLIFY( ao,iao,jao )

!get matrix in CSR format
CALL MSR_to_CSR( A , ao , jao , iao , Test=.FALSE. )

!send matrix into SPARSKIT
CALL PRINT_MatrixAnalysis_CSR( ao , jao , iao , Unit=Unit_ , &
  KEY="System"//TRIM(STR(System,"(i5.2)")),TYPE="UNK")

!wrapup
DEALLOCATE( ao , iao , jao )

!!--end--
END SUBROUTINE


SUBROUTINE PRINT_IterTable( IterTable , Unit )
!!#### PURPOSE
!! Print out the iterative table.

!!#### REQUIRED INPUT
CHARACTER(*),POINTER :: IterTable(:,:)

!!#### OPTIONAL INPUT
INTEGER,OPTIONAL,INTENT(IN) :: Unit

!!#### LOCAL VARIABLES
INTEGER :: Unit_

!!--begin--

Unit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,Unit)
IF( ASSOCIATED(IterTable) )THEN
 CALL PRINT_Table( IterTable , Unit=Unit_ )
ELSE
 WRITE(Unit_,"(a)")"No output because IterTable was not allocated."
END IF

!!--end--
END SUBROUTINE



SUBROUTINE GNUPLOT_LO_ScalarFluxF(Unit)
USE VAR_ScalarFluxes  ,ONLY: LO_ScalarFluxF              !!((04-C-VAR_ScalarFluxes.f90))
USE VAR_QuasiDiffusion,ONLY: LO_Mesh                     !!((46-B-VAR_QuasiDiffusion.f90))
INTEGER :: j,Unit
REAL(KIND_MSH) :: FC(2)
!!--begin--

DO j=1,NUM_Faces(LO_Mesh)
 FC = FaceCentroid(LO_Mesh,j)
 WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x,e21.13,1x)")&
   FC(1),FC(2),LO_ScalarFluxF(1,j),EXACT_PhiF(LO_Mesh,j)
END DO

!!--end--
END SUBROUTINE


SUBROUTINE GNUPLOT_LO_CurrentFN(Unit)
USE VAR_Currents      ,ONLY: LO_CurrentFN                !!((04-C-VAR_Currents.f90))
USE VAR_QuasiDiffusion,ONLY: LO_Mesh                     !!((46-B-VAR_QuasiDiffusion.f90))
INTEGER :: j,Unit
REAL(KIND_MSH) :: FC(2)
!!--begin--

DO j=1,NUM_Faces(LO_Mesh)
 FC = FaceCentroid(LO_Mesh,j)
 WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x,e21.13,1x)")&
   FC(1),FC(2),LO_CurrentFN(1,j),EXACT_JFN(LO_Mesh,j)
END DO

!!--end--
END SUBROUTINE


SUBROUTINE GNUPLOT_LO_CurrentC(Unit)
USE VAR_Currents      ,ONLY: LO_CurrentFN                !!((04-C-VAR_Currents.f90))
USE VAR_QuasiDiffusion,ONLY: LO_Mesh                     !!((46-B-VAR_QuasiDiffusion.f90))
INTEGER :: Unit

INTEGER :: i
REAL(KIND_MSH) :: CC(2)
REAL(KIND_QDF) :: JC(2),ExJC(2)

!!--begin--

DO i=1,NUM_Cells(LO_Mesh)
 CC   = CellCentroid(LO_Mesh,i)
 JC   = RECONSTRUCT_CurrentC(LO_Mesh,i,LO_CurrentFN(1,:))
 ExJC = EXACT_JC(LO_Mesh,i)
 WRITE(Unit,"(f12.7,1x,f12.7,1x,4(e21.13,1x))")&
   CC(1),CC(2),JC(1),JC(2),ExJC(1),ExJC(2)
END DO

!!--end--
END SUBROUTINE



SUBROUTINE GNUPLOT_LO_ScalarFluxC(Unit)
USE VAR_ScalarFluxes  ,ONLY: LO_ScalarFluxC              !!((04-C-VAR_ScalarFluxes.f90))
USE VAR_QuasiDiffusion,ONLY: LO_Mesh                     !!((46-B-VAR_QuasiDiffusion.f90))
INTEGER :: i,Unit
REAL(KIND_MSH) :: CC(2)
!!--begin--
DO i=1,NUM_Cells(LO_Mesh)
 CC = CellCentroid(LO_Mesh,i)
 WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x,e21.13,1x)")&
   CC(1),CC(2),LO_ScalarFluxC(1,i),EXACT_PhiC(LO_Mesh,i)
END DO

!!--end--
END SUBROUTINE



SUBROUTINE GNUPLOT_LO_ScalarFluxTri(Unit)
USE VAR_ScalarFluxes,ONLY: LO_ScalarFluxF,LO_ScalarFluxC !!((04-C-VAR_ScalarFluxes.f90))
USE VAR_QuasiDiffusion,ONLY: LO_Mesh                     !!((46-B-VAR_QuasiDiffusion.f90))

INTEGER :: i,n,j,k1,k2,Nk,k,l,Unit
REAL(KIND_MSH) :: CC(2),V1(2),V2(2),FC(2)
REAL(KIND_MSH) :: VertVals(NUM_Verts(LO_Mesh)),NewVal
INTEGER        :: NumVals(NUM_Verts(LO_Mesh))
!!--begin--

!get vertex values
NumVals = 0
VertVals=-100.
DO j=1,NUM_Faces(LO_Mesh)
 NewVal = LO_ScalarFluxF(1,j)
 DO l=1,SIZE(LO_Mesh%Faces(j)%VertList)
  k = LO_Mesh%Faces(j)%VertList(l)
  IF( IsBoundaryVert(LO_Mesh,k) .AND. .NOT.IsBoundaryFace(LO_Mesh,j) )CYCLE
  NumVals(k) = NumVals(k) + 1
  N = NumVals(k)
  VertVals(k) = ( REAL(N-1)*VertVals(k) + NewVal )/REAL(N)
 END DO
END DO

DO i=1,NUM_Cells(LO_Mesh)
 CC = CellCentroid(LO_Mesh,i)
 DO n=1,NUM_Faces(LO_Mesh%Cells(i))
  j=ABS(LO_Mesh%Cells(i)%FaceList(n))
  FC = FaceCentroid(LO_Mesh,j)
  Nk = SIZE(LO_Mesh%Faces(j)%VertList)
  k1 = LO_Mesh%Faces(j)%VertList(1)
  k2 = LO_Mesh%Faces(j)%VertList(Nk)
  V1 = Vert(LO_Mesh,k1)
  V2 = Vert(LO_Mesh,k2)
  WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x)")&
   CC(1),CC(2),LO_ScalarFluxC(1,i)
  WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x)")&
   V1(1),V1(2),VertVals(k1)
  WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x)")&
   FC(1),FC(2),LO_ScalarFluxF(1,j)
  WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x)")&
   CC(1),CC(2),LO_ScalarFluxC(1,i)
  WRITE(Unit,*)
  WRITE(Unit,*)

  WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x)")&
   CC(1),CC(2),LO_ScalarFluxC(1,i)
  WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x)")&
   V2(1),V2(2),VertVals(k2)
  WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x)")&
   FC(1),FC(2),LO_ScalarFluxF(1,j)
  WRITE(Unit,"(f12.7,1x,f12.7,1x,e21.13,1x)")&
   CC(1),CC(2),LO_ScalarFluxC(1,i)
  WRITE(Unit,*)
  WRITE(Unit,*)

 END DO
END DO

!!--end--
END SUBROUTINE


END MODULE
