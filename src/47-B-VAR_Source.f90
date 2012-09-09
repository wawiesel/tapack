!!# MODULE <<VAR_Source>>
MODULE VAR_Source

!!## PURPOSE
!! The source variables.

!!## GLOBAL USER MODULES
USE KND_Source                              !!((02-A-KND_Source.f90))
USE USR_Source,ONLY: TYPE_Source,LEN_Source !!((35-B-USR_Source.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## GLOBAL VARIABLES
CHARACTER(LEN_Source),POINTER :: LABEL_sources(:) => NULL()
TYPE(TYPE_Source)    ,POINTER :: Source(:)=>NULL()
!
!!* External Source within a cell: <ExtSourceCellFunction(n,g,i)>
!   n = order
!   g = energy group
!   i = cell index
REAL(KIND_ExtSource)  ,POINTER :: ExtSourceCellFunction(:,:,:)=>NULL()
!
!!* Total Source within a cell: <TotSourceCellFunction(n,g,i)>
!   n = order
!   g = energy group
!   i = cell index
REAL(KIND_ExtSource)  ,POINTER :: TotSourceCellFunction(:,:,:)=>NULL()

LOGICAL :: Using_AnalyticExtSourceOnly=.FALSE.


!## PUBLIC ACCESS LIST
PUBLIC :: KIND_ExtSource
PUBLIC :: TYPE_Source
PUBLIC :: LABEL_Sources
PUBLIC :: Source
PUBLIC :: ExtSourceCellFunction
PUBLIC :: TotSourceCellFunction
PUBLIC :: Using_AnalyticExtSourceOnly

END MODULE
