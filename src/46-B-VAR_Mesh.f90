!!# VARIABLE MODULE: <<VAR_Mesh>>
MODULE VAR_Mesh


!!## PURPOSE
!! Contains all the variables associated with
!! the Mesh.



!!## GLOBAL USER MODULES
USE USR_Mesh !!((14-B-USR_Mesh.f90))



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE



!!## INFORMATION
CHARACTER(*),PARAMETER :: mod_ ="VAR_Mesh"
CHARACTER(*),PARAMETER :: file_="46-B-VAR_Mesh.f90"



!!## GLOBAL VARIABLES
TYPE(TYPE_Mesh),TARGET  :: Mesh
INTEGER        ,POINTER :: FrozenVertSet(:)=>NULL()



!!## PUBLIC ACCESS LIST
PUBLIC :: Mesh,FrozenVertSet


END MODULE
