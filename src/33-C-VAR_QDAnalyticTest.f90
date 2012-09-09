!!# MODULE <<VAR_QDAnalyticTest>>
MODULE VAR_QDAnalyticTest

!!## PURPOSE
!! Variables for the analytic test of Quasidiffusion.


!!## MODULES
USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
USE KND_QuasiDiffusion !!((02-A-KND_QuasiDiffusion.f90))
USE PAR_Constants_Rdp  !!((02-A-PAR_Constants_Rdp.f90))


!!## IMPLICITNESS
IMPLICIT NONE
PUBLIC


!!## PARAMETER LIST
REAL(KIND_Rdp),PARAMETER :: c_20=20._KIND_Rdp
REAL(KIND_Rdp),PARAMETER :: c_2_by_3=2._KIND_Rdp/3._KIND_Rdp
INTEGER       ,PARAMETER :: Average_=1,Centroid_=2,Balance_=3
CHARACTER(*)  ,PARAMETER :: KEY_EvaluationMethod(3) = &
  (/"Average ","Centroid","Balance "/)

!!## LOCAL VARIABLES
INTEGER        :: AnalyticTestCase=0
REAL(KIND_QDF) :: alpha = 5._KIND_QDF
REAL(KIND_QDF) :: C     = 1_KIND_QDF
REAL(KIND_QDF) :: phi0  = 1._KIND_QDF
REAL(KIND_QDF) :: Exx0=1._KIND_QDF,Exx1=1._KIND_QDF,Exx2=1._KIND_QDF
REAL(KIND_QDF) :: Eyy0=1._KIND_QDF,Eyy1=1._KIND_QDF,Eyy2=1._KIND_QDF
REAL(KIND_QDF) :: Exy0=1._KIND_QDF
REAL(KIND_QDF) :: siga=0.5_KIND_QDF
REAL(KIND_QDF) :: sigt=1.0_KIND_QDF
INTEGER        :: EvaluationMethod_J   = Centroid_
INTEGER        :: EvaluationMethod_Phi = Centroid_
INTEGER        :: EvaluationMethod_Q   = Centroid_
INTEGER        :: EvaluationMethod_E   = Centroid_
LOGICAL        :: Using_AnalyticSource = .FALSE.



END MODULE

















