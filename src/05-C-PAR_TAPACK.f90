!!# PARAMETERS MODULE  <<PAR_TAPACK>>
MODULE PAR_TAPACK

!!## PURPOSE
!! Provide the runtime parameters needed for TAPACK.

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## EXTERNAL PARAMETERS
!
!! * available spatial discretizations for the transport solution
INTEGER     ,PARAMETER :: Mesh_ = 2
CHARACTER(*),PARAMETER :: KEY_SpatialDiscretization(1:2) = &
  (/"   ","MSH"/)
CHARACTER(*),PARAMETER :: NAME_SpatialDiscretization(1:2) = &
  (/"None                   ",&
    "Implicit Arbitrary Mesh"/)
!
!
!! * available energy discretizations for the transport solution
INTEGER     ,PARAMETER :: EnergyGroups_ = 2
CHARACTER(*),PARAMETER :: KEY_EnergyDiscretization(1:2) = &
  (/"   ","NRG"/)
CHARACTER(*),PARAMETER :: NAME_EnergyDiscretization(1:2) = &
  (/"None         ",&
    "Energy Groups"/)
!
!
!! * available angular discretizations for the transport solution
INTEGER     ,PARAMETER :: DiscreteOrdinates_ = 2
CHARACTER(*),PARAMETER :: KEY_AngularDiscretization(1:2) = &
  (/"   ","DOR"/)
CHARACTER(*),PARAMETER :: NAME_AngularDiscretization(1:2) = &
  (/"None              ",&
    "Discrete Ordinates"/)
!
!
!! * available time treatments for the transport solution
INTEGER     ,PARAMETER :: SteadyState_ = 2
CHARACTER(*),PARAMETER :: KEY_TimeTreatment(1:2) = &
  (/"   ","SST"/)
CHARACTER(*),PARAMETER :: NAME_TimeTreatment(1:2) = &
  (/"None        ",&
    "Steady State"/)
!
!
!! * available source modules
INTEGER,PARAMETER :: Source_ = 2
CHARACTER(*),PARAMETER :: KEY_SourceModule(1:2) = &
  (/"   ","SRC"/)
CHARACTER(*),PARAMETER :: NAME_SourceModule(1:2) = &
  (/"None        ",&
    "Source Panda"/)
!
!
!! * available cross section modules
INTEGER,PARAMETER :: XSMonkey_ = 2
CHARACTER(*),PARAMETER :: KEY_CrossSectionModule(1:2) = &
  (/"   ","XSE"/)
CHARACTER(*),PARAMETER :: NAME_CrossSectionModule(1:2) = &
  (/"None                     ",&
    "Cross Section (XS) Monkey"/)
!
!
!! * available nuclide transmutation modules
INTEGER     ,PARAMETER :: NucSpider_ = 2
CHARACTER(*),PARAMETER :: KEY_NuclideTransmutationModule(1:2) = &
 (/"   ","NCS"/)
CHARACTER(*),PARAMETER :: NAME_NuclideTransmutationModule(1:2) = &
 (/"None          ",&
   "Nuclide Spider"/)
!
!
!! * available transport method modules
INTEGER     ,PARAMETER :: MoCshort_ = 2
INTEGER     ,PARAMETER :: MoCLong_  = 3
INTEGER     ,PARAMETER :: FEM_      = 4
CHARACTER(*),PARAMETER,DIMENSION(1:4) :: KEY_TransportModule = &
  (/"   ",&
    "MCS",&
    "MCL",&
    "FEM"/)
CHARACTER(*),PARAMETER,DIMENSION(1:4) :: NAME_TransportModule = &
  (/"None                             ",&
    "Method of Characteristics (short)",&
    "Method of Characteristics (long) ",&
    "Finite Element Method            "/)
!
!
!! * available acceleration methods modules
INTEGER,PARAMETER :: DSA_             = 2
INTEGER,PARAMETER :: TSA_             = 3
INTEGER,PARAMETER :: QuasiDiffusion_  = 4
CHARACTER(*),PARAMETER,DIMENSION(1:4) :: KEY_AccelerationModule = &
  (/"   ",&
    "DSA",&
    "TSA",&
    "QDF"/)
CHARACTER(*),PARAMETER,DIMENSION(1:4) :: NAME_AccelerationModule = &
  (/"None                            ",&
    "Diffusion Synthetic Acceleration",&
    "Transport Synthetic Acceleration",&
    "Quasi-Diffusion                 "/)
!
!
!! * <TAPACK> outputs
CHARACTER(*),PARAMETER,DIMENSION(1:30) :: KEY_Outputs = &
  (/"AngularFluxV             ",&
    "AngularFluxF             ",&
    "AngularFluxC             ",&
    "ScalarFluxV              ",&
    "ScalarFluxF              ",&
    "ScalarFluxC              ",&
    "CurrentV                 ",&
    "CurrentF                 ",&
    "CurrentFN                ",&
    "CurrentC                 ",&
    "ScalarFlux-CellRegions   ",&
    "Current-Exiting          ",&
    "EddingtonsV              ",&
    "EddingtonsF              ",&
    "EddingtonsC              ",&
    "LO-ScalarFluxV           ",&
    "LO-ScalarFluxF           ",&
    "LO-ScalarFluxC           ",&
    "LO-CurrentFN             ",&
    "LO-CurrentC              ",&
    "LO-CBoundary             ",&
    "LO-ScalarFlux-CellRegions",&
    "LO-Current-Exiting       ",&
    "LO-SystemInfo            ",&
    "LO-IterTable             ",&
    "CharacteristicInfo       ",&
    "InterpInfo               ",&
    "TimeSummary              ",&
    "TimeTable                ",&
    "KEY-Outputs              "/)

CHARACTER(*),PARAMETER,DIMENSION(1:30) :: DESCRIPTION_Outputs = &
  (/"Angular Flux at Verts, $\Psi^(V)$                      ",&
    "Angular Flux at Faces, $\Psi^(F)$                      ",&
    "Angular Flux at Cells, $\Psi^(C)$                      ",&
    "Scalar Flux at Verts from $\Psi^(V)$ integrations      ",&
    "Scalar Flux at Faces from $\Psi^(F)$ integrations      ",&
    "Scalar Flux at Cells from $\Psi^(C)$ integrations      ",&
    "Current at Vertices from $\Psi^(V)$ integrations       ",&
    "Current at Faces from $\Psi^(F)$ integrations          ",&
    "Normal Current at Faces from $\Psi^(F)$ integrations   ",&
    "Current at Cells from $\Psi^(C)$ integrations          ",&
    "Scalar flux in Regions of Cells                        ",&
    "Current Exiting the domain                             ",&
    "Eddington factors at Verts from $\Psi^(V)$ integrations",&
    "Eddington factors at Faces from $\Psi^(F)$ integrations",&
    "Eddington factors at Cells from $\Psi^(C)$ integrations",&
    "Low-Order Scalar flux at Verts                         ",&
    "Low-Order Scalar flux at Faces                         ",&
    "Low-Order Scalar flux at Cells                         ",&
    "Low-Order Normal Current at Faces                      ",&
    "Low-Order Current at Cells                             ",&
    "Boundary Condition factors                             ",&
    "Low-order Scalar flux in regions of cells              ",&
    "Low-order Current exiting the domain                   ",&
    "Low-Order System Info                                  ",&
    "Low-Order Iteration Table                              ",&
    "Characteristic Information                             ",&
    "Interpolation Information                              ",&
    "Time Summary                                           ",&
    "Time Table                                             ",&
    "a list of the keys available for outputs               "/)


!! * <TAPACK> input edits
CHARACTER(*),PARAMETER,DIMENSION(1:12) :: KEY_InputEdits = &
  (/"QuadratureSet        ",&
    "MomentTest           ",&
    "NuclearData          ",&
    "BoundaryConditions   ",&
    "Mesh                 ",&
    "LO-Mesh              ",&
    "StreamingInterpolants",&
    "InteriorCells        ",&
    "IncomingDirections   ",&
    "SweepOrder           ",&
    "KEY-InputEdits       ",&
    "InputFile            "/)

CHARACTER(*),PARAMETER,DIMENSION(1:12) :: DESCRIPTION_InputEdits = &
  (/"Ordinates and weights of the quadrature set                          ",&
    "A test of the moments-taking routines for the quadrature set         ",&
    "Cross sections, decay constants, etc.                                ",&
    "Boundary conditions                                                  ",&
    "Spatial Mesh for high-order transport computations                   ",&
    "Spatial Mesh for low-order computations                              ",&
    "verts used in the method of characteristics interpolation            ",&
    "ray tracing determination of interior cells at every vertex/direction",&
    "incoming directions for each vertex on the boundary                  ",&
    "sweep ordering for the mesh                                          ",&
    "a list of the keys available for input edits                         ",&
    "the input file stripped of blank lines and comments                  "/)

CHARACTER(*),PARAMETER,DIMENSION(1:4) :: KEY_SurGE = &
  (/"Faces  ",&
    "Cells  ",&
    "Both   ",&
    "Sandbox"/)

!
!
!! * job parameters
INTEGER,PARAMETER :: Exit_      = -1
INTEGER,PARAMETER :: Converged_ =  0
!
!
!! * output types
INTEGER,PARAMETER :: InputEcho_  = 1
INTEGER,PARAMETER :: Solution_   = 2
!
!
!! * maximum length of file
INTEGER,PARAMETER :: LEN_file   = 200
!! * maximum length of label field
INTEGER,PARAMETER :: LEN_label  = 200

!
!! * system boundary factor and E generation options
!
INTEGER,PARAMETER :: TAP_EvalPsiFirst    = 1
INTEGER,PARAMETER :: TAP_EvalFactorFirst = 2
INTEGER,PARAMETER :: TAP_PsiKnown        = 3
!
!! * the evaluation method for boundary factors and
!!   E's (or Psi if we are evaluating Psi first.)
!
INTEGER,PARAMETER :: TAP_InterpNearest        = 0
INTEGER,PARAMETER :: TAP_AverageArithmetic    = 1
INTEGER,PARAMETER :: TAP_IntegrateTrapezoidal = 2
INTEGER,PARAMETER :: TAP_InterpBiVar          = 3
INTEGER,PARAMETER :: TAP_InterpCShep          = 4

CHARACTER(6),PARAMETER :: VERSION='2.24.0';

END MODULE
