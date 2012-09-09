!!# MODULE <<PAR_QuasiDiffusion>>
MODULE PAR_QuasiDiffusion

!!## PURPOSE
!! Provide the runtime parameters needed for QuasiDiffusion.

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## EXTERNAL PARAMETERS


!! * system formulation options
!
INTEGER,PARAMETER :: QDF_System01 = 1
INTEGER,PARAMETER :: QDF_System02 = 2
INTEGER,PARAMETER :: QDF_System03 = 3
INTEGER,PARAMETER :: QDF_System04 = 4
INTEGER,PARAMETER :: QDF_System05 = 5
!
CHARACTER(*),PARAMETEr :: QDF_KEY_System(1:5) = (/"GGK0",& !Goldin,Goldina,Kolpakov on essential meshes only
                                                  "GGK ",& !Goldin,Goldina,Kolpakov on cell based meshes
                                                  "AK  ",& !Aristova,Kolpakov on cell based
                                                  "JM  ",& !Jim Morel on cell based
                                                  "JMAC"/) !Jim Morel on cell-based PLUS acceleration only capability
!! * system solver options, <Solver>
!
INTEGER,PARAMETER :: QDF_Direct    = 01
INTEGER,PARAMETER :: QDF_Iterative = 02
!
CHARACTER(*),PARAMETER :: QDF_KEY_Solver(1:2)=(/"Direct   ",&
                                                "Iterative"/)
!
!
!! * system subsolver options, <SubSolver>
!
INTEGER,PARAMETER :: QDF_SMLIB_GMRES       = 01
INTEGER,PARAMETER :: QDF_SMLIB_BiCGStab    = 02
INTEGER,PARAMETER :: QDF_SMLIB_CGS         = 03
INTEGER,PARAMETER :: QDF_SMLIB_BandedGauss = 04
INTEGER,PARAMETER :: QDF_LAPACK_DenseGauss = 05
INTEGER,PARAMETER :: QDF_SPARSKIT_BiCGstab = 06
INTEGER,PARAMETER :: QDF_Botchev_BiCGstab2 = 07
INTEGER,PARAMETER :: QDF_nspiv             = 08
INTEGER,PARAMETER :: QDF_SPARSKIT_CG       = 09
INTEGER,PARAMETER :: QDF_SPARSKIT_CGNR     = 10
INTEGER,PARAMETER :: QDF_SPARSKIT_BCG      = 11
INTEGER,PARAMETER :: QDF_SPARSKIT_DBCG     = 12
INTEGER,PARAMETER :: QDF_SPARSKIT_TFQMR    = 13
INTEGER,PARAMETER :: QDF_SPARSKIT_FOM      = 14
INTEGER,PARAMETER :: QDF_SPARSKIT_GMRES    = 15
INTEGER,PARAMETER :: QDF_SPARSKIT_FGMRES   = 16
INTEGER,PARAMETER :: QDF_SPARSKIT_DQGMRES  = 17
!!
CHARACTER(*),PARAMETER :: QDF_KEY_SubSolver(1:17)=(/"SMLIB-GMRES      ",&
                                                    "SMLIB-BiCGStab   ",&
                                                    "SMLIB-CGS        ",&
                                                    "SMLIB-BandedGauss",&
                                                    "LAPACK-DenseGauss",&
                                                    "SPARSKIT-BiCGstab",&
                                                    "Botchev-BiCGstab2",&
                                                    "nspiv            ",&
                                                    "SPARSKIT-CG      ",&
                                                    "SPARSKIT-CGNR    ",&
                                                    "SPARSKIT-BCG     ",&
                                                    "SPARSKIT-DBCG    ",&
                                                    "SPARSKIT-TFQMR   ",&
                                                    "SPARSKIT-FOM     ",&
                                                    "SPARSKIT-GMRES   ",&
                                                    "SPARSKIT-FGMRES  ",&
                                                    "SPARSKIT-DQGMRES "/)
!
CHARACTER(*),PARAMETER :: QDF_KEY_ReorderStrategies(1:5)=(/"standard   ",&
                                                           "RCM        ",&
                                                           "RevRCM     ",&
                                                           "MinDegree  ",&
                                                           "MinNeighbor"/)
!
INTEGER,PARAMETER :: QDF_PRECONDITIONER_ILU = 1
INTEGER,PARAMETER :: QDF_PRECONDITIONER_ApproxInv = 2
!
CHARACTER(*),PARAMETER :: QDF_KEY_Preconditioners(1:2)=(/"ILU      ",&
                                                         "ApproxInv"/)
!
!
!! * the options for evaluating areas of half-cells <HalfCellShape>
!
INTEGER,PARAMETER :: QDF_FaceBisect = 1
INTEGER,PARAMETER :: QDF_Corners    = 2
CHARACTER(*),PARAMETER :: QDF_KEY_HalfCellShape(1:2)=(/"FaceBisect",&
                                                       "Corners   "/)
!
!
!! * the options for evaluating interfaces to use:
!!   S is Strong, C is Current, W is Weak, F is Flux and
!!   EF is Eddington and Flux combined <InterfaceType>
!
INTEGER,PARAMETER :: QDF_SC_PLUS_WF  = 1
INTEGER,PARAMETER :: QDF_SF_PLUS_WC  = 2
INTEGER,PARAMETER :: QDF_SC_PLUS_WEF = 3
INTEGER,PARAMETER :: QDF_SEF_PLUS_WC = 4
!
CHARACTER(*),PARAMETER :: QDF_KEY_InterfaceType(1:4)=(/"SC+WF ",&
                                                       "SF+WC ",&
                                                       "SC+WEF",&
                                                       "SEF+WC"/)
!
!
!! * the options for evaluating boundary conditions
INTEGER,PARAMETER :: QDF_Goldin        = 1
INTEGER,PARAMETER :: QDF_Larsen        = 2
INTEGER,PARAMETER :: QDF_reflective    = 3
INTEGER,PARAMETER :: QDF_fixedPhi      = 4
INTEGER,PARAMETER :: QDF_fixedJ        = 5
INTEGER,PARAMETER :: QDF_functionPhi   = 6
INTEGER,PARAMETER :: QDF_functionJ     = 7
INTEGER,PARAMETER :: QDF_functionMixed = 8
!
CHARACTER(*),PARAMETER :: QDF_KEY_LOBC(1:8)=(/"Goldin            ",&
                                              "Larsen            ",&
                                              "Reflective        ",&
                                              "fixedPhi          ",&
                                              "fixedJ            ",&
                                              "functionPhi       ",&
                                              "functionJ         ",&
                                              "functionMixed     "/)
!
!! * scalar flux function in a cell generation options
INTEGER,PARAMETER :: QDF_FLAT         = 1
INTEGER,PARAMETER :: QDF_LINEAR_LLS   = 2
INTEGER,PARAMETER :: QDF_LINEAR_GAUSS = 3
CHARACTER(5),PARAMETER :: QDF_KEY_CellFunction(1:3)=(/"Flat ",&
                                                      "LLS  ",&
                                                      "Gauss"/)
!! * scalar flux function in a cell fixup options
INTEGER,PARAMETER :: QDF_NONE = 1
INTEGER,PARAMETER :: QDF_TAU  = 2
INTEGER,PARAMETER :: QDF_OI3  = 3
INTEGER,PARAMETER :: QDF_Min0 = 4
INTEGER,PARAMETER :: QDF_Switch = 5
INTEGER,PARAMETER :: QDF_TAUN  = 6
INTEGER,PARAMETER :: QDF_TAUDN  = 7
CHARACTER(6),PARAMETER :: QDF_KEY_NonlinearFixup(1:7)=(/"None  ",&
                                                        "Tau   ",&
                                                        "OI3   ",&
                                                        "Min0  ",&
                                                        "Switch",&
                                                        "TauN  ",&
                                                        "TauDN "/)

END MODULE
