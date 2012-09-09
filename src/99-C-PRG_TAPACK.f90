!!# PROGRAM >>TAPACK<<
PROGRAM PRG_TAPACK

!!## PURPOSE
!! Solve particle transport problems through a
!! Transport Algorithms PACKage.

!!## CHANGES
!! Search for [vX.YZ] to find changes throughout the source.
!!
!! v2.23
!! [waw] 
!! Major change to the way output files are handled: now all files
!! are dumped to current directory by default.


!!## MODULES
USE FUN_STR,ONLY: STR
USE FUN_DTIME,ONLY: DTIME !!((08-B-FUN_DTIME.f90))
USE USR_fdbk   ,ONLY: & !!((08-C-USR_fdbk.f90))
    fdbk_comment,fdbk_warning,OutputUnit,TYPE_fdbk,UpdateAndDump,DEALLOCATE_fdbk
USE USR_SimpleList,ONLY: ADD_TO_LIST !!((09-B-USR_SimpleList.f90))
USE PAR_TAPACK,ONLY: VERSION
USE VAR_TAPACK,ONLY:  & !!((66-C-VAR_TAPACK.f90))
    TAP_what,TAP_time,iter,dt0,dt,tt,AnyRefined,min_iter,max_iter,&
    IterationsExceeded
USE TBX_TAPACK,ONLY:  & !!((82-C-TBX_TAPACK.f90))
    INPUT_TAPACK,SETUP_TAPACK,REFINE_TAPACK,&
    SOLVE_TAPACK_HighOrder,SOLVE_TAPACK_LowOrder,&
    PRINT_Solution,PRINT_SolutionAux,PRINT_InputEcho,CHECK_TAPACK,&
    WRAPUP_TAPACK
USE VAR_ScalarFluxes,ONLY: KIND_ScalarFlux,&
    ScalarFluxC,LO_ScalarFluxC,LastScalarFluxC,LastLO_ScalarFluxC
!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## LOCAL VARIABLES
!! * feedback object <fdbk>
TYPE(TYPE_fdbk) :: fdbk
REAL(KIND_ScalarFlux) :: lastsf

!!--begin--


!=============================================
!0. intialize random seed and timer
!=============================================
!! * Welcome message.
CALL UpdateAndDump( fdbk_comment , fdbk , s="[[TAP]] Welcome to the &
  &Transport Algorithms Package, TAPACK version=["//TRIM(VERSION)//"]")
CALL RANDOM_SEED()
     CALL ADD_TO_LIST(TAP_what,"Start")
     dt(1) = DTIME(TAP_time,tt)
!=============================================



!=============================================
!1. read input
!=============================================
CALL INPUT_TAPACK( fdbk )
     CALL ADD_TO_LIST(TAP_what,"Input")
     dt(1) = DTIME(TAP_time,tt)
fdbk%PauseLevel = fdbk_warning+1 !only pause execution for errors
!=============================================



!=============================================
!2. problem setup
!=============================================
CALL SETUP_TAPACK( fdbk )
     CALL ADD_TO_LIST(TAP_what,"Setup")
     dt(2) = DTIME(TAP_time,tt)
!=============================================



!=============================================
!3. print the input
!=============================================
CALL PRINT_InputEcho( fdbk )
     CALL ADD_TO_LIST(TAP_what,"EchoInput")
     dt(3) = DTIME(TAP_time,tt)
!=============================================



!=============================================
!4. solve/refine loop
!=============================================
iter = 0
DO

 !4.1. run the high order solver to get the angular flux
 CALL SOLVE_TAPACK_HighOrder( dt0 , iter , fdbk )
      CALL ADD_TO_LIST(TAP_what,"SolveHO("//TRIM(STR(iter))//")")
      dt(4) = dt(4) + DTIME(TAP_time,tt)
!  IF( ASSOCIATED(ScalarFluxC) )THEN
!     lastsf=0
!     IF( ASSOCIATED(LastScalarFluxC) )lastsf=LastScalarFluxC(1,1)
!     WRITE(*,*)'iter,hi,lasthi=',iter,ScalarFluxC(1,1),lastsf
!  END IF

 !4.2. run the low order solver to get the new source
 CALL SOLVE_TAPACK_LowOrder( dt0 , iter ,  fdbk )
      CALL ADD_TO_LIST(TAP_what , "SolveLO("//TRIM(STR(iter))//")")
      dt(5) = dt(5) + DTIME(TAP_time,tt)
!  IF( ASSOCIATED(LO_ScalarFluxC) )THEN
!     lastsf=0
!     IF( ASSOCIATED(LastLO_ScalarFluxC) )lastsf=LastLO_ScalarFluxC(1,1)
!     WRITE(*,*)'iter,lo,lastlo=',iter,LO_ScalarFluxC(1,1),lastsf
!  END IF
 
 !4.3. refine and calculate residuals
 AnyRefined = REFINE_TAPACK( dt0 , iter , fdbk )
      CALL ADD_TO_LIST(TAP_what , "Refine("//TRIM(STR(iter))//")")
      dt(6) = dt(6) + DTIME(TAP_time,tt)

 !4.4. check calculations
 CALL CHECK_TAPACK( dt0 , iter , fdbk )
      CALL ADD_TO_LIST(TAP_what , "Check("//TRIM(STR(iter))//")")
      dt(7) = dt(7) + DTIME(TAP_time,tt)


 !4.5. kick out check
 IF( .NOT.AnyRefined .AND. iter>=MIN_iter )EXIT

 !4.6. print the solutions at this iteration to a special file
 CALL PRINT_Solution( fdbk , iter=iter )
 CALL PRINT_SolutionAux( fdbk , iter=iter )
     CALL ADD_TO_LIST(TAP_what , "Print("//TRIM(STR(iter))//")")
      dt(8) = dt(8) + DTIME(TAP_time,tt)


 iter = iter + 1

 !4.7. kick out on exceeding iteration count
 IF( iter>MAX_iter )THEN

  IterationsExceeded = .TRUE.

  CALL UpdateAndDump(fdbk_warning,fdbk,s="[[TAP]] We have exceeded &
    &the maximum number of iterations: "//TRIM(STR(MAX_iter))//".")

  EXIT

 END IF

END DO
!=============================================



!=============================================
!5. print the final solution
!=============================================
CALL PRINT_Solution( fdbk )
CALL PRINT_SolutionAux( fdbk )
!=============================================


!=============================================
!6. wrapup the process
!=============================================
CALL WRAPUP_TAPACK( fdbk )
CALL DEALLOCATE_fdbk(fdbk)
!=============================================

!!--end--
END PROGRAM






