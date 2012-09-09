MODULE USR_pThread
!!#### PURPOSE
!! Defines a parallel-thread type, used to manage
!! the parallel threading of calculations.

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### DERIVED TYPES
!! @ threading variable to manage parallel computations
TYPE TYPE_order
 INTEGER        ,POINTER :: order(:)
ENDTYPE
TYPE TYPE_pThread
 TYPE(TYPE_order),POINTER :: path(:)
ENDTYPE
PUBLIC :: TYPE_pThread,TYPE_order
INTERFACE ALLOCATE_pthread
 MODULE PROCEDURE ALLOCATE_pthread0
 MODULE PROCEDURE ALLOCATE_pthread1
END INTERFACE

PUBLIC :: ALLOCATE_pthread,DEALLOCATE_pthread
PUBLIC :: ALLOCATE_pthread0
PUBLIC :: ALLOCATE_pthread1

CONTAINS

SUBROUTINE ALLOCATE_pthread1( pthread , Nm , Np , No )

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_pthread),POINTER :: pthread(:)

!!#### REQUIRED INPUT
!! @ number of directions <Nm>
!! @ number of paths <Np>
!! @ number of orderings <No>
INTEGER,INTENT(IN) :: Nm
INTEGER,INTENT(IN) :: Np(:)
INTEGER,INTENT(IN) :: No(:,:)

!!#### LOCAL VARIABLES
INTEGER :: m,Np_,No_,p,o
!!--begin--

ALLOCATE( pthread(1:Nm) )
DO m=1,Nm
 Np_ = Np(m)
 ALLOCATE( pthread(m)%path(Np_) )
 DO p=1,Np_
  No_ = No(p,m)
  ALLOCATE( pthread(m)%path(p)%order(No_) )
  DO o=1,No_
   pthread(m)%path(p)%order(o) = 0
  END DO
 END DO
END DO

!!--end--
END SUBROUTINE


SUBROUTINE ALLOCATE_pthread0( pthread , Nm0 , No0 )

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_pthread),POINTER :: pthread(:)

!!#### REQUIRED INPUT
!! @ number of directions <Nm0>
!! @ number of orderings <No0>
INTEGER,INTENT(IN) :: Nm0
INTEGER,INTENT(IN) :: No0

!!#### LOCAL VARIABLES
INTEGER,ALLOCATABLE :: Np(:),No(:,:)
INTEGER :: Nm

!!--begin--

Nm = Nm0
ALLOCATE( Np(Nm) )
Np = SPREAD(1,dim=1,Ncopies=Nm)
ALLOCATE( No(1,Nm) )
No = No0

CALL ALLOCATE_pthread( pthread , Nm=Nm , Np=Np , No=No )

DEALLOCATE( Np , No )

!!--end--
END SUBROUTINE



SUBROUTINE DEALLOCATE_pthread( pthread )

!!#### REQUIRED INPUT/OUTPUT
TYPE(TYPE_pthread),POINTER :: pthread(:)

!!#### LOCAL VARIABLES
INTEGER :: m,p

!!--begin--

DO m=1,SIZE(pthread)
 DO p=1,SIZE(pthread(m)%path)
  DEALLOCATE( pthread(m)%path(p)%order )
 END DO
 DEALLOCATE( pthread(m)%path )
END DO
DEALLOCATE( pthread )

!!--end--
END SUBROUTINE


END MODULE
