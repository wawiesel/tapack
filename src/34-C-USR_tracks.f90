!!# USER MODULES: <tracks>
MODULE USR_tracks

!!## PURPOSE
!! Routines that read and write 'tracks' files.


!!## EXTERNAL
USE KND_Characteristics !!((03-C-KND_Characteristics.f90))
USE SUB_Reallocate      !!((04-B-SUB_Reallocate.f90))
USE FUN_Error           !!((04-A-FUN_Error.f90))
USE FUN_SIZEa           !!((06-B-FUN_SIZEa.f90))
USE FUN_NewUnit         !!((04-B-FUN_NewUnit.f90))
USE SUB_CLEARn          !!((04-A-SUB_CLEARn.f90))


IMPLICIT NONE
PUBLIC

!!## MODULE PROCEDURES
CONTAINS



!!### FUNCTION: <INIT_tracks>
FUNCTION INIT_tracks(Nd,Nm,Nk,omeg,posi) RESULT(Unit)

!!#### PURPOSE
!! Initialize a tracking file ("tracks") which saves
!! the track information for long characteristics and
!! return the <Unit> connected to the file.

!!#### REQUIRED INPUT
INTEGER                   :: Nd,Nm,Nk
REAL(KIND_qmc),INTENT(IN) :: omeg(:,:),posi(:,:)

!!#### REQUIRED OUTPUT
INTEGER :: Unit

!!#### LOCAL VARIABLES
INTEGER :: k,m

!!--begin--

Unit = NewUnit()
OPEN(Unit=Unit,File="tracks",FORM="Unformatted",STATUS="Replace")
WRITE(Unit)Nd,Nm,Nk
DO m=1,Nm
 WRITE(Unit)omeg(1:3,m)
END DO
DO k=1,Nk
 WRITE(Unit)posi(1:2,k)
END DO

!!--end--
END FUNCTION



!!### FUNCTION: <OPEN_tracks>
FUNCTION OPEN_tracks(Nd,Nm,Nk,omeg,posi) RESULT(Unit)

!!#### PURPOSE
!! Open a tracking file ("tracks") which has
!! the track information for long characteristics and
!! return the <Unit> connected to the file.

!!#### REQUIRED OUTPUT
INTEGER                :: Nd,Nm,Nk
REAL(KIND_qmc),POINTER :: omeg(:,:),posi(:,:)

!!#### REQUIRED OUTPUT
INTEGER :: Unit

!!#### LOCAL VARIABLES
INTEGER :: k,m

!!--begin--

Unit = NewUnit()
OPEN(Unit=Unit,File="tracks",FORM="Unformatted",STATUS="Old")
READ(Unit)Nd,Nm,Nk

DO m=1,Nm
 READ(Unit)omeg(1:3,m)
END DO
DO k=1,Nk
 READ(Unit)posi(1:Nd,k)
END DO

!!--end--
END FUNCTION



!!### SUBROUTINE: <CLOSE_tracks>
SUBROUTINE CLOSE_tracks(Unit)

!!#### PURPOSE
!! Close the tracking file associated with <Unit>.

!!#### REQUIRED INPUT
INTEGER :: Unit

!!--begin--

CLOSE(Unit)

!!--end--

END SUBROUTINE




!!### SUBROUTINE: <SAVE_track>
SUBROUTINE SAVE_track(Unit,m,k,Ns,tcel1,segs1,psi0)

!!#### PURPOSE
!! Save a single track to the tracking file connected to
!! <Unit>.

!!#### REQUIRED INPUT
INTEGER                :: Unit
INTEGER                :: m,k
INTEGER                :: Ns
INTEGER       ,POINTER :: tcel1(:)
REAL(KIND_qmc),POINTER :: segs1(:)
REAL(KIND_qmc)         :: psi0

!!--begin--

WRITE(Unit)m
WRITE(Unit)k

WRITE(Unit)Ns

WRITE(Unit)tcel1(1:Ns-1)
WRITE(Unit)segs1(1:Ns  )

WRITE(Unit)psi0

!!--end--
END SUBROUTINE




!!### SUBROUTINE: <LOAD_track>
SUBROUTINE LOAD_track(Unit,m,k,Ns,tcel1,segs1,psi0)

!!#### PURPOSE
!! Load a single track from the tracking file connected to
!! <Unit>.

!!#### REQUIRED INPUT
INTEGER                :: Unit

!!#### REQUIRED OUTPUT
INTEGER                :: m,k
INTEGER                :: Ns
INTEGER       ,POINTER :: tcel1(:)
REAL(KIND_qmc),POINTER :: segs1(:)
REAL(KIND_qmc)         :: psi0

!!--begin--

CALL CLEARn( tcel1 )
CALL CLEARn( segs1 )

READ(Unit)m
READ(Unit)k

READ(Unit)Ns

ALLOCATE( tcel1(Ns-1) , segs1(Ns) )

READ(Unit)tcel1(1:Ns-1)
READ(Unit)segs1(1:Ns  )

READ(Unit)psi0

!!--end--
END SUBROUTINE




!!### SUBROUTINE: <LOAD_tracks>
SUBROUTINE LOAD_tracks(Nd,Nm,Nk,omeg,posi,tcel,segs,psi0)

!!#### PURPOSE
!! Load all tracks at once from the tracking
!! file.

!!#### REQUIRED OUTPUT
INTEGER :: Nd,Nm,Nk
REAL(KIND_qmc),POINTER :: omeg(:,:)
REAL(KIND_qmc),POINTER :: posi(:,:)
INTEGER       ,POINTER :: tcel(:,:,:)
REAL(KIND_qmc),POINTER :: segs(:,:,:)
REAL(KIND_qmc),POINTER :: psi0(:,:)

!!#### LOCAL VARIABLES
INTEGER :: Ns
INTEGER :: m,k,n
INTEGER :: Unit

!!--begin--

CALL CLEARn(omeg)
CALL CLEARn(posi)
CALL CLEARn(tcel)
CALL CLEARn(segs)
CALL CLEARn(psi0)

Unit=NewUnit()
OPEN(Unit,file="tracks",FORM="Unformatted",STATUS="Old")

READ(Unit)Nd,Nm,Nk

ALLOCATE( omeg(3,Nm) )
ALLOCATE( posi(Nd,Nk) )
ALLOCATE( tcel(09,Nm,Nk) ) ; tcel = Error(1)
ALLOCATE( segs(10,Nm,Nk) ) ; segs = Error(1._KIND_qmc)
ALLOCATE( psi0(Nm,Nk) )

DO m=1,Nm
 READ(Unit)omeg(1:3,m)
END DO

DO k=1,Nk
 READ(Unit)posi(1:Nd,k)
END DO

DO n=1,Nk*Nm

 READ(Unit)m
 READ(Unit)k

 READ(Unit)Ns

 IF( Ns>SIZE(segs,1) )THEN
  CALL REALLOCATE( tcel , (/Ns,0,0/) , fill=Error(1) )
  CALL REALLOCATE( segs , (/Ns,0,0/) , fill=Error(1._KIND_qmc) )
 END IF

 READ(Unit)tcel(1:Ns-1,m,k)
 READ(Unit)segs(1:Ns  ,m,k)

 READ(Unit)psi0(m,k)

END DO

CLOSE(Unit)

!!--end--
END SUBROUTINE



!!### SUBROUTINE: <SAVE_tracks>
SUBROUTINE SAVE_tracks(Nd,Nm,Nk,omeg,posi,tcel,segs,psi0)

!!#### PURPOSE
!! Save all tracks at once from the tracking
!! file.

!!#### REQUIRED INPUT
INTEGER :: Nd,Nm,Nk
REAL(KIND_qmc),POINTER :: omeg(:,:)
REAL(KIND_qmc),POINTER :: posi(:,:)
INTEGER       ,POINTER :: tcel(:,:,:)
REAL(KIND_qmc),POINTER :: segs(:,:,:)
REAL(KIND_qmc),POINTER :: psi0(:,:)

!!#### LOCAL VARIABLES
INTEGER :: Ns
INTEGER :: m,k,n
INTEGER :: Unit

!!--begin--

Unit=NewUnit()
OPEN(Unit,file="tracks",FORM="Unformatted",STATUS="Replace")

WRITE(Unit)Nd,Nm,Nk


DO m=1,Nm
 WRITE(Unit)omeg(1:3,m)
END DO

DO k=1,Nk
 WRITE(Unit)posi(1:Nd,k)
END DO

DO n=1,Nk*Nm

 WRITE(Unit)m
 WRITE(Unit)k

 WRITE(Unit)Ns

 Ns = SIZEa( segs(:,m,k) )

 WRITE(Unit)tcel(1:Ns-1,m,k)
 WRITE(Unit)segs(1:Ns  ,m,k)

 WRITE(Unit)psi0(m,k)

END DO

CLOSE(Unit)

!!--end--
END SUBROUTINE



END MODULE
