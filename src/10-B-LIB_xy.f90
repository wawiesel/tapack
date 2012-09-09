!!# LIBRARY MODULE <<LIB_xy>>
MODULE LIB_xy

!!## PURPOSE
!! A library of routines for $2D$ computational geometry.

!!## DEPENDENCIES
!! * basic kinds
!! * defined constants (<c_PI>$=\pi$, etc.)
!! * the error function (<Error(X)> is the error number for
!!   the type of <X>
!! * add all computational geometry routines
USE KND_IntrinsicTypes                   !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp                    !!((02-A-PAR_Constants_Rdp.f90))
USE FUN_Error                            !!((04-A-FUN_Error.f90))
USE TBX_ComputationalGeometry            !!((09-A-TBX_ComputationalGeometry.f90))
USE FUN_xyAREA                           !!((04-B-FUN_xyAREA.f90))
USE FUN_xySAREA                          !!((03-A-FUN_xySAREA.f90))
USE FUN_Upcase                           !!((03-A-FUN_Upcase.f90))
USE FUN_Default                          !!((04-A-FUN_Default.f90))
USE VAR_ComputationalGeometry            !!((03-A-VAR_ComputationalGeometry.f90))
USE FUN_STR                              !!((05-B-FUN_STR.f90))
USE VAR_Units                            !!((03-A-VAR_Units.f90))
USE FUN_xyROTATE                         !!((05-B-FUN_xyROTATE.f90))
USE FUN_xyTRANSLATE                      !!((05-B-FUN_xyTRANSLATE.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

INTEGER,PARAMETER :: Unit_noisy=86
PRIVATE :: Unit_noisy


!!## PUBLIC ACCESS LIST
PUBLIC :: xyOUTFACES_PgU
PUBLIC :: xyIS_CONVEX_CCW_Pg
PUBLIC :: GNUPLOT_Pg,GNUPLOT_Ls,GNUPLOT_Sli,GNUPLOT_P
PUBLIC :: GNUPLOT_OUTFACES
!! test shapes

!!## CONTAINED PROCEDURES
CONTAINS






SUBROUTINE GNUPLOT_Pg( Unit , N , Pg )
INTEGER       ,INTENT(IN)  :: Unit
INTEGER       ,INTENT(IN)  :: N
REAL(KIND_Rdp),INTENT(IN)  :: Pg(2,N)
REAL(KIND_Rdp) :: P1(2),P2(2)
INTEGER :: i1,i2
!!--begin--

i1 = N
i2 = 1
DO

 P1 = Pg(:,i1)
 P2 = Pg(:,i2)

 WRITE(Unit,*)P1
 WRITE(Unit,*)P2
 WRITE(Unit,*)

 i1 = MOD(i1,N)+1
 i2 = i2 + 1

 IF( i2>N )THEN
  EXIT
 END IF

END DO
WRITE(Unit,*)

!!--end--
END SUBROUTINE



SUBROUTINE GNUPLOT_Sli( Unit , N , Sli )
INTEGER       ,INTENT(IN)  :: Unit
INTEGER       ,INTENT(IN)  :: N
REAL(KIND_Rdp),INTENT(IN)  :: Sli(3,N)
REAL(KIND_Rdp)  :: Qp(2,4)
INTEGER :: i

!!--begin--

DO i=1,N-1
 !get the parallelogram
 Qp(:,1) = (/ Sli(1,i  ),Sli(2,i)   /)
 Qp(:,2) = (/ Sli(1,i+1),Sli(2,i+1) /)
 Qp(:,3) = (/ Sli(1,i+1),Sli(3,i+1) /)
 Qp(:,4) = (/ Sli(1,i  ),Sli(3,i)   /)

 CALL GNUPLOT_Pg( Unit , 4 , Qp )
END DO

!!--end--
END SUBROUTINE



SUBROUTINE GNUPLOT_P( Unit , P )
INTEGER       ,INTENT(IN)  :: Unit
REAL(KIND_Rdp),INTENT(IN)  :: P(2)
!!--begin--

WRITE(Unit,*)P
WRITE(Unit,*)
WRITE(Unit,*)

!!--end--
END SUBROUTINE


SUBROUTINE GNUPLOT_Ls( Unit , Ls )
INTEGER       ,INTENT(IN)  :: Unit
REAL(KIND_Rdp),INTENT(IN)  :: Ls(2,2)
!!--begin--

WRITE(Unit,*)Ls(:,1)
WRITE(Unit,*)Ls(:,2)
WRITE(Unit,*)
WRITE(Unit,*)

!!--end--
END SUBROUTINE



SUBROUTINE GNUPLOT_U( Unit , U )
INTEGER       ,INTENT(IN)  :: Unit
REAL(KIND_Rdp),INTENT(IN)  :: U(2)
!!--begin--

WRITE(Unit,*)(/0._KIND_Rdp,0._KIND_Rdp/)
WRITE(Unit,*)U
WRITE(Unit,*)
WRITE(Unit,*)

!!--end--
END SUBROUTINE






SUBROUTINE GNUPLOT_OutFaces( N , Pg , U )
USE KND_IntrinsicTypes                   !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_xyLINESEGMENT                    !!((05-B-FUN_xyLINESEGMENT.f90))
USE PAR_Constants_Rdp                    !!((02-A-PAR_Constants_Rdp.f90))
USE FUN_NewFile                          !!((05-B-FUN_NewFile.f90))
INTEGER :: N
REAL(KIND_Rdp) :: Pg(2,N),U(2)
REAL(KIND_Rdp) :: Ls(2,N)
INTEGER :: i,i1,i2
INTEGER :: Unit_outface,Unit_inface,Unit_outverts,Unit_inverts
LOGICAL :: OUTFACES(N),OUTVERTS(N)

!!--begin--

Unit_outface = NewFile("outface")
Unit_inface = NewFile("inface")
Unit_outverts = NewFile("outverts")
Unit_inverts = NewFile("inverts")

OUTFACES = xyOUTFACES_PgU( N , Pg , U , OUTVERTS )

DO i=1,N

 !get indices
 i1 = i
 i2 = MOD(i,N) + 1

 Ls = xyLINESEGMENT_PP( Pg(:,i1) , Pg(:,i2) )

 IF( OUTFACES(i) )THEN
  CALL GNUPLOT_Ls( Unit_outface , Ls )
 ELSE
  CALL GNUPLOT_Ls( Unit_inface , Ls )
 END IF

 IF( OUTVERTS(i) )THEN
  CALL GNUPLOT_P( Unit_outverts , Pg(:,i) )
 ELSE
  CALL GNUPLOT_P( Unit_inverts , Pg(:,i) )
 END IF

END DO

!!--end--
END SUBROUTINE



FUNCTION xyOUTFACES_PgU( N , Pg , U , OUTVERTS ) RESULT(OUTFACES)
USE KND_IntrinsicTypes                   !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_xyROTATE                         !!((05-B-FUN_xyROTATE.f90))
USE FUN_xyDIRECTION                      !!((04-B-FUN_xyDIRECTION.f90))
USE FUN_xyVECTOR                         !!((03-A-FUN_xyVECTOR.f90))
USE FUN_xyDOT                            !!((03-A-FUN_xyDOT.f90))
USE PAR_Constants_Rdp                    !!((02-A-PAR_Constants_Rdp.f90))

INTEGER,INTENT(IN) :: N
REAL(KIND_Rdp),INTENT(IN) :: Pg(2,N)
LOGICAL,OPTIONAL,INTENT(OUT) :: OUTVERTS(N)
LOGICAL :: OUTFACES(N)
INTEGER :: i,i1,i2
REAL(KIND_Rdp) :: D(2),U(2)

!!--begin--

DO i=1,N

 !get indices
 i1 = i
 i2 = MOD(i,N) + 1

 !get outward normal <D> of face from <i1> to <i2>
 D = xyROTATE_U( &
       xyDIRECTION_V( &
         xyVECTOR_PP( Pg(:,i1) , Pg(:,i2) ) &
       ) , -c_PI_by_2 )

 !check outward normal for positive direction
 !with respect to <U>
 OUTFACES(i) = xyDOT_VV( D , U )>=0._KIND_Rdp

END DO

IF( PRESENT(OUTVERTS) )THEN
 i1=N
 i2=1
 DO i=1,N
  OUTVERTS(i) = OUTFACES(i1) .AND. OUTFACES(i2)
  i1 = i
  i2 = MOD(i,N) + 1
 END DO
END IF

!!--end--
END FUNCTION


FUNCTION xyIS_CONVEX_CCW_Pg(n,pg,angle,each_pass) RESULT(pass)
INTEGER :: n
REAL(8) :: pg(2,n)
LOGICAL :: pass
REAL(8),INTENT(OUT),OPTIONAL :: angle(n)
LOGICAL,INTENT(OUT),OPTIONAL :: each_pass(n)
REAL(8) :: dx1,dx2,dy1,dy2,r1,r2
REAL(8) :: c,d,tol_
LOGICAL :: this_pass
LOGICAL,PARAMETER :: VERBOSE=.FALSE.
INTEGER :: i,next,last
pass=.TRUE.
tol_=sqrt(epsilon(1.d0))
i=n
next=1
dx1=pg(1,next)-pg(1,i)
dy1=pg(2,next)-pg(2,i)
r1=sqrt(dx1**2+dy1**2)
DO i=1,n
    next=MOD(i,n)+1
    dx2=pg(1,next)-pg(1,i)
    dy2=pg(2,next)-pg(2,i)
    r2=sqrt(dx2**2+dy2**2)
    c=dx1*dy2-dx2*dy1
    d=c/(r1*r2)
    this_pass=(d>=-tol_)
    pass=pass.AND.this_pass
    IF( PRESENT(angle) .OR. PRESENT(each_pass) )THEN
        IF( PRESENT(angle) )THEN
            angle(i)=ASIN(d)
        END IF
        IF( PRESENT(each_pass) )THEN
            each_pass(i)=this_pass
        END IF
    END IF
    IF( VERBOSE )THEN
        last=i-1
        IF( last==0 )THEN
            last=n
        END IF
        WRITE(*,'(a,i4,a,i4,a,es21.12,a,l3)')'from side=',last,' to side=',i,' angle=',ASIN(d),' pass=',this_pass
    END IF
    IF( .NOT.(PRESENT(angle).OR.PRESENT(each_pass)) .AND. (.NOT.pass) )THEN
        RETURN
    END IF
    dx1=dx2
    dy1=dy2
    r1=r2
END DO

END FUNCTION xyIS_CONVEX_CCW_Pg




END MODULE
