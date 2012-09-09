!!# FUNCTION MODULE <<FUN_xySLICE>>
MODULE FUN_xySLICE


!!## PURPOSE
!! Return slices <Sli> of shapes made when ``cut'' by angle <theta>.
!! The actual angle theta is greatly preferred to the direction
!! U=(/cos(theta),sin(theta)/) as there can be a considerable loss
!! in precision when theta=pi/2.



!!## AUTHORS
!! [waw] W. A. Wieselquist



!!## CHANGES
!! 1 Sep. 2011 [waw] the slice area also needs to be scaled when the polygon is
!!                   but by a factor scale**2!
!! 28 August 2011 [waw]



!!## EXTERNAL MODULES
USE KND_IntrinsicTypes,ONLY: KIND_Rdp,KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
USE SUB_Sort_quick,ONLY: Sort=>Sort_quick !!((03-A-SUB_Sort_quick.f90))
USE FUN_xyINTEGRALXX
USE FUN_xyINTEGRALXY
USE FUN_xyINTEGRALYY

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## VARIABLES
REAL(KIND_Rdp),PARAMETER :: PI=3.1415926535897932384626433832795_KIND_Rdp !ACOS(-1._KIND_Rdp)
LOGICAL,SAVE :: Using_BB_Shift=.TRUE.
LOGICAL,SAVE :: Using_Scale_Factor=.TRUE.
LOGICAL,SAVE :: VERBOSE_SLICE=.FALSE.

!!## PROCEDURE OVERLOADING
INTERFACE xySLICE_Pgt
 MODULE PROCEDURE xySLICE_Pgt_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: xySLICE_Pgt
PUBLIC :: xySLICE_Pgt_Rdp
PUBLIC :: VERBOSE_SLICE

!!## CONTAINED PROCEDURES
 CONTAINS


SUBROUTINE xySLICE_Pgt_Rdp( n, pg0, theta0, TOL, &
    ns, sli, a, c, vin, vout, ein, eout, nvl, vleft, eleft, nvr, vright, eright, &
    corig, crot )

!input
INTEGER,INTENT(IN)  :: n                   !number of sides
REAL(KIND_Rdp),INTENT(IN) :: pg0(2,n)             !counter-clockwise points (xi,yi)=pg(1:2,i) for i=1..n
REAL(KIND_Rdp),INTENT(IN)  :: theta0              !angle to slice with
REAL(KIND_Rdp),INTENT(IN)  :: TOL                 !relative tolerance for thin slice fix
!output
INTEGER,INTENT(OUT) :: ns                  !number of slices
REAL(KIND_Rdp),INTENT(OUT) :: sli(3,n)            !u,vmin,vmax coordinates of slice in rotated coordinate system
                               !(ui,vmini,vmaxi)=sli(1:3,i) for i=1..ns (maximum of n)
REAL(KIND_Rdp),INTENT(OUT) :: a                   !area of polygon
REAL(KIND_Rdp),INTENT(OUT) :: c(2)                !centroid of polygon
INTEGER,INTENT(OUT) :: vin(n),vout(n)      !vertex indices of the incoming and outgoing vertices in the
                               !original polygon with vout(1)=vout(n)=0 by definition
INTEGER,INTENT(OUT) :: ein(n-1),eout(n-1)  !edge indices of the incoming and outgoing edges

INTEGER,INTENT(OUT) :: nvl,nvr             !number of hanging verts on the left and right edges
INTEGER,INTENT(OUT) :: vleft(n),vright(n)      
INTEGER,INTENT(OUT) :: eleft(n-1),eright(n-1)  
REAL(KIND_Rdp),INTENT(OUT),OPTIONAL :: corig(3),crot(3)
                         
!local
REAL(KIND_Rdp) :: cn(2),scale,xv(n),yin(n),yout(n),yv(n),theta,pg(2,n)
INTEGER :: i,j,order(n)
LOGICAL :: is_outgoing(n)
REAL(KIND_Rdp) :: barx2_b,barxy_b,bary2_b
REAL(KIND_Rdp) :: barx2_a,barxy_a,bary2_a


!transform theta
theta=PI/2._KIND_Rdp-theta0
pg=pg0

!shift
IF( Using_BB_Shift )THEN
  CALL SHIFT_BOUNDING_RECTANGLE(n,pg,c)
ELSE
  c(1:2)=0._KIND_Rdp
END IF

!get area
a = AREA(n,pg)
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'area=',a
END IF

!scale
IF( Using_Scale_Factor )THEN
  scale       = SQRT(a)
ELSE
  scale       = 1._KIND_Rdp
END IF  
pg = pg/scale
a  = a/(scale**2)
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'scale factor=',scale
    WRITE(*,*)'after scaling'
    WRITE(*,'(999(1x,es24.16))')(pg(2,i),i=1,n)
    WRITE(*,'(999(1x,es24.16))')(pg(1,i),i=1,n)
END IF

!Calculate the centroid and shift the polygon.
cn(1:2) = CENTROID( n, pg, a )
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'centroid='
    WRITE(*,'(2(1x,es24.16))')cn(1:2)
END IF

pg(1,1:n) = pg(1,1:n) - cn(1)
pg(2,1:n) = pg(2,1:n) - cn(2)
c(1:2)    = c(1:2) + cn(1:2)
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'after shifting to the true centroid'
    WRITE(*,'(999(1x,es24.16))')(pg(2,i),i=1,n)
    WRITE(*,'(999(1x,es24.16))')(pg(1,i),i=1,n)
END IF

IF( PRESENT(corig) )THEN
 barx2_b = xyINTEGRALXX_Pg(n,pg)
 barxy_b = xyINTEGRALXY_Pg(n,pg)
 bary2_b = xyINTEGRALYY_Pg(n,pg)
 corig = (/barx2_b,bary2_b,barxy_b/)
END IF


!Rotate the n-sided polygon in place about the origin (its centroid).
CALL ROTATE( n, pg, theta )
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'after rotate'
    WRITE(*,'(999(1x,es24.16))')(pg(2,i),i=1,n)
    WRITE(*,'(999(1x,es24.16))')(pg(1,i),i=1,n)
END IF

IF( PRESENT(crot) )THEN
 barx2_a = xyINTEGRALXX_Pg(n,pg)
 barxy_a = xyINTEGRALXY_Pg(n,pg)
 bary2_a = xyINTEGRALYY_Pg(n,pg)
 crot = (/barx2_a,bary2_a,barxy_a/)
END IF

!Order the slices from left to right and determine the outgoing ones
!by that ordering.
CALL ORDER_SLICE(n,pg,xv,yv,order,is_outgoing)
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'is_outgoing in terms of original verts'
    WRITE(*,'(99(1x,g23.1))')is_outgoing
    WRITE(*,*)'final order (y,x)'
    WRITE(*,'(99(1x,es23.16))')yv
    WRITE(*,'(99(1x,es23.16))')xv
    WRITE(*,'(99(1x,i23))')order
END IF

!Set y-values available on top and bottom sides and interpolate
!to get the other y-values.
CALL INTERPOLATE_SLICE( n, order, pg, is_outgoing, &
          vin, vout, yin, yout )
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'after interpolate yout/yin'
    WRITE(*,'(99(1x,es24.16))')(yout(i),i=1,n)
    WRITE(*,'(99(1x,es24.16))')(yin(i),i=1,n)
    WRITE(*,*)'after interpolate vout/vin '
    WRITE(*,'(99(1x,i24))')(vout(i),i=1,n)
    WRITE(*,'(99(1x,i24))')(vin(i),i=1,n)
END IF

!Assemble canonical slices.
sli(1,1:n) = xv(1:n)
sli(2,1:n) = yin(1:n)
sli(3,1:n) = yout(1:n)
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'before collapse sli'
    WRITE(*,'(99(1x,es24.16))')(sli(3,j),j=1,n)
    WRITE(*,'(99(1x,es24.16))')(sli(2,j),j=1,n)
    WRITE(*,'(99(1x,es24.16))')(sli(1,j),j=1,n)
END IF

!Remove thin slices in place (on return only ns values are used).
ns=n
CALL COLLAPSE_THIN_SLICES( ns, vin, vout, sli, TOL )
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'after collapse vout/vin '
    WRITE(*,'(99(1x,i24))')(vout(i),i=1,ns)
    WRITE(*,'(99(1x,i24))')(vin(i),i=1,ns)
    WRITE(*,*)'after collapse sli'
    WRITE(*,'(99(1x,es24.16))')(sli(3,j),j=1,ns)
    WRITE(*,'(99(1x,es24.16))')(sli(2,j),j=1,ns)
    WRITE(*,'(99(1x,es24.16))')(sli(1,j),j=1,ns)
END IF

!Determine the incoming and outgoing edges.
CALL GET_EDGES(ns,vin,vout,ein,eout)
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'eout/ein'
    WRITE(*,'(99(1x,i24))')(eout(j),j=1,ns-1)
    WRITE(*,'(99(1x,i24))')(ein(j),j=1,ns-1)
END IF

!Find hanging verts.
CALL LOOP_ITERATOR(n,vout(1),vin(1),vleft,nvl)
CALL LOOP_ITERATOR(n,vin(ns),vout(ns),vright,nvr)

!Only now do we set the outgoing verts to 0 on the left and right.
!IF THEY ARE THE SAME---otherwise we have a special situation
IF( vout(1)==vin(1) )THEN
  vout(1)=0
END IF
IF( vout(ns)==vin(ns) )THEN
  vout(ns)=0
END IF

!Find hanging edges.
eleft=0
eright=0

!Re-scale the slices.
sli(1:3,1:ns) = sli(1:3,1:ns)*scale
pg=pg*scale

END SUBROUTINE xySLICE_Pgt_Rdp


SUBROUTINE SHIFT_BOUNDING_RECTANGLE(n,pg,c)
INTEGER :: n
REAL(KIND_Rdp) :: pg(2,n),c(2)
!LOCALS
INTEGER :: i

!For better numerics, shift the polygon near (0,0) using the bounding rectangle.
c(1)        = 0.5_KIND_Rdp*( MINVAL(pg(1,1:n)) + MAXVAL(pg(1,1:n)) )
c(2)        = 0.5_KIND_Rdp*( MINVAL(pg(2,1:n)) + MAXVAL(pg(2,1:n)) )
pg(1,1:n) = pg(1,1:n) - c(1)
pg(2,1:n) = pg(2,1:n) - c(2)
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'shift bounding rectangle'
    WRITE(*,'(999(1x,es24.16))')(pg(2,i),i=1,n)
    WRITE(*,'(999(1x,es24.16))')(pg(1,i),i=1,n)
END IF
END SUBROUTINE


FUNCTION SLICE_AREA(n,sli) RESULT(AREA)
INTEGER :: n
REAL(KIND_Rdp) :: sli(3,n)
REAL(KIND_Rdp) :: sminbar,smaxbar,dt
REAL(KIND_Rdp) :: AREA
INTEGER :: i

AREA = 0._KIND_Rdp

DO i=1,n-1
    sminbar=0.5_KIND_Rdp*(sli(2,i+1)+sli(2,i))
    smaxbar=0.5_KIND_Rdp*(sli(3,i+1)+sli(3,i))
    dt=sli(1,i+1)-sli(1,i)
    AREA = AREA + dt*(smaxbar-sminbar)
END DO

END FUNCTION SLICE_AREA


!returns 0 if start=finish
SUBROUTINE LOOP_ITERATOR(n,start,finish,list,i)
INTEGER :: n,start,finish,i
INTEGER :: list(n)
INTEGER :: j
list(1:n)=0
i=0
IF( start>n .OR. finish>n .OR. start<1 .OR. finish<1 )THEN
    RETURN
ELSE
    IF( START .NE. FINISH )THEN
        j=start
        DO WHILE(.TRUE.)
            j=NEXT_CCW(j,n)
            !WRITE(*,*)'test j=',j
            IF( j.NE.finish )THEN
                i=i+1
                list(i)=j
                !WRITE(*,*)'added that j to element i=',i
            ELSE
                EXIT
            END IF
        END DO
    END IF
END IF
END SUBROUTINE



SUBROUTINE ORDER_SLICE(n,pg,xv,yv,order,is_outgoing)
!input
INTEGER :: n                   !number of sides
REAL(KIND_Rdp) :: pg(2,n)             !counter-clockwise points (xi,yi)=pg(1:2,i) for i=1..n
REAL(KIND_Rdp) :: xv(n),yv(n)
!output
INTEGER :: order(n)
LOGICAL :: is_outgoing(n)
                               !original polygon with vout(1)=vout(n)=0 by definition                         
!local
INTEGER :: i,j,ixl,ixr
INTEGER :: ordery(n)
INTEGER :: order2(n)

i=1
DO j=1,n
  xv(j)    = pg(1,i)
  yv(j)    = pg(2,i)
  order(j) = j
  i = NEXT_CCW(i,n)
END DO
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'pre x-sort (y,x)'
    WRITE(*,'(99(1x,es23.16))')yv
    WRITE(*,'(99(1x,es23.16))')xv
    WRITE(*,'(99(1x,i23))')order
END IF

CALL SORT( xv(1:n), order(1:n) )
i=1
DO j=1,n
    yv(j) = pg(2,order(j))
    i = NEXT_CCW(i,n)
END DO
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'post x-sort (y,x)'
    WRITE(*,'(99(1x,es23.16))')yv
    WRITE(*,'(99(1x,es23.16))')xv
    WRITE(*,'(99(1x,i23))')order
END IF

DO j=2,n
    IF( ABS(xv(1)-xv(j))>EPSILON(1._KIND_Rdp) )EXIT
END DO
ixl=j-1
if( ixl-1>0 )then
    CALL SORT( yv(1:ixl), ordery(1:ixl) )
    order2=order
    DO i=1,ixl
        order(i)=order2(ordery(i))
    END DO
    IF( VERBOSE_SLICE )THEN
        WRITE(*,*)'post y-sort 1 (y,x)'
        WRITE(*,'(99(1x,es23.16))')yv
        WRITE(*,'(99(1x,es23.16))')xv
        WRITE(*,'(99(1x,i23))')order
    END IF
end if
DO j=n-1,ixl+1,-1
    IF( ABS(xv(n)-xv(j))>EPSILON(1._KIND_Rdp) )EXIT
END DO
ixr=j+1
if( n-ixr>0 )then
    CALL SORT( yv(ixr:n), ordery(ixr:n) )
    order2=order
    DO i=ixr,n
        order(i)=order2(ixr-1+ordery(i))
    END DO
    IF( VERBOSE_SLICE )THEN
        WRITE(*,*)'post y-sort 2 (y,x)'
        WRITE(*,'(99(1x,es23.16))')yv
        WRITE(*,'(99(1x,es23.16))')xv
        WRITE(*,'(99(1x,i23))')order
    END IF
end if

ixl=order(1)
ixr=order(n)
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'ixl,ixr'
    WRITE(*,*)ixl,ixr
END IF

j=ixl
is_outgoing=.TRUE.
DO i=1,n
    is_outgoing(j)=.FALSE.
    IF( j==ixr )EXIT
    j=NEXT_CCW(j,n)
END DO

END SUBROUTINE ORDER_SLICE

FUNCTION NEXT_CCW( i, n ) RESULT(next)
INTEGER :: i,n,next
next=MOD(i,n)+1
IF( next.LE.0 )THEN
  next=next+n
END IF
END FUNCTION NEXT_CCW

REAL(KIND_Rdp) FUNCTION AREA( n, pg ) 
INTEGER :: n
REAL(KIND_Rdp) :: pg(2,n)
INTEGER :: i,next
AREA = 0._KIND_Rdp
DO i=1,n
  next=NEXT_CCW(i,n)
  AREA = AREA + pg(1,i)*pg(2,next) - pg(1,next)*pg(2,i)
END DO
AREA = 0.5*AREA

IF( AREA.LE.0 )THEN
  WRITE(*,*)'area=',AREA
  STOP 'polygon is not counter-clockwise'
END IF
END FUNCTION AREA

FUNCTION CENTROID( n, pg, area ) RESULT(c)
INTEGER :: n
REAL(KIND_Rdp) :: pg(2,n),area,c(2)
REAL(KIND_Rdp) :: xc,yc,a,b
INTEGER :: i,next
xc=0
yc=0
DO i=1,n
  next=NEXT_CCW(i,n)
  a = ( pg(1,i)*pg(2,next) - pg(1,next)*pg(2,i) )
  xc = xc + ( pg(1,i) + pg(1,next) )*a
  yc = yc + ( pg(2,i) + pg(2,next) )*a
END DO
b  = 1.0/(6*area)
xc = xc*b
yc = yc*b
c=(/xc,yc/)
END FUNCTION CENTROID


SUBROUTINE ROTATE( n, pg, theta )
INTEGER :: n
REAL(KIND_Rdp) :: pg(2,n),theta
INTEGER :: i
REAL(KIND_Rdp) :: x,y,r,cos_t,sin_t,cos_dt,sin_dt,cos_tp,sin_tp
DO i=1,n
  x = pg(1,i)
  y = pg(2,i)
  r = SQRT( x**2 + y**2 )
  cos_t = x/r
  sin_t = y/r
  cos_dt = COS(theta) 
  sin_dt = SIN(theta)
  cos_tp = cos_t*cos_dt - sin_t*sin_dt
  sin_tp = sin_t*cos_dt + cos_t*sin_dt
  pg(1,i)=r*cos_tp
  pg(2,i)=r*sin_tp
END DO
END SUBROUTINE ROTATE


SUBROUTINE INTERPOLATE_SLICE( n, order, pg, is_outgoing, &
   vin,vout, yin,yout )
!remember: is_outgoing is a function of original vert indices as in pg
INTEGER :: n,order(n),vin(n),vout(n)
REAL(KIND_Rdp) :: pg(2,n),yin(n),yout(n)
LOGICAL :: is_outgoing(n)
INTEGER :: i,o,ibl,ibr,itl,itr
REAL(KIND_Rdp) :: inf,xv(n)
inf=HUGE(1._KIND_Rdp)
yin  = -inf
yout = +inf
vin   =  0
vout  =  0
DO i=1,n
  o = order(i)
  xv(i)=pg(1,o)
  IF( is_outgoing(o) )THEN
    vout(i)  = o
    vin(i)   = 0
    yout(i)  = pg(2,o)
  ELSE
    vin(i)   = o
    vout(i)  = 0
    yin(i)   = pg(2,o)      
  END IF
END DO
yout(1)=yin(1)
yout(n)=yin(n)
vout(1)=vin(1)
vout(n)=vin(n)
IF( VERBOSE_SLICE )THEN
    WRITE(*,*)'yout/yin A'
    WRITE(*,'(99(1x,es24.16))')(yout(i),i=1,n)
    WRITE(*,'(99(1x,es24.16))')(yin(i),i=1,n)
    WRITE(*,*)'vout/vin A'
    WRITE(*,'(99(1x,i24))')(vout(i),i=1,n)
    WRITE(*,'(99(1x,i24))')(vin(i),i=1,n)
END IF

DO i=2,n-1
  IF( vin(i).EQ.0 )THEN
    DO ibr=i+1,n
      IF( vin(ibr).NE.0 )EXIT
    END DO
    DO ibl=i-1,1,-1
      IF( vin(ibl).NE.0 )EXIT
    END DO
    yin(i) = yin(ibl) + &
              (     xv(i) -    xv(ibl) )* &
              (  yin(ibr) -   yin(ibl) )/ &
              (   xv(ibr) -    xv(ibl) )
  ELSE IF( vout(i).EQ.0 )THEN
    DO itr=i+1,n
      IF( vout(itr).NE.0 )EXIT
    END DO
    DO itl=i-1,1,-1
      IF( vout(itl).NE.0 )EXIT
    END DO
    yout(i) = yout(itl) + &
              (     xv(i) -    xv(itl) )* &
              ( yout(itr) -  yout(itl) )/ &
              (   xv(itr) -    xv(itl) )
  END IF
END DO

END SUBROUTINE INTERPOLATE_SLICE

SUBROUTINE GET_EDGES( n, vin, vout, ein, eout )
INTEGER :: n,vin(n),vout(n)
INTEGER :: ein(n-1),eout(n-1)  !edge indices of the incoming and outgoing edges in the
                               !original polygon
INTEGER :: i
ein(1)=vin(1)
DO i=2,n-1
    IF( vin(i) .NE. 0 )THEN
      ein(i)=vin(i)
    ELSE
      ein(i)=ein(i-1)
    END IF
END DO
eout(n-1)=vout(n)
DO i=n-2,1,-1
    IF( vout(i+1) .NE. 0 )THEN
      eout(i)=vout(i+1)
    ELSE
      eout(i)=eout(i+1)
    END IF
END DO
END SUBROUTINE GET_EDGES


SUBROUTINE COLLAPSE_THIN_SLICES( n, vin, vout, sli, TOL )
INTEGER :: n,vin(n),vout(n)
REAL(KIND_Rdp) :: sli(3,n),TOL
INTEGER :: ns,i,j,k,ihr,ihl
REAL(KIND_Rdp) :: thin_dw,dw
ihl=0
ihr=0
ns=n
thin_dw=TOL*(sli(1,n)-sli(1,1))

!collapse contiguous thin slices on the left
DO k=1,ns-1
    dw=sli(1,2)-sli(1,1)
    IF( dw .LE. thin_dw )THEN
        IF( vout(2).EQ.0 )THEN
            vout(2)=vout(1)
        ELSE IF( vin (2).EQ.0 )THEN
            vin(2)=vin(1)
        END IF
        CALL DELETE_PARTITION(1,ns,vin,vout,sli)
    ELSE 
        EXIT
    END IF
    IF( VERBOSE_SLICE )THEN
        WRITE(*,*)'sli (left) on fix iteration k=',k
        WRITE(*,'(99(1x,es24.16))')(sli(3,j),j=1,ns)
        WRITE(*,'(99(1x,es24.16))')(sli(2,j),j=1,ns)
        WRITE(*,'(99(1x,es24.16))')(sli(1,j),j=1,ns)
        WRITE(*,*)'vout/vin'
        WRITE(*,'(99(1x,i24))')(vout(j),j=1,ns)
        WRITE(*,'(99(1x,i24))')(vin(j),j=1,ns)
    END IF
END DO


!collapse contiguous thin slices on the right
DO k=1,ns-1
    dw=sli(1,ns)-sli(1,ns-1)
    IF( dw .LE. thin_dw )THEN
        IF( vout(ns-1).EQ.0 )THEN
            vin(ns)=vin(ns-1)
        ELSE IF( vin (ns-1).EQ.0 )THEN
            vout(ns)=vout(ns-1)
        END IF
        CALL DELETE_PARTITION(ns-1,ns,vin,vout,sli)
    ELSE 
        EXIT
    END IF
    IF( VERBOSE_SLICE )THEN
        WRITE(*,*)'sli (right) on fix iteration k=',k
        WRITE(*,'(99(1x,es24.16))')(sli(3,j),j=1,ns)
        WRITE(*,'(99(1x,es24.16))')(sli(2,j),j=1,ns)
        WRITE(*,'(99(1x,es24.16))')(sli(1,j),j=1,ns)
        WRITE(*,*)'vout/vin'
        WRITE(*,'(99(1x,i24))')(vout(j),j=1,ns)
        WRITE(*,'(99(1x,i24))')(vin(j),j=1,ns)
    END IF
END DO

!collapse contiguous thin slices in the middle
i=2
DO k=2,ns-2
    dw=sli(1,i+1)-sli(1,i)
    IF( dw .LE. thin_dw )THEN
        IF( vout(i+1).EQ.0 )THEN
            vout(i+1)=vout(i)
        ELSE IF( vin (i+1).EQ.0 )THEN
            vin(i+1)=vin(i)
        END IF
        CALL DELETE_PARTITION(i,ns,vin,vout,sli)
    END IF
    IF( VERBOSE_SLICE )THEN
        WRITE(*,*)'sli (center) on fix iteration i=',i
        WRITE(*,'(99(1x,es24.16))')(sli(3,j),j=1,ns)
        WRITE(*,'(99(1x,es24.16))')(sli(2,j),j=1,ns)
        WRITE(*,'(99(1x,es24.16))')(sli(1,j),j=1,ns)
        WRITE(*,*)'vout/vin'
        WRITE(*,'(99(1x,i24))')(vout(j),j=1,ns)
        WRITE(*,'(99(1x,i24))')(vin(j),j=1,ns)
    END IF
    i = i + 1
    IF( i>=ns )EXIT
END DO

n=ns
END SUBROUTINE COLLAPSE_THIN_SLICES

!erases partition i and moves all other partitions left 1
SUBROUTINE DELETE_PARTITION(i,ns,vin,vout,sli)
INTEGER :: i,ns
INTEGER :: vin(ns),vout(ns)
REAL(KIND_Rdp) :: sli(3,ns)

vin(i:ns-1) = vin(i+1:ns)
vout(i:ns-1) = vout(i+1:ns)

sli(1,i+1)         = SMALLEST(sli(1,i),sli(1,i+1))
sli(2,i+1)         = MIN(sli(2,i),sli(2,i+1))
sli(3,i+1)         = MAX(sli(3,i),sli(3,i+1))
sli(1:3,i:ns-1) = sli(1:3,i+1:ns)

ns=ns-1

END SUBROUTINE DELETE_PARTITION

REAL(KIND_Rdp) FUNCTION SMALLEST(a,b)
REAL(KIND_Rdp) :: a,b,c
IF( ABS(a)<ABS(b) )THEN
  c=a
ELSE
  c=b
END IF
SMALLEST=c
END FUNCTION SMALLEST



END MODULE



