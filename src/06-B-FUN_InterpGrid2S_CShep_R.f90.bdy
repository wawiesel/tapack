!! Init.
IF( PRESENT(errmsg) )CALL CLEAR(errmsg)

!! Determine <N>
N       = SIZE(r,2)

!! Determine optionals.
MAX_NC_ = DEFAULT(MIN(40,N-1),MAX_NC)
NC_     = DEFAULT(17,NC)
MAX_NW_ = DEFAULT(MIN(40,N-1),MAX_NW)
NW_     = DEFAULT(MERGE(CEILING(1.5*NC_),30,N<=200),NW)
NR_     = DEFAULT(N,NR)

!! maximum fixups
IF( NC_>MAX_NC_ )NC_ = MAX_NC_
IF( NW_>MAX_NW_ )NW_ = MAX_NW_

!! Allocate stuff.
ALLOCATE( RW(1:N) , A(1:9,1:N) , LCELL(1:NR_,1:NR_) , LNEXT(1:N) )

!! Small size error.
IF( N<10 )THEN
 IER = -1010
 IF( PRESENT(errmsg) )THEN
  errmsg = "In order to determine the Shephard cubic interpolating&
    & polynomial, at least 10 data points are required. &
    & You provided "//TRIM(STR(N))//"."
 END IF
 GOTO 666
END IF

!! Determine the interpolating surface.
CALL CSHEP2(N,&           
            REAL(r(1,:),KIND_Rdp), &     !X
            REAL(r(2,:),KIND_Rdp), &     !Y
            REAL(F(:),KIND_Rdp), &       !F
            NC_,NW_,NR_,LCELL,LNEXT,XMIN_,&
            YMIN_,DX_,DY_,RMAX_,RW,A,IER)

IF( IER/=0 )THEN
 IF( PRESENT(errmsg) )THEN
  SELECT CASE(IER)
   CASE(1) ; write(errmsg,"(a)")"Parameter N, NC, NW, or NR is outside its &
                                &valid range.  Try passing your own values. [CSHEP2]"
   CASE(2) ; write(errmsg,"(a)")"Duplicate nodes were encountered. [CSHEP2]"
   CASE(3) ; write(errmsg,"(a)")"All nodes are collinear. [CSHEP2]"
   CASE DEFAULT ; write(errmsg,"(a)")"Unknown Error. [CSHEP2]"
  END SELECT
 END IF
 GOTO 666
END IF

!! Set function values on the grid.
DO iy=1,SIZE(yout)
 DO ix=1,SIZE(xout)
  Fout(ix,iy) = CS2VAL( REAL(xout(ix),KIND_Rdp), &
                        REAL(yout(iy),KIND_Rdp),&
                        N,&
                        REAL(r(1,:),KIND_Rdp),&
                        REAL(r(2,:),KIND_Rdp),&
                        REAL(F(:),KIND_Rdp),&
                        NR_,LCELL,LNEXT,XMIN_,YMIN_,DX_,DY_,RMAX_,RW,A)
 END DO
END DO

666 CONTINUE

IF( IER/=0 )THEN
 Fout = ERROR(Fout(1,1))
END IF

!! Deallocate
DEALLOCATE( RW , A , LCELL , LNEXT )

!! Return optional output.
IF( PRESENT(errint) )errint=IER
IF( PRESENT(XMIN) )XMIN=XMIN_
IF( PRESENT(YMIN) )YMIN=YMIN_
IF( PRESENT(DX) )DX=DX_
IF( PRESENT(DY) )DY=DY_
IF( PRESENT(RMAX) )RMAX=RMAX_