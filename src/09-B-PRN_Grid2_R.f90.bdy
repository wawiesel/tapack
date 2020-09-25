CALL CLEAR(x_)
IF( PRESENT(x) )THEN
 fmt_x = FMT(x(1),F="E",Pre="(",Post=")")
 x_ = STR(x                    ,FMT=STR(fmt_x))
ELSE
 fmt_x = FMT(1   ,F="I",Pre="(",Post=")")
 x_ = STR(Sequence(1,1,SIZE(x)),FMT=STR(fmt_x))
END IF

CALL CLEAR(y_)
IF( PRESENT(y) )THEN
 fmt_y = FMT(y(1),F="E",Pre="(",Post=")")
 y_(0)         = " "
 Y_(1:SIZE(y)) = STR(y                      ,FMT=STR(fmt_y))
ELSE
 fmt_y = FMT(1   ,F="I",Pre="(",Post=")")
 y_(0)         = " "
 Y_(1:SIZE(y)) = STR(Sequence(1,1,SIZE(y)-1),FMT=STR(fmt_y))
END IF

fmt_z=FMT(z(1,1))
CALL Print_Array2(Z,DEFAULT(window_unit,Unit),&
  NSEP=0,NPER=SIZE(x_),xHDR=x_,yHDR=y_,FMT=STR(fmt_z))

!clear formats
fmt_x=""
fmt_y=""
fmt_z=""