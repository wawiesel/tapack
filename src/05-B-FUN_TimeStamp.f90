MODULE FUN_Timestamp
!!#### PURPOSE
!! Returns the current YMDHMS date as a time stamp.

!!#### AUTHOR
!! John Burkardt, modified 31 May 2001

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PUBLIC

!!#### PARAMETERS
character( len = 9 ), parameter, dimension(12) :: month = (/ &
    "January  ", "February ", "March    ", "April    ", &
    "May      ", "June     ", "July     ", "August   ", &
    "September", "October  ", "November ", "December " /)

CONTAINS


function TimeStamp( ) result(string)
  character ( len = 8 ) date
  character ( len = 8 ) ampm

character(len(ampm)+len(month)+23) :: string
  integer d
  integer h
  integer m
  integer mm
  integer n
  integer s
  integer values(8)
  integer y
  character ( len = 10 )  time
  character ( len = 5 ) zone
!
  call date_and_time ( date, time, zone, values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = "AM"
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = "Noon"
    else
      ampm = "PM"
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = "PM"
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = "Midnight"
      else
        ampm = "AM"
      end if
    end if
  end if

  write ( string, "(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)" ) &
    trim ( month(m) ), d, y, h, ":", n, ":", s, ".", mm, trim ( ampm )

  return
endfunction


function TimeStampM( ) result(string)
  character ( len = 8 ) date

character(len(month)+23) :: string
  integer d
  integer h
  integer m
  integer mm
  integer n
  integer s
  integer values(8)
  integer y
  character ( len = 10 )  time
  character ( len = 5 ) zone
!
  call date_and_time ( date, time, zone, values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  write ( string, "(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3)" ) &
    trim ( month(m) ), d, y, h, ":", n, ":", s, ".", mm

  return
endfunction


ENDMODULE
