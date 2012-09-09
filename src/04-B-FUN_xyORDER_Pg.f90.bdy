imin = 1

do i = 2, N
  if ( &
    ( Pg(1,i) < Pg(1,imin) ) .or. &
    ( Pg(1,i) == Pg(1,imin) .and. Pg(2,i) < Pg(2,imin) ) ) then
    imin = i
  end if
end do

!circular shift the values to the right orientation
order_Pg = (/ (j,j=1,N) /)
order_Pg = CSHIFT(order_Pg,imin-N-1)