!!#### PURPOSE
!! Interpolate (x0,y0) between two points x and y.  It is
!! assumed that (x0,y0) is between the two points, such 
!! that this is true interpolation.

!!#### METHOD
!! Simple linear interpolation.

f2 = (x0-x(k(1)))/(x(k(2))-x(k(1)))
f1 = 1._KIND_y - f2
y0 = f1*y(k(1)) + f2*y(k(2))
