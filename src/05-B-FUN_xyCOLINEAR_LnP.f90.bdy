COLINEAR = xyCOLINEAR_P3( &
  RESHAPE( (/ Ln(:,1) , P , Ln(:,1) + c_1*Ln(:,2) /) , (/2,3/) ) , &
  tol , err )
