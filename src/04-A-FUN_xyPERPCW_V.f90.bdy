!fix it to be clockwise if y=0 and x>0 (branch cut)
IF( V(2)==c_0 .AND. V(1)>c_0 )THEN
 xyV = (/ c_0 , -V(1) /)
ELSE
 xyV = -xyPERPCCW_V( V )
END IF 