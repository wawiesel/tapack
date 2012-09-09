!fix it to be clockwise if y=0 and x>0 (branch cut)
IF( U(2)==c_0 .AND. U(1)>c_0 )THEN
 xyU = (/ c_0 , -U(1) /)
ELSE
 xyU = -xyPERPCCW_U( U )
END IF 