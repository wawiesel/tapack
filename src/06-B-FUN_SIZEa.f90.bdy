DO N=SIZE(ARRAY),1,-1
 IF( .NOT.IsError(ARRAY(N)) )EXIT
END DO
