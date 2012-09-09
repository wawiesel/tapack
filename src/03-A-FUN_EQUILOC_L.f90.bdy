loc = 0

DO i=1,SIZE(list)
 IF( list(i) .AND. val )THEN
  loc = i
  RETURN
 END IF
END DO
