
IF( Unformatted_ )THEN
 
 READ(Unit)l
 READ(Unit)m
 READ(Unit)n
 IF( l>0 .AND. m>0 .AND. n>0 )THEN
  ALLOCATE( P(l,m,n) )
  DO n=LBOUND(P,3),UBOUND(P,3)
   DO m=LBOUND(P,2),UBOUND(P,2) 
    DO l=LBOUND(P,1),UBOUND(P,1)
     READ(Unit)P(l,m,n)
    END DO
   END DO
  END DO
 END IF

ELSE
 
 READ(Unit,*)l
 READ(Unit,*)m
 READ(Unit,*)n
 IF( l>0 .AND. m>0 .AND. n>0 )THEN
  ALLOCATE( P(l,m,n) )
  DO n=LBOUND(P,3),UBOUND(P,3)
   DO m=LBOUND(P,2),UBOUND(P,2) 
    DO l=LBOUND(P,1),UBOUND(P,1)
     READ(Unit,100,IOSTAT=ios)P(l,m,n)
     IF( ios/=0 )THEN
      P(l,m,n) = Error( P(l,m,n) )
     END IF
    END DO
   END DO
  END DO
 END IF

END IF

