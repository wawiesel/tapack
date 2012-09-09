
!initialize
S=>NULL()

!quick return if X is not associated
IF( .NOT.ASSOCIATED(VSin) )RETURN

!allocate
ALLOCATE( S(1:SIZE(VSin)) )

!USE <STR>
DO i=1,SIZE(VSin)
 S(i) = STR( char(VSin(i)) , FMT , AdjustLeft , AdjustRight )
END DO