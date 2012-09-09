
!initialize
S=>NULL()

!quick return if X is not associated
IF( .NOT.ASSOCIATED(X) )RETURN

!allocate
ALLOCATE( S )

!USE <STR>
S = STR( X , FMT , AdjustLeft , AdjustRight )
