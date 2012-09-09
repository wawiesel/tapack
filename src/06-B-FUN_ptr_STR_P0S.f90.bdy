
!initialize
!S=>NULL()

!quick return if X is not associated
!IF( .NOT.ASSOCIATED(Sin) )RETURN

!allocate
!ALLOCATE( S )

!USE <STR>
S = STR( Sin , FMT , AdjustLeft , AdjustRight )
