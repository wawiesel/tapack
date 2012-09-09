!# MODULE <<USR_Iptr>>
MODULE USR_Iptr

!!## PURPOSE
!! Provide a "pointer of integers" type.


!!## DETAILS
!! Useful for having an array of lists
!! of different sizes.


!!## DERIVED TYPES
TYPE TYPE_Iptr
 INTEGER,POINTER :: list(:)=>NULL()
END TYPE

PUBLIC :: TYPE_Iptr
PUBLIC :: REALLOCATE_IPtr

CONTAINS

SUBROUTINE REALLOCATE_IPtr(IPtr,dN)
TYPE(TYPE_IPtr),POINTER :: IPtr(:)
INTEGER,INTENT(IN) :: dN
TYPE(TYPE_IPtr),POINTER :: IPtr2(:)
IF( ASSOCIATED(IPtr) )THEN
    IPtr2=>IPtr
    IPtr=>NULL()
    ALLOCATE(IPtr(SIZE(IPtr2)+dN))
    DO n=1,SIZE(IPtr2)
        IPtr(n)%list=>IPtr2(n)%list
        IPtr2(n)%list=>NULL()
    END DO
    DEALLOCATE(IPtr2)
ELSEIF( dN>0 )THEN
    ALLOCATE(Iptr(dN))
END IF

END SUBROUTINE

END MODULE
