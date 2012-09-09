!!### USER MODULE: <USR_SimpleList>
MODULE USR_SimpleList

!!## PURPOSE
!! The SimpleList is a simple pointer List of strings or
!! integers.  A user can <ADD> to the list, <REMOVE>
!! from the list and <FINALIZE> the list to the exact
!! size for the number of items.


!!## DETAILS
!! @ Items are added to the list in constant time,
!! @ removed from the list in linear time, and
!! @ the list is finalized in constant time.


!!## NOTES
!! For lists of strings, you may find the index of a
!! paricular string with <INDEXa(LIST,VAL)>.  For lists
!! of integers or reals, you may find the location of
!! a particular value with <EQUILOC(LIST,VAL)>.


!!## AUTHOR
!! William Wieselquist


!!## VERSION
!! 1.1


!!## LAST MODIFIED
!! 001.2006


!!## CONTACT
!! william.wieselquist AT gmail.com


!!## MODULES
!! @ function to return the index of a string in an array of strings
!! @ function to return the trimmed length (really trimmed SIZE) of an array of strings
!! @ reallocation routine
!! @ string clearing routine
USE FUN_INDEXa         !!((08-B-FUN_INDEXa.f90))
USE FUN_LEN_TRIMa      !!((03-A-FUN_LEN_TRIMa.f90))
USE SUB_Reallocate     !!((04-B-SUB_Reallocate.f90))
USE SUB_CLEAR          !!((04-A-SUB_CLEAR.f90))
USE FUN_SIZEa          !!((06-B-FUN_SIZEa.f90))
USE FUN_Error          !!((04-A-FUN_Error.f90))
USE FUN_EQUILOC        !!((03-A-FUN_EQUILOC.f90))
USE ISO_varying_string !!((03-A-ISO_varying_string.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## INTERFACES
INTERFACE ADD_TO_LIST
 MODULE PROCEDURE ADD_TO_LIST_S
 MODULE PROCEDURE ADD_TO_LIST_I
END INTERFACE

INTERFACE REMOVE_FROM_LIST
 MODULE PROCEDURE REMOVE_FROM_LIST_S
 MODULE PROCEDURE REMOVE_FROM_LIST_I
END INTERFACE

INTERFACE FINALIZE_LIST
 MODULE PROCEDURE FINALIZE_LIST_S
 MODULE PROCEDURE FINALIZE_LIST_I
END INTERFACE

INTERFACE SEARCH_LIST
 MODULE PROCEDURE SEARCH_LIST_I
 MODULE PROCEDURE SEARCH_LIST_S
END INTERFACE

INTERFACE ACCESS_LIST
 MODULE PROCEDURE ACCESS_LIST_I
 MODULE PROCEDURE ACCESS_LIST_S
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: ADD_TO_LIST
PUBLIC :: REMOVE_FROM_LIST
PUBLIC :: FINALIZE_LIST
PUBLIC :: ACCESS_LIST
PUBLIC :: SEARCH_LIST

!!## LOCAL PARAMETERS
!! @ reallocation block size
INTEGER,PARAMETER :: DEFAULT_dN=5

!!### MODULE PROCEDURES
CONTAINS


SUBROUTINE ADD_TO_LIST_S( List , Entry )
!!#### PURPOSE
!! Add an Entry to the List.

!!#### REQUIRED OUTPUT
CHARACTER(*),POINTER    :: List(:)

!!#### REQUIRED INPUT
CHARACTER(*),INTENT(IN) :: Entry

!!#### LOCAL VARIABLES
INTEGER :: i

!!--begin--
IF( .NOT.ASSOCIATED(List) )THEN
 ALLOCATE( List(1:DEFAULT_dN) )
 CALL CLEAR(List)
 i = 0
ELSE
 i = LEN_TRIMa(List)
 IF( i==SIZE(List) )THEN
  CALL Reallocate( List , DEFAULT_dN , fill=REPEAT(" ",LEN(List)))
 ENDIF
ENDIF

List(i+1) = Entry

!!--end--
ENDSUBROUTINE


SUBROUTINE REMOVE_FROM_LIST_S( List , Entry )
!!#### PURPOSE
!! Remove an entry from the list.

!!#### REQUIRED OUTPUT
CHARACTER(*),POINTER    :: List(:)

!!#### REQUIRED INPUT
CHARACTER(*),INTENT(IN) :: Entry

!!#### LOCAL VARIABLES
INTEGER :: i

!!--begin--
IF( ASSOCIATED(List) )THEN
 i = INDEXa(List,Entry)
 CALL Clear(List(i))
 List(i:SIZE(List)-1) = List(i+1:SIZE(List))
 CALL Clear(List(SIZE(List)))
ENDIF

!!--end--
ENDSUBROUTINE


SUBROUTINE FINALIZE_LIST_S( List )
!!#### PURPOSE
!! Finalize the list by allocation to its LEN_TRIMa length.

!!#### REQUIRED INPUT/OUTPUT
CHARACTER(*),POINTER :: List(:)

!!#### LOCAL VARIABLES
INTEGER :: dn

!!--begin--
IF( ASSOCIATED(List) )THEN
 dn=LEN_TRIMa(List)-SIZE(List)
 CALL Reallocate( List , dn )
ENDIF

!!--end--
ENDSUBROUTINE


FUNCTION SEARCH_LIST_S( List , Entry )  RESULT(i)
!!#### PURPOSE
!! Search a list for a specific entry and
!! return the index (0 if not found and -1 if
!! list is not associated).

!!#### REQUIRED INPUT
CHARACTER(*),POINTER    :: List(:)

!!#### REQUIRED INPUT
CHARACTER(*),INTENT(IN) :: Entry

!!#### REQUIRED OUTPUT
INTEGER :: i

!!--begin--
IF( .NOT.ASSOCIATED(List) )THEN
 i = -1
ELSE
 i = INDEXa(List,Entry)
END IF

!!--end--
END FUNCTION



FUNCTION ACCESS_LIST_S( List , i ) RESULT(Entry)
!!#### PURPOSE
!! Access an element of the list.

!!#### REQUIRED INPUT
CHARACTER(*),POINTER    :: List(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
TYPE(varying_string) :: Entry

!!--begin--

IF( .NOT.ASSOCIATED(List) )THEN
 Entry = ""
ELSE
 IF( i>LEN_TRIMa(List) .OR. i<1 )THEN
  Entry = ""
 ELSE
  Entry = List(i)
 END IF
END IF

!!--end--
END FUNCTION




FUNCTION SEARCH_LIST_I( List , Entry )  RESULT(i)
!!#### PURPOSE
!! Search a list for a specific entry and
!! return the index (0 if not found and -1 if
!! list is not associated).

!!#### REQUIRED INPUT
INTEGER,POINTER    :: List(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Entry

!!#### REQUIRED OUTPUT
INTEGER :: i

!!--begin--
IF( .NOT.ASSOCIATED(List) )THEN
 i = -1
ELSE
 i = EQUILOC(List,Entry)
END IF

!!--end--
END FUNCTION



FUNCTION ACCESS_LIST_I( List , i ) RESULT(Entry)
!!#### PURPOSE
!! Access an element of the list.

!!#### REQUIRED INPUT
INTEGER,POINTER    :: List(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: i

!!#### REQUIRED OUTPUT
INTEGER :: Entry

!!--begin--

IF( .NOT.ASSOCIATED(List) )THEN
 Entry = ERROR(Entry)
ELSE
 IF( i>SIZEa(List) .OR. i<1 )THEN
  Entry = ERROR(Entry)
 ELSE
  Entry = List(i)
 END IF
END IF

!!--end--
END FUNCTION




SUBROUTINE ADD_TO_LIST_I( List , Entry )
!!#### PURPOSE
!! Add an Entry to the List.

!!#### REQUIRED OUTPUT
INTEGER,POINTER    :: List(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Entry

!!#### LOCAL VARIABLES
INTEGER :: i

!!--begin--
IF( .NOT.ASSOCIATED(List) )THEN
 ALLOCATE( List(1:DEFAULT_dN) )
 List = ERROR(Entry)
 i = 0
ELSE
 i = SIZEa(List)
 IF( i==SIZE(List) )THEN
  CALL Reallocate( List , DEFAULT_dN , fill=ERROR(Entry) )
 ENDIF
ENDIF

List(i+1) = Entry

!!--end--
ENDSUBROUTINE


SUBROUTINE REMOVE_FROM_LIST_I( List , Entry )
!!#### PURPOSE
!! Remove an entry from the list.

!!#### REQUIRED OUTPUT
INTEGER,POINTER    :: List(:)

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Entry

!!#### LOCAL VARIABLES
INTEGER :: i

!!--begin--
IF( ASSOCIATED(List) )THEN
 i = EQUILOC(List,Entry)
 IF( i>0 )THEN
  List(i) = ERROR(Entry)
  List(i:SIZE(List)-1) = List(i+1:SIZE(List))
  List(SIZE(List)) = ERROR(Entry)
 END IF
ENDIF

!!--end--
ENDSUBROUTINE


SUBROUTINE FINALIZE_LIST_I( List )
!!#### PURPOSE
!! Finalize the list by allocation to its SIZEa length.

!!#### REQUIRED INPUT/OUTPUT
INTEGER,POINTER :: List(:)

!!#### LOCAL VARIABLES
INTEGER :: dn

!!--begin--
IF( ASSOCIATED(List) )THEN
 dn=SIZEa(List)-SIZE(List)
 CALL Reallocate( List , dn )
ENDIF

!!--end--
ENDSUBROUTINE



ENDMODULE
