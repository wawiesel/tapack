MODULE SUB_Replace
!!#### PURPOSE
!! Replaces occurences of a target substring in a string, returning
!! a new string.

!!#### USAGE
!!
!!  CALL Replace( Target , OldString , ReplaceWith , NewString [, CASESEN , NREPLACES] )
!
!! where Target is the target for replacement, OldString is the old version of the
!! string, ReplaceWith is what to replace the target with, and NewString is
!! the new version, post-replace.  The optional input CASESEN (default is true)
!! is whether the search is case sensitive.  The number of replaces may be returned
!! through optional output NREPLACES.


!!#### AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.com

!!#### DEPENDENCIES
USE SUB_Find !!((05-B-SUB_Find.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

INTERFACE Replace
 MODULE PROCEDURE Replace0
 MODULE PROCEDURE Replace1
ENDINTERFACE
PUBLIC :: Replace


CONTAINS


PURE SUBROUTINE Replace0(Target,OldString,ReplaceWith,NewString,CaseSensitive,NReplaces)
!!#### REQUIRED INPUT
!! @ search string <Target>
!! @ master string <OldString>
!! @ replace string <ReplaceWith>
CHARACTER(*),INTENT(IN) :: Target
CHARACTER(*),INTENT(IN) :: OldString
CHARACTER(*),INTENT(IN) :: ReplaceWith

!!#### REQUIRED OUTPUT
!! @ master string with appropriate replaces [NewString]
CHARACTER(*),INTENT(OUT) :: NewString

!!#### OPTIONAL OUTPUT
INTEGER,OPTIONAL,INTENT(OUT) :: NReplaces
LOGICAL,OPTIONAL,INTENT(IN)  :: CaseSensitive
!!#### LOCAL VARIABLES
INTEGER         :: i1,i2,j1,j2,k
INTEGER         :: NFinds
INTEGER,POINTER :: Locs(:)

!!--begin--
!find starting points of substring OldString in string Target
NULLIFY( Locs )

CALL Find(Target,OldString,Locs,CASESEN=CaseSensitive)

IF( .NOT.ASSOCIATED(Locs) )THEN
 NFinds = 0
 NewString = OldString
 GOTO 666
ELSE
 NFinds = SIZE(Locs)
END IF

!now start looping
i1 = 1
j1 = 1
DO k=1,NFinds

 !get next location in <OldString>
 i2 = Locs(k)-1

 !get location in <NewString> corresponding to the same difference as <OldString>
 j2 = j1 + (i2-i1)

 !set the chars all the way up to the replacement
 NewString(j1:j2) = Oldstring(i1:i2)

 !set the chars of the replacement
 ! update indices first
 j1 = j2 + 1
 j2 = j1 + LEN(ReplaceWith)-1
 NewString(j1:j2) = ReplaceWith

 ! update the <OldString> indices
 i1 = i2 + 1
 i2 = i1 + LEN(Target)-1

END DO

!finish
NewString(j2+1:) = OldString(i2+1:)


!calc bypass
666 CONTINUE

!return number of replaces made
IF( PRESENT(NReplaces) )THEN
 NReplaces = NFinds
ENDIF

!deallocate Locs
IF( ASSOCIATED(Locs) )DEALLOCATE( Locs )

!!--end--
ENDSUBROUTINE


PURE SUBROUTINE Replace1(Target,OldString,ReplaceWith,NewString,CaseSensitive,NReplaces)
!!#### REQUIRED INPUT
!! @ search string [Target]
!! @ master string [OldString]
!! @ replace string [ReplaceWith]
CHARACTER(*),INTENT(IN) :: Target
CHARACTER(*),INTENT(IN) :: OldString(:)
CHARACTER(*),INTENT(IN) :: ReplaceWith

!!#### REQUIRED OUTPUT
!! @ master string with appropriate replaces [NewString]
CHARACTER(*),POINTER    :: NewString(:)

!!#### OPTIONAL OUTPUT
INTEGER,OPTIONAL,INTENT(OUT) :: NReplaces
LOGICAL,OPTIONAL,INTENT(IN)  :: CaseSensitive

!!#### LOCAL VARIABLES
INTEGER :: NReplaces0
INTEGER :: n

!!--begin--
!setup
IF( PRESENT(NReplaces) )THEN
 NReplaces = 0
END IF

!loop
DO n=1,SIZE(OldString)

 !! Replace
 CALL Replace0(Target,OldString(n),ReplaceWith,NewString(n),&
   CaseSensitive,NReplaces=NReplaces0)

 !! Return the number of replaces made.
 IF( PRESENT(NReplaces) )THEN
  NReplaces = NReplaces + NReplaces0
 END IF

END DO

!!--end--
ENDSUBROUTINE


ENDMODULE
