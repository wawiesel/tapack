!!# MODULE <<LIB_GenericPhrases>>
MODULE LIB_GenericPhrases

!!## PURPOSE
!! Provides the user with some generic phrases to help
!! ease writing error/warning messages.

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!## DEPENDENCIES
USE FUN_Sentence                    !!((04-B-FUN_Sentence.f90))
USE ISO_varying_string              !!((03-A-ISO_varying_string.f90))

!!## DETAILS
!! The generic phrases provided are detailed below.
!
!! * NOT_EQUAL(mod,proc,var,wrong,right)
!!   Returns a standard not found phrase:
!
!!    "In module = {mod}, procedure = {proc}, variable = {var},
!!     the value ({wrong}) must be equal to {right}."
!
!! * NOT_FOUND(mod,proc,var,wrong,right [,delim])
!!   Returns a standard not found phrase:
!
!!    "In module = {mod}, procedure = {proc}, variable = {var},
!!     the value ({wrong}) must be one of: {right}."
!
!!   where {right} will be delim-separated if delim is present.

!! * OUT_OF_BOUNDS(mod,proc,var,wrong,lower,higher)
!!   Returns a standard out-of-bounds phrase:
!
!!    "In module = {mod}, procedure = {proc}, variable = {var},
!!     the value ({wrong}) must satisfy <lower> <= {var} < <higher>."
!
!! * MODPROC(mod,proc)
!!   Returns a standard module/procedure phrase:
!
!!    "In module = {mod}, procedure = {proc}, "
!
!! * PROC(proc)
!!   Returns a standard procedure phrase:
!
!!    "In procedure = {proc}, "

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## INTERFACES
INTERFACE NOT_EQUAL
 MODULE PROCEDURE NOT_EQUAL_
ENDINTERFACE

INTERFACE NOT_FOUND
 MODULE PROCEDURE NOT_FOUND_
ENDINTERFACE

INTERFACE OUT_OF_BOUNDS
 MODULE PROCEDURE OUT_OF_BOUNDS_
ENDINTERFACE

INTERFACE MODPROC
 MODULE PROCEDURE MODPROC_
ENDINTERFACE

INTERFACE PROC
 MODULE PROCEDURE PROC_
ENDINTERFACE


!!## ACCESS
PUBLIC :: NOT_FOUND
PUBLIC :: OUT_OF_BOUNDS
PUBLIC :: MODPROC
PUBLIC :: PROC
PUBLIC :: NOT_EQUAL

CONTAINS


PURE FUNCTION NOT_EQUAL_( mod , proc , var , wrong , right ) RESULT(Phrase)
!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: mod,proc,var,wrong,right

!!#### REQUIRED OUTPUT
!! * output phrase <Phrase>
TYPE(varying_string) :: Phrase

!!--begin--

!! Set phrase to the right value.
Phrase = right

!! Assemble the phrase.
Phrase = "In module = "// TRIM(mod) //&
             ", procedure = "// TRIM(proc) //&
             ", variable/parameter, "// TRIM(var) //&
             ", had the value ("//TRIM(wrong)//&
             ") but it should have been "//TRIM(Phrase)//"."
!!--end--
ENDFUNCTION


PURE FUNCTION NOT_FOUND_( mod , proc , var , wrong , right , &
  delim ) RESULT(Phrase)

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: mod,proc,var,wrong
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: right(:)

!!#### OPTIONAL INPUT
CHARACTER(LEN=1,KIND=KIND_S),INTENT(IN),OPTIONAL :: delim

!!#### REQUIRED OUTPUT
!! * output phrase <Phrase>
TYPE(varying_string) :: Phrase

!!--begin--
!! Put the elements of the array right into one long string using
!! the sentence structure.
Phrase = Sentence(right,delim=delim,beg="(/",end="/)")

!! Assemble the phrase.
Phrase = "In module = "// TRIM(mod) //&
             ", procedure = "// TRIM(proc) //&
             ", variable = "// TRIM(var) //&
             ", the value ("//TRIM(wrong)//&
             ") must be one of:  "//TRIM(Phrase)//"."
!!--end--
END FUNCTION



PURE FUNCTION OUT_OF_BOUNDS_( mod , proc , var , wrong , lower , higher ) RESULT(Phrase)

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: mod,proc,var,wrong,lower,higher

!!#### REQUIRED OUTPUT
TYPE(varying_string) :: Phrase

!!--begin--
Phrase = "In module = "// TRIM(mod) //&
         ", procedure = "// TRIM(proc) //&
         ", variable = "// TRIM(var) //&
         ", the value ("//TRIM(wrong)//")"//&
         ") must satisfy "//&
         TRIM(lower)//" <= "//TRIM(var)//" < "//TRIM(higher)//"."
!!--end--
ENDFUNCTION



PURE FUNCTION MODPROC_( mod , proc ) RESULT(Phrase)

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: mod,proc

!!#### REQUIRED OUTPUT
TYPE(varying_string) :: Phrase

!!--begin--

!! Assemble phrase.
Phrase = "In module = "// TRIM(mod) //&
           ", procedure = "// TRIM(proc) //", "
!!--end--
ENDFUNCTION



PURE FUNCTION PROC_( proc ) RESULT(Phrase)
!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: proc

!!#### REQUIRED OUTPUT
TYPE(varying_string) :: Phrase

!!--begin--
!! Assemble phrase.
Phrase = "In procedure = "// TRIM(proc) //", "

!!--end--
ENDFUNCTION


ENDMODULE
