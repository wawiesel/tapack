!!## MODULE: FUNCTION  HashP
MODULE FUN_HashP
!!### PURPOSE
!! Turns string into a number based upon a prime-number oriented
!! hashing rule.

!!### DETAILS
!! Testing this hash on a dictionary of words, these rules resulted
!! in a very even distribution of hash values---the number of words
!! far outweighed the number of available integers, but approximately
!! the same number of words were hashed to the same value.
!! Words that hashed to the same value were not similar, thus collision
!! resolution would be somewhat simple, to use <PrimeHash> in a
!! hash table implementation.

!!### AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.com

!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!### EXTERNAL PROCEDURES
USE FUN_Default                     !!((04-A-FUN_Default.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE

!!### INTEFACE
INTERFACE HashP
 MODULE PROCEDURE HashP_S
END INTERFACE

!!### PUBLIC ACCESS
PUBLIC :: HashP


!!### LOCAL PARAMETERS
!! @ mapping of ascii indices to hash numbers
!! @ ascii value of lower case "a"
!! @ ascii value of lower case "z"
!! @ DEFAULT value for optional case sensitivity input [DEFAULT_CaseSensitive]
!! @ DEFAULT value for optional full spectrum input [DEFAULT_FullSpectrum]
INTEGER,PARAMETER :: hash_map(0:127) = &
(/000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,&
  000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,&
  001,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,&
  002,003,005,007,011,013,017,019,023,029,000,000,000,000,000,000,&
  000,031,037,041,043,047,053,059,061,067,071,073,079,083,089,097,&
  101,103,107,109,113,127,131,137,139,149,151,000,000,000,000,001,&
  000,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,&
  239,241,251,257,263,269,271,277,281,000,000,000,000,000,000,000/)
INTEGER,PARAMETER :: ascii_a                 = IACHAR("a")
INTEGER,PARAMETER :: ascii_z                 = IACHAR("z")
LOGICAL,PARAMETER :: DEFAULT_CaseSensitive   = .FALSE.
LOGICAL,PARAMETER :: DEFAULT_FullSpectrum    = .FALSE.

!!## MODULE PROCEDURES
CONTAINS


!!### PURE FUNCTION: HashP_S
PURE FUNCTION HashP_S( word , &
  CaseSensitive , &
  FullSpectrum  ) &
  RESULT(Hash)

!!#### REQUIRED INPUT
!! @ input word to hash <word>
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: word

!!#### OPTIONAL INPUT
!! @ whether or not to generate a case sensitive hash <CaseSensitive>
!! @ to have negative hash values and thus represent the whole spectrum
!!   of integers, words with an odd number of characters
!!   receive negative hash values if full spectrum is true <FullSpectrum>
LOGICAL,OPTIONAL,INTENT(IN) :: CaseSensitive
LOGICAL,OPTIONAL,INTENT(IN) :: FullSpectrum

!!#### REQUIRED OUTPUT
!! @ integer hash value <HASH>
INTEGER  :: HASH

!!#### LOCAL VARIABLES
LOGICAL :: LOCAL_CaseSensitive !LOCAL value for the optional CaseSensitive input
LOGICAL :: LOCAL_FullSpectrum  !LOCAL value for the optional FullSpectrum input
INTEGER :: i,j                 !character index
INTEGER :: ascii               !ascii value for ith character
INTEGER :: LEN_word            !length of the word
INTEGER,PARAMETER :: limit = HUGE(HASH)/1000
!!--begin--
!set options
LOCAL_CaseSensitive = DEFAULT( DEFAULT_CaseSensitive , CaseSensitive )
LOCAL_FullSpectrum  = DEFAULT( DEFAULT_FullSpectrum  , FullSpectrum  )

!get word length
LEN_word = LEN(word)

!get the hash
HASH = 0
IF( .NOT.LOCAL_CaseSensitive )THEN
 DO i=1,LEN_word
  ascii = IACHAR(word(i:i))
  IF( ascii_a<=ascii .AND. ascii<=ascii_z )THEN
   ascii = ascii - 32
  END IF
  j = LEN_word-i+1
  HASH = HASH + ( j + j**2 + j**3 - 2 )*hash_map(ascii)
  IF( HASH>limit )HASH = HASH/2 + MOD(HASH,limit)
 END DO
ELSE
 DO i=1,LEN_word
  ascii = IACHAR(word(i:i))
  j = LEN_word-i+1
  HASH = HASH + ( j + j**2 + j**3 - 2 )*hash_map(ascii)
  IF( HASH>limit )HASH = HASH/2 + MOD(HASH,limit)
 END DO
END IF

!make odd-length words have negative hashes if FullSpectrum==.TRUE.
IF( LOCAL_FullSpectrum )THEN
 IF( MOD(LEN_word,2)/=0 )THEN
  HASH = -HASH
 ENDIF
ENDIF

!!--end--
END FUNCTION


END MODULE
