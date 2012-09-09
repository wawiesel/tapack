!!## MODULE: PARAMETERS for  Units
MODULE PAR_Units

!!### PURPOSE
!! This module contains the unit-numbers for important
!! interface devices---namely the command-window <window_unit>
!! and the keyboard <keyboard_unit>.



!!### HISTORY
! 1.[waw] Reorganizer = William A. Wieselquist
!         Modified    = 105.2006
!         Contact     = william.wieselquist AT gmail.com



!!### DEFAULT IMPLICIT
IMPLICIT NONE



!!### DEFAULT ACCESS
PUBLIC



!!### PARAMETERS DECLARATIONS
!! @ keyboard input interface unit <keyboard_unit>
!! @ command window output interface unit <window_unit>
INTEGER,PARAMETER :: KEYBOARD_UNIT = 5
INTEGER,PARAMETER :: WINDOW_UNIT   = 6
!
!! @ Number and value of preconnected units
INTEGER,PARAMETER :: NUMBER_OF_PRECONNECTED_UNITS_ = 3
INTEGER,PARAMETER :: PRECONNECTED_UNITS_(1:NUMBER_OF_PRECONNECTED_UNITS_) = &
                         (/ 0, keyboard_unit, window_unit /)
!
!! @ Values returned to IOSTAT for end of record and end of file
INTEGER,PARAMETER :: END_OF_RECORD = -2
INTEGER,PARAMETER :: END_OF_FILE   = -1
!
!! @ Largest allowed unit number
INTEGER,PARAMETER :: MAX_UNIT_NUMBER_ = 1000



END MODULE
