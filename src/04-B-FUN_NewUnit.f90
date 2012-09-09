!!# MODULE <<FUN_NewUnit>>
MODULE FUN_NewUnit

!!## PURPOSE
!! This function returns a unit not currently in use.  It has no
!! required arguments but optional arguments:
!!
!!  * default_input_unit
!!  * default_output_unit
!!  * preconnected_units
!!  * end_of_record
!!  * end_of_file
!!  * max_unit_number
!!
!! and if they are not present, the optional arguments
!! take on the value of the parameters declared in c_Units.

!!## AUTHOR
!! William Wieselquist | william.wieselquist@gmail.com

!!## SOURCE
!! This code was odified from code in SMLib v1.0
!! which contained the following notice:
!
!   Copyright (c) 1994 Unicomp, Inc.  All rights reserved.
!
!   Developed at Unicomp, Inc.
!
!   Permission to USE, copy, modify, and distribute this
!   software is freely granted, provided that this notice
!   is preserved.
!
!   NAG F90

!!## EXTERNAL PARAMETERS
USE PAR_Units,ONLY: MAX_UNIT_NUMBER_,PRECONNECTED_UNITS_   !!((02-A-PAR_Units.f90))

!!## GLOBAL VARIABLES
USE VAR_Units,ONLY: DEFAULT_INPUT_UNIT,DEFAULT_OUTPUT_UNIT !!((03-A-VAR_Units.f90))

!!## DEFAULT IMPLICIT
implicit none

!!## DEFAULT ACCESS
private

!!## ACCESS
public :: NewUnit


contains


function NewUnit( input_unit , &
                  output_unit , &
                  preconnected_units , &
                  max_unit_number )  result (result)

!!#### OPTIONAL INPUT
integer,optional,intent(in) :: input_unit
integer,optional,intent(in) :: output_unit
integer,optional,intent(in),target :: preconnected_units(:)
integer,optional,intent(in) :: max_unit_number
!!#### REQUIRED OUTPUT
integer :: result

!!#### LOCALS
logical :: exists, opened, MustDeallocate
integer :: ios
integer :: input_unit_
integer :: output_unit_
integer,pointer :: preconnected_units__(:)
integer :: max_unit_number__

!!--begin--
!resolve optional arguments
IF( PRESENT(input_unit) )THEN
 input_unit_  = input_unit
ELSE
 input_unit_  = DEFAULT_INPUT_UNIT
ENDIF

IF( PRESENT(output_unit) )THEN
 output_unit_  = output_unit
ELSE
 output_unit_  = DEFAULT_OUTPUT_UNIT
ENDIF

IF( PRESENT(max_unit_number) )THEN
 max_unit_number__ = max_unit_number
ELSE
 max_unit_number__ = max_unit_number_
ENDIF

MustDeallocate = .FALSE.
IF( PRESENT(preconnected_units) )THEN
 preconnected_units__=>preconnected_units
ELSE
 ALLOCATE( preconnected_units__(1:SIZE(preconnected_units_)) )
 MustDeallocate = .TRUE.
 preconnected_units__=preconnected_units_
ENDIF

result = -1
do result = 10, max_unit_number__
 if (result == INPUT_UNIT_ .or. &
     result == OUTPUT_UNIT_) cycle
 if (any (result == PRECONNECTED_UNITS__)) cycle
 inquire (unit = result,  &
          exist = exists,  &
          opened = opened,  &
          iostat = ios)
 if (exists .and. .not. opened .and. ios == 0) exit
end do

IF( MustDeallocate )THEN
 DEALLOCATE( preconnected_units__ )
ELSE
 preconnected_Units__ => NULL()
END IF

end function NewUnit


end module
