!!# MODULE <<VAR_Units>>
MODULE VAR_Units

!!## PURPOSE
!! This module contains the unit-numbers for default
!! units.

!!## EXTERNAL PARAMETERS
USE PAR_Units,ONLY: keyboard_unit,window_unit !!((02-A-PAR_Units.f90))

!!## VARIABLES
!! * Default input and output units
INTEGER :: DEFAULT_INPUT_UNIT  = keyboard_unit
INTEGER :: DEFAULT_OUTPUT_UNIT = window_unit

END MODULE
