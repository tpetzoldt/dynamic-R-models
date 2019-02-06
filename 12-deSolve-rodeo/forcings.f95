! GENERATED CODE -- YOU PROBABLY DO NOT WANT TO EDIT THIS

include 'C:/Program Files/R/R-3.5.2/library/rodeo/fortran/forcingsGenericMethods.f95'
module forcings
use forcings_generic
implicit none
private TSeries, readTS, interpol
contains

function S_in (time, dflt) result (res)
  double precision, intent(in):: time, dflt
  character(len=256), parameter:: file='input.txt'
  character(len=256), parameter:: col='input'
  integer, parameter:: lweight= -1
  logical, save:: firstCall= .TRUE.
  integer, save:: latest= 1
  type(TSeries), save:: x
  double precision, parameter:: NA= huge(0d0)
  character(len=512):: errmsg
  double precision:: res
  if (isnan(dflt)) then
  include 'C:/Program Files/R/R-3.5.2/library/rodeo/fortran/forcingsInclude.f95'
  else
    res= dflt
  end if
end function
end module
