module functions
use forcings ! Imports module 'forcings'

implicit none

contains

  double precision function lamda (lamda1, lamda2, CALG)
    double precision, intent(in):: lamda1, lamda2, CALG
    lamda= lamda1+lamda2*CALG
  end function

  double precision function O2_sat (Temp)
    double precision, intent(in):: Temp
    O2_sat= exp(7.7117d0 - 1.31403d0*log(Temp + 45.93d0))
  end function

  double precision function pulse (time, s_start, s_duration, s_in) result(ret)
    double precision, intent(in):: time, s_start, s_duration, s_in

    if ((s_start < time) .and. (time <= (s_start + s_duration))) then
      ret = s_in
    else
      ret = 0.0D0
    end if
  end function 

end module



