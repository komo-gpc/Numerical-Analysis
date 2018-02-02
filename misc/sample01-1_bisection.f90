!
! Bisection method
!   f(x) = x**3 - 5
!
program bisection_method
  implicit none

  ! definition
  integer :: n
  real(8) :: a, b, c, fc
  ! ----------

  ! initial value
  a = 1.0
  b = 2.0

  ! bisection method loop
  do n = 1, 20
    c = ( a + b ) / 2.0
    fc = c**3 - 5.0

    if( fc < 0.0 ) then
      a = c
    else
      b = c
    end if

    ! output
    write(*,*) 'step number =',n,'; c=',c
  end do

end program
