!
! Newton method
!   f(x) = x**3 - 5
!
program newton_method
  implicit none

  ! definition
  integer :: n
  real(8) :: x, fx, dfx
  ! ----------

  ! initial value
  x = 2.0

  ! newton method loop
  do n = 1, 10
    fx = x**3 - 5.0
    dfx = 3.0 * x**2

    x = x - fx / dfx

    ! output
    write(*,*) 'step number =',n,'; x=',x
  end do

end program
