program taylor_expansion
  implicit none

  ! -- definition

  integer :: i, n
  real(8) :: a
  real(8) :: x(11)
  real(8) :: f(11)

  ! --- main process ---

  a = 0.0d0

  x(:) = (/ (-0.5d0+i*0.1d0,i=0,10) /)
  f(:) = 0.0d0

  do i = 1, 11
    do n = 0, 4
      f(i) = f(i) + df(n,a) * ( x(i) - a )**n / factorial(n)
    end do

    write(*,*) x(i), f(i)
  end do

  stop

contains
  function factorial( k ) result( res )
    implicit none

    ! --- definition ---

    integer :: k
    integer :: res
    integer :: i
    
    ! --- main process ---

    res = 1
    do i = 1, k
      res = res * i
    end do

    return
  end function

  function df( k, a ) result( res )
    implicit none

    ! --- definition ---

    integer :: k
    real(8) :: a
    real(8) :: res
    real(8) :: tmp(0:4)

    ! --- main process ---

    tmp(0:4) = (/ exp(a), &
                  exp(a), &
                  exp(a), &
                  exp(a), &
                  exp(a) /)

    res = tmp(k)

    return
  end function

end program
