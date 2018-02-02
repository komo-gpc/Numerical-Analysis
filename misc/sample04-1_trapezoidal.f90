program trapezoidal_rule
  implicit none

  ! -- definition

  integer, parameter :: NUM = 4

  integer :: n, m

  real(8) :: T(0:NUM-1) ! trapezoidal area
  real(8) :: area ! small area
  real(8) :: a, b, L

  ! --- main process ---

  a = 0.0d0
  b = 2.0d0
  L = b - a

  T(0) = L / 2.0d0 * ( 2.0d0**a + 2.0d0**b )

  do n = 1, NUM-1
    area = 0.0d0
    do m = 1, 2**(n-1)
      area = area + L / 2.0d0**(n) * ( 2.0d0**( a + dble(2*m-1) * L / 2.0d0**(n) ) )
    end  do
    T(n) = T(n-1) / 2.0d0 + area
  end do

  write(*,*) '{ n, T(n) } :', NUM-1, T(NUM-1)

  stop
end program
