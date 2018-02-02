program sample_LU_decomp
  implicit none

  integer, parameter :: N = 3

  real(8) :: a(N)
  real(8) :: b(N)
  real(8) :: c(N)
  real(8) :: d(N)
  real(8) :: l(N)

  real(8) :: X(N)
  real(8) :: Y(N)
  real(8) :: Z(N)

  integer :: i
  !---------------------------------------------------------------------------

  a(:) =  2.0d0
  b(:) = -1.0d0
  c(:) = -1.0d0

  Y(1) = -1.0d0
  Y(2) =  2.0d0
  Y(3) =  1.0d0

  d(1) = a(1)
  do i = 2, N
    l(i) = b(i) / d(i-1)
    d(i) = a(i) - l(i) * c(i-1)
  end do

  Z(1) = Y(1)
  do i = 2, N
    Z(i) = Y(i) - l(i) * Z(i-1)
  end do

  X(N) = Z(N) / d(N)
  do i = N-1, 1, -1
    X(i) = ( Z(i) - c(i) * X(i+1) ) / d(i)
  end do

  do i = 1, N
    write(*,*) ' X(',i,') = ',X(i)
  end do

  stop
end program
