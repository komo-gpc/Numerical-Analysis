program sample_Jacobi
  implicit none

  integer, parameter :: N = 3

  real(8) :: eps = 1.0d-12

  real(8) :: A(N,N)
  real(8) :: Y(N)
  real(8) :: X(N)

  real(8) :: s, e
  real(8) :: c1, c2
  real(8) :: Z(N)

  integer :: i, j, k
  !---------------------------------------------------------------------------

  A(1,1) =  3.0d0
  A(1,2) =  2.0d0
  A(1,3) =  1.0d0
  A(2,1) =  1.0d0
  A(2,2) =  3.0d0
  A(2,3) = -2.0d0
  A(3,1) =  2.0d0
  A(3,2) = -1.0d0
  A(3,3) =  4.0d0

  Y(1) =  4.0d0
  Y(2) =  6.0d0
  Y(3) = -3.0d0

  X(:) = 0.0d0

  k = 0
  do
    s = 0.0d0
    e = 0.0d0

    do i = 1, N
      c1 = 0.0d0
      c2 = 0.0d0

      do j = 1, i-1
        c1 = c1 + A(i,j) * X(j)
      end do
      do j = i+1, N
        c2 = c2 + A(i,j) * X(j)
      end do

      Z(i) = 1.0d0 / A(i,i) * ( Y(i) - c1 - c2 )

      s = s + abs( Z(i) )
      e = e + abs( Z(i) - X(i) )
    end do

    k = k + 1

    if( e/s < eps ) exit

    X(:) = Z(:)

  end do

  write(*,*) k, Z(:)

  stop
end program
