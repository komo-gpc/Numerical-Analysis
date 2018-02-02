program sample_Gauss
  implicit none

  integer, parameter :: N = 3

  real(8) :: A(N,N)
  real(8) :: X(N)
  real(8) :: Y(N)

  real(8) :: b, c

  integer :: i, j, k
  !---------------------------------------------------------------------------

  A(1,1) =  1.0d0
  A(1,2) = -2.0d0
  A(1,3) =  3.0d0
  A(2,1) =  2.0d0
  A(2,2) =  1.0d0
  A(2,3) =  0.0d0
  A(3,1) =  1.0d0
  A(3,2) =  2.0d0
  A(3,3) = -1.0d0

  Y(1) = 1.0d0
  Y(2) = 5.0d0
  Y(3) = 5.0d0

  do k = 1, N-1
    do i = k+1, N
      b = A(i,k) / A(k,k)
      do j = k+1, N
        A(i,j) = A(i,j) - b * A(k,j)
      end do
      Y(i) = Y(i) - b * Y(k)
    end do
  end do

  X(N) = Y(N) / A(N,N)
  do i = N-1, 1, -1
    c = 0.0d0
    do k = i+1, N
      c = c + A(i,k) * X(k)
    end do
    X(i) = ( Y(i) - c ) / A(i,i)
  end do

  do i = 1, N
    write(*,*) ' X(',i,') = ',X(i)
  end do

  stop
end program
