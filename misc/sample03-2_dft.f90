! ------------------------------------------------------------------------------
!
! sample program: discrete Fourier transform
!
! ------------------------------------------------------------------------------
program discrete_Fourier_transform
  implicit none

  ! --- definition ---

  integer, parameter :: num = 4
  real(8), parameter :: pi = 3.141592653589793d0
  complex(8), parameter :: i = ( 0.0d0, 1.0d0 )

  integer :: n, m
  real(8) :: f( 0:num-1 )
  complex(8) :: DFT( -num/2:num/2 )

  ! --- main process ---

  f( 0:num-1 ) = (/ 0.0d0, 0.5d0, 1.0d0, 0.5d0 /)
  DFT( -num/2:num/2 ) = 0.0d0

  do n = -num/2, num/2
    do m = 0, num-1
      DFT(n) = DFT(n) + f(m) / dble(num) * exp( -2.0d0 * pi * i * dble(m*n) / dble(num) )
    end do

    write(*,*) 'F(',n,') = ',real( DFT(n) ),' + i * ',aimag( DFT(n) )
  end do

  stop
end program
