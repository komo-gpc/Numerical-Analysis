program heun
  implicit none

  integer, parameter :: N = 4
  real(8), parameter :: tmax = 2.0d0

  integer :: i
  real(8) :: dt = tmax/N
  real(8) :: t, y, k1, k2

  t = 0.0d0
  y = 1.0d0
  k1 = 0.0d0
  k2 = 0.0d0

  write(*,'(a6,4a19)') 'step', 't', 'y', 'k1', 'k2'

  do i = 0, N
    write(*,'(i6,1x,4(f18.12,1x))') i, t, y, k1, k2

    t  = t + dt
    k1 = y * ( 5.0d0 - y )
    k2 = ( y + k1 * dt ) * ( 5.0d0 - ( y + k1 * dt ) )
    y  = y + dt / 2.0d0 * ( k1 + k2 )
  end do

end program
