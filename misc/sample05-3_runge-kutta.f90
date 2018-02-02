program runge_kutta
  implicit none

  integer, parameter :: N = 4
  real(8), parameter :: tmax = 2.0d0

  integer :: i
  real(8) :: dt = tmax/N
  real(8) :: t, y, k1, k2, k3, k4

  t = 0.0d0
  y = 1.0d0
  k1 = 0.0d0
  k2 = 0.0d0
  k3 = 0.0d0
  k4 = 0.0d0

  write(*,'(a6,6a19)') 'step', 't', 'y', 'k1', 'k2', 'k3', 'k4'

  do i = 0, N
    write(*,'(i6,1x,6(f18.12,1x))') i, t, y, k1, k2, k3, k4

    t  = t + dt
    k1 = y * ( 5.0d0 - y )
    k2 = ( y + k1 * dt * 0.5d0 ) * ( 5.0d0 - ( y + k1 * dt * 0.5d0 ) )
    k3 = ( y + k2 * dt * 0.5d0 ) * ( 5.0d0 - ( y + k2 * dt * 0.5d0 ) )
    k4 = ( y + k3 * dt ) * ( 5.0d0 - ( y + k3 * dt ) )
    y  = y + dt / 6.0d0 * ( k1 + 2.0d0 * k2 + 2.0d0 * k3 + k4 )
  end do

end program
