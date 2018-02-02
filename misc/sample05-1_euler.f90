program euler
  implicit none

  integer, parameter :: N = 4
  real(8), parameter :: tmax = 2.0d0

  integer :: i
  real(8) :: dt = tmax/N
  real(8) :: t, y

  t = 0.0d0
  y = 1.0d0

  write(*,'(a6,2a19)') 'step', 't', 'y'

  do i = 0, N
    write(*,'(i6,1x,2(f18.12,1x))') i, t, y

    t = t + dt
    y = y + dt * ( y * ( 5.0d0 - y ) )
  end do

end program
