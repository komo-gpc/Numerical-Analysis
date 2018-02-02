program euler
  implicit none

  integer, parameter :: N = 200
  real(8), parameter :: tmax = 20.0d0

  integer :: i
  real(8) :: dt = tmax/N
  real(8) :: t, y1, y2
  real(8) :: p1, p2

  t = 0.0d0
  y1 = 1.0d0
  y2 = 0.0d0

  write(*,'(a6,3a19)') 'step', 't', 'y1', 'y2'

  do i = 0, N
    write(*,'(i6,1x,3(f18.12,1x))') i, t, y1, y2

    t = t + dt
    p1 = y1 + dt * y2
    p2 = y2 + dt * ( -y1 )
    y1 = p1
    y2 = p2
  end do

end program
