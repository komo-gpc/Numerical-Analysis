program advection
  implicit none

  integer, parameter :: xmax = 100
  real(8), parameter :: tmax = 2.0d0
  real(8), parameter :: dt = 1.0d-3
  real(8), parameter :: dx = 1.0d-2
  real(8), parameter :: c  = 5.0d-1

  integer :: n, x
  real(8) :: u0(0:xmax)
  real(8) :: u1(0:xmax)
  real(8) :: a

  a = -c * dt / dx * 0.5d0

  do x = 0, xmax
    if( x >= 45 .and. x <= 55 ) then
      u0(x) = 1.0d0
    else
      u0(x) = 0.0d0
    end if
  end do
  
  do n = 1, int( tmax / dt )
    write(*,'(101(f18.12,1x))') u0(:)

    do x = 1, xmax-1
      u1(x) = u0(x) + a * ( u0(x+1) - u0(x-1) )
    end do

    u1(0) = u0(xmax-1)
    u1(xmax) = u0(1)

    u0(:) = u1(:)
  end do

  stop
end program
