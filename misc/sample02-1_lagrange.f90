program sample_lagrange
  implicit none

  ! ----- definition -----

  ! parameter
  integer, parameter :: num_pt = 3   ! number of data points
  integer, parameter :: num_lp = 100 ! number of interpolation

  real(8), parameter :: px(num_pt) = (/ -1.0d0,   0.0d0,  1.0d0  /) ! x-array of data points
  real(8), parameter :: py(num_pt) = (/  0.05d0,  0.0d0,  0.05d0 /) ! y-array of data points

  real(8), parameter :: dx = 2.0d0 ! distance between first and last data points

  ! work
  integer :: i, j, n

  real(8) :: x, y
  real(8) :: lx

  ! ----- main process -----

  ! open file for data output
  open(unit=10, file='output_lagrange.csv')

  do i = 1, num_lp+1
    x = -1.0d0 + dx / dble( num_lp ) * dble( i - 1 )
    y = 0.0d0

    ! execute lagrange interpolation
    do j = 1, num_pt
      lx = 1.0d0

      do n = 1, num_pt
        if( n /= j ) lx = lx * ( x - px(n) ) / ( px(j) - px(n) )
      end do

      y = y + lx * py(j)
    end do

    ! write data to file
    write(unit=10, fmt='(2f20.16)') x, y
  end do

  ! close file
  close(unit=10)

  stop
end program
