program sample_spline
  implicit none

  ! ----- definition -----

  ! parameter
  integer, parameter :: num_pt = 5   ! number of data points
  integer, parameter :: num_lp = 100 ! number of interpolation

  real(8), parameter :: px(num_pt) = (/ -1.0d0,  -0.5d0,  0.0d0,   0.5d0,   1.0d0  /) ! x-array of data points
  real(8), parameter :: py(num_pt) = (/  0.05d0,  0.2d0,  0.0d0,  -0.2d0,  -0.05d0 /) ! y-array of data points

  ! work
  integer :: i, j

  real(8) :: u( num_pt   )
  real(8) :: v( num_pt-2 )
  real(8) :: h( num_pt-1 )

  real(8) :: h2( num_pt-2, num_pt-2 )

  real(8) :: a, b, c, d
  real(8) :: x, y

  ! ----- main process -----

  ! determine the U vector
  u(:) = 0.0d0

  do j = 1, num_pt-1
    h(j) = px(j+1) - px(j)
  end do
  do j = 1, num_pt-2
    v(j) = ( ( py(j+2) - py(j+1) ) / h(j+1) - ( py(j+1) - py(j) ) / h(j) ) * 6.0d0
  end do

  h2(:,:) = 0.0d0

  do j = 1, num_pt-2
  do i = 1, num_pt-2
    if( i == j+1 ) h2(i,j) = h(j+1)
    if( i == j   ) h2(i,j) = 2.0d0 * ( h(j+1) + h(j) )
    if( i == j-1 ) h2(i,j) = h(j)
  end do
  end do

  call MATRIX_SOLVER_tridiagonal( h2(:,:), v(:), u(2:num_pt-1) )

  ! open file for data output
  open(unit=10, file='output_spline.csv')

  do j = 1, num_pt-1
    a = ( u(j+1) - u(j) ) / ( 6.0d0 * ( px(j+1) - px(j) ) )
    b = u(j) / 2.0d0
    c = ( py(j+1) - py(j) ) / ( px(j+1) - px(j) ) - ( u(j+1) + 2.0d0 * u(j) ) * ( px(j+1) - px(j) ) / 6.0d0
    d = py(j)

    do i = 1, num_lp / num_pt + 1
      x = px(j) + ( px(j+1) - px(j) ) / dble( num_lp / num_pt ) * dble( i - 1 ) 
      y = a * ( x - px(j) )**3 + b * ( x - px(j) )**2 + c * ( x - px(j) ) + d

      ! write data to file
      write(unit=10, fmt='(2f20.16)') x, y
    end do

  end do

  ! close file
  close(unit=10)

  stop

contains
  
  !-----------------------------------------------------------------------------
  !> solve tridiagonal matrix with Thomas's algorithm
  subroutine MATRIX_SOLVER_tridiagonal( &
       CM, &
       IV, &
       OV  )
    implicit none

    ! argument
    real(8), intent(in)  :: CM(:,:) ! coefficient matrix (tridiagonal matrix)
    real(8), intent(in)  :: IV(:)   ! input vector
    real(8), intent(out) :: OV(:)   ! output vector

    ! work
    real(8) :: ud( size( IV(:) ) ) ! upper  diagonal
    real(8) :: md( size( IV(:) ) ) ! middle diagonal
    real(8) :: ld( size( IV(:) ) ) ! lower  diagonal

    real(8) :: c( size( IV(:) ) )
    real(8) :: d( size( IV(:) ) )

    integer :: N
    integer :: i, j, k
    !---------------------------------------------------------------------------

    N = size( IV(:) )

    ! decomposite coefficient matrix
    do j = 1, N
    do i = 1, N
      if( i == j+1 ) ud(j) = CM(i,j)
      if( i == j   ) md(j) = CM(i,j)
      if( i == j-1 ) ld(j) = CM(i,j)
    end do
    end do

    ! foward reduction
    c(1) = ud(1) / md(1)
    d(1) = IV(1) / md(1)
    do k = 2, N
       c(k) =           ud(k)            / ( md(k) - ld(k) * c(k-1) )
       d(k) = ( iv(k) - ld(k) * d(k-1) ) / ( md(k) - ld(k) * c(k-1) )
    enddo

    OV(N) = d(N)

    ! backward substitution
    do k = N-1, 1, -1
       OV(k) = d(k) - c(k) * OV(k+1)
    enddo

    return
  end subroutine MATRIX_SOLVER_tridiagonal

end program
