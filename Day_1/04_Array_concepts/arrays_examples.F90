   implicit none

   real, dimension(15)       :: A
   real, dimension(-4:0,0:2) :: B
   real C(5,3), D(0:4,0:2)
   
   integer i, ierr
   integer, dimension(10) :: ints
   character(len=5), dimension(3) :: colors
   real, dimension(4)             :: heights
   
   integer, dimension(:), allocatable :: years
   real, dimension(:,:,:), allocatable :: temperature
   
   integer, dimension(5) :: V = (/1,4,8,12,10/)
   integer, dimension(3) :: W = (/1,2,2/)
   
   integer, parameter :: &
        days_per_month (12) = (/  &
        31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31  /)

   integer :: m(12)
   
   print *, 'An uninitialized array A: ', A
   A = 0
   print *, 'A after initialization: ', A
   print *,'-------------------------------------------------'

   
   C = 1
   B = 0
   B(-1:0,1:2) = C(1:2,2:3) + 1
   print *,'Array sections:'
   print *, 'B(-1:0,1:2) = C(1:2,2:3) + 1 : ', B
   print *,'-------------------------------------------------'

   D = 0.5
   B = C * D - B**2
   print *,'Array expressions:'
   print *, 'B = C * D - B**2 : ', B
   print *,'-------------------------------------------------'

   print *,'Array inquiries:'
   print *, 'Size A: ', size(A)
   print *, 'Shape A: ', shape(A)
   print *, 'Size B: ', size(B)
   print *, 'Shape B: ', shape(B)
   print *, 'Size C: ', size(C)
   print *, 'Shape C: ', shape(C)
   print *, 'LBOUND (B,1): ', lbound(B,1)
   print *, 'UBOUND (B,2): ', ubound(B,2)
   print *,'-------------------------------------------------'

   heights = (/5.10, 5.6, 4.0, 3.6/)
   colors = (/'RED  ','GREEN','BLUE '/)
   ! note padding so strings are 5 chars
   ints    = (/ 100, (i, i=1,8), 100 /)

   print *, 'colors: ',colors
   print *, 'ints: ',ints
   print *,'-------------------------------------------------'

   allocate(years(10), stat=ierr)
   allocate(temperature(4,5,3), stat=ierr)

   print *,'Allocatable arrays:'
   print *, 'is years allocated?: ', allocated(years)
   print *, 'is temperature allocated?: ', allocated(temperature)
   deallocate(temperature)
   print *, 'is temperature allocated?: ', allocated(temperature)
   print *,'-------------------------------------------------'

   print *,'Vector valued subscripts:'
   print *, 'A before init: ', A
   print *, 'V: ', V
   A(V) = 3.5
   print *, 'A(V): ', A
   print *,'-------------------------------------------------'

   
end program
