   implicit none

   real, dimension(15)       :: A, AA
   real, dimension(-4:0,0:2) :: B
   real C(5,3), D(0:4,0:2), CC(5,3)
   
   real :: Y(2,2) = -999.0
   real, dimension(2,2) :: X  = reshape([1.0,3.0,2.0,4.0],[2,2])
   real, dimension(2,2) :: ID = reshape([2.0,0.0,0.0,2.0],[2,2])

   real, dimension(2,3) :: g

   integer i, ierr
   integer, dimension(10) :: ints
   character(len=5), dimension(3) :: colors
   real, dimension(4)             :: heights
   
   integer, dimension(:), allocatable :: years
   real, dimension(:,:,:), allocatable :: temperature
   
   integer, dimension(5) :: V = (/1,4,8,12,10/)
   integer, dimension(3) :: W = (/1,2,2/)
   
   print *
   print *, 'An uninitialized array A: ', A
   A = 0
   print *
   print *, 'A after initialization: ', A
   write(6,*) repeat('-',50)

   
   C = 1
   B = 0
   B(-1:0,1:2) = C(1:2,2:3) + 1
   print *,'Array sections:'
   print *, 'B(-1:0,1:2) = C(1:2,2:3) + 1 : ', B
   write(6,*) repeat('-',50)

   D = 0.5
   B = C * D - B**2
   print *,'Array expressions:'
   print *, 'B = C * D - B**2 : ', B
   write(6,*) repeat('-',50)

   print *,'Array inquiries:'
   print *, 'Size A: ', size(A)
   print *, 'Shape A: ', shape(A)
   print *, 'Size B: ', size(B)
   print *, 'Shape B: ', shape(B)
   print *, 'Size C: ', size(C)
   print *, 'Shape C: ', shape(C)
   print *, 'LBOUND (B,1): ', lbound(B,1)
   print *, 'UBOUND (B,2): ', ubound(B,2)
   write(6,*) repeat('-',50)

   heights = (/5.10, 5.6, 4.0, 3.6/)
   colors = (/'RED  ','GREEN','BLUE '/)
   ! note padding so strings are 5 chars
   ints = (/ 100, (i, i=1,8), 100 /)

   print *, 'colors: ',colors
   print *, 'ints: ',ints
   write(6,*) repeat('-',50)

   print *, 'Shape AA (used as source) ',shape(AA)
   CC = reshape(AA, (/5, 3/))
   print *, 'Re-shaped AA (assigned to CC) ',shape(CC)
   write(6,*) repeat('-',50)

   print *,'Allocatable arrays:'

   allocate(years(10), stat=ierr)
   allocate(temperature(4,5,3), stat=ierr)

   print *, 'is years allocated?: ', allocated(years)
   print *, 'is temperature allocated?: ', allocated(temperature)
   deallocate(temperature)
   print *, 'is temperature allocated?: ', allocated(temperature)

   print *,'Masked array assignment'
   where ( ID /= 0 ) Y = X/ID
   print *,Y(1,1), Y(2,1)
   print *,Y(1,2), Y(2,2)
   write(6,*) repeat('-',50)

   print *,'Vector valued subscripts:'
   print *, 'A before init: ', A
   print *, 'V: ', V
   A(V) = 3.5
   print *, 'A(V): ', A
   write(6,*) repeat('-',50)

   print *,'Where construct:'
   g = reshape( (/ 1.,0.,3.,0.,5.,0. /), shape(g))
   where (g /= 0.)
      g = 1./g
   elsewhere
      g = -999.
   end where
   print *,g
   
end program
