      program testingComplex   
      implicit none  

      real    :: x, y
      complex :: cx1, cx2, cx3

      x   = 2.67
      y   = -0.349
      cx1 = (3.0, 5.0)            ! cx1 = 3.0 + 5.0i
      cx2 = cmplx (1.0/2.0, -7.0) ! cx2 = 0.5 – 7.0i 
      cx3 = cmplx (x, y)          ! cx3 = x + yi

      print *, 'cx1:     ', cx1
      print *, 'cx2:     ', cx2
      print *, 'cx3:     ', cx3
      print *, 'cx1+cx3: ', cx1+cx3
      print *, 'cx1-cx2: ', cx1-cx2
      print *, 'cx1*cx2: ', cx1*cx2
      print *, 'cx1/cx2: ', cx1/cx2

      end program testingComplex
