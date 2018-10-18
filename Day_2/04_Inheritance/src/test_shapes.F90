program test_shapes

   use rectangle_mod
   use circle_mod
   use square_mod
   implicit none

   type(rectangle), target :: rect
   type(circle), target :: circ
   type(square), target :: sq

   circ =  new_circle( 1.0 )
   rect =  new_rectangle( length=2.0, width=2.0 )
   sq =  new_square( 3.0 )
   print *, 'Circle area ',circ%get_area()
   print *, 'Rectangle area ',rect%get_area()
   print *, 'Square area ',sq%get_area()
   
end program test_shapes
