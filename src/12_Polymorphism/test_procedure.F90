program test_shapes

   use polygon_mod
   use rectangle_mod
   use circle_mod
   use square_mod
   implicit none

   type(rectangle) :: rect, myRect
   type(circle)    :: circ, myCirc
   type(square)    :: sq, mySq

   call initialize_polygon(myCirc, 1, radius=1.0)
   call initialize_polygon(myRect, 2, length=2.0, width=2.0)
   call initialize_polygon(mySq,   3, length=2.0)


   print *, 'Circle area ',myCirc%get_area(), myCirc%color
   print *, 'Rectangle area ',myRect%get_area(), myRect%color
   print *, 'Square area ',mySq%get_area(), mySq%color

CONTAINS

subroutine initialize_polygon(plg, color, radius, length, width)
    class(polygon) :: plg
    integer :: color
    real, optional :: radius
    real, optional :: length, width

    plg%color = color
    
    SELECT TYPE (plg)
    type is (polygon)
    class is (circle)
        if (present(radius))  then
           plg%radius = radius
        else
           plg%radius = 0
        endif
    class is (rectangle)
        if (present(length))  then
           plg%length = length
        else
           plg%length = 0
        endif
        if (present(width)) then
            plg%width = width
        else
            plg%width = 0
        endif
    class default
         stop 'initialize: unexpected type for plg object!'
    end select 
end subroutine initialize_polygon
   
end program test_shapes
