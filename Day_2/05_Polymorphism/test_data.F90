program test_shapes

   use polygon_mod
   use rectangle_mod
   use circle_mod
   use square_mod
   implicit none

   type(rectangle) :: myRect
   type(circle)    :: myCirc
   type(square)    :: mySq

   call initialize_polygon(myCirc, 1, radius=1.0)
   call initialize_polygon(myRect, 2, length=2.0, width=2.0)
   call initialize_polygon(mySq,   3, length=2.0)


   print *, 'Circle area ',myCirc%get_area(), myCirc%color
   print *, 'Rectangle area ',myRect%get_area(), myRect%color
   print *, 'Square area ',mySq%get_area(), mySq%color

   call init_data(myCirc)
   call init_data(myRect)
   call init_data(mySq)
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

subroutine init_data(plg)
  class(polygon) :: plg
  class(polygon), allocatable :: alp
  
  select type (plg)
  type is (polygon)
    allocate(polygon::alp)
    select type(alp)
    type is (polygon)
      alp = plg   ! copy sh
    end select
  type is (circle)
    allocate(circle::alp)
    select type(alp)
    type is (circle)
      alp = plg   ! copy sh
    end select
  type is (rectangle)
    allocate(rectangle::alp)
     select type (alp)
     type is (rectangle)
       alp = plg ! copy plg
     end select
  type is (square)
    allocate(square::alp)
    select type (alp)
    type is (square)
      alp = plg   ! copy plg
    end select
  end select
  WRITE(*,'(a15,1x,f18.9,1x,a7,1x,i3)') 'Polygon Area:', alp%get_area(), 'Color:', alp%color
end subroutine init_data
   
end program test_shapes
