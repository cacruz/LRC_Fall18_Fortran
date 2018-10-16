module rectangle_mod

   use polygon_mod   
   implicit none
   private
   public rectangle ! derived type
   public new_rectangle ! constructor
   
   type, extends(polygon) :: rectangle
      real :: length, width
   contains
      procedure :: get_area
   end type rectangle
   
contains
   
   function new_rectangle(length, width) result(a_rectangle)
      real, intent(in) :: length, width
      type(rectangle) :: a_rectangle
      
      a_rectangle%length = length
      a_rectangle%width = width
      
   end function new_rectangle
   
   real function get_area( this )
      class(rectangle), intent(in) :: this

      get_area = this%width * this%length

   end function get_area

end module rectangle_mod
