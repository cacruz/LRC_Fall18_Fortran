module circle_mod

   use polygon_mod   
   implicit none
   private
   public circle
   public new_circle
   
   type, extends(polygon) :: circle
      real :: radius
   contains
      procedure :: get_area
   end type circle
   
   real, parameter :: PI = 4.0*atan(1.0)
   
contains

   function new_circle(radius) result(a_circle)
      real, intent(in) :: radius
      type(circle) :: a_circle
      
      a_circle%radius = radius
      
   end function new_circle
   

   real function get_area( this )
      class(circle), intent(in) :: this

      get_area =  PI * this%radius

   end function get_area

end module circle_mod
