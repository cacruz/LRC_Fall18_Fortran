module polygon_mod

   use shapes_mod   
   implicit none
   private
   public polygon
   
   type, extends(shape) :: polygon
      integer :: color
   contains
      procedure :: get_area
      procedure :: set_color
   end type polygon
   
contains

   real function get_area( this )
      class(polygon), intent(in) :: this

      get_area = huge(1.0)

   end function get_area

   subroutine set_color(plg, color)
      class(polygon), intent(inOut) :: plg
      integer, intent(in) :: color
      plg%color = color
   end subroutine set_color

end module polygon_mod
