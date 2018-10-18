module ellipse_mod

   use shapes_mod   
   implicit none
   private
   public ellipse
   
   type, extends(shape) :: ellipse
      ! No data
   contains
      procedure :: get_area
   end type ellipse
   
contains

   real function get_area( this )
      class(ellipse), intent(in) :: this

      get_area = huge(1.0)

   end function get_area

end module ellipse_mod
