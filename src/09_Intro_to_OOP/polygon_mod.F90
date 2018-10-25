module polygon_mod

   use shapes_mod   
   implicit none
   private
   public polygon
   
   type, extends(shape) :: polygon
      ! No data
   contains
      procedure :: get_area
   end type polygon
   
contains

   real function get_area( this )
      class(polygon), intent(in) :: this

      get_area = huge(1.0)

   end function get_area

end module polygon_mod
