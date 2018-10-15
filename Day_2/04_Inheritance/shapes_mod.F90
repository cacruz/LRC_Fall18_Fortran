module shapes_mod

   implicit none
   private
   public shape
   
   type, abstract :: shape
      ! No data
   contains
      procedure(get_shape_area), deferred  :: get_area
   end type shape

   abstract interface
      real function get_shape_area( this )
         import                   :: shape
         class(shape), intent(in) :: this
      end function get_shape_area
   end interface

end module shapes_mod
