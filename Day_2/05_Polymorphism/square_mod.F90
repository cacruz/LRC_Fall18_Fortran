module square_mod

   use rectangle_mod   
   implicit none
   private
   public square ! derived type
   public new_square ! constructor
   
   type, extends(rectangle) :: square
   contains
      procedure :: get_area
   end type square
   
contains
   
   function new_square(length) result(a_square)
      real, intent(in) :: length
      type(square) :: a_square
      
      a_square%length = length
      
   end function new_square
   
   real function get_area( this )
      class(square), intent(in) :: this

      get_area = this%length * this%length

   end function get_area

end module square_mod
