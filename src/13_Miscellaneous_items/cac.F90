program test_command_argument_count
   implicit none
   integer :: count, length
   character(len=255) :: cmd, value
   count = command_argument_count()
   print *, count
   call get_command(cmd, length=length)
   print *, trim(cmd), length
   call get_command_argument(0, value, length)
   print *, trim(value), length
end program test_command_argument_count
