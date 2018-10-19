   use ISO_FORTRAN_ENV
   implicit none
   integer, parameter :: MAXLEN_ARG=16
   character(len=MAXLEN_ARG) :: arg1, arg2 
   character(len=64) :: myShell
   real, dimension(5) :: x = [1., 2., 3., 4. ,5.]
   real, parameter :: pi = 9801./(1103*sqrt(8.))
   integer :: nx, ny
   complex :: C = (0.0,pi)
   integer(int64) :: count_rate, count_max
   
   print *, 'Number of args: ',command_argument_count()  
   call get_command_argument(1, VALUE=arg1)
   call get_command_argument(2, VALUE=arg2) 
   read(arg1,'(i2)') nx
   read(arg2,'(i2)') ny
   print *,'------------------------------------------------------------'   
   print *, 'User input: nx = ',nx,' ny = ',ny   
   print *,'------------------------------------------------------------'
   call get_environment_variable('SHELL', myShell)
   print *,'Value of SHELL: ',trim(myShell)
   print *,'------------------------------------------------------------'
   print *, 'ISO_FORTRAN_ENV constants IO units: ',INPUT_UNIT, &
        OUTPUT_UNIT, ERROR_UNIT 
   print *, 'ISO_FORTRAN_ENV size constants: ',NUMERIC_STORAGE_SIZE, &
        CHARACTER_STORAGE_SIZE, FILE_STORAGE_SIZE
   print *,'------------------------------------------------------------'
   print *, 'x: ',x
   print *,'------------------------------------------------------------'   
   print *,'complex C: ',C
   print *,'------------------------------------------------------------'   
   call system_clock(count_rate, count_max)
   print *, 'Clock resolution: ',real(count_max, real64) / count_rate

end program
