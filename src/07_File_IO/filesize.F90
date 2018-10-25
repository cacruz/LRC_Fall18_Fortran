
   real :: x=1.0
   integer :: file_size

   print *,'Writing a single precision variable to a file'
   
   open (unit=10, file='seq_uf.dat', form='unformatted')
   write(10) x
   close(10)
   inquire(file='seq_uf.dat', size=file_size)
   print *,'default unformatted file size: ',file_size

   open (unit=10, file='dir_uf.dat', form='unformatted', access='direct', recl=kind(x))
   write(10, rec=1) x
   close(10)
   inquire(file='dir_uf.dat', size=file_size)
   print *,'recl specific file size: ',file_size

end
