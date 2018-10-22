program PrintObs
   use NestedTypes
   implicit none

   TYPE(WeatherOb), dimension(10) :: wxOb
   integer :: i

   open(unit=10, file='WeatherObs.txt')
   read(10,*)  ! Skip reading first line of data file 
   !**************************************************************
   ! The 5 columns of data in 'WeatherObs.dat' are:
   ! Temp K    Humidity     Precip       Wind m/s    Wind Dir
   ! Complete the read statement below to read in all 5 components
   ! of the TYPE(WeatherOb) variable: wxOb 
   !***************************************************************
   do i = 1, 10
      read(10,*) wxOb(i)%tempK
   end do
   
   print *, 'Temperature:', wxOb(5)%tempK, 'K'
   print *, 'MaxTemp:', maxval(wxOb%tempK)
   print *, 'Humidity:', wxOb(5)%humidity 
   print *, 'Precip:', wxOb(5)%precip, 'cm'
   print *, 'Accum. Precip',  sum(wxOb%precip), 'cm'
   print *, 'Windspeed:', wxOb(5)%wind%windMps, 'm/s' 
   print *, 'Wind from:', wxOb(5)%wind%windDir, 'degrees' 

end program PrintObs
