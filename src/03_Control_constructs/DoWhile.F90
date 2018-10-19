program DoWhile
   implicit none
      
   integer :: dayOfYear = 0
   real    :: totalPrecip = 0.0
   real    :: avgYearlyPrecip = 40.5
   real, dimension(365) :: precip
   
   ! generate some fake precip data, where random daily max is .27 inch, so avg
   ! should be about .135 (x 365 = 49.3)
   call random_number(precip)
   precip = .27 * precip
   
   DO WHILE (totalPrecip < avgYearlyPrecip)
      dayOfYear = dayOfYear +1
      IF (dayOfYear > 365) THEN
         print *, 'Precip was below normal this year'
         STOP
      END IF
      totalPrecip = totalPrecip + precip(dayOfYear)
   END DO
   print *, 'Precip surpassed yearly average on day', dayOfYear
   
end program
