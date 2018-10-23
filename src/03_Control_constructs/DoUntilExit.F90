program DoUntilExit
   implicit none
      
   integer :: dayOfYear = 0
   real    :: totalPrecip = 0.0
   real    :: avgYearlyPrecip = 40.5
   real, dimension(365) :: precip
   
   ! generate some fake precip data, where random daily max is .27 inch, so avg
   ! should be about .135 (x 365 = 49.3)
   call random_number(precip)   ! 365 random values between 0.0 and 1.0
   precip = .27 * precip
   
   DO 
      dayOfYear = dayOfYear +1
      IF (dayOfYear > 365) THEN
         print *, 'Precip was below normal this year'
         STOP
      END IF
      totalPrecip = totalPrecip + precip(dayOfYear)
      IF (totalPrecip > avgYearlyPrecip) EXIT
   END DO
   print *, 'Precip surpassed yearly average on day', dayOfYear
   
end program
