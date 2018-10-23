program Kelvin2Fahrenheit

    ! Read in file of Kelvin temps from a list of unknown length.
    ! Output the list in both Kelvin and Fahrenheit, using headings from line 13

    implicit none
    real :: tempK, tempF

    open (unit=10, file='KelvinList.txt', err=200, status='old')
    open (unit=20, file='FahrenheitList.txt', err=200, status='replace')
 
    ! Write a heading for the output file
    WRITE (unit=20, fmt='(2(A10,3X))') 'Kelvin', 'Fahrenheit'

    do
       READ (unit=10, fmt='(F7.2)', END=100) tempK
       tempF = (tempK - 273.15) * 9 / 5 + 32
       !*******************************************************************************
       !Complete the statement below to write tempK and tempF columns below the heading
       !*******************************************************************************
       WRITE 
    end do
100 continue

    close(10)
    close(20)
    stop

200 PRINT *, 'Error opening file'

end program Kelvin2Fahrenheit
