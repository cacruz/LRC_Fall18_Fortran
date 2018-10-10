      program testingLogical  
      implicit none
      INTEGER :: YEAR
      LOGICAL :: LEAP_FLAG
      
      YEAR = 2004
      LEAP_FLAG = .FALSE.             
      IF (MOD(YEAR,4) .EQ. 0)   LEAP_FLAG = .TRUE.
      IF (MOD(YEAR,100) .EQ. 0) LEAP_FLAG = .FALSE.
      IF (MOD(YEAR,400) .EQ. 0) LEAP_FLAG = .TRUE.
      
      PRINT*, 'Is ', YEAR, ' a leap year? ', LEAP_FLAG
      end program testingLogical
