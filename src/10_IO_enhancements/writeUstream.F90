      PROGRAM writeUstream
      IMPLICIT NONE

      INTEGER :: myvalue = 20181025
      INTEGER :: mypos

      ! Open the file
      OPEN(UNIT=11, FILE="ustream.demo", STATUS="NEW", ACCESS="STREAM")

      WRITE(11) "Fortran "              !  8 bytes
      WRITE(11) "Training at LRC"       ! 15 bytes
      INQUIRE(UNIT=11, POS=mypos)
      PRINT *, "Myvalue will be written at position ", mypos
      WRITE(11) myvalue                 !  4 bytes

      ! Close the file
      CLOSE(UNIT=11)

      END PROGRAM writeUstream
