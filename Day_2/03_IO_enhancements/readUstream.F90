      PROGRAM readUstream
      IMPLICIT NONE

      CHARACTER(len=8) :: string   ! 8 bytes
      INTEGER :: n, fileSize

      OPEN(UNIT=42, FILE="ustream.demo", STATUS="OLD", ACCESS="STREAM")

      INQUIRE(FILE="ustream.demo", SIZE=fileSize)
      WRITE(*,*) 'The file has', fileSize, 'bytes'

      READ(42, POS=6) string     ! we want to read 3 bytes starting at position 4
      WRITE(*,*) string          ! should produce "an Train"
      READ(42, POS=24) n
      WRITE(*,*) n

      END PROGRAM readUstream
