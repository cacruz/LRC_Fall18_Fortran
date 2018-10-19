      program exampleAsyncIO

      implicit none

      integer(kind=4), parameter :: isize=1000000, icol=5

      call validWrite()
      call invalidWrite()
      call doingAsyncIO()

!----------------------------------------------------------------------
      CONTAINS
!----------------------------------------------------------------------

      subroutine validWrite()
        integer, dimension(isize):: a
        integer idvar, i
        OPEN(10, FILE='goodFile.demo', &
                 ASYNCHRONOUS="yes", FORM='unformatted')
        a = (/(i,i=1,isize)/)
        WRITE(10, ASYNCHRONOUS="yes", ID=idvar) a
        WAIT(ID=idvar)
        CLOSE(10)
      end subroutine validWrite

      subroutine invalidWrite()
        integer, dimension(isize):: a
        integer idvar, i
        OPEN(90, FILE='badFile.demo', &
                 ASYNCHRONOUS="yes", FORM='unformatted')
        a = (/(i,i=1,isize)/)
        WRITE(90, ASYNCHRONOUS="yes", ID=idvar) a
        idvar = 999           ! Valid id is destroyed.
        WAIT(ID=idvar)
        CLOSE(90)
      end subroutine invalidWrite

      subroutine doingAsyncIO( )
        integer(kind=4) :: i, j, k
        integer(kind=4), dimension(icol)       :: handle
        integer(kind=4), dimension(isize,icol), ASYNCHRONOUS :: a
        integer(kind=4), dimension(isize,icol), ASYNCHRONOUS :: a1

        ! Open the file for both synchronous and asynchronous I/O.

        OPEN (UNIT         = 20, &
              FILE         = 'asynchIO_yes.demo', &
              !STATUS       = 'scratch', &
              FORM         = "unformatted", &
              ACCESS       = "direct", &
              RECL         = isize*4,  &
              ASYNCHRONOUS = 'yes' )

        ! This loop overlaps the initialization of a(:,j) with
        ! asynchronous write statements.
 
        ! NOTE: The array is written out one column at a time.
        !       Since the arrays in Fortran are arranged in column
        !       major order, each WRITE statement writes out a
        !       contiguous block of the array.
        do j = 1, icol
           a(:,j) = (/ (i*j,i=1,isize) /)
           write(20, ASYNCHRONOUS='yes', id=handle(j), rec=j) a(:,j)
        enddo
 
        ! Wait for all writes to complete before reading.
        do j = 1, icol
           wait(id=handle(j))
        enddo
 
        ! Reads in the first record.
        read(20, ASYNCHRONOUS='yes', id=handle(1), rec=1) a1(:,1)

        do j = 2, icol
           k = j - 1
 
           ! Waits for a previously initiated read to complete.
           wait(id=handle(k))
 
           ! Initiates the next read immediately.
           read(20, ASYNCHRONOUS='yes', id=handle(j), rec=j) a1(:,j)
 
           ! While the next read is going on, we do some processing here.
           do i = 1, isize
              if (a(i,k) .NE. a1(i,k)) then
                 print *, "(",i,",",k,") expected ", a(i,k), " got ", a1(i,k)
              end if
           end do
        end do

        ! Finish the last record.
        wait(id=handle(icol))

        do i = 1, isize
           if (a(i,icol) .NE. a1(i,icol)) then
              print *, "(",i,",",icol,") expected ", a(i,icol), " got ", a1(i,icol)
           end if
        end do

        close(20)
      end subroutine doingAsyncIO

      end program exampleAsyncIO
