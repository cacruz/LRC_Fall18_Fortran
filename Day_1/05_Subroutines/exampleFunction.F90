program exampleFunction

      implicit none

      real(kind=4) :: x1=4.0, y1=5.0, z1=6.0, ave1
      real(kind=4) :: x2=1.7, y2=4.3, z2=9.5, ave2

      ave1 = calcAverage (x1, y1, z1)
      ave2 = calcAverage (x2, y2, z2)

      write (*,'(a,3(f4.1),a,f5.1)')'The average of', x1, y1, z1, ' is ', ave1
      write (*,'(a,3(f4.1),a,f7.3)')'The average of', x2, y2, z2, ' is ', ave2

CONTAINS

      FUNCTION calcAverage (a, b, c) result(av)
       implicit none
       real(kind=4), intent(in) :: a, b, c
       real(kind=4)             :: av

       av = (a + b + c)/3.0

      END FUNCTION calcAverage

end program exampleFunction

