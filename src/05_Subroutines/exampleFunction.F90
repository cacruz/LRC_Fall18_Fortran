program exampleFunction

      implicit none

      real(kind=4) :: x1=4.0, y1=5.0, z1=6.0, ave1
      real(kind=4) :: x2=1.7, y2=4.3, z2=9.5, ave2
      real(kind=4) :: av3

      ave1 = calcAverage (x1, y1, z1)
      ave2 = calcAverage (x2, y2, z2)

      write (*,'(a,3(f4.1),a,f5.1)')'The average of', x1, y1, z1, ' is ', ave1
      write (*,'(a,3(f4.1),a,f7.3)')'The average of', x2, y2, z2, ' is ', ave2

      av3 = calcAverage3((/x2, y2, z2, x1, z1, y1/))
      print*, av3

CONTAINS

      FUNCTION calcAverage (a, b, c) result(av)
       implicit none
       real(kind=4), intent(in) :: a, b, c
       real(kind=4)             :: av

       av = (a + b + c)/3.0

      END FUNCTION calcAverage

      real FUNCTION calcAverage2 (a, b, c)
       implicit none
       real(kind=4), intent(in) :: a, b, c

       calcAverage2 = (a + b + c)/3.0

      END FUNCTION calcAverage2

      real FUNCTION calcAverage3 (A)
       implicit none
       real(kind=4), intent(in) :: A(:)

       print*, MINVAL(A), MAXVAL(A)
       calcAverage3 = SUM(A)/SIZE(A)

      END FUNCTION calcAverage3

end program exampleFunction

