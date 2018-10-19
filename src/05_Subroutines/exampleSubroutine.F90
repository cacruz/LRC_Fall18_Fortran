program exampleSubroutine

      implicit none

      real(kind=4) :: x1=4.0, y1=5.0, z1=6.0, sum1, ave1
      real(kind=4) :: x2=4.0, y2=5.0, z2=6.0, sum2, ave2

      call sumAverage(x1, y1, z1, sum1, ave1)
      write (*,'(a,f5.1,3x,a,f5.1)') "Sum=", sum1, &
                                      "Average=", ave1
      call sumAverage(x2, y2, z2, sum2, ave2)
      write (*,'(a,f5.1,3x,a,f5.1)') "Sum=", sum2, &
                                     "Average=", ave2

CONTAINS
      subroutine sumAverage (a, b, c, sm, av)
         real(kind=4), intent(in) :: a, b, c
         real(kind=4), intent(out) :: sm, av
         sm = a + b + c
         av = sm / 3.0
         return
      end subroutine sumAverage

end program exampleSubroutine
