program exampleSubroutine_opt

      implicit none

      real(kind=4) :: x1=4.0, y1=5.0, z1=6.0, sum1, ave1
      real(kind=4) :: x2=9.0, y2=3.0, z2=6.0, sum2, ave2

      call sumAverage(x1, y1, z1, sum1, ave1)
      call sumAverage(x2, y2, z2, sum2, ave2, .TRUE.)

CONTAINS
      subroutine sumAverage (a, b, c, sm, av, doPrint)
         real(kind=4), intent(in) :: a, b, c
         logical, intent(in), optional :: doPrint
         real(kind=4), intent(out) :: sm, av
         sm = a + b + c
         av = sm / 3.0
         if (PRESENT(doPrint)) then
            if (doPrint) then
               write (*,'(a,f5.1,3x,a,f5.1)') "Sum=", sm, &
                                     "Average=", av
            end if
         end if
         return
      end subroutine sumAverage

end program exampleSubroutine_opt

