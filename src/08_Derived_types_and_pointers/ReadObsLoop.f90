   do i = 1, 10
      read(10,*) wxOb(i)%tempK, wxOb(i)%humidity, wxOb(i)%precip, &
        & wxOb(i)%wind%windMps, wxOb(i)%wind%windDir
   end do
