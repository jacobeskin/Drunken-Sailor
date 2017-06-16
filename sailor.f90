program sailor
  !Compilation: $ make -f makesailor
  !Running : ./sailor.exe 

  !Drunken sailor problem with no wind, east wind and west wind. Harbor 15
  !blocks east of the bar. Wind factor forces a direction randomly with a
  !probability of 1/100. Output is average time in each case. Average times
  !calculated from 10000 walkers per case. 

  
  !Modules used
  use mtdefs
  use mtmod

  implicit none

  integer, parameter :: rkk = selected_real_kind(10,40)
  integer, parameter :: N_walkers = 10000
  real(kind=rkk) :: u, v, w, aven, aven_ew, aven_ww 
  integer :: i, x, y, x_ew, y_ew, x_ww, y_ww, n, n_ew, n_ww
  integer :: sumn, sumn_ew, sumn_ww

  ! Initializing the random number generator
  call sgrnd(getseed(info=1))

  ! Starting the runs. Now there is no limit in steps. 
  sumn = 0
  sumn_ew = 0
  sumn_ww = 0
  do i=1,N_walkers
     ! Reset walks
     x = 0
     y = 0
     x_ew = 0
     y_ew = 0
     x_ww = 0
     y_ww = 0
     n = 0
     n_ew = 0
     n_ww = 0
     do while((x<15).or.(x_ew<15).or.(x_ww<15)) 
        u = igrnd(1,2) ! 1 for moving in x, 2 for moving in y
        v = igrnd(0,1) ! 0 for moving left or down, 1 for rigth or up
        w = igrnd(1,100) ! For rthe wind effect
        
        ! We are preforming 3 different walks at the same time, where
        ! one will end and others might continue, so lets treat them
        ! all separately with the condition that the generation of a step
        ! applies only if the walker has not reached the quay.

        ! Normal walker
        if (x<15) then
           
           if (u==1) then
              if (v==0) then
                 x = x-1
              else if (v==1) then
                 x = x+1
              end if
           else if (u==2) then
              if (v==0) then
                 y = y-1
              else if (v==1) then
                 y = y+1
              end if
           end if
           n = n+1
        end if

        ! Walker with east wind
        if (x_ew<15) then
           
           if (w==30) then
              x_ew = x_ew-1
           else if (w/=30) then
              if (u==1) then
                 if (v==0) then
                    x_ew = x_ew-1
                 else if (v==1) then
                    x_ew = x_ew+1
                 end if
              else if (u==2) then
                 if (v==0) then
                    y_ew = y_ew-1
                 else if (v==1) then
                    y_ew = y_ew+1
                 end if
              end if
           end if
           n_ew = n_ew+1
        end if

        ! Walker with west wind
        if (x_ww<15) then
           
           if (w==50) then
              x_ww = x_ww+1
           else if (w/=50) then
              if (u==1) then
                 if (v==0) then
                    x_ww = x_ww-1
                 else if (v==1) then
                    x_ww = x_ww+1
                 end if
              else if (u==2) then
                 if (v==0) then
                    y_ww = y_ww-1
                 else if (v==1) then
                    y_ww = y_ww+1
                 end if
              end if
           end if
           n_ww = n_ww+1
        end if
           
     end do
     
     sumn = sumn+n
     sumn_ew = sumn_ew+n_ew
     sumn_ww = sumn_ww+n_ww
     
  end do

  aven = sumn/N_walkers
  aven_ew = sumn_ew/N_walkers
  aven_ww = sumn_ww/N_walkers

  print *, 
  print'("Average time, windless walk:", 4x, f10.3, x, "minutes")', aven*60
  print *,
  print'("Average time, eastwind walk:", 4x, f10.3, x, "minutes")', aven_ew*60
  print *,
  print'("Average time, westwind walk:", 4x, f10.3, x, "minutes")', aven_ww*60
  print *,

end program sailor

  
