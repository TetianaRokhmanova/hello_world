!#######################################################################
! parameters
!-----------------------------------------------------------------------
! N    - overall number of vortexes,
! Nfixed    - number of fixed vortexes
! Rlx - box width
! Rly - box hight
!#######################################################################
!=======================================================================
      program vortexes
      implicit none
      integer(kind=4) :: N, Nfixed
      integer(kind=4) :: is1, is2, i
      real(kind=8) :: Rlx,Rly
      real(kind=8) :: x(1000),y(1000)

      N=100
      Nfixed=10
      Rlx=DSQRT(DBLE(N))
      Rly=Rlx
      is1=1372
      is2=5423
      call srandom(is1,is2)
      call coordinates(N, x, y)
      do i=1, N
      print *, x(i), y(i)
      enddo
         ! call Distance(Rlx,Rly,x1,y1,x2,y2,Dij)
        !  print *, Dij
        
       
      stop
      end
!#######################################################################
      subroutine coordinates(N, x, y)
      implicit none
      integer(kind=4) :: i
      integer(kind=4), intent(in) :: N
      real(kind=8), intent(out) :: x(N),y(N)
      real(kind=8) :: Rlx,Rly,xx        
      Rlx=DSQRT(DBLE(N))
      Rly=Rlx
      do i=1, N
            call random_number(xx)
            x(i)=Rlx*xx
            call random_number(xx)
            y(i)=Rly*xx
      enddo 
      return
      end
!#######################################################################
      !  subroutine Distance(Rlx,Rly,x1,y1,x2,y2,Dij)
      !  implicit none
      !   real(kind=8), intent(in) :: Rlx,Rly,x1,y1,x2,y2
      !   real(kind=8), intent(out) :: Dij
      !   real(kind=8) :: R2x,R2y,xij,yij
      !   integer(kind=8) :: i,j
      !   R2x=Rlx/2.d0 !d0=10^0
      !   R2y=Rly/2.d0
      !   xij=DABS(x1-x2)
      !   yij=DABS(y1-y2)
      !       if (xij.gt.R2x) xij=Rlx-xij
      !     if (yij.gt.R2y) yij=Rly-yij
      !      Dij=DSQRT(xij**2+yij**2)
      !    return
      !  end
      
      
      !    subroutine EnergyForOne(N,x,y,k,Energy)
      ! implicit none
      !  integer(kind=8), intent(in) :: N,k,x(N),y(N)
      !    real(kind=8), intent(out) :: Energy
      !    real(kind=8) :: Rlx,Rly
      !    integer(kind=8) :: i,j
      !     x1=x(k)
      !   y1=y(k)
      !   do i=1, N
      !   call Distance(Rlx,Rly,x1,y1,x2,y2,Dij)
      !   if (x2=x1,y2=y1) then 

      !  Energy=0.d0+LOG(Dij)
      ! enddo
      ! return
      ! end
      

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine srandom(is1,is2)
      implicit none
      integer(kind=4), intent(inout) :: is1,is2
      integer(kind=4), allocatable :: iseed(:)
      integer(kind=4) :: n,i,itop

      itop=2**30
      if(is1 > itop) is1=is1-itop
      if(is2 > itop) is2=is2-itop
      call random_seed(size=n)
            allocate(iseed(n))
      do i=1,n,2
            iseed(i)=is1
      enddo
      if(n > 1) then
            do i=2,n,2
                  iseed(i)=is2
            enddo
      endif
      call random_seed(put=iseed(1:n))
      deallocate(iseed)

      end
