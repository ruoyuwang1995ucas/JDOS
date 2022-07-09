program pdos
use config
use input
use task!
use smearing
implicit none
include 'mpif.h'

integer:: proc_Rank, proc_Tot, ierror ! mpi parameters
real (kind=8),allocatable :: w0_list(:),w_wt_arr(:,:),jdos_arr(:),jdos_sub(:)
real(kind=8) :: sig,blk_1,blk_2,jdos,jdos_decay
integer:: num_w0
integer:: sub_size ! the size of sub w_list
integer:: i,j,k ! iterator

!sig=0.03

call jdos_config(sig,sub_size)
call gaussian_parameter(sig,blk_1,blk_2)
call phonopy(w_wt_arr)
call sampling_pts(w0_list) ! read w0 list
 
num_w0=size(w0_list)   
!sub_size=3
allocate(jdos_sub(sub_size))
allocate(jdos_arr(num_w0))

! set initial values of jdos_arr
do i=1,num_w0
  jdos_arr(i)=w0_list(i)
enddo

! initialize MPI 
call MPI_INIT(ierror)
call MPI_COMM_SIZE(MPI_COMM_WORLD, proc_Tot,ierror)
call MPI_COMM_RANK(MPI_COMM_WORLD, proc_Rank, ierror) 
! scatter data to proc 0-3
call MPI_Scatter(jdos_arr, sub_size, MPI_DOUBLE_PRECISION, jdos_sub, sub_size, MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierror);
! task per proc

do j=1,sub_size
  !jdos_sub(i,1)=w0_sub(i)
  !print *, 'start jdos cal. of',jdos_sub(j),'at',proc_Rank
  call jdos_w0(jdos_sub(j),blk_1,blk_2,w_wt_arr,jdos)
  jdos_sub(j)=jdos
  print *,jdos,'after', proc_Rank
enddo
!print *,'before', proc_Rank, 'jdos:',jdos_sub

call MPI_Gather(jdos_sub,sub_size,MPI_DOUBLE_PRECISION, jdos_arr, sub_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierror)
!print *, jdos for root proc
if (proc_Rank == 0) then
  open(2,file='jdos.dat',action='write', status='replace')
  do k = 1,num_w0
    write(2,*) w0_list(k), jdos_arr(k) 
  enddo
  close(2)
endif

call MPI_FINALIZE(ierror)

end program pdos



