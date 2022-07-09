module input
    implicit none
    character(len=*),parameter::w_wt_file="array"
    character(len=*),parameter::w0_file="sampling_points"

contains
    subroutine sampling_pts(w0_list)
        implicit none
        real(kind=8),allocatable,intent(out) :: w0_list(:)
        integer::n,i
        open(1,file=w0_file,action='read',status='old')
            read(1,*) n
            allocate(w0_list(n))
            do i=1,n
                read (1,*) w0_list(i)
            end do
        close(1)

    end subroutine

    subroutine phonopy(w_wt_arr) ! read from
        implicit none
        real(kind=8),allocatable,intent(out) :: w_wt_arr(:,:)
        integer :: n
        integer :: i
        open(11,file=w_wt_file,action='read',status='old')
        ! read number of points
        read(11,*), n
        allocate (w_wt_arr(n,2))
        do i = 1,n
            read(11,*) w_wt_arr(i,1), w_wt_arr(i,2)
        end do
        close(11)
    end subroutine phonopy

end module input
