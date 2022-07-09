! Routines to perform specific tasks
module task
    use smearing
    implicit none

contains
    subroutine jdos_w0(w0,blk_1,blk_2,w_wt_arr,jdos)
        implicit none
        real (kind=8),intent(in) :: w0, blk_1,blk_2
        real (kind=8) :: d1, d2, delta_1, delta_2
        real (kind=8),allocatable,intent(in):: w_wt_arr(:,:)
        integer:: npts,i,j,k
        real (kind=8), intent(out)::jdos

        npts = size(w_wt_arr,1)
        jdos=0.
        do i = 1, npts
            d1=w_wt_arr(i,1)-w0
            delta_1=gaussian(d1,blk_1,blk_2)*w_wt_arr(i,2)
            delta_2=0.
            do j =1, npts
                do k =1, npts
                    d2=w_wt_arr(i,1)+w_wt_arr(j,1)-w_wt_arr(k,1)
                    delta_2=delta_2+gaussian(d2,blk_1,blk_2)*w_wt_arr(j,2)*w_wt_arr(k,2)
                end do
            end do
            jdos=jdos+delta_1*delta_2
        end do
    end subroutine jdos_w0

    subroutine jdos_w0_decay(w0,blk_1,blk_2,w_wt_arr,jdos)
        implicit none
        real (kind=8),intent(in) :: w0, blk_1,blk_2
        real (kind=8) :: d1, d2, delta_1, delta_2
        real (kind=8),allocatable,intent(in):: w_wt_arr(:,:)
        integer:: npts,i,j,k
        real (kind=8), intent(out)::jdos

        npts = size(w_wt_arr,1)
        jdos=0.
        do i = 1, npts
            d1=w_wt_arr(i,1)-w0
            delta_1=gaussian(d1,blk_1,blk_2)*w_wt_arr(i,2)
            delta_2=0.
            do j =1, npts
                do k =1, npts
                    d2=w_wt_arr(i,1)-w_wt_arr(j,1)-w_wt_arr(k,1)
                    delta_2=delta_2+gaussian(d2,blk_1,blk_2)*w_wt_arr(j,2)*w_wt_arr(k,2)
                end do
            end do
            jdos=jdos+delta_1*delta_2
        end do
    end subroutine jdos_w0_decay


end module
