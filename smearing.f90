module smearing
    implicit none
    real(kind=8),parameter:: pi=4*atan(1.d0)
    private ! All entities are now module private by default
    public gaussian, gaussian_parameter! explicitly export public entities

contains
    subroutine gaussian_parameter(sig,blk_1,blk_2)
        real(kind=8),intent(in)::sig
        real(kind=8),intent(out)::blk_1,blk_2
        blk_1=1/(2*pi*sig**2)**0.5
        blk_2=1/(2*sig**2)
    end subroutine

    function gaussian(x,blk_1,blk_2) result(g)
        implicit none
        real(kind=8),intent(in) :: x,blk_1,blk_2
        real(kind=8):: g
        g=exp(-x**2*blk_2)*blk_1
    end function
end module
