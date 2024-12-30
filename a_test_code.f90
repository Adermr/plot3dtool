program main
    use plot3dtool
    implicit none
#    include "plot3d_var.f90"
    integer i,j,k,imb,i1
    !
    fn = 'out.xyz'
    call plot3d_readin(fn,VARSET)
            allocate(iblank(maxval(il),maxval(jl),maxval(kl),mb),stat=i1)      
    call iblank_compute(iblank,VARSET)
    fn = 'trans_formatted.xyz'
    call plot3d_write_formatted_withib(fn,iblank,VARSET)
end
