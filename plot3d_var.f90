        integer mb,nvar
        integer,dimension(:),pointer:: il,jl,kl
        real*8,dimension(:,:,:,:,:),pointer:: var
        integer,dimension(:,:,:,:),pointer::iblank
        character(len=1000):: fn
#define VARSET mb,nvar,il,jl,kl,var
