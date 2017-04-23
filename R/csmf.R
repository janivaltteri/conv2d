csmf <- function(data,ker){
    if(!(ncol(ker)==nrow(ker))){
        stop('csmf accepts only square kernels')
    }
    if(!(nrow(ker) %% 2 == 1)){
        stop('csm accepts only odd kernel sizes')
    }
    if((nrow(data) < nrow(ker)) || (ncol(data) < ncol(ker))){
        stop('data matrix smaller than the kernel')
    }
    ksizecount <- floor(nrow(ker)/2.0)
    kerinds <- seq(from=-ksizecount,to=ksizecount,length=ncol(ker))
    pdata <- data
    for(i in 1:ksizecount){
        for(j in 1:ksizecount){
            pdata <- cbind(pdata[,1],pdata,pdata[,ncol(pdata)])
            pdata <- rbind(pdata[1,],pdata,pdata[nrow(pdata),])
        }
    }
    if(!is.loaded('conv2d')) dyn.load('conv2d.so')
    pres <- .Fortran('csmf',PACKAGE='conv2d',
                     matrix(0.0,nrow=nrow(pdata),ncol=ncol(pdata)),
                     as.double(pdata),
                     as.double(ker),
                     as.integer(nrow(pdata)),
                     as.integer(ncol(pdata)),
                     as.integer(nrow(data)),
                     as.integer(ncol(data)),
                     as.integer(ncol(ker)),
                     as.integer(ksizecount))[[1]]
    res <- pres[(1+ksizecount):(nrow(data)+ksizecount),(1+ksizecount):(ncol(data)+ksizecount)]
    return(res)
}
