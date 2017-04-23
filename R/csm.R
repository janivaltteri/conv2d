csm <- function(data,ker){
    if(!(ncol(ker)==nrow(ker))){
        stop('csm accepts only square kernels')
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
    pres <- matrix(0.0,nrow=nrow(pdata),ncol=ncol(pdata))
    for(i in (1+ksizecount):(nrow(data)+ksizecount)){
        for(j in (1+ksizecount):(ncol(data)+ksizecount)){
            for(k in kerinds){
                for(l in kerinds){
                    pres[i,j] <- pres[i,j] + ker[k+ksizecount+1,l+ksizecount+1]*pdata[i+k,j+l]
                }
            }
        }
    }
    res <- pres[(1+ksizecount):(nrow(data)+ksizecount),(1+ksizecount):(ncol(data)+ksizecount)]
    return(res)
}
