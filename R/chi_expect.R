chi_expect<-function(x,y){
    tab=table(x,y)
    for (i in 1:nrow(tab)) {
        if (i==1) df=NULL
        df=rbind(df,tab[i,])
        if (i==nrow(tab)){
            df=rbind(df,colSums(tab))
            df=as.data.frame(df)
            df$rowsum=c(rowSums(tab),sum(tab))
        }
    }
    for (i in 1:(nrow(df)-1)) {
        for (j in 1:(ncol(df)-1)) {
            df[i,j]=df[i,j]*(df[i,ncol(df)]+df[nrow(df),j])/df[nrow(df),ncol(df)]
        }
        if (i==(nrow(df)-1)) df=df[-nrow(df),-ncol(df)]
    }
    message=paste0('expected number <5: ',
           round(sum(df<5)/dim(df)[1]/dim(df)[2]*100),'%','(',
           sum(df<5),'/',dim(df)[1]*dim(df)[2],'). ',
           'the minimum is: ',min(df))
    list(df,message)
}


