# To flat and strech data
#
# param data dataframe
# param strech string of name
#
# return a dataframe
# export
#
# examples flat_strech(data)
# examples flat_strech(data,"strech")
flat_strech<-function(data,strech){
        strech_ncol=match(strech,names(data))
        freq_dataf=data.frame(data[,strech_ncol])
        rongqi=c()
        for (i in 1:nrow(data)){
            timesrep=freq_dataf[i,]
            linshirongqi=data[rep(i,timesrep),]
            rongqi=rbind(rongqi,linshirongqi)
        }
        rongqi=rongqi[,colnames(data) %not% strech]
        if (!is.data.frame(rongqi)){
            rongqi=data.frame(rongqi)
            colnames(rongqi) <- (colnames(data) %not% strech)
        }
        return(rongqi)
}
