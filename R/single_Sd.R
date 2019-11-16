# get sd
#
# @param x number, dataframe or matrix
# @param group group variable for dataframe or matrix
# @param wide wide a logical word. The formation of result, while TRUE as wide form, FLASE as long form.
# @param round round
# @importFrom set %not%
# @return string
#
# @examples
# #number character
# Sd(1:100)
#
# #dataframe
# a=1:100
# b=c(rep(10,20),rep(1,20),rep(50,40),rep(4,20))
# g1=c(rep(1,20),rep(2,70),rep(3,10))
# df=data.frame(a,b,g1)
# Sd(df)
#
# #one group
# Sd(df,'g1')
#
# #two or more groups
# g2=c(rep('a',50),rep('b',50))
# df=data.frame(a,b,g1,g2)
# Sd(df,c('g1','g2'))
#
Sd<-function(x,group,wide=TRUE,round=2){
    fun_temp<-function(x,round){
        round(sd(x,na.rm = TRUE),round)
    }
    fen_temp_x<-function(x,round){
        if (any(is.data.frame(x),is.matrix(x))){
            for (i in 1:ncol(x)) {
                if (i==1){
                    median_range_res=c()
                    colname=c()
                }
                x.i=x[,i]
                if (any(is.numeric(x.i),is.integer(x.i))){
                    median_range_res=c(median_range_res,
                                       fun_temp(x.i,round))
                    colname=c(colname,colnames(x)[i])
                }
            }
            data.frame(colname=colname,
                       result=median_range_res)
        }else{
            fun_temp(x,round)
        }
    }
    #start
    if (missing(group)){
        fen_temp_x(x,round)
    }else{
        for (i in 1:length(group)) {
            if (i==1){
                gp=x[,group[i]]
            }else{
                gp.i=x[,group[i]]
                gp=paste0(gp,'mycbindandmysplit',gp.i)
            }
        }
        gp.ui=unique(gp)
        x.left=x[,colnames(x) %not% group]
        if ((length(colnames(x) %not% group))==1){
            x.left=data.frame(x.left)
            colnames(x.left)=colnames(x) %not% group
        }
        for (i in 1:length(gp.ui)) {
            if (i==1) res=NULL
            res.ii=x.left[gp==gp.ui[i],]
            if (ncol(x.left)==1){
                res.ii=data.frame(res.ii)
                colnames(res.ii)=colnames(x.left)
            }
            res.i=fen_temp_x(res.ii,round)
            res.i$mycbindandmysplit=gp.ui[i]
            res=rbind(res,res.i)
        }
        group.df=reshape2::colsplit(res$mycbindandmysplit,'mycbindandmysplit',group)
        res=cbind(group.df,res[,-ncol(res)])
        if (wide){
            for (i in 1:length(group)) {
                if (i==1){
                    group.data=as.character(res[,group[i]])
                }else{
                    group.data=paste0(group.data,
                                      as.character(res[,group[i]]))
                }

            }
            res=res[,colnames(res) %not% group]
            res$group=group.data
            res=reshape_towide(data = res,j = 'group',prefix = 'result')
            colnames(res)=do::Replace(colnames(res),
                                      pattern = c('result:',
                                                  'mycbindandmysplit:_'))
            rownames(res)=1:nrow(res)
        }
        res
    }
}
