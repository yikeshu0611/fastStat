# title count and percent for categorical variable
#
# param x number, dataframe or matrix
# param group group variable for dataframe or matrix
# param wide a logical word. The formation of result, while TRUE as wide form, FLASE as long form.
# param round round
#
# importFrom set %not%
# return dataframe
#
#
# examples
# df=data.frame(sex=c('man','female','female','man','man','man','female'),
#               hypertension=c('yes','yes','no','no','yes','yes','yes'),
#               trt=c('control','trt','control','trt','trt','control','trt'))
# count_percent(x = df,group = 'trt')
#
count_percent <- function(x,group,wide=TRUE,round=2){
    fun_temp<-function(x,round){
        table.x=table(x)
        res<-cbind(names(table.x),
                   paste0(table.x,'/',sum(table.x),
                          '(',paste0(digital(prop.table(table.x)*100,round),"%)")))
        colnames(res)=c('colname','result')
        as.data.frame(res)
    }
    fen_temp_x<-function(x,round){
        if (any(is.data.frame(x),is.matrix(x))){
            for (i in 1:ncol(x)) {
                if (i==1){
                    res=fun_temp(x[,i],round)
                    res[,1]=paste0(colnames(x)[i],':',res[,1])
                }else{
                    res.i=fun_temp(x[,i],round)
                    res.i[,1]=paste0(colnames(x)[i],':',res.i[,1])
                    res=rbind(res,res.i)
                }
            }
            res
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
        res_no.group=fen_temp_x(x.left,2)
        res_no.group[,'result']=do::Replace0(res_no.group[,'result'],c('/.*'))
        colnames(res_no.group)[2]='total'
        res[,'result']=do::Replace0(res[,'result'],'/.*')
        res.merge=merge(res,res_no.group,by = 'colname')
        res.merge[,'result']=as.numeric(as.character(res.merge[,'result']))
        res.merge[,'total']=as.numeric(as.character(res.merge[,'total']))
        res.merge[,'result']=paste0(res.merge[,'result'],'/',res.merge[,'total'],
                                    '(',
                                    paste0(digital(res.merge[,'result']/res.merge[,'total']*100,round),'%'),
                                    ')'
                                    )
        res=res.merge[,colnames(res.merge) %not% 'total']
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
            res$var=do::Replace0(res$colname,':.*')
            for (i in 1:ncol(x.left)) {
                if (i==1) res.order=NULL
                res.i=res[res$var==colnames(x.left)[i],]
                res.i=res.i[order(res.i$colname),]
                res.order=rbind(res.order,res.i)
            }
            res=res.order[,-ncol(res.order)]
            rownames(res)=1:nrow(res)
        }
    res
    }
}
