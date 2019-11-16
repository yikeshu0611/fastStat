# @title Calculate Percentage for Categorical Variable
#
# @param x number, dataframe or matrix
# @param group group variable for dataframe or matrix
# @param direction calculate of direction, which should be one of g, group, v or var, v as defaulted.
# @param round digital round
# @param wide a logical word. The formation of result, while TRUE as wide form, FLASE as long form.
#
#
# @importFrom set %not%
# @return dataframe
#
# @examples
# df=data.frame(sex=c('man','female','female','man','man','man','female'),
#               hypertension=c('yes','yes','no','no','yes','yes','yes'),
#               trt=c('control','trt','control','trt','trt','control','trt'))
# Percent(x = df,group = 'trt')
#
Percent <- function(x,group,direction='v',round=2,wide=TRUE){
    fun_temp<-function(x){
        table.x=table(x)
        res<-cbind(names(table.x),
                   paste0(table.x,'/',sum(table.x)))
        colnames(res)=c('colname','result')
        as.data.frame(res)
    }
    fen_temp_x<-function(x){
        if (any(is.data.frame(x),is.matrix(x))){
            for (i in 1:ncol(x)) {
                if (i==1){
                    res=fun_temp(x[,i])
                    res[,1]=paste0(colnames(x)[i],':',res[,1])
                }else{
                    res.i=fun_temp(x[,i])
                    res.i[,1]=paste0(colnames(x)[i],':',res.i[,1])
                    res=rbind(res,res.i)
                }
            }
            res
        }else{
            fun_temp(x)
        }
    }
    #start
    if (missing(group)){
        res<-fen_temp_x(x)
        res[,2]=
            round(as.numeric(as.character(do::Replace0(res[,2],'/.*')))/
                      as.numeric(as.character(do::Replace0(res[,2],'.*/')))*
                      100,
                  round)
        return(res)
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
            if (i==1){
                res.ii=x.left[gp==gp.ui[i],]
                if (ncol(x.left)==1){
                    res.ii=data.frame(res.ii)
                    colnames(res.ii)=colnames(x.left)
                }
                res=fen_temp_x(res.ii)
                res$mycbindandmysplit=gp.ui[i]
            }else{
                res.ii=x.left[gp==gp.ui[i],]
                if (ncol(x.left)==1){
                    res.ii=data.frame(res.ii)
                    colnames(res.ii)=colnames(x.left)
                }
                res.i=fen_temp_x(res.ii)
                res.i$mycbindandmysplit=gp.ui[i]
                res=rbind(res,res.i)
            }
        }
        #check direction
        if (!any(direction == c('group','g','var','v'))){
            stop('direction should be one of g, group, v or var.')
        }
        #direction='group'
        if (any(direction=='group',direction=='g')){
            if (wide){
                res_wide=reshape_towide(data = res,id = 'colname',
                                            j = 'mycbindandmysplit',prefix = 'result')
                colnames(res_wide)=do::Replace(colnames(res_wide),
                                               pattern = c('result:',
                                                           'mycbindandmysplit:_'))
                rownames(res_wide)=1:nrow(res_wide)
                for (i in 2:(length(gp.ui)+1)) {
                    res_wide[,i]=
                        round(as.numeric(as.character(do::Replace0(res_wide[,i],'/.*')))/
                                  as.numeric(as.character(do::Replace0(res_wide[,i],'.*/')))*
                                  100,
                              round)
                }
                return(res_wide)
            }else{
                group.df=reshape2::colsplit(res$mycbindandmysplit,'mycbindandmysplit',group)
                res=cbind(group.df,res[,-ncol(res)])
                res[,3]=
                    round(as.numeric(as.character(do::Replace0(res[,3],'/.*')))/
                              as.numeric(as.character(do::Replace0(res[,3],'.*/')))*
                              100,
                          round)
                return(res)
            }
        }
        #direction=var or v
        if (any(direction=='var',direction=='v')){
            res_no.group=fen_temp_x(x.left)
            res_no.group[,'result']=do::Replace0(res_no.group[,'result'],c('/.*'))
            colnames(res_no.group)[2]='total'
            res[,'result']=do::Replace0(res[,'result'],'/.*')
            res.merge=merge(res,res_no.group,by = 'colname')
            res.merge[,'result']=as.numeric(as.character(res.merge[,'result']))
            res.merge[,'total']=as.numeric(as.character(res.merge[,'total']))
            res.merge[,'result']=paste0(res.merge[,'result'],'/',res.merge[,'total'])
            res=res.merge[,colnames(res.merge) %not% 'total']
            if (wide){
                res_wide=reshape_towide(data = res,id = 'colname',
                                            j = 'mycbindandmysplit',prefix = 'result')
                colnames(res_wide)=do::Replace(colnames(res_wide),
                                               pattern = c('result:',
                                                           'mycbindandmysplit:_'))
                res_wide$var=do::Replace0(res_wide$colname,':.*')
                for (i in 1:ncol(x.left)) {
                    if (i==1) res.order=NULL
                    res.i=res_wide[res_wide$var==colnames(x.left)[i],]
                    res.i=res.i[order(res.i$colname),]
                    res.order=rbind(res.order,res.i)
                }
                res_wide=res.order[,-ncol(res.order)]
                rownames(res_wide)=1:nrow(res_wide)
                for (i in 2:(length(gp.ui)+1)) {
                    res_wide[,i]=
                        round(as.numeric(as.character(do::Replace0(res_wide[,i],'/.*')))/
                                  as.numeric(as.character(do::Replace0(res_wide[,i],'.*/')))*
                                  100,
                              round)
                }
                return(res_wide)
            }else{
                group.df=reshape2::colsplit(res$mycbindandmysplit,'mycbindandmysplit',group)
                res=cbind(group.df,res[,-ncol(res)])
                res[,3]=
                    round(as.numeric(as.character(do::Replace0(res[,3],'/.*')))/
                              as.numeric(as.character(do::Replace0(res[,3],'.*/')))*
                              100,
                          round)
                return(res)
            }
        }
    }
}
