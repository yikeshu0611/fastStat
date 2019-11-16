#' Looping for logrank Regression
#'
#' @param data data
#' @param time time variable
#' @param event event variable
#' @param variable variable names for logrank regression. If missing, it will be column names of data except y
#' @param round digital round, 3 is defaulted
#' @param order_by.p logical. TRUE, defaulted, means increasing order in p value
#'
#'
#' @return  logrank regression results
#' @export
#'
#' @examples
#' uv_logrank(data = mtcars,
#'     time = 'qsec',event = 'vs')
uv_logrank <- function(data,time,event,variable,
                       round=3,order_by.p=TRUE){
    data=delet_na_df.mtr(data)
    if (missing(variable)) variable=colnames(data) %not% c(time,event)
    for (i in 1:length(variable)) {
        if (i==1) res=NULL
        formu=paste0('survival::Surv(',time,',',event,')~',variable[i])
        logrank.i=survival::survdiff(formula=as.formula(formu),data = data)
        res=c(res,survdiff_p.value(logrank.i))
    }
    res=round(res,round)
    star=ifelse(res <= 0.001,'***',
                ifelse(res <= 0.01, '** ',
                       ifelse(res <= 0.05, '*  ',
                              ifelse(res <= 0.1,".  ",' '))))
    res.cbind=data.frame(cbind(variable,p.value=res,star))
    if (order_by.p){
        res.cbind=res.cbind[order(res.cbind$p.value),]
    }
    return(res.cbind)
}
