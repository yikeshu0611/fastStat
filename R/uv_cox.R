#' Looping for Univariable Cox Regression
#'
#' @param data data
#' @param time time variable
#' @param event event variable
#' @param variable variable names for univariable cox regression. If missing, it will be column names of data except y and adjust
#' @param adjust adjust variable names for univariable cox regression
#' @param round digital round, 3 is defaulted
#' @param p_threshold threshold for p value to show star. 0.05 is defaulted
#' @param order_by.hr logical. TRUE means order in or by decreasing. FLASE is defaulted
#'
#'
#' @return univariable cox regression results
#' @export
#'
#' @examples
#' uv_cox(data = mtcars,
#'     time = 'qsec',event = 'vs')
uv_cox <- function(data,time,event,variable,adjust,round=3,
                     p_threshold=0.05,order_by.hr=TRUE){
    #delet na
    data=delet_na_df.mtr(data)
    if (is.factor(data[,event])) stop(event,' shoud be numeric not factor')
    if (missing(adjust)){
        if (missing(variable)) variable=colnames(data) %not% c(time,event)
        for (i in 1:length(variable)) {
            if (i==1){
                res=NULL
                class=NULL
            }
            if (is.factor(data[,variable[i]])){
                rep_len=length(levels(data[,variable[i]]))-1
                if (rep_len>1){
                    rep_class=rep(i+1,rep_len)
                    class=c(class,rep_class)
                }else{
                    class=c(class,0)
                }
            }else{
                class=c(class,0)
            }

            formu=paste0('survival::Surv(',time,',',event,')~',variable[i])
            cox.i=survival::coxph(as.formula(formu),
                        data = data)
            cox.sum=summary(cox.i)
            cox_coef1=as.data.frame(cox.sum$coefficients)[,c(3,4,5)]
            cox_coef2=as.data.frame(cox.sum$conf.int)[,c(1,3,4)]
            res.cbind=cbind(cox_coef2,cox_coef1)
            res.i=round(res.cbind,round)
            res=rbind(res,res.i)
        }
        res2=cbind(res,class)
        if (order_by.hr){
            res3=res2[order(res2$class, res2$`exp(coef)`,decreasing = TRUE),-ncol(res2)]
        }else{
            res3=res2[,-ncol(res2)]
        }
        star=ifelse(res3$`exp(coef)`>=6,'   |***',
                    ifelse(res3$`exp(coef)`>=3 & res3$`exp(coef)` <6 , '   |** ',
                           ifelse(res3$`exp(coef)`>1 & res3$`exp(coef)` <3, '   |*  ',
                                  ifelse(res3$`exp(coef)`>=1/3 & res3$`exp(coef)`<1,"  *|   ",
                                         ifelse(res3$`exp(coef)`>= 1/6 & res3$`exp(coef)` < 1/3,' **|   ',
                                                ifelse(res3$`exp(coef)`==1,'   |   ','***|   '))))))
        res3$star=ifelse(res3$`Pr(>|z|)`<=p_threshold,star,"")
        return(res3)
    }else{
        if (missing(variable)) variable=colnames(data) %not% c(time,event,adjust)
        for (i in 1:length(variable)) {
            if (i==1){
                res=NULL
                class=NULL
            }
            if (is.factor(data[,variable[i]])){
                rep_len=length(levels(data[,variable[i]]))-1
                if (rep_len>1){
                    rep_class=rep(i+1,rep_len)
                    class=c(class,rep_class)
                }else{
                    class=c(class,0)
                }
            }else{
                class=c(class,0)
            }
            #cox for adjust
            if (i==1){
                formu=paste0('survival::Surv(',time,',',event,')~',paste0(adjust,collapse = '+'))
                cox.i=survival::coxph(as.formula(formu),
                                      data = data)
                cox.sum=summary(cox.i)
                cox_coef1=as.data.frame(cox.sum$coefficients)
                nub_row=nrow(cox_coef1)
            }
            #logistci for variable
            formu=paste0('survival::Surv(',time,',',event,')~',paste0(c(adjust,variable[i]),collapse = '+'))
            cox.i=survival::coxph(as.formula(formu),
                                  data = data)
            cox.sum=summary(cox.i)
            cox_coef1=as.data.frame(cox.sum$coefficients)[-(1:nub_row),c(3,4,5)]
            cox_coef2=as.data.frame(cox.sum$conf.int)[-(1:nub_row),c(1,3,4)]
            res.cbind=cbind(cox_coef2,cox_coef1)
            res.i=round(res.cbind,round)
            res=rbind(res,res.i)
        }
        res2=cbind(res,class)
        if (order_by.hr){
            res3=res2[order(res2$class, res2$`exp(coef)`,decreasing = TRUE),-ncol(res2)]
        }else{
            res3=res2[,-ncol(res2)]
        }
        star=ifelse(res3$`exp(coef)`>=6,'   |***',
                    ifelse(res3$`exp(coef)`>=3 & res3$`exp(coef)` <6 , '   |** ',
                           ifelse(res3$`exp(coef)`>1 & res3$`exp(coef)` <3, '   |*  ',
                                  ifelse(res3$`exp(coef)`>=1/3 & res3$`exp(coef)`<1,"  *|   ",
                                         ifelse(res3$`exp(coef)`>= 1/6 & res3$`exp(coef)` < 1/3,' **|   ',
                                                ifelse(res3$`exp(coef)`==1,'   |   ','***|   '))))))
        res3$star=ifelse(res3$`Pr(>|z|)`<=p_threshold,star,"")
        return(res3)
    }
}
