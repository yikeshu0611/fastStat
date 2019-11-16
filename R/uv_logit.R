#' Looping for Univariable Logistic Regression
#'
#' @param data data
#' @param y y
#' @param variable variable names for univariable logistic regression. If missing, it will be column names of data except y and adjust
#' @param adjust adjust variable names for univariable logistic regression
#' @param round digital round, 3 is defaulted
#' @param p_threshold threshold for p value to show star. 0.05 is defaulted
#' @param order_by.or logical. TRUE means order in or by decreasing. FLASE is defaulted
#' @importFrom stats binomial
#' @return univariable logistic regression results
#' @export
#'
#' @examples
#' uv_logit(data = mtcars,y = 'vs')
uv_logit <- function(data,y,variable,adjust,round=3,
                     p_threshold=0.05,order_by.or=TRUE){
    data=delet_na_df.mtr(data)
    if (missing(adjust)){
        if (missing(variable)) variable=colnames(data) %not% y
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
            formu=paste0(y,'~',variable[i])
            logit.i=glm(as.formula(formu),
                        data = data, family = binomial(link = "logit"))
            logit.sum=summary(logit.i)
            logit_coef=as.data.frame(logit.sum$coefficients)
            logit_var=logit_coef[-1,]
            confint=as.data.frame(suppressWarnings(suppressMessages(exp(confint(logit.i)))))[-1,]
            res.cbind=cbind(or=exp(logit_var[,'Estimate']),
                            confint,
                            logit_var[,c('Std. Error','z value','Pr(>|z|)')])
            res.i=round(res.cbind,round)
            res=rbind(res,res.i)
        }
        res2=cbind(res,class)
        if (order_by.or){
            res3=res2[order(res2$class, res2$or,decreasing = TRUE),-ncol(res2)]
        }else{
            res3=res2[,-ncol(res2)]
        }
        star=ifelse(res3$or>=6,'   |***',
                    ifelse(res3$or>=3 & res3$or <6 , '   |** ',
                           ifelse(res3$or>1 & res3$or <3, '   |*  ',
                                  ifelse(res3$or>=1/3 & res3$or<1,"  *|   ",
                                         ifelse(res3$or>= 1/6 & res3$or < 1/3,' **|   ',
                                                ifelse(res3$or==1,'   |   ','***|   '))))))
        res3$star=ifelse(res3$`Pr(>|z|)`<=p_threshold,star,"")
        return(res3)
    }else{
        if (missing(variable)) variable=colnames(data) %not% c(y,adjust)
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
            #logistic for adjust
            if (i==1){
                formu=paste0(y,'~',paste0(c(adjust),collapse = '+'))
                logit.i=glm(as.formula(formu),
                            data = data, family = binomial(link = "logit"))
                logit.sum=summary(logit.i)
                logit_coef=as.data.frame(logit.sum$coefficients)
                nub_row=nrow(logit_coef)
            }
            #logistci for variable
            formu=paste0(y,'~',paste0(c(adjust,variable[i]),collapse = '+'))
            logit.i=glm(as.formula(formu),
                        data = data, family = binomial(link = "logit"))
            logit.sum=summary(logit.i)
            logit_coef=as.data.frame(logit.sum$coefficients)
            logit_var=logit_coef[-c(1:nub_row),]
            confint=as.data.frame(suppressWarnings(suppressMessages(confint(logit.i))))[-c(1:nub_row),]
            res.cbind=cbind(or=exp(logit_var[,'Estimate']),
                            confint,
                            logit_var[,c('Std. Error','z value','Pr(>|z|)')])
            res.i=round(res.cbind,round)
            res=rbind(res,res.i)
            res.i
        }
        res2=cbind(res,class)
        if (order_by.or){
            res3=res2[order(res2$class, res2$or,decreasing = TRUE),-ncol(res2)]
        }else{
            res3=res2[,-ncol(res2)]
        }
        star=ifelse(res3$or>=6,'   |***',
                    ifelse(res3$or>=3 & res3$or <6 , '   |** ',
                           ifelse(res3$or>1 & res3$or <3, '   |*  ',
                                  ifelse(res3$or>=1/3 & res3$or<1,"  *|   ",
                                         ifelse(res3$or>= 1/6 & res3$or < 1/3,' **|   ',
                                                ifelse(res3$or==1,'   |   ','***|   '))))))
        res3$star=ifelse(res3$`Pr(>|z|)`<=p_threshold,star,"")
        return(res3)
    }
}
