#' Multivariable Logistic Regression
#'
#' @param data data
#' @param time time variable
#' @param event event variable
#' @param x variable names for univariable logistic regression. If missing, it will be column names of data except y and adjust
#' @param direction direction for stepwise regression. Four options: no, backward, forward and both. Defaulted is no
#' @param summary logical. Whether to return summary results. TRUE as defaulted
#' @param ... arguments passed to step() function.
#'
#' @return multivariable logistic regression results
#' @export
#'
#' @examples
#' mv_cox(data = mtcars,
#'     time = 'qsec',event = 'am',
#'     direction = 'both')
mv_cox <- function(data,time,event,x,
                   direction='no',summary=TRUE,...){
    #delet na
    data=delet_na_df.mtr(data)
    if (is.factor(data[,event])) stop(event,' shoud be numeric not factor')
    if (missing(x)) x=colnames(data) %not% c(time,event)
    formu=paste0('survival::Surv(',time,',',event,')~',paste0(x,collapse = '+'))
    cox=survival::coxph(as.formula(formu), data = data)
    if (direction=='no'){
        if (summary){
            summary(cox)
        }else{
            cox
        }
    }else{
        cox=step(object = cox, direction = direction,...)
        message('\n\n\n\n-----------\n\n\n\n')
        if (summary){
            summary(cox)
        }else{
            cox
        }
    }
}
