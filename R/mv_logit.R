#' Multivariable Logistic Regression
#'
#' @param data data
#' @param y y varaible
#' @param x variable names for univariable logistic regression. If missing, it will be column names of data except y and adjust
#' @param direction direction for stepwise regression. Four options: no, backward, forward and both. Defaulted is no
#' @param summary logical. Whether to return summary results. TRUE as defaulted
#' @param ... arguments passed to step() function
#'
#' @return multivariable logistic regression results
#' @export
#'
#' @examples
#' mv_logit(data = mtcars,y = 'am',
#'     variable = c('cyl','disp'))
mv_logit <- function(data,y,x,direction='no',summary=TRUE,...){
    #delet na
    data=delet_na_df.mtr(data)
    if (missing(x)) x=colnames(data) %not% y
    formu=paste0(y,'~',paste0(x,collapse = '+'))
    logit=glm(as.formula(formu),data = data,
              family = binomial(link = "logit"))
    if (direction=='no'){
        if (summary){
            summary(logit)
        }else{
            logit
        }
    }else{
        logit=step(object = logit, direction = direction,...)
        message('\n\n\n\n-----------\n\n\n\n')
        if (summary){
            summary(logit)
        }else{
            logit
        }
    }
}
