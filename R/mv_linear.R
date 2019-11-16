#' Multivariable Linear Regression
#'
#' @param data data
#' @param y y varaible
#' @param x variable names for univariable linear regression. If missing, it will be column names of data except y and adjust
#' @param direction direction for stepwise regression. Four options: no, backward, forward and both. Defaulted is no
#' @param summary logical. Whether to return summary results. TRUE as defaulted
#' @param ... arguments passed to step() function
#' @importFrom stats glm gaussian step
#' @return multivariable linear regression results
#' @export
#'
#' @examples
#' mv_linear(data = rock,y = 'perm',
#'     direction = 'both')
mv_linear <- function(data,y,x,direction='no',summary=TRUE,...){
    #delet na
    data=delet_na_df.mtr(data)
    if (missing(x)) x=colnames(data) %not% y
    formu=paste0(y,'~',paste0(x,collapse = '+'))
    linear=glm(as.formula(formu),data = data,
               family = gaussian(link = "identity"))
    if (direction=='no'){
        if (summary){
            summary(linear)
        }else{
            linear
        }
    }else{
        linear_step=step(object = linear,
                         direction = direction,...)
        message('\n\n\n\n-----------\n\n\n\n')
        if (summary){
            summary(linear_step)
        }else{
            linear_step
        }
    }
}
