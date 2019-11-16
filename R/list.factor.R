#' Return All Factor Variables
#' @description Return all factor variables in a dataframe or matrix
#'
#' @param x a dataframe or matrix
#' @param levels logical. TRUE to display levels for factor variable.
#'
#' @return factor variable names and levels
#' @export
#'
#' @examples
#' jh=data.frame(x=c(1,2,3,1),
#'     k=c(4,5,6,7),
#'     h=c('a','a','b','b'))
#' list.factor(jh)
list.factor <- function(x,levels=FALSE){
    x.check=any(is.data.frame(x),is.matrix(x))
    if (!x.check) stop('x must be a dataframe or matrix')
    for (i in 1:ncol(x)) {
        if (i==1) {
            res=NULL
            level=NULL
        }
        if (is.factor(x[,i])){
            res=c(res,colnames(x)[i])
            level=c(level,paste0(levels(x[,i]),collapse = ','))
        }
    }
    if (levels){
        res=as.data.frame(cbind(factor.var=res,
                                levels=level))
        if (ncol(res)==0){
            message('your data has no factor variable')
        }else{
            res
        }
    }else{
        if (length(res)==0){
            message('your data has no factor variable')
        }else{
            res
        }
    }
}
