#' Set Refer for Factor
#' @description Convert data to be factor and set reference.
#' @param x the data that you want to set
#' @param value refering level
#'
#' @return refered factor value
#' @export
#'
#' @examples
#' to.refer(mtcars$vs) = 1
`to.refer<-` <- function(x,value){
    if (length(value)!=1) stop('value must be 1 length')
    if (!any(value == unique(x))) stop('value does not exist in x')
    x=factor(x)
    factor(x,levels = c(value,levels(x) %not% value))
}
