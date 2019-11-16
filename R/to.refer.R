#' Set Refer for Factor
#' @description Convert data to be factor and set reference.
#' @param x the data that you want to set
#' @param refer refering level
#'
#' @return refered factor refer
#' @export
#'
#' @examples
#' to.refer(mtcars$vs,1)
to.refer <- function(x,refer){
    if (length(refer)!=1) stop('refer must be 1 length')
    if (!any(refer == unique(x))) stop('refer does not exist in x')
    x=factor(x)
    factor(x,levels = c(refer,levels(x) %not% refer))
}
