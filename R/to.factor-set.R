#' @title Set Factor Class
#'
#' @param x the data that you want to set
#' @param value levels, the first value is the reference. If the length of value is 1, no levels will be given to x
#'
#' @return factor x
#' @export
#'
#' @examples
#' to.factor(mtcars$gear) <- c(4,3,5)
`to.factor<-` <- function(x,value){
    if (length(value)==1){
        x=factor(x)
    }else{
        x=factor(x,levels = value)
    }
    x
}
