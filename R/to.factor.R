#' @title Set Factor Class
#'
#' @param x the data that you want to set
#' @param levels levels, the first levels is the reference. If the length of levels is 1, no levels will be given to x
#'
#' @return factor x
#' @export
#'
#' @examples
#' to.factor(mtcars$gear,c(4,3,5))
#' to.factor(mtcars$gear)
to.factor <- function(x,levels){
    if (missing(levels)){
        factor(x)
    }else{
        factor(x,levels = levels)
    }
}
