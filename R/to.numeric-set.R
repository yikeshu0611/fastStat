#' Change to Numeric Form
#'
#' @param x vector
#' @param value anything, which will be ignored
#'
#' @return numeric data
#' @export
#'
#' @examples
#' x=c(1,2,3)
#' to.factor(x) <- 1
#' to.numeric(x) <- 1
`to.numeric<-` <- function(x,value){
    as.numeric(as.character(x))
}
