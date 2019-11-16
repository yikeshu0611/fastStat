#' Change to Numeric Form
#'
#' @param x vector
#'
#' @return numeric data
#' @export
#'
#' @examples
#' x=c(1,2,3)
#' to.factor(x) <- 1
#' to.numeric(x)
to.numeric <- function(x){
    as.numeric(as.character(x))
}
