#' Set Digital Number
#'
#' @param x vector, dataframe or matrix
#' @param round digital number
#'
#' @return character with the same digital number
#' @export
#'
#' @examples
#' digital(1.2,4)
digital <- function(x,round){
    digital.i <- function(x,round){
        if (is.numeric(x)){
            x=round(x,round)
            format(x,nsmall = round)
        }else{
            format(x,digits = round,nsmall = round)
        }
    }
    if (any(is.data.frame(x),is.matrix(x))){
        for (i in 1:ncol(x)) {
            x[,i]=digital.i(x[,i],round)
        }
        x
    }else{
        digital.i(x,round)
    }
}
