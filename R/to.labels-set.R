#' Give Labels to Factor
#'
#' @param x factor or numeric variable
#' @param value labels separated by colon
#'
#' @return factor variable with lables, the first lable will be treated as reference.
#' @export
#'
#' @examples
#' to.labels(x=mtcars$am) <- c('0:Female','1:Man')
`to.labels<-`<-function(x,value){
    levels=do::Replace0(value,':.*')
    labels=do::Replace0(value,'.*:')
    factor(x,levels = levels,labels = labels)
}
