#' Give Labels to Factor
#'
#' @param x factor or numeric variable
#' @param labels labels separated by colon
#'
#' @return factor variable with lables, the first lable will be treated as reference.
#' @export
#'
#' @examples
#' to.labels(x=mtcars$am,labels=c('0:Female','1:Man'))
to.labels<-function(x,labels){
    levels=do::Replace0(labels,':.*')
    labels=do::Replace0(labels,'.*:')
    factor(x,levels = levels,labels = labels)
}
