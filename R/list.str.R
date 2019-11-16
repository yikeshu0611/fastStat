#' Structure for Data
#'
#' @param x a dataframe or matrix
#' @param n the maximum level number to display
#'
#' @return a dataframe contains variable names and class
#' @export
#'
#' @examples
#' jh=data.frame(x=c(1,2,3,1),
#'               k=c(4,5,6,7),
#'               h=c('a','a','b','b'))
#' list.str(x = jh)
list.str <- function(x,n=3){
    if (is.vector(x)) x=data.frame(x)
    if (is.factor(x)) x=data.frame(x)
    if (is.numeric(x)) x=data.frame(x)
    x.check= any(is.data.frame(x),is.matrix(x))
    if (!x.check) stop('x must be a dataframe or matrix')
    for (i in 1:ncol(x)) {
        if (i==1){
            class=NULL
            level=NULL
            level.NO=NULL
            NAs=NULL
        }
        NAs=c(NAs,paste0(round(sum(is.na(x[,i]))/length(x[,i])*100,2),'(',sum(is.na(x[,i])),')'))
        class=c(class,class(x[,i]))
        level.unique=unique_no_na(x[,i])
        level.len=length(level.unique)
        level.NO=c(level.NO,level.len)
        if (class(x[,i])=='factor'){
            if (level.len <= n){
                level=c(level,paste0(levels(x[,i]),collapse = ','))
            }else{
                level=c(level,paste0(levels(x[,i])[1:n],collapse = ','))
            }
        }else{
            if (level.len<=n){
                level=c(level,paste0(level.unique,collapse = ','))
            }else{
                level=c(level,paste0(
                    paste0(level.unique[1:n],collapse = ','),'...'))
            }
        }
    }
    res=data.frame(var=colnames(x),class,NAs=NAs,
                   level.NO=do::equal_length(level.NO," ",6),
                   level=do::equal_length(level," "))
    colnames(res)[3]=paste0('NAs','(',nrow(x),')')
    res=res[order(res[,'class']),]
    rownames(res)=1:nrow(res)
    res
}
