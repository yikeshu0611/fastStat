#' Return All Numeric Variables in A Dataframe
#'
#' @param df a dataframe
#'
#' @return numeric variable names
#' @export
#'
#' @examples
#' jh=data.frame(x=c(1,2,3,1),
#'     k=c(4,5,6,7),
#'     h=c('a','a','b','b'))
#' list.numeric(jh)
list.numeric <- function(df){
    df.check=is.data.frame(df)
    if (!df.check) stop('df must be a dataframe')
    for (i in 1:ncol(df)) {
        if (i==1) res=NULL
        if (is.numeric(df[,i])) res=c(res,colnames(df)[i])
    }
    if (length(res)==0){
        message('your data has no numeric variable')
    }else{
        res
    }
}
