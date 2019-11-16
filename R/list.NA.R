#' @title Return Na Count and Percentage
#' @description Return Na count and percentage for each variable in a dataframe or matrix.
#' @param x a numeric vector, a dataframe or matix
#'
#' @return A dataframe contains NA variable names, NA count and percentage
#' @export
#'
#' @examples
#'  jh=data.frame(x=c(1,2,3,1),
#'      k=c(4,5,6,7),
#'      h=c('a','a',NA,'D'),
#'      f=c(1,2,NA,NA))
#' list.NA(jh)
list.NA <- function(x){
    x.check=any(is.data.frame(x),is.matrix(x),
                 all(is.vector(x),is.numeric(x)))
    if (!x.check) stop('x must be a dataframe or a numeric vector')
    if (any(is.data.frame(x),is.matrix(x))){
        for (i in 1:ncol(x)) {
            if (i==1) {
                res=NULL
                count=NULL
                percent=NULL
            }
            x.i=x[,i]
            if (any(is.na(x.i))){
                res=c(res,colnames(x)[i])
                count.i=sum(is.na(x.i))
                count=c(count,count.i)
                percent=c(percent,round(count.i/length(x.i)*100,2))
            }
        }
        res=as.data.frame(cbind(na.vrb=res,
                                count=count,
                                percent=percent))
        if (ncol(res)==0){
            message('your data has no NA.')
        }else{
            res=res[order(res$count,decreasing = TRUE),]
            rownames(res)=1:nrow(res)
            res
        }
    }else{
        data.frame(
        cbind(NAs='NA',count=sum(is.na(x)),
              percent=round(sum(is.na(x))/length(x)*100,2)))
    }

}
