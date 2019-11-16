#' Summary for Data
#'
#' @param x numeric
#' @param round digital number
#'
#' @return a dataframe with min, max, quantile 25 and 75, mean, median, sd and NA
#' @export
#'
#' @examples
#' list.summary(mtcars)
list.summary <- function(x,round=2){
    min=NULL
    max=NULL
    q25=NULL
    q75=NULL
    mean=NULL
    median=NULL
    sd=NULL
    NAs=NULL
    if (is.vector(x)) x=data.frame(x)
    for (i in 1:ncol(x)) {
        x.i=x[,i]
        if (!is.numeric(x.i)) x.i=is.numeric(is.character(x.i))
        min=c(min,round(Min(x.i),round))
        max=c(max,round(Max(x.i),round))
        q25=c(q25,round(q25(x.i),round))
        q75=c(q75,round(q75(x.i),round))
        mean=c(mean,round(Mean(x.i),round))
        median=c(median,round(Median(x.i),round))
        sd=c(sd,round(Sd(x.i),round))
        NAs=c(NAs,paste0(round(sum(is.na(x[,i]))/length(x[,i])*100,2),'(',sum(is.na(x[,i])),')'))
    }
    res=data.frame(min=min,max=max,q25=q25,q75=q75,
               mean=mean,median=median,sd=sd,NAs)
    rownames(res)=colnames(x)
    res
}
