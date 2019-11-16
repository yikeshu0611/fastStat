#' @title Normal Distribution Test
#' @description Using Jarque Bera test, shapiro wilk test and Kolmogorov Smirnov
#'     test for one numeric object or numeric object in dataframe or matrix. Na
#'     is omitted in each object.
#'
#' @param x numerica object or dataframe and matrix
#' @param num.names numeric column names for dataframe and matrix. If missing,
#'     all numeric column names will be given.
#' @importFrom stats shapiro.test ks.test
#' @return a dataframe containing kurtosis, skewness and p value for Jarque
#'     Bera test, shapiro wilk test and Kolmogorov Smirnov test. In star column,
#'     star represents p > 0.05, while underline taking the opposite.
#'
#' @export
#'
#' @examples
#' set.seed(2019)
#' rn1=rnorm(100,0,2)
#' df=data.frame(rn1=rnorm(100,0,2),
#'               rn2=rnorm(100,2,4))
#' #normal test for one object
#' normal(rn1)
#'
#' #normal test for dataframe
#' normal(df)
normal <- function(x,num.names){
    if (any(!is.data.frame(x),!is.matrix(x))){
        x=data.frame(x)
    }
    if (missing(num.names)) num.names=list.numeric(x)
    if (length(num.names)>0){
        for (i in 1:length(num.names)) {
            if (i==1) {
                skew=NULL
                kurt=NULL
                shapiro.wilk=NULL
                Kolmogorov.Smirnov=NULL
                Jarque.Bera=NULL
            }
            goal.dd=x[,num.names[i]]
            goal.dd=goal.dd[!is.na(goal.dd)] #delet na
            skew=c(skew,e1071::skewness(goal.dd,na.rm = T,2))
            kurt=c(kurt,e1071::kurtosis(goal.dd,na.rm = T,2))
            shapiro.wilk=c(shapiro.wilk,
                           shapiro.test(goal.dd)$p.value)
            Kolmogorov.Smirnov=c(Kolmogorov.Smirnov,
                                 ks.test(goal.dd,'pnorm',
                                         mean = mean(goal.dd),
                                         sd = sd(goal.dd))$p.value)
            Jarque.Bera=c(Jarque.Bera,tseries::jarque.bera.test(goal.dd)$p.value)
            if (i==length(num.names)){
                res=cbind(variable=num.names,
                          kurtosis=round(kurt,5),
                          skewness=round(skew,5),
                          Jarque.Bera=round(Jarque.Bera,5),
                          shapiro.wilk=round(shapiro.wilk,5),
                          Kolmogorov.Smirnov=round(Kolmogorov.Smirnov,5))
                res=as.data.frame(res)
                for (i in 2:6) {
                    res[,i]=as.numeric(as.character(res[,i]))
                }
                star=ifelse(res[,4:6]>=0.05,'*','_')
                if (!is.matrix(star)){
                    star=data.frame(t(star))
                    delet=1
                }else{
                    delet=2
                }
                star_col=paste0(star[,1],star[,2])
                star_col=paste0(star_col,star[,3])
                res=data.frame(res)
                res$star=star_col
                if (delet==1) res=res[,-1]
                return(res)
            }
        }
    }else{
        message('no numeric object in your data')
    }

}
