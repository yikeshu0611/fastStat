#' Extract P Value after survdiff() function
#'
#' @param survdiff the results of survdiff() function
#' @importFrom stats pchisq
#' @return p value
#' @export
#'
#' @examples
#' library(survival)
#' diff_result=survdiff(Surv(qsec,vs)~cyl,data=mtcars)
#' survdiff_p.value(diff_result)
survdiff_p.value <- function(survdiff){
    #the code is form survdiff() function in 'survival' package
    #the code can be used to calculate degree of freedom, p value excep chi
    #however you really can not extract p.value for survdiff() function
    if (is.matrix(survdiff$obs)) {
        otmp <- apply(survdiff$obs, 1, sum)
        etmp <- apply(survdiff$exp, 1, sum)
    }else {
        otmp <- survdiff$obs
        etmp <- survdiff$exp
    }
    df <- (etmp > 0)
    if (sum(df) < 2) {
        chi <- 0
        return(1)
    } else {
        temp2 <- ((otmp - etmp)[df])[-1]
        vv <- (survdiff$var[df, df])[-1, -1, drop = FALSE]
        chi <- sum(solve(vv, temp2) * temp2)
        survdiff.pvalue=1 - pchisq(chi, length(temp2))
        return(survdiff.pvalue)
    }
}
