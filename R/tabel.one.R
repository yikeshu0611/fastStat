#' @title Get Summary Table
#' @description Get the first summary table when study.
#'
#' @param data data that will be summarized
#' @param group one or more group variable names
#' @param mean_sd variable names for mand and standard deviation. in the results represents plus and minus
#' @param median_q4 variable names for median and 25 and 75 quantiles
#' @param median_range variable names for median and range
#' @param count_percent variable names for count and percentage
#' @param mean variable names for mean
#' @param median variable names for median
#' @param max variable names for max
#' @param min variable names for min
#' @param sd variable names for standard deviation
#' @param q25 variable names for 25 quantile
#' @param q75 variable names for 75 quantile
#' @param count variable names for count
#' @param percent variable names for percentage
#' @param count.percent.direction calculate of direction for count, percent and count_percent arguments, which should be one of g, group, v or var, v as defaulted
#' @param round digital round. 2 is defaulted
#' @param t.test two-side t test
#' @param anova two-side anova
#' @param wilcox.test two-side wilcox test
#' @param kruskal.test two-side kruskal test
#' @param chisq.test two-side chisq test
#' @param fisher.test two-side fisher test
#' @param weighted weight for data
#' @param statistics a logical object. TRUE to display the statistic information. Default is FALSE
#'
#' @importFrom set %not%
#' @importFrom stats as.formula aov median quantile sd
#' @return a summary matrix
#' @export
#'
#' @examples
#' table_one(data = mtcars,group='vs',
#'           mean_sd = 'wt',
#'           count_percent  = c('gear','am')
#' )
#'
#'table_one(data = mtcars,
#'group='vs',
#'
#'mean_sd = 'wt',
#'t.test = 'wt',
#'
#'count_percent  = c('gear','am','cyl'),
#'chisq.test = c('am','gear'),
#'fisher.test = c('cyl'),
#'
#'round = 3
#')
table_one <- function(data,group,
                      mean_sd,
                      median_q4,
                      median_range,
                      count_percent,
                      mean,
                      median,
                      max,
                      min,
                      sd,
                      q25,
                      q75,
                      count,
                      percent,
                      round=2,
                      count.percent.direction='v',
                      t.test,anova,
                      wilcox.test,kruskal.test,
                      chisq.test,fisher.test,
                      weighted,
                      statistics=FALSE){
    #missing value delete
    data=delet_na_df.mtr(data)
    #if weighted
    if (!missing(weighted)){
        data=flat_strech(data,weighted)
    }
    res=NULL
    get_x <- function(x,group){
        if (!missing(group)){
            x_here=data[,c(x,group)]
        }else{
            x_here=data[,x]
            if (length(x)==1){
                x_here=data.frame(x_here)
                colnames(x_here)=x
            }
        }
        x_here
    }
    if (!missing(mean_sd)){
        x=get_x(x = mean_sd,group = group)
        res.i=mean_sd.h(x = x,group = group,wide = T,round = round)
        res=plyr::rbind.fill(res,res.i)
    }
    if (!missing(median_q4)){
        x=get_x(x = median_q4,group = group)
        res.i=median_q4(x = x,group = group,wide = T,round = round)
        res=plyr::rbind.fill(res,res.i)
    }
    if (!missing(median_range)){
        x=get_x(x =median_range,group = group)
        res.i=median_range(x = x,group = group,wide = T,round = round)
        res=plyr::rbind.fill(res,res.i)
    }
    if (!missing(count_percent)){
        x=get_x(x = count_percent,group = group)
        res.i=count_percent(x = x,group = group,wide = T,round = round)
        var.only=do::Replace0(res.i$colname,':.*')
        number.only=do::Replace0(res.i$colname,'.*:')
        res.i$colname=number.only
        for (i in 1:length(count_percent)) {
            if (i==1) res.final=NULL
            res.final=plyr::rbind.fill(res.final,
                                       plyr::rbind.fill(data.frame(colname=count_percent[i]),
                                                        res.i[var.only==count_percent[i],]))
        }
        res=plyr::rbind.fill(res,res.final)
    }
    if (!missing(mean)){
        x=get_x(x = mean,group = group)
        res.i=Mean(x = x,group = group,wide = T,round = round)
        res=plyr::rbind.fill(res,res.i)
    }
    if (!missing(median)){
        x=get_x(x = median,group = group)
        res.i=Median(x = x,group = group,wide = T,round = round)
        res=plyr::rbind.fill(res,res.i)
    }
    if (!missing(max)){
        x=get_x(x = max,group = group)
        res.i=Max(x = x,group = group,wide = T,round = round)
        res=plyr::rbind.fill(res,res.i)
    }
    if (!missing(min)){
        x=get_x(x = min,group = group)
        res.i=Min(x = x,group = group,wide = T,round = round)
        res=plyr::rbind.fill(res,res.i)
    }
    if (!missing(sd)){
        x=get_x(x = sd,group = group)
        res.i=Sd(x = x,group = group,wide = T,round = round)
        res=plyr::rbind.fill(res,res.i)
    }
    if (!missing(q25)){
        x=get_x(x = q25,group = group)
        res.i=q25(x = x,group = group,wide = T,round = round)
        res=plyr::rbind.fill(res,res.i)
    }
    if (!missing(q75)){
        x=get_x(x = q75,group = group)
        res.i=q75(x = x,group = group,wide = T,round = round)
        res=plyr::rbind.fill(res,res.i)
    }
    if (!missing(count)){
        x=get_x(x = count,group = group)
        res.i=Count(x = x,group = group,wide = T,direction = count.percent.direction)
        var.only=do::Replace0(res.i$colname,':.*')
        number.only=do::Replace0(res.i$colname,'.*:')
        res.i$colname=number.only
        for (i in 1:length(Count)) {
            if (i==1) res.final=NULL
            res.final=plyr::rbind.fill(res.final,
                                       plyr::rbind.fill(data.frame(colname=count[i]),
                                                        res.i[var.only==count[i],]))
        }
        res=plyr::rbind.fill(res,res.final)
    }
    if (!missing(percent)){
        x=get_x(x = percent,group = group)
        res.i=Percent(x = x,group = group,wide = T,round = round,direction = count.percent.direction)
        var.only=do::Replace0(res.i$colname,':.*')
        number.only=do::Replace0(res.i$colname,'.*:')
        res.i$colname=number.only
        for (i in 1:length(percent)) {
            if (i==1) res.final=NULL
            res.final=plyr::rbind.fill(res.final,
                                       plyr::rbind.fill(data.frame(colname=percent[i]),
                                                        res.i[var.only==percent[i],]))
        }
        res=plyr::rbind.fill(res,res.final)
    }
    res.colname=colnames(res)
    res=as.matrix(res)
    if (any(is.na(res))) res=data.frame(ifelse(is.na(res),"",res))
    colnames(res)=res.colname
    ###########################################
    #go to test
    test.final=NULL
    #t.test
    if (!missing(t.test)){
        for (i in 1:length(t.test)) {
            if (i==1){
                p_value=NULL
                statistic=NULL
            }
            formu=paste(t.test[i], '~', group)
            test.res=t.test(as.formula(formu),data=data)
            p_value=c(p_value,digital(test.res$p.value,round))
            levene.res=car::leveneTest(y = data[,t.test[i]],
                                       group=factor(data[,group]))
            if (unique_no_na(levene.res$`Pr(>F)`) < 0.05){
                msge=paste0('*** ',t.test[i],' levene p: ',digital(unique_no_na(levene.res$`Pr(>F)`),round))
                warning(msge)
            }
            statistic=c(statistic,
                        paste0(names(test.res$statistic),':',digital(test.res$statistic,round),';',
                               names(test.res$parameter),':',test.res$parameter,
                               ';levene_p:',digital(unique_no_na(levene.res$`Pr(>F)`),round)))
            if (i==length(t.test)){
                test.df=data.frame(cbind(var=t.test,
                                         p_value=digital(p_value,round),
                                         statistic=do::equal_length(statistic," ")))
            }
        }
        test.final=rbind(test.final,test.df)
    }
    #anova
    if (!missing(anova)){
        for (i in 1:length(anova)) {
            if (i==1){
                p_value=NULL
                statistic=NULL
            }
            formu=paste0(anova[i],'~',group)
            test.res=summary(aov(formula = as.formula(formu),data = data))[[1]]
            p_value=c(p_value,digital(unique_no_na(test.res$`Pr(>F)`),round))
            levene.res=car::leveneTest(y = data[,anova[i]],
                                       group=factor(data[,group]))
            if (unique_no_na(levene.res$`Pr(>F)`) < 0.05){
                msge=paste0('*** ',anova[i],' levene p: ',digital(unique_no_na(levene.res$`Pr(>F)`),round))
                warning(msge)
            }
            statistic=c(statistic,
                        paste0('F:',digital(unique_no_na(test.res$`F value`),round),';',
                               'df:',paste0(test.res$Df,collapse = ','),
                               ';levene_p:',digital(unique_no_na(levene.res$`Pr(>F)`),round))
            )
            if (i==length(anova)){
                test.df=data.frame(cbind(var=anova,
                                         p_value=digital(p_value,round),
                                         statistic=do::equal_length(statistic," ")))
            }
        }
        test.final=rbind(test.final,test.df)
    }
    # wilcox.test
    if (!missing(wilcox.test)){
        for (i in 1:length(wilcox.test)) {
            if (i==1){
                p_value=NULL
                statistic=NULL
            }
            formu=paste(wilcox.test[i], '~', group)
            test.res=wilcox.test(as.formula(formu),data=data)
            p_value=c(p_value,digital(test.res$p.value,round))
            levene.res=car::leveneTest(y = data[,wilcox.test[i]],
                                       group=factor(data[,group]))
            statistic=c(statistic,
                        paste0(names(test.res$statistic),':',digital(test.res$statistic,round),';',
                               test.res$alternative))
            if (i==length(wilcox.test)){
                test.df=data.frame(cbind(var=wilcox.test,
                                         p_value=digital(p_value,round),
                                         statistic=do::equal_length(statistic," ")))
            }
        }
        test.final=rbind(test.final,test.df)
    }
    #kruskal.test
    if (!missing(kruskal.test)){
        for (i in 1:length(kruskal.test)) {
            if (i==1){
                p_value=NULL
                statistic=NULL
            }
            formu=paste0(kruskal.test[i],'~',group)
            test.res=kruskal.test(as.formula(formu),data=data)
            p_value=c(p_value,digital(test.res$p.value,round))
            statistic=c(statistic,
                        paste0(names(test.res$statistic),':',digital(test.res$statistic,round),';',
                               names(test.res$parameter),':',test.res$parameter))
            if (i==length(kruskal.test)){
                test.df=data.frame(cbind(var=kruskal.test,
                                         p_value=digital(p_value,round),
                                         statistic=do::equal_length(statistic," ")))
            }
        }
        test.final=rbind(test.final,test.df)
    }
    #chisq.test
    if (!missing(chisq.test)){
        for (i in 1:length(chisq.test)) {
            if (i==1){
                p_value=NULL
                statistic=NULL
            }
            test.res=chisq.test(table(data[,chisq.test[i]],
                                      g=data[,group])
            )
            chi_exp_res=chi_expect(data[,chisq.test[i]],data[,group])
            exp_min=sum(chi_exp_res[[1]]<5)/dim(chi_exp_res[[1]])[1]/dim(chi_exp_res[[1]])[2]*100
            if (exp_min>20){
                msge=paste0('*** ',chisq.test[i],' ',
                            chi_exp_res[[2]])
                warning(msge)
            }
            p_value=c(p_value,digital(test.res$p.value,round))
            statistic=c(statistic,
                        paste0('chi-square:',digital(test.res$statistic,round),';',
                               'df:',test.res$parameter))
            if (i==length(chisq.test)){
                test.df=data.frame(cbind(var=chisq.test,
                                         p_value=digital(p_value,round),
                                         statistic=do::equal_length(statistic," ")))
            }
        }
        test.final=rbind(test.final,test.df)
    }
    #fisher.test
    if (!missing(fisher.test)){
        for (i in 1:length(fisher.test)) {
            if (i==1){
                p_value=NULL
                statistic=NULL
            }
            test.res=fisher.test(table(data[,fisher.test[i]],
                                       g=data[,group])
            )
            p_value=c(p_value,digital(test.res$p.value,round))
            statistic=c(statistic,
                        paste0('fisher:',test.res$alternative))
            if (i==length(fisher.test)){
                test.df=data.frame(cbind(var=fisher.test,
                                         p_value=digital(p_value,round),
                                         statistic=do::equal_length(statistic," ")))
            }
        }
        test.final=rbind(test.final,test.df)
    }
    #test over
    if (!is.null(test.final)){
        if (statistics){
            res=vlookup(res,1,test.final,1,c(2,3))
        }else{
            res=vlookup(res,1,test.final,1,2)
        }
    }
    res_colname=colnames(res)
    res=as.data.frame(res)
    colnames(res)=res_colname
    colnames(res)[1]='variable'
    return(res)
}





