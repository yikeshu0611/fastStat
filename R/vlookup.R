vlookup <- function(lookup_data,
                    lookup_ncol,
                    return_data,
                    search_col,
                    return_col){
    if (!is.numeric(lookup_ncol)) stop('lookup_ncol must be one number')
    if (!is.numeric(search_col)) stop('search_col must be one number')
    if (!is.numeric(return_col)) stop('return_col must be number')
    if (length(lookup_ncol) != 1) stop('the length of lookup_ncol must be 1')
    if (length(search_col) != 1) stop('the length of search_col must be 1')
    if (is.matrix(lookup_data)){
        col_names=colnames(lookup_data)
        lookup_data=as.data.frame(lookup_data)
        colnames(lookup_data)=col_names
    }
    for (j in 1:length(return_col)) {
        for (i in 1:nrow(lookup_data)) {
            if (i==1) lookupdata=NULL
            x.i=lookup_data[i,lookup_ncol]
            rownames(return_data)=return_data[,search_col]
            lookupdata=c(lookupdata,as.character(return_data[as.character(x.i),
                                                             return_col[j]]))
            if (i==nrow(lookup_data)){
                lookupdata=ifelse(is.na(lookupdata),"",lookupdata)
                lookup_data$vlookup=lookupdata
                colnames(lookup_data)[ncol(lookup_data)]=
                    colnames(return_data)[return_col[j]]
            }
        }
    }
    return(lookup_data)
}

