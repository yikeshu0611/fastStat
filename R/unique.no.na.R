unique_no_na <-function(x){
    x.1=unique(x)
    x.1[!is.na(x.1)]
}
