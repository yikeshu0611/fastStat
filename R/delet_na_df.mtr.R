#' @importFrom stats na.omit
delet_na_df.mtr<-function(data){
    if (any(is.na(data))){
        nrow.origin=nrow(data)
        data=na.omit(data)
        nrow.new=nrow(data)
        diff=nrow.origin-nrow.new
        message()
        msg=paste0(diff,' rows were deleted because of missing value.')
        message(msg)
        message()
        message()
        data
    }else{
        data
    }
}
