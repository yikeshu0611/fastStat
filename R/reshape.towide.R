reshape_towide <- function(data,id,j,prefix,suffix){
  if (!is.data.frame(data)){
    data=data.frame(data)
  }
  data = data[order(data[,j]),]
  #trans-data
  #the other part except prefix
  #prefix
  if (all(!missing(prefix),missing(suffix))){
    id.left.names=colnames(data) %not% c(prefix,j)
    trans.data=data[,c(j,prefix)]
  }else if (all(missing(prefix),!missing(suffix))){
    #suffix
    id.left.names=colnames(data) %not% c(suffix,j)
    trans.data=data[,c(j,suffix)]
  }else if (all(missing(prefix),missing(suffix))){
    #var.names
    id.left.names=colnames(data) %not% j
    trans.data=data[,j]
  }else{
    stop('prefix, suffix can not be used together.')
  }
  if (length(id.left.names)==1){
    do.id = data[,id.left.names]
  }else if (length(id.left.names)>1){
    do.id = data[,id.left.names]
    for (i in 1:length(id.left.names)) {
      if (i == 1){
        do.id=data[,id.left.names[i]]
      }else{
        do.id=paste0(do.id,'idsplitid',data[,id.left.names[i]])
      }
    }
  }else if (length(id.left.names)==0){
    idsplitid.j=make.unique(as.character(trans.data[,j]),sep = 'idsplitid')
    idsplitid.j=reshape2::colsplit(idsplitid.j,'idsplitid',c('j','id'))
    do.id = ifelse(is.na(idsplitid.j[,2]),0,idsplitid.j[,2]) +1
    id.left.names='nrow'
  }
  data2=cbind(do.id,trans.data)
  wide.1=stats::reshape(data = data2,
          idvar = 'do.id',
          timevar = j,
          direction = "wide",sep = "")
  wide.2=wide.1[,-1]
  if (!missing(suffix)){
    suffix=suffix[order(nchar(suffix),decreasing = TRUE)]
    #location of suffix and trans suffix to prefix
    for (i in 1:length(suffix)) {
      suffix.char=do::left(colnames(wide.2),nchar(suffix[i]))
      suffix.loc=(1:length(suffix.char))[suffix[i] == suffix.char]
      name.suffix=do::Replace0(colnames(wide.2)[suffix.loc],suffix[i])
      colnames(wide.2)[suffix.loc]=paste0(name.suffix,suffix[i])
    }
  }
  wide.3=reshape2::colsplit(wide.1[,1],'idsplitid',id.left.names)
  if (is.data.frame(wide.3)){
    wide.3=data.frame(wide.3)
    colnames(wide.3)=id.left.names
  }
  wide=cbind(wide.3,wide.2)
  return(wide)
}
