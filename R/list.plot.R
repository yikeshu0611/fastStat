#' Scatter Plot for Single Value
#'
#' @param x vector, dataframe or matrix
#' @param label labels for points. If missing, defaulted, no labels will be added. If label equals x, id will be added. If label equals y, y value will be added.
#' @importFrom graphics plot text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 ggplot aes_string geom_point ggtitle theme_classic
#' @importFrom utils menu
#' @return sactter
#' @export
#'
#' @examples
#' \donttest{
#'     list.plot(mtcars)
#' }
#'
list.plot <- function(x,label='x'){
    if (is.vector(x)) x=data.frame(x)
    if (label=='x') labels=1:nrow(x)
    for (i in 1:ncol(x)) {
        if (i==1) notes=NULL
        if (label=='y') labels=x[,i]
        if (label=='xy') labels=paste0(1:nrow(x),'; ',x[,i])
        if (label=='yx') labels=paste0(x[,i],'; ',1:nrow(x))
        gg=ggplot(x,
                  aes_string(1:nrow(x),
                             colnames(x)[i]))+
            geom_point(color="red")+
            geom_text_repel(label=labels)+
            theme_classic() +
            ggtitle(label = paste0(colnames(x)[i],'(',i,'/',ncol(x),')'))
        plot(gg)
        if (i < ncol(x)){
            res=menu(c('next','quit','notes'))
            if (res==2) {
                if (!is.null(notes)){
                    return(notes)
                }else{
                    break(i)
                }
            }else if (res==3) {
                note <- readline(paste0("Notes for ",colnames(x)[i],' :'))
                note.i=data.frame(variable=colnames(x)[i],note=note)
                notes=rbind(notes,note.i)
                gg=ggplot(x,
                          aes_string(1:nrow(x),
                                     colnames(x)[i]))+
                    geom_point(color="red")+
                    geom_text_repel(label=labels)+
                    theme_classic() +
                    ggtitle(label = paste0(colnames(x)[i],'(',i,'/',ncol(x),')'),subtitle=note)
                plot(gg)
            }
        }else if(i == ncol(x)){
            res=menu(c('quit','notes'))
            if (res==1) {
                if (!is.null(notes)){
                    return(notes)
                }else{
                    break(i)
                }
            }else if (res==2) {
                note <- readline(paste0("Notes for ",colnames(x)[i],' :'))
                note.i=data.frame(variable=colnames(x)[i],note=note)
                notes=rbind(notes,note.i)
                gg=ggplot(x,
                          aes_string(1:nrow(x),
                                     colnames(x)[i]))+
                    geom_point(color="red")+
                    geom_text_repel(label=labels)+
                    theme_classic() +
                    ggtitle(label = paste0(colnames(x)[i],'(',i,'/',ncol(x),')'),subtitle=note)
                plot(gg)
                if (!is.null(notes)){
                    return(notes)
                }else{
                    break(i)
                }
            }
        }
    }
}
