utils::globalVariables(c("name","value","facet"))
#' Plot multidimensional cognitive test scores
#' 
#' Plot index scores from five dimensional cognitive testing. 
#' Includes option to facet.
#'
#' @param ds complete data frame
#' @param id colname of id column. Base for colouring
#' @param sub_plot main outcome scores variable to plot
#' @param scores variables to subset for plotting. Has to follow standard 
#' naming (is to be changed)
#' @param dom_names domain names for axis naming
#' @param facet.by variable to base facet_grid on
#'
#' @return ggplot element
#' 
#' @import ggplot2 
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_longer all_of ends_with
#' 
#' @export
#'
#' @examples
#' index_plot(stRoke::score[score$event=="A",])
index_plot <- function(ds,id="id",sub_plot="_is",
                       scores=c("_is","_lo","_up","_per"),
                       dom_names=c("immediate","visuospatial","verbal",
                                   "attention","delayed","total"),
                       facet.by=NULL){

  if (length(facet.by)>1){stop("facet.by can be NULL or of length 1 only.")}
  
  df_plot<-ds|>
    dplyr::select(c(id,
                    tidyr::all_of(facet.by),
             tidyr::ends_with(scores)))|>
    tidyr::pivot_longer(cols=-c(id,tidyr::all_of(facet.by)))|>
    subset(grepl(sub_plot,name))|>
    dplyr::mutate(value=as.numeric(value),
           name=factor(name,labels = dom_names))
  
  if (!is.null(facet.by)){
    colnames(df_plot)<-c("id","facet","name","value")
  } else {
    colnames(df_plot)<-c("id","name","value")
  }
  
  if (sub_plot=="_is"){
    index_plot<-df_plot|>
      ggplot2::ggplot(mapping = ggplot2::aes(x=name, y=value, color=factor(id), 
                                             group=factor(id))) + 
      ggplot2::geom_point() +
      ggplot2::geom_path() +
      ggplot2::expand_limits(y=c(40,160)) +
      ggplot2::scale_y_continuous(breaks=seq(40,160,by=10)) +
      ggplot2::ylab("Index Score") +
      ggplot2::xlab("Domain")+
      ggplot2::labs(colour = "ID")
  }
  
  if (sub_plot=="_per"){
    index_plot<-df_plot|>
      ggplot2::ggplot(mapping = ggplot2::aes(x=name, y=value, 
                                             fill=factor(id)))+
      ggplot2::geom_col(position = "dodge") +
      ggplot2::expand_limits(y=c(0,100)) +
      ggplot2::scale_y_continuous(breaks=seq(0,100,by=10)) +
      ggplot2::xlab("Cognitive domains") +
      ggplot2::ylab("Percentile") + 
      ggplot2::labs(fill = "ID")
  }
  
  if (!is.null(facet.by)){
    index_plot + facet_grid(cols=vars(facet)) +
      ggplot2::theme(axis.text.x = element_text(angle = 90, 
                                                vjust = 0.5, hjust=1)) 
    
  } else {
    index_plot
  }
  
}

