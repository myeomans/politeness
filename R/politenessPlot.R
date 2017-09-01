#' Politeness plot
#'
#' @description Plots the prevalence of politeness features in documents, divided by a binary covariate.
#' @param df_polite a data.frame with politeness features calculated from a document set, as outputed by \code{\link{politeness}}.
#' @param split a vector with exactly two unique values. must have a length equal to the number of documents included in \code{df_polite}.
#' @param split_levels character vector of length 2 default NULL. Labels for covariate levels for legend. If NULL, this will be inferred from \code{split}.
#' @param split_name character default NULL. Name of the covariate for legend.
#' @param split_cols character vector of length 2. Name of colors to use.
#' @param top_title character default "". Title of plot.
#' @param drop_blank numeric of threshold,
#' @details Length of \code{split} must be the same as number of rows of \code{df_polite}.
#' @return a ggplot of the magnitude of politeness features split by \code{split}.
#' @examples
#'

politenessPlot<-function(df_polite,
                         split=NULL,
                         split_levels=NULL,
                         split_name=NULL,
                         split_cols=c("navy","firebrick"),
                         top_title = "",
                         drop_blank=0.05){

  # confirm that split only has two values
  if( length(unique(split)) !=2){
    stop("split must have exactly 2 unique values")
  }
  if( length(split) != nrow(df_polite)){
    stop("split must be same length as document set")
  }

  num_features <- ncol(df_polite)
  l_polite_split <- split(df_polite, split)

  if(is.null(split_levels)){
    split_levels <- names(l_polite_split)
  }

  split.data<-data.frame(feature=rep(colnames(df_polite),2),
                         count=c(colMeans(l_polite_split[[1]],na.rm=T),
                                 colMeans(l_polite_split[[2]],na.rm=T)),
                         cond=c(rep(split_levels[1],num_features),
                                rep(split_levels[2],num_features)),
                         se=rep(NA_real_,num_features*2))

  if( setequal(unique(unlist(df_polite)),0:1) ){
    map.type<-"Percentage of Documents Using Strategy"
    split.data$se<-sqrt(((split.data$count)*(1-split.data$count))/nrow(df_polite))
    selected<-colnames(df_polite)[colMeans(df_polite)>=drop_blank]
  } else {
    map.type<-"Average Strategy Use per Document"
    split.data$se<-sqrt((split.data$count)/nrow(df_polite))
    selected<-colnames(df_polite)[colMeans(df_polite)>=drop_blank]
  }
  split.data<-split.data[split.data$feature%in%selected,]
  ######################################################
  wide<-reshape(split.data, idvar = "feature", timevar = "cond", direction = "wide")
  wide$count.total<-rowMeans(wide[,grepl("count",names(wide))])
  wide$slogodds<-slogodds(wide[,paste0("count.",split_levels[1])],
                          wide[,paste0("count.",split_levels[2])])$slor
  f.order<-unique(wide$feature)[order(wide$slogodds)]
  split.data$feature<-factor(split.data$feature, ordered=T,levels=f.order)
  split.data$cond<-factor(split.data$cond,ordered=T,levels=rev(split_levels))
  ######################################################
  ggplot(data=split.data,
         aes(x=feature,y=count,fill=cond),width=2) +
    geom_bar(position=position_dodge(width = 0.8),
             stat="identity") +
    geom_errorbar(aes(ymin=count-se, ymax=count+se), width=0.3,
                  position=position_dodge(width = 0.8)) +
    coord_flip() +
    scale_x_discrete(name="") +
    scale_fill_manual(breaks = split_levels,values=split_cols, name=split_name) +
    scale_y_continuous(name=map.type, breaks = seq(0,1,.25), labels=paste0(seq(0,100,25),"%")) +
    theme_bw(base_size=14) +
    ggtitle(top_title) +
    theme(plot.title = element_text(hjust = 0.5),
          text=element_text(family="Times"))
}
