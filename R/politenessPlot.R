#' Politeness plot
#'
#' @description Plots
#' @param df_polite a data.frame with politeness features, as outputed by \code{\link{politeness}}.
#' @param split logical vector of how to split \code{df_polite}.
#' @param split_levels character vector of length 2. First value correponds to cases where \code{split} is FALSE and second value when TRUE.
#' @param split_name
#' @param split_cols character vector of length 2.
#' @param top_title character of title of plot
#' @param drop.bank numeric of threshold
#' @details Length of \code{split} must be the same as number of rows of \code{df_polite}.
#' @return a ggplot plot Showing
#' @examples
#'

politenessPlot<-function(df_polite,
                         split=NA,
                         split_levels=NA,
                         split_name=NA,
                         split_cols=c("navy","firebrick"),
                         top_title,
                         drop_blank=0.05){

  split.data<-data.frame(feature=rep(colnames(df_polite),2),
                         count=c(colMeans(df_polite[!split,],na.rm=T),
                                 colMeans(df_polite[split,],na.rm=T)),
                         cond=c(rep(split_levels[1],ncol(df_polite)),
                                rep(split_levels[2],ncol(df_polite))),
                         se=rep(NA,ncol(df_polite)*2))

  if(all(sort(unique(unlist(df_polite)))==0:1)){
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
    scale_fill_manual(values=split_cols, name=split_name) +
    scale_y_continuous(name=map.type, breaks = seq(0,1,.25), labels=paste0(seq(0,100,25),"%")) +
    theme_bw(base_size=14) +
    ggtitle(top_title) +
    theme(plot.title = element_text(hjust = 0.5),
          text=element_text(family="Times"))
}
