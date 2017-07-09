politenessPlot<-function(polite.data,
                         split=NA,
                         split.levels=NA,
                         split.name=NA,
                         top.title,
                         drop.blank=TRUE){
  if(drop.blank) polite.data<-polite.data[,colMeans(polite.data)!=0]
  split.data<-data.frame(feature=rep(colnames(polite.data),2),
                         count=c(colMeans(polite.data[!split,],na.rm=T),
                                 colMeans(polite.data[split,],na.rm=T)),
                         cond=c(rep(split.levels[1],ncol(polite.data)),
                                rep(split.levels[2],ncol(polite.data))),
                         se=rep(NA,ncol(polite.data)*2))
  if((sort(unique(unlist(polite.data)))==0:1)){
    split.data$se<-sqrt(((split.data$count)*(1-split.data$count))/nrow(split.data))
    map.type<-"Fraction of Documents Using Strategy"
  } else {
    map.type<-"Average Strategy Use per Document"
  }
  ######################################################
  wide<-reshape(split.data, idvar = "feature", timevar = "cond", direction = "wide")
  wide$count.total<-rowMeans(wide[,grepl("count",names(wide))])
  # Re-order features base on mutual information!!
  f.order<-(wide$feature)[order(wide$count.total,decreasing=T)]
  split.data$cond<-factor(cond, ordered=T,levels=f.order)
  ######################################################
  ggplot(data=split.data,
         aes(x=feature,y=count,fill=cond),
         width=2) +
    geom_bar(position=position_dodge(width = 0.8),
             stat="identity") +
    geom_errorbar(aes(ymin=count-se, ymax=count+se), width=0.3,
                  position=position_dodge(width = 0.8)) +
    coord_flip() +
    scale_y_continuous(name=map.type) +
    scale_fill_manual(values=c("navy","firebrick"), name=split.name) +
    theme_bw() +
    ggtitle(top.title) +
    theme(plot.title = element_text(hjust = 0.5))
}
