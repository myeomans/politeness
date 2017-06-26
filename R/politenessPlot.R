politenessPlot<-function(text.data,
                         split=NA,
                         top.title,
                         split.name=NA,
                         split.levels=NA,
                         map.type=NA,
                         drop.blank=TRUE){
  polite.data<-data.frame(politeness::politeness(text.data,binary=T))
  if(drop.blank) polite.data<-polite.data[,colMeans(polite.data)!=0]
  split.data<-data.frame(feature=rep(colnames(polite.data),2),
                         count=c(colMeans(polite.data[!split,],na.rm=T),
                                 colMeans(polite.data[split,],na.rm=T)),
                         cond=c(rep(split.levels[1],ncol(polite.data)),
                                rep(split.levels[2],ncol(polite.data))))
  split.data$se<-NA
  if(grepl("Fraction",map.type)){
    split.data$se<-sqrt(((split.data$count)*(1-split.data$count))/nrow(split.data))
  }
  ######################################################
  ggplot(data=split.data,
         aes(x=feature,y=count,fill=factor(cond)),
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
