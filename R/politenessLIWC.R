require(ggplot2)


messages<-read.csv("martha_data.csv",stringsAsFactors = F)

######################################################
feature.map<-function(x) return(rowMeans(x>0))

yy<-yeomansroot::word_list(messages$message, "communal", "average")

#xx<-apply(sapply(messages$message,politeness),1:2, as.numeric)

xx<-t(yeomansroot::LIWCwrap(messages$message))

######################################################
split.data<-data.frame(feature=rep(rownames(xx),2),
                       count=c(feature.map(xx[,(!split)]),feature.map(xx[,(split)])),
                       cond=c(rep("control",nrow(xx)),rep("warm",nrow(xx))))
split.data$feature<-relevel

# negative emotion words
# positive emotion words
# words suggesting cognitive mechanisms
# self-references
# negations
# verb tense
# unique words

reorder(split.data$feature, split.data$count)
split.data$se<-sqrt(((split.data$count)*(1-split.data$count))/nrow(split.data))

ggplot(data=split.data, 
                aes(x=feature,y=count,fill=factor(cond)),
                width=2) +
  geom_bar(position=position_dodge(width = 0.8),
           stat="identity") + 
  geom_errorbar(aes(ymin=count-se, ymax=count+se), width=0.3,
                position=position_dodge(width = 0.8)) + 
  coord_flip() +
  scale_y_continuous(name="Fraction of Messages Using Strategy") +
  scale_fill_manual(values=c("navy","firebrick"), name="Condition") +
  theme_bw() +
  ggtitle("Politeness Strategies") + 
  theme(plot.title = element_text(hjust = 0.5))