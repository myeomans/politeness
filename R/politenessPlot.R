# require(ggplot2)
# source("politenessTools.R")
# source("politeness.R")
#
# messages<-read.csv("NiceNegotiation_Study1Final_6417_CLEAN.csv",stringsAsFactors = F)
#
# messages<-messages[(messages$Droppedout==0)&(messages$PassedAC==1)&(messages$FollowedInstructions==1),]
# text.vector<-messages$Message
# split<-(messages$ToughWarm..0.Tough..1.Warm==1)
# split.name<-"Message Style"
# split.levels<-c("Tough","Warm")
#
#
# # load("pairs.RData")
# # pair.set<-(pair.data$TriPart!="Moderate")&(pair.data$parent_TriPart!="Moderate")
# # text.vector<-pair.data[pair.set,"body"]
# # split<-(pair.data[pair.set,"TriPart_gap"]==0)
# # split.name<-"Reply Type"
# # split.levels<-c("Cross-Party","Intra-Party")
#
# ######################################################
# polite.wrap<-function(x){
#   as.numeric(politeness(iconv(x,"latin1", "ASCII", sub="")))
# }
#
# xx<-list()
# for (z in 1:length(text.vector)){
#   xx[[z]]<-as.numeric(politeness(iconv(text.vector[z],"latin1", "ASCII", sub="")))
#   print(z)
# }
#
#
# rownames(xx)<-names(politeness(text.vector[1]))
# #xx<-apply(sapply(text.vector,politeness),1:2, as.numeric)
# map.type<-"Fraction of Documents Using Strategy"
# feature.map<-function(x) return(rowMeans(x>0))
# #map.type<-"Average Strategy Use per Document"
# #feature.map<-function(x) return(rowMeans(x))
# ######################################################
# split.data<-data.frame(feature=rep(rownames(xx),2),
#                        count=c(feature.map(xx[,(!split)]),feature.map(xx[,(split)])),
#                        cond=c(rep(split.levels[1],nrow(xx)),rep(split.levels[2],nrow(xx))))
#
# split.data$se<-NA
# if(grepl("Fraction",map.type)){
#   split.data$se<-sqrt(((split.data$count)*(1-split.data$count))/nrow(split.data))
# }
#
# ######################################################
# ggplot(data=split.data,
#        aes(x=feature,y=count,fill=factor(cond)),
#        width=2) +
#   geom_bar(position=position_dodge(width = 0.8),
#            stat="identity") +
#   geom_errorbar(aes(ymin=count-se, ymax=count+se), width=0.3,
#                 position=position_dodge(width = 0.8)) +
#   coord_flip() +
#   scale_y_continuous(name=map.type) +
#   scale_fill_manual(values=c("navy","firebrick"), name=split.name) +
#   theme_bw() +
#   ggtitle("Politeness Strategies") +
#   theme(plot.title = element_text(hjust = 0.5))
