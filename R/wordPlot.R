# messages<-read.csv("martha_data.csv",stringsAsFactors = F)
# require(devtools)
# devtools::install_github("myeomans/DTMtools")
#
# require(ggplot2)
# require(dplyr)
# require(ggrepel)
# textRating <- scale(messages$warmth)
#
# textMatrix<-DTMtools::DTM(messages$message, ngrams=1:3,sparse=0.95,
#                           stop.words = F, punct=T, wstem="all")
# #textMatrix<-quanteda::dfm(messages$message)
# # Function to compute (variance-weighted) log-odds
# # x is a V-length count vector for the numerator group
# # y is a V-length count vector for the denominator group
# #  prior is the pseudo count
# slogodds <- function(x,y,prior) {
#   pcons <- (x+ prior)/sum(x + prior)
#   plibs <- (y+ prior)/sum(y + prior)
#
#   ocons <- pcons/(1-pcons)
#   olibs <- plibs/(1-plibs)
#
#   lor <- log(ocons) - log(olibs)
#   sd.lor <-  sqrt(1/(x + prior) + 1/(y + prior))
#
#   list(lor=lor, slor=(lor/sd.lor))
# }
#
# # Function to compute mutual information
# mi <- function(i, j) {
#   counts <- table(i,j)
#   N <- sum(counts)
#   tab <- counts/N
#   null <- rowSums(tab) %o% colSums(tab)
#   logterm <- ifelse(tab > 0, log2(tab/null), 0)
#   mi <- sum(tab*logterm)
#   return(mi)
# }
#
# # Compute variance-weighted log-odds and mutual information
# cmp.slor.mis <- function(textMatrix, rating) {
#   prior <- 1
#   cons <- Matrix::colSums(textMatrix[c(rating) >= 0,])  # Conservative posts
#   libs <- Matrix::colSums(textMatrix[c(rating) < 0,])  # Liberal posts
#   slor <- slogodds(cons, libs, prior)$slor  # Use the variance-weighted log-odds
#   mis <- apply(textMatrix, 2, function(x) mi(x>0, rating))
#   results <- data.frame(slor =  slor, mis = mis) %>% mutate(col = slor > 0)
#   return(results)
# }
# text.results <- cmp.slor.mis(textMatrix, textRating)
# # Display mutual information plot
# grayline<- quantile(text.results$mis,.7)
#
# ggplot(data=text.results) +
#   #geom_point(aes(x=slor, y=mis, color=slor), size=0) +
#   geom_text(aes(x=slor, y=mis, label=colnames(textMatrix)),
#   # geom_text_repel(aes(x=slor, y=mis, label=colnames(textMatrix)),
#   #                 segment.size = 0.01,
#   #                 label.padding=0.1,
#             alpha=ifelse(text.results$mis > grayline, 1, text.results$mis * (150)*(0.005/grayline)),
#             color=ifelse(text.results$col, "firebrick", "dodgerblue"),
#             size=4,
#             nudge_y=0.00) +
#   guides(fill=FALSE) +
#   # scale_color_gradient2(low="dodgerblue", mid="gray97", high="firebrick", midpoint=0,
#   #                       breaks=c(min(text.results$slor)+0.5, max(text.results$slor)-0.5),
#   #                       labels=c("More liberal","More conservative")) +
#   theme_bw() +
#   labs(title="Linguistic Markers of Warmth",
#        x="Variance-weighted log-odds ratio",
#        y="Mutual Information",
#        color="Warmth") +
#   theme(plot.title = element_text(hjust = 0.5, size=20, family="Times",face="bold"),
#         axis.title = element_text(family="Times",face="bold", size=20), legend.position="none")
