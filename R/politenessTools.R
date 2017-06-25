#options(java.parameters = "-Xmx8g")

require(qdap)
require(tm)
require(quanteda)

#require(rJava)
#require(coreNLP)
#initCoreNLP("/stanford-corenlp/", mem="8g")
################################################################

hedge.list<-readLines("modeldata/hedges.txt")
positive.list<-readLines("modeldata/positive-words.txt")
negative.list<-readLines("modeldata/negative-words.txt")
liwc.lists<-quanteda::dictionary(file="modeldata/LQ.dic")

polite.dicts<-liwc.lists[c("ipron@ImpersonalPronouns","swear@Swear","negate@Negations")]
polite.dicts[["pause@FilledPause"]]<-c("er","sigh","hm*","uh*","um*")
polite.dicts[["intitle@InformalTitle"]]<-c("dude*", "bro*", "boss", "bud", "buddy", "champ", "man", "guy*", "guy", "brotha", "sista", "son", "sonny", "chief")
polite.dicts[["title@FormalTitle"]]<-c("sir", "ma'am", "maam", "mister", "mr*", "ms*", "madam", "miss", "gentleman", "lady")

################################################################
row.to.char<-function(deps){
  return(paste0(deps$type,"(",
                deps$governor,"-",
                deps$governorIdx,", ",
                deps$dependent,"-",
                deps$depIndex,")"))
}

core.parser<-function(text){
  sentences<-as.list(qdap::sent_detect(text))
  parses<-list()
  for (s in 1:length(sentences)){
    a.s<-coreNLP::annotateString(sentences[[s]])
    dep.table<-coreNLP::getDependency(a.s, type="collapsed")
    dep.table<-dep.table[dep.table$sentence==1,]
    dep.char<-c()
    for (x in 1:nrow(dep.table)){
      dep.char<-c(dep.char,row.to.char(dep.table[dep.table$depIndex==x,]))
    }
    parses[[s]]<-dep.char
  }
  all.parses=do.call(c, parses)
  return(list(sentences=sentences,
              parses=parses,
              all.parses=all.parses))
}

textcounter<-function (counted, texts, words=F, fixed = T) {
  counts <- rep(0, length(texts))
  if(words){
    for (x in counted) counts <- counts + (texts==x)
  }else {
    for (x in counted) {
      counts <- counts + sapply(gregexpr(x, texts, fixed = fixed),
                                function(z) ifelse(z[1] == (-1), 0, length(z)))
    }
  }
  return(counts)
}

cleantext<-function (ex, language = "english", stop.words = TRUE) {
  ex <- tolower(ex)
  if (language == "english") {
    ex <- ctxpand(ex)
  }
  ex <- gsub("[[:punct:]]", " ", ex)
  ex <- gsub("[[:cntrl:]]", " ", ex)
  if (length(stop.words) > 1) {
    ex <- tm::removeWords(ex, stop.words)
  }
  else if (stop.words) {
    ex <- tm::removeWords(ex, tm::stopwords(language))
  }
  ex <- tm::removeNumbers(ex)
  ex <- tm::stripWhitespace(ex)
  return(as.character(ex))
}

ctxpand<-function(text){
  text<-sapply(text, function(x) gsub("let's", "let us", x, fixed=T))
  text<-sapply(text, function(x) gsub("i'm", "i am", x, fixed=T))
  text<-sapply(text, function(x) gsub("won't", "will not", x, fixed=T))
  text<-sapply(text, function(x) gsub("can't", "cannot", x, fixed=T))
  text<-sapply(text, function(x) gsub("shan't", "shall not", x, fixed=T))
  text<-sapply(text, function(x) gsub("'d", " would", x, fixed=T))
  text<-sapply(text, function(x) gsub("'ve", " have", x, fixed=T))
  text<-sapply(text, function(x) gsub("'s", " is", x, fixed=T))
  text<-sapply(text, function(x) gsub("'ll", " will", x, fixed=T))
  text<-sapply(text, function(x) gsub("'re", " are", x, fixed=T))
  text<-sapply(text, function(x) gsub("n't", " not", x, fixed=T))
  text<-sapply(text, function(x) gsub("u.s.a.", "usa", x, fixed=T))
  text<-sapply(text, function(x) gsub("u.s.", "usa", x, fixed=T))
  text<-sapply(text, function(x) gsub("e.g.", "eg", x, fixed=T))
  text<-sapply(text, function(x) gsub("i.e.", "ie", x, fixed=T))
  return(text)}


LIWCwrap<-function (text, dict = liwc.lists, binary = F, ...) {
  CTB <- as.matrix(array(0, c(length(text), length(dict))))
  WC <- qdap::word_count(text)
  wc1 <- (!is.na(WC))
  CTD <- as.matrix(quanteda::dfm(text[wc1], dictionary = dict,
                                 verbose = F, ...))[, 1:length(dict)]
  if (is.null(nrow(CTD)))
    CTD <- CTD/WC[wc1]
  if (!is.null(nrow(CTD)))
    CTD <- apply(CTD, 2, function(x) x/WC[wc1])
  CTB[wc1, ] <- CTD
  colnames(CTB) <- substr(names(dict), 0, unlist(gregexpr("@", names(dict))) - 1)
  if (binary)
    CTB <- 1 * (CTB == 0)
  return(CTB)
}
