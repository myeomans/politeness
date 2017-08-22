
#' Text Counter
#' @description
#' @param counted
#' @param texts
#' @param words logical. Default FALSE.
#' @param fixed logical. Default TRUE.
#' @return a numeric vector
#' @keywords internal
textcounter<-function (counted, texts, words=F, fixed = T) {

  if(words){
    counts<-unlist(mclapply(texts,function(x) sum(unlist(x)%in%counted)))
  }else {
    for (x in counted) {
      counts <- rep(0, length(texts))
      counts <- counts + sapply(gregexpr(x, texts, fixed = fixed),
                                function(z) ifelse(z[1] == (-1), 0, length(z)))
    }
  }
  return(counts)
}

#' Clean text
#' @description
#' @param ex
#' @param language string. Default "english".
#' @param stop.words logical. Default TRUE
#' @return a character vector
#' @keywords internal
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

#'
#' @description
#' @param text a character vector of texts.
#' @return a character vector
#' @keywords internal
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
  return(text)
}

