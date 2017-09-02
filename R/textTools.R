#' Text Counter
#'
#' @description Counts total prevalence of a set of items in each of a set of texts.
#' @param counted character vector of items to search for in the texts.
#' @param texts character vector of to-be-searched text.
#' @param words logical. Default FALSE. Does \code{counted} contain words, or sequences of chracters?
#' @param fixed logical. Default TRUE. Use literal characters instead of regular expressions?
#' @return numeric vector as long as \code{texts} indicating total frequencies of \code{counted} items.
#' @keywords internal
#'
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

#' Clean Text
#' @description Basic text cleaning
#' @param text character text to be cleaned
#' @param language string. Default "english".
#' @param stop.words logical. Default TRUE
#' @return a character vector
#' @keywords internal
cleantext<-function (text, language = "english", stop.words = TRUE) {
  text <- tolower(text)
  if (language == "english") {
    text <- ctxpand(text)
  }
  text <- gsub("[[:punct:]]", " ", text)
  text <- gsub("[[:cntrl:]]", " ", text)
  if (length(stop.words) > 1) {
    text <- tm::removeWords(text, stop.words)
  }
  else if (stop.words) {
    text <- tm::removeWords(text, tm::stopwords(language))
  }
  text <- tm::removeNumbers(text)
  text <- tm::stripWhitespace(text)
  return(as.character(text))
}

#' Contraction Expander
#' @description Expands Contractions
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

