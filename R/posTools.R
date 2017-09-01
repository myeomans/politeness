################################################################
# Workflow for SpaCy
################################################################
# require(spacyr)
# spacyr::spacy_initialize(python_executable = "/anaconda/bin/python")
################################################################

#' Spacy Parser
#' @description Return POS tags from natural language.
#' @param txt a character vector of texts.
#' @return list of compiled POS-tagged items.
#' @keywords internal
spacyParser<-function(txt){
  parsedtxt <- spacy_parse(txt, dependency=T,lemma=F,pos=T,tag=T,entity=T)
  parsedtxt$pos.nums<-paste0("(",parsedtxt$token_id,"-",parsedtxt$token,"-",parsedtxt$tag,")")
  parsedtxt$head_token<-lapply(1:nrow(parsedtxt),headTokenGrab, data=parsedtxt)
  parsedtxt[parsedtxt$dep_rel=="ROOT",c("dep_rel","head_token","head_token_id")]<-c("root","ROOT",0)
  parsedtxt$pos.nums<-paste0("(",parsedtxt$token_id,"-",parsedtxt$token,"-",parsedtxt$tag,")")
  parsedtxt$parses<-paste0(parsedtxt$dep_rel, "(",parsedtxt$head_token,"-",parsedtxt$head_token_id,", ",parsedtxt$token,"-",parsedtxt$token_id,")")
  parsedtxt$w.nums<-paste0(parsedtxt$token,"-",parsedtxt$token_id)
  all.parses<-lapply(unique(parsedtxt$doc_id),function(x) unlist(parsedtxt[parsedtxt$doc_id==x,"parses"]))
  all.pos.nums<-lapply(unique(parsedtxt$doc_id),function(x) unlist(parsedtxt[parsedtxt$doc_id==x,"pos.nums"]))
  nonums=mclapply(all.parses,gsub, pattern="-[0-9][0-9][0-9]",replacement="")
  nonums=mclapply(nonums,gsub, pattern="-[0-9][0-9]",replacement="")
  nonums=mclapply(nonums,gsub, pattern="-[0-9]",replacement="")
  w.nums<-lapply(unique(parsedtxt$doc_id),function(x) unlist(parsedtxt[parsedtxt$doc_id==x,"w.nums"]))
  return(list(parses=all.parses,
              pos.nums=all.pos.nums,
              nonums=nonums,
              w.nums=w.nums))
}

#' Head Token Grab
#' @description Compile dependency relation for each row of a SpaCy parse table
#' @param x row in SpaCy parse table
#' @param data data.frame SpaCy parse table
#' @return word tagged as referent from dependencey
#' @keywords internal
#'
headTokenGrab<-function(x, data){
  return(data[(data$doc_id==data[x,]$doc_id)&(data$sentence_id==data[x,]$sentence_id)&(data$token_id==data[x,"head_token_id"]),"token"])
}
################################################################
# Workflow for coreNLP
################################################################
# options(java.parameters = "-Xmx8g")
# require(rJava)
# require(coreNLP)
# initCoreNLP("/stanford-corenlp/", mem="8g")
################################################################

#' Core Parser
#' @description (deprecated) Part-Of-Speech tagging using Stanford CoreNLP.
#' @param text a character vector of texts.
#' @return list of
#' @keywords internal
#'
coreParser<-function(text){
  sentences<-as.list(qdap::sent_detect(text))
  parses<-list()
  pos.nums<-list()
  for (s in 1:length(sentences)){
    a.s<-coreNLP::annotateString(sentences[[s]])
    pos.table<-coreNLP::getToken(a.s)[,c("id","token","POS")]
    dep.table<-coreNLP::getDependency(a.s, type="collapsed")
    dep.table<-dep.table[dep.table$sentence==1,]
    dep.char<-c()
    for (x in 1:nrow(dep.table)){
      dep.char<-c(dep.char,rowToChar(dep.table[dep.table$depIndex==x,]))
    }
    parses[[s]]<-dep.char
    pos.nums[[s]]<-paste0("(",apply(pos.table,1,paste, collapse="-"),")")
  }
  all.parses=do.call(c, parses)
  all.pos.nums=do.call(c, pos.nums)
  nonums=mclapply(all.parses,gsub, pattern="-[0-9][0-9][0-9]",replacement="")
  nonums=mclapply(nonums,gsub, pattern="-[0-9][0-9]",replacement="")
  nonums=mclapply(nonums,gsub, pattern="-[0-9]",replacement="")
  w.nums<-substr(all.parses, sapply(all.parses, function(x) gregexpr(",",x,fixed=T)[[1]][1])+2, nchar(all.parses)-1)
  return(list(parses=all.parses,
              pos.nums=all.pos.nums,
              nonums=nonums,
              w.nums=w.nums))
}

#' Row To Char
#' @description constructs POS-tagged words from coreNLP parse table.
#' @param deps row from coreNLP dependency table
#' @param data a data.frame
#' @return a character
#' @keywords internal
#'

rowToChar<-function(deps){
  return(paste0(deps$type,"(",
                deps$governor,"-",
                deps$governorIdx,", ",
                deps$dependent,"-",
                deps$depIndex,")"))
}
