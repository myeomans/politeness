# require(spacyr)
# spacyr::spacy_initialize(python_executable = "/anaconda/bin/python")

################################################################
spacy.parser<-function(txt){
  parsedtxt <- spacy_parse(txt, dependency=T,lemma=F,pos=T,tag=T,entity=T)
  parsedtxt$pos.nums<-paste0("(",parsedtxt$token_id,"-",parsedtxt$token,"-",parsedtxt$tag,")")
  parsedtxt$head_token<-lapply(1:nrow(parsedtxt),head_token_grab, data=parsedtxt)
  parsedtxt[parsedtxt$dep_rel=="ROOT",c("dep_rel","head_token","head_token_id")]<-c("root","ROOT",0)
  parsedtxt$pos.nums<-paste0("(",parsedtxt$token_id,"-",parsedtxt$token,"-",parsedtxt$tag,")")
  parsedtxt$parses<-paste0(parsedtxt$dep_rel, "(",parsedtxt$head_token,"-",parsedtxt$head_token_id,", ",parsedtxt$token,"-",parsedtxt$token_id,")")
  parsedtxt$w.nums<-paste0(parsedtxt$token,"-",parsedtxt$token_id)
  all.parses<-lapply(unique(parsedtxt$doc_id),function(x) unlist(parsedtxt[parsedtxt$doc_id==x,"parses"]))
  all.pos.nums<-lapply(unique(parsedtxt$doc_id),function(x) unlist(parsedtxt[parsedtxt$doc_id==x,"pos.nums"]))
  all.w.nums<-lapply(unique(parsedtxt$doc_id),function(x) unlist(parsedtxt[parsedtxt$doc_id==x,"w.nums"]))
  return(list(parses=all.parses,pos.nums=all.pos.nums,w.nums=all.w.nums))
}
head_token_grab<-function(x, data){
  return(data[(data$doc_id==data[x,]$doc_id)&(data$sentence_id==data[x,]$sentence_id)&(data$token_id==data[x,"head_token_id"]),"token"])
}
################################################################

# options(java.parameters = "-Xmx8g")
# require(rJava)
# require(coreNLP)
# initCoreNLP("/stanford-corenlp/", mem="8g")
################################################################
row.to.char<-function(deps){
  return(paste0(deps$type,"(",
                deps$governor,"-",
                deps$governorIdx,", ",
                deps$dependent,"-",
                deps$depIndex,")"))
}
################################################################
core.parser<-function(text){
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
      dep.char<-c(dep.char,row.to.char(dep.table[dep.table$depIndex==x,]))
    }
    parses[[s]]<-dep.char
    pos.nums[[s]]<-paste0("(",apply(pos.table,1,paste, collapse="-"),")")
  }
  all.parses=do.call(c, parses)
  all.pos.nums=do.call(c, pos.nums)
  return(list(sentences=sentences,
              parses=parses,
              all.parses=all.parses,
              all.pos.nums=all.pos.nums))
}
################################################################
