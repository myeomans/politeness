
#options(java.parameters = "-Xmx8g")
#require(rJava)
#require(coreNLP)
#initCoreNLP("/stanford-corenlp/", mem="8g")
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
  for (s in 1:length(sentences)){
    s.s=sentences[[s]]
    a.s<-coreNLP::annotateString(s.s)
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
################################################################
