############################################################
# TEMPORARY TEMPORARY TEMPORARY TEMPORARY TEMPORARY TEMPORARY
############################################################
# This file only for development... Will be removed soon!
############################################################
# TEMPORARY TEMPORARY TEMPORARY TEMPORARY TEMPORARY TEMPORARY
############################################################

load("data/polite_test.Rda")
source("R/dictTools.R")
source("R/textTools.R")
source("R/posTools.R")
source("R/politeness.R")
source("R/math.R")

spacyr::spacy_initialize(python_executable = "/anaconda/bin/python")
#spacyr::spacy_finalize()

#load("R/polite_test.Rda")

#options(java.parameters = "-Xmx8g")
#require(rJava)
#require(coreNLP)
#initCoreNLP("/stanford-corenlp/", mem="8g")

#
# text<-study1$message
# parser="spacy"
# binary=FALSE
# features<-list()
# ########################################################
# politeness(text,binary=F,parser="spacy")
