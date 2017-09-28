## ----setup, echo=FALSE-------------------------------------------------------
knitr::opts_chunk$set(comment = NA, echo=FALSE, message = FALSE, warning = FALSE)
library(politeness)
data("phone_offers")
data("feature_table")

## ------------------------------------------------------------------------
knitr::kable(feature_table,align=rep("c",4),
             col.names = c("Feature Name",
                           "POS Tags",
                           "Description",
                           "Example"),
             caption = "Table 1: Politeness Features")

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  # install.packages("spacyr")
#  spacyr::spacy_initialize(python_executable = "PYTHON_PATH")
#

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
df_politeness_count <- politeness(phone_offers$message, binary=FALSE, num_mc_cores=getOption("mc.cores", 2L))
df_politeness_count[20:30,1:5]

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
df_politeness <- politeness(phone_offers$message, binary=TRUE, num_mc_cores=getOption("mc.cores", 2L))
df_politeness[20:30,1:5]

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  politeness::politenessPlot(df_politeness,
#                             split=phone_offers$condition,
#                             split_levels = c("Tough","Warm"),
#                             split_name = "Condition")

## ---- fig.width=6, fig.height=8, eval=TRUE-------------------------------
politeness::politenessPlot(df_politeness,
                           split=phone_offers$condition,
                           split_levels = c("Tough","Warm"),
                           split_name = "Condition")

## ---- echo=TRUE, eval=TRUE-----------------------------------------------

df_polite_train <- politeness(phone_offers$message, drop_blank=FALSE, num_mc_cores=getOption("mc.cores", 2L))

df_polite_holdout<-politeness(bowl_offers$message, drop_blank=FALSE, num_mc_cores=getOption("mc.cores", 2L))

project<-politenessProjection(df_polite_train,
                              phone_offers$condition,
                              df_polite_holdout)

mean(project$test_proj[bowl_offers$condition==1])

mean(project$test_proj[bowl_offers$condition==0])



## ---- echo=TRUE, eval=TRUE-----------------------------------------------
fpt<-findPoliteTexts(phone_offers$message,
                     df_polite_train,
                     phone_offers$condition)

## ---- echo=FALSE, eval=TRUE----------------------------------------------
for(x in 1:nrow(fpt)){
  print(as.character(fpt[x,1]))
}


