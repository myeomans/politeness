## ----setup, echo=FALSE--------------------------------------------------------
knitr::opts_chunk$set(comment = NA, echo=FALSE, message = FALSE, warning = FALSE)

## ----echo=TRUE----------------------------------------------------------------
library(politeness)

## -----------------------------------------------------------------------------
data("feature_table")

knitr::kable(feature_table,align=rep("c",4),
             col.names = c("Feature Name",
                           "POS Tags",
                           "Description",
                           "Example"),
             caption = "Table 1: Politeness Features")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # install.packages("spacyr")
#  spacyr::spacy_initialize(python_executable = "PYTHON_PATH")
#  

## -----------------------------------------------------------------------------
data("phone_offers")

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
df_politeness_count <- politeness(phone_offers$message, metric="count")
df_politeness_count[20:30,1:5]

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
df_politeness <- politeness(phone_offers$message, metric="binary")
df_politeness[20:30,1:5]

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  politeness::featurePlot(df_politeness,
#                          split=phone_offers$condition,
#                          split_levels = c("Tough","Warm"),
#                          split_name = "Condition")

## ----fig.width=6, fig.height=8, eval=TRUE-------------------------------------
politeness::featurePlot(df_politeness,
                        split=phone_offers$condition,
                        split_levels = c("Tough","Warm"),
                        split_name = "Condition")

## ----echo=TRUE, eval=TRUE-----------------------------------------------------

df_polite_train <- politeness(phone_offers$message)

df_polite_holdout<-politeness(bowl_offers$message)

project<-trainModel(df_polite_train,
                              phone_offers$condition,
                              df_polite_holdout)

mean(project$test_proj[bowl_offers$condition==1])

mean(project$test_proj[bowl_offers$condition==0])



## ----echo=TRUE, eval=TRUE-----------------------------------------------------
fpt_most<-exampleTexts(phone_offers$message,
                       phone_offers$condition,
                       type="most")
fpt_least<-exampleTexts(phone_offers$message,
                        phone_offers$condition,
                        type="least")

## ----echo=FALSE, eval=TRUE----------------------------------------------------
print("Most Polite")
print(fpt_most)
print("Least Polite")
print(fpt_least)

## ----out.width = "650px"------------------------------------------------------
knitr::include_graphics("executionTime.png")

