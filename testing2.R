require(dplyr)

data<-readxl::read_xlsx("testdata.xlsx")
meta<-readxl::read_xlsx("testmetadata.xlsx")
meta<-meta[-1,]

heading <- c(names(data))
labels <-unlist(c(dplyr::select(meta,Label)),use.names=FALSE)
identical(heading,labels) # includes all labels in metadata?

nullList<-meta %>% 
  filter(Nullble == "no") %>%
  select(Label)
nullList<-rep(nullList)

for (item in nullList){
  selected<- data %>%
    dplyr::select(item)
}

isnull<-data.frame(is.na(selected))
isnull
class(isnull)

as.data.frame(which(isnull=="TRUE", arr.ind = TRUE)) # store the result in a data frame
# Null in 
# row num + 1 for the actual table 

            #--------------------------------------------------------------------------------------------#
            #   1. metadata 자동적으로 읽어서 rule file 만들기                                           #
            #   2. result 어떤식으로 show 해야 할지                                                      #
            #--------------------------------------------------------------------------------------------#

# metadata analysis 
as.data.frame(na.omit(select(meta, Label, X__1, X__2))) 
# x__1 : min value 
# x__2 : max value 

rule <- editrules::editset(c("col1 >= 0", "col1 <= 10","col3 >= 1", "col3 <= 100"))
rule

errors <- data.frame(editrules::violatedEdits(rule, data))
errors
as.data.frame(which(errors=="TRUE", arr.ind = TRUE)) # tells which rule has been violated 

location<- data.frame((editrules::localizeErrors(rule, data))$adapt)
location
as.data.frame(which(location=="TRUE", arr.ind = TRUE))
# 문제점 : Null value도 error로 읽어버림 
