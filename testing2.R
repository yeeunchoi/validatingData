# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "readxl", "editrules", "knitr")
ipak(packages)
#==================================================================================================================#

# generating two text files : summary and result (output)
writeLines(paste("Results","\n" ),"results.txt") #writing lines into the file 

# open data/ metadata files 
orgdata<-readxl::read_xlsx("testdata.xlsx") #original data
# assign row number  
data <- orgdata %>%  # data with row numbers 
  mutate(rownum = 1:n())

meta<-readxl::read_xlsx("testmetadata.xlsx")
meta<-meta[-1,] # omitting the first row of the metadata b/c it contains nothing

# to be included in summary file 
datarow<-nrow(orgdata) # num of rows in data
datacol<-ncol(orgdata) # num of cols in data

#writeLines(paste("Number of Columns in the data : ", datacol, '\n',
#                 "Number of Rows in the data : ", datarow, '\n'), "results.txt")

metaitem<-nrow(meta)    # num of items in metadata, which should be identical to num of cols in data

# making lists for header and label to check if they match
heading <- c(names(orgdata)) # header of data
heading
labels <-unlist(c(dplyr::select(meta,Label)),use.names=FALSE) # Labels of metadata
identical(heading,labels) # includes all labels in metadata?

### NULL CHECKING
# a list of columns that should never contain null 
# convert every character into lower case ? > necessary??
nullList<-meta %>% 
  filter(Nullble == "no") %>% 
  select(Label)
nullList<-rep(nullList)

# filtering columns that should not contain null values
for (item in nullList){
  selected<- data %>%
    dplyr::select(item)
}
# store in isnull data frame
isnull<-data.frame(is.na(selected))
isnull
numnull<-length(isnull[isnull == TRUE])  # count null 갯수 to be stored in summary text file
export <- as.data.frame(which(isnull=="TRUE", arr.ind = TRUE)) # store the result in a data frame to be stored in result text file 
# row num + 1 for the actual table 

export
# list of row numbers to be printed                     
alist<-c((as.numeric(sort(export[,"row"]))))
unique(alist)

# data 에 잘못되었는지 보여주려면 row num만 필요함 
# actual row number도 있어야함 

# include column and row number and should be formatted 
for (i in alist){ # index number
  cat(paste(data[i,]), '\n',file = "testing.txt", sep = "\t", append = T)
  }

# metadata analysis 
# for numerical data only 
a<-as.data.frame(na.omit(select(meta, Label, X__1, X__2))) 
# x__1 : min value 
# x__2 : max value 
# read them automatically using sprintf() function 
# store them in a vector

for (i in 1:nrow(a)){
  min <- paste(a[i,1], ">=", a[i,2])
  cat(min, '\n', file = "rules.txt", append = T)
  max <- paste(a[i,1], "<=", a[i,3])
  cat(max, '\n', file = "rules.txt", append = T)
}

rule <- editrules::editfile("rules.txt")
# store rules in summary text file???  THEY must be store in some way 

errors <- data.frame(editrules::violatedEdits(rule, data))

E1 <- as.data.frame(which(errors=="TRUE", arr.ind = TRUE)) # tells which rule has been violated 
E1
numincon<-nrow(E1)

blist <- c((as.numeric(sort(E1[,"row"]))))
blist

for (i in blist){
  cat(paste(data[i,]),'\n', file = "testing2.txt", sep = "\t", append=T)
} # appending to data frame?

################################################################################################################################################
# Summary 
# 1. Total row x column counts
# 2. Null & Inconsistency counts
#
# Result 
# result tables 
# Issue Number 
# Row Number 
# Error Type -> column matching, Null Value, Inconsistent value
# Error Message -> "The value " " in "column" is not valid." "NA in "column" is not acceptable." "Columns and labels in metadata do not match."
################################################################################################################################################

                          #======================================================================#
                          #   To do lists:
                          #     1. results file에 column이랑 row number 추가 
                          #     2. result table formatting (function?)
                          #     3. Make this as RM for a better visualization & Explanation
                          #======================================================================#

# formatting test
new<- knitr::kable(data, format = "rst")
new

write.table(new, file = "format.txt")
class(new)
  