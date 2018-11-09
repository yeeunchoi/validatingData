# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr", "readxl", "stringr", "editrules")
ipak(packages)
#==================================================================================================================#

# generating two text files : summary and result (output)
writeLines("Summary", "summary.txt") #writing lines into the file 
writeLines("Results", "result.txt")

# open data/ metadata files 
data<-readxl::read_xlsx("testdata.xlsx")
meta<-readxl::read_xlsx("testmetadata.xlsx")
meta<-meta[-1,] # omitting the first row of the metadata b/c it contains nothing

# making lists for header and label to check if they match
heading <- c(names(data)) # header of data
labels <-unlist(c(dplyr::select(meta,Label)),use.names=FALSE) # Labels of metadata
identical(heading,labels) # includes all labels in metadata?
# if not matching, 
  # terminate the program

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
length(isnull[isnull == TRUE])  # count null 갯수 to be stored in summary text file
as.data.frame(which(isnull=="TRUE", arr.ind = TRUE)) # store the result in a data frame to be stored in result text file 
# row num + 1 for the actual table 
write.table(export, file= "testing.txt", sep=" ") # write data table into a text file 

# metadata analysis 
# for numerical data only 
a<-as.data.frame(na.omit(select(meta, Label, X__1, X__2))) 
# x__1 : min value 
# x__2 : max value 
# read them automatically using sprintf() function 
# store them in a vector

a
length(a)
nrow(a)

rules <- c()

for (i in 1:nrow(a)){
  min <- paste(a[i,1], ">=", a[i,2])
  max <- paste(a[i,1], "<=", a[i,3])
  rules<- c(min, max)
}
rules

rule <- editrules::editset(c("col1 >= 0", "col1 <= 10","col3 >= 1", "col3 <= 100"))
# store rules in summary text file???  THEY must be store in some way 

errors <- data.frame(editrules::violatedEdits(rule, data))
errors
E1 <- as.data.frame(which(errors=="TRUE", arr.ind = TRUE)) # tells which rule has been violated 

location<- data.frame((editrules::localizeErrors(rule, data))$adapt)
location
E2 <- as.data.frame(which(location=="TRUE", arr.ind = TRUE))

#########################################################
# list of row numbers to be printed                     
l<-c((as.numeric(sort(E2[,"row"]))))
# data 에 잘못되었는지 보여주려면 row num만 필요함 
# actual row number도 있어야함 
for (i in l){
  print(data[i,])
}
