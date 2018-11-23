#################################################
datapath <- "testdata.xlsx"
metapath <- "testmetadata.xlsx"
#################################################

      ###Loading Packages###
# A function to check if necessary packages are downloaded
# if downloaded, load them to the program
# if not, download the package and load
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# Required Packages
packages <- c("dplyr", "readxl", "editrules", "xlsx")
ipak(packages)

      ###Readeing Data Files###
# Original Data
orgdata <- readxl::read_xlsx(datapath)
# generating a new data with assigned row numbers for future reference
nrow<- nrow(orgdata) + 1 # should add 1 in order to have the same row number as the actual excel file 
data <- cbind(rownum = 2:nrow, orgdata)
data
# Metadata
metadata <- readxl::read_excel(metapath)
# Omitting the first row b/c it contains nothing
metadata<- metadata[-1,]

      ###Validation of Column###
# make a list of columns in the data
heading <- c(names(orgdata))
# make a list of items in the metadata
labels <- unlist(c(dplyr::select(metadata, Label)), use.names = FALSE)
# check to see if two are identical
ismatching<-identical(heading, labels)


      ###Detection of Null###
# make a list of columns that should never contain null based on the information given in metadata
# change every character to lower case for NULLABLE part only 
nullList <- metadata %>%
  filter(sapply(Nullble,tolower) %in% c("no","f","false")) %>%   
  # filter where the nullable section says no, ('no','f','false' )
  select(Label)                 
# and select labels to store them in a vector 
nullList<- rep(nullList)

# filtering columns that should not contain null values 
for (item in nullList){
  selected<- data %>%
    dplyr::select(item)
}
isnull <- data.frame(is.na(selected))
nullresult <- as.data.frame(which(isnull == "TRUE", arr.ind = TRUE)) # telling the row num and col num of the data cell containing null
# removing duplicates and extract row numbers to filter the rows out
rowlist1 <- unique(c((as.numeric(sort(nullresult[,"row"])))))

variables = ncol(data)
iterations = length(rowlist1)

# initiate to create result table that tells which rows contains null   
df1 <- data.frame(matrix(ncol = variables, nrow = iterations))

for (i in 1:iterations){
  df1[i,]<- data[rowlist1[i],]
}
# setting up column names
colnames(df1)<- c(names(data))

df1<- df1%>% mutate(errortype = "Null Type")
      ###Detection of Null###
# Setting up rules/restrictions
# numerical rules
numrule <- as.data.frame(na.omit(select(metadata, Label, X__1, X__2)))  # X__1: min / X__2 : max

for (i in 1:nrow(numrule)){
  min <- paste(numrule[i,1], ">=", numrule[i,2]) # setting up minimum value 
  cat(min, '\n', file = "rules.txt", append = T) # write into rule text file 
  max <- paste(numrule[i,1], "<=", numrule[i,3]) # setting up maximum value
  cat(max, '\n', file = "rules.txt", append = T) # write into rule text file
}

## manually adding 
# cat(" rules ", file = "rules.txt", append = T)

rule <- editrules::editfile("rules.txt") # genearting rules 
errors <- data.frame(editrules::violatedEdits(rule, data)) # apply the rules against data
E <- as.data.frame(which(errors=="TRUE", arr.ind = TRUE)) # tells which rule has been violated 

rowlist2 <-  unique(c((as.numeric(sort(E[,"row"]))))) # extrat the row numbers
df2 <- data.frame(matrix(ncol = variables, nrow = length(rowlist2)))
colnames(df2)<- c(names(data)) # setting up column names
for (i in 1:length(rowlist2)){
  df2[i,]<- data[rowlist2[i],]
}
df2<- df2 %>% mutate(errortype = "Inconsistent Value")
tdf<-rbind(df1, df2)
tdf # error information 

df2

write.xlsx(tdf, "testing.xlsx", sheetName = "testing", row.names = FALSE)

###################################################################################################################################################
      ###Generating Result File###
# Number of items in the data to be reported 
#cat(paste("Number of rows in the data : ", nrow(orgdata),'\n'), file = "results.txt", append = T)
#cat(paste("Number of columns in the data : ", ncol(orgdata),'\n'), file = "results.txt", append = T)
#cat(paste("Number of items in the metadata : ", nrow(metadata),'\n'), file = "results.txt", append = T)
#cat(paste("Do data column names and items in metadata match? : ", ismatching,'\n'), file = "results.txt", append = T)

# df1 : null detection 
#cat(paste("\n","Null Detection Result","\n"), file = "results.txt", append = T)
#cat(paste("Number of NA values in the data : ", length(isnull[isnull == TRUE]),'\n'), file = "results.txt", append = T)
#write.table(df1, "results.txt",sep = '\t', row.names = FALSE, append = T)

# df2 : inconsistency detection 
#cat(paste("\n","Inconsistency Detection Result","\n"), file = "results.txt", append = T)
#cat(paste("Number of Inconsistent values in the data : ", nrow(E),'\n'), file = "results.txt", append = T)
#write.table(df2, "results.txt", sep= '\t', row.names=FALSE, append = T)
###################################################################################################################################################




