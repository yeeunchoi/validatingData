### 1. Raw Data (Input)

require(readxl)
require(dplyr)
require(stringr)

# both data and metadata files 
read_data <- function(datapath){
  read_xlsx(datapath)
}

# for data file 
data_list <- function(data){
  datalist <- c(names(data))
}

# for metadate file ONLY
meta_list <- function(metadata, label){
  require(dplyr)
  metalist <- rep(select(metadata, label)) ## NA should be omitted 
}


### 2. Validating Columns 

# checking if the data file does not contain header
header <- function (data){
  # check if there is a header in data file 
}

# checking if the item in the data file matches with the labels in the metadata using two lists generated
  # returns TRUE or FALSE 
ismatching<- function (datalist, metalist){
  identical(datalist,metalist)
} ## if returns false, cleaning process can no longer be processed 


### 3. Detecting Null 

# A function that creates a list that contains items that should not contain null 
null_list <- function (metadata, nullable, Label){
  nullList <- data %>% 
    filter(Nullble == "NO") %>% 
    select(Label)
  nullList<- rep(nullList)
  return(nullList)
}
  
# A function that checks null 
null_check <- function (nlist, data){
  for (item in nlist){
    selected<- data %>%
      select(item)
  }
  isnull<-data.frame(is.na(selected))
  isnull%>%
    filter_all(any_vars(str_detect(.,pattern = "TRUE")))
}

### 4. Detecting Inconsistencies 
# automatic function writer
# manual rule writer
numrule <- function(rulefilepath, metadata){
  cat("# Numerical Rules", "\n",
      "rules","\n",
      file = rulefilepath,
      append = TRUE)
}

catrule <- function(rulefilepath, metadata){
  cat("# Categorical RuleS", "\n",
    "rules", "\n",
      file = rulefilepath,
      append = TRUE )
}

addRule <- function(rulefilepath, rules){
  cat("rules", "\n",
      file = rulefilepath, 
      append = TRUE)
}


# detecting inconsistencies 
### DETECING INCONSISTENCIES ###
# OPEN EDITFILE
EF <- editfile("C:/Users/ADMIN/Desktop/연구데이터/증상 치법의 유의조합_C14050_김안나/rules.txt")

# violatedERRORS : telling which rules have been violated by which data value
DE1<- violatedEdits(EF,data)
DE1 <- data.frame(DE1)

newdata <- data.frame(lapply(DE1, function(x) if(is.logical(x)) { 
  return(as.character(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)

# Filtering out the rows that contain inconsistencies 
newdata %>%
  filter_all(any_vars(str_detect(., pattern = "TRUE")))

# localizeERRORS : telling the exact location where inconsistencies happen in the data table
DE2 <- localizeErrors(EF,data)
DE2 <- data.frame(DE2$adapt)

# FUNCTION to change logical elements to character
newdata <- data.frame(lapply(DE2, function(x) if(is.logical(x)) { 
  return(as.character(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)

# Filtering out the rows that contain inconsistencies 
newdata %>%
  filter_all(any_vars(str_detect(., pattern = "TRUE")))
