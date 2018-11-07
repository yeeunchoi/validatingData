
####cleandata####
### 1. Raw Data (Input)

# both data and metadata files 
read_data <- function(datapath){
  readxl::read_xlsx(datapath)
}
# data <- read_data(filepath)로 해서 데이터 불러와야함 

read_meta <- function(metapath){
  meta <- readxl::read_xlsx(metapath)
  meta <- meta[-1,]
}
# for data file 
data_list <- function(data){
  datalist <- c(names(data))
}

# for metadate file ONLY
meta_list <- function(metadata,label){
  metalist <- c(dplyr::select(metadata,label)) ## NA should be omitted 
  metalist<-unlist(metalist, use.names=FALSE)
}


### 2. Validating Columns 

#--------------------------------------------------------------------------------------------------#
# checking if the data file does not contain header
header <- function (data){
  # check if there is a header in data file 
}
#--------------------------------------------------------------------------------------------------#


# checking if the item in the data file matches with the labels in the metadata using two lists generated
  # returns TRUE or FALSE 
ismatching<- function (datalist, metalist){
  identical(datalist,metalist)
} ## if returns false, cleaning process can no longer be processed 


### 3. Detecting Null 

# A function that creates a list that contains items that should not contain null 
null_list <- function (metadata, nullable, Label){
  require(dplyr)
  nullList <- metadata %>% 
    filter(nullable == "no") %>% 
    select(Label)
  
  n<- rep(nullList)
  return(n)
}




  # A function that checks null 
null_check <- function (nlist, data){
  for (item in nlist){
    selected<- data %>%
      dplyr::select(item)
  }
  isnull<-data.frame(is.na(selected))
  isnull%>%
    dplyr::filter_all(any_vars(stringr::str_detect(.,pattern = "TRUE")))
}

### 4. Detecting Inconsistencies 

#--------------------------------------------------------------------------------------------------#
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
#--------------------------------------------------------------------------------------------------#

addRule <- function(rulefilepath, rules){
  cat(rules, "\n",
      file = rulefilepath, 
      append = TRUE)
} # must write one at a time


violatedRule <- function(data, filepath){
  rules <- editrules::editfile(filepath)
  errors <- data.frame(editrules::violatedEdits(rules, data))
  newdata <- data.frame(lapply(errors, function(x) if(is.logical(x)) { 
    return(as.character(x))
  } else {  
    return(x) 
  }
  ), stringsAsFactors=FALSE)
  
  newdata %>%
    dplyr::filter_all(any_vars(stringr::str_detect(., pattern = "TRUE")))
  # write the results in text file 
}

localize<- function(data,filepath){
  rules <- editruls::editfile(filepath)
  errors<- data.frame((editrules::localizeErrors(rules, data))$adapt)
  newdata <- data.frame(lapply(DE2, function(x) if(is.logical(x)) { 
    return(as.character(x))
  } else {  
    return(x) 
  }
  ), stringsAsFactors=FALSE)
  newdata %>%
    dplyr::filter_all(any_vars(stringr::str_detect(., pattern = "TRUE")))
  # write the results in a text file 
}

### 5. Results (OUTPUT)

result<- function(filename, lines){
  result <- file(filename)
  writeLines(c(lines), result)
  close(result)
}
