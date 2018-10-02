## ALL THE REQUIRED PACKAGES HERE ##
require(editrules)
require(stringr)
require(dplyr)
require(readxl)

### READING FILE ###
## IF the file is excel
data <-read_xlsx("C:/Users/ADMIN/Desktop/연구데이터/in vitro/in vitro_info.xlsx") # if each sheet has to be examined, use "sheet = n " argument
head(data)
colnames(data)
head(data)
class(data)
isnull <- data.frame(is.na(data)) 
isnull <- mutate(isnull, rownum = 1:n())

head(isnull)
isnull%>%
  filter_all(any_vars(str_detect(.,pattern = "TRUE"))) %>%
  select(rownum) %>%
  head()

head(data)
######################
ES <- editset("mesure_1 >= 0")
data <- violatedEdits(ES,data)
data <- data.frame(data)
head(data)

data <- data.frame(lapply(data, function(x) if(is.logical(x)) { 
  return(as.character(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)

# Filtering out the rows that contain inconsistencies 
data %>%
  filter_all(any_vars(str_detect(., pattern = "TRUE")))

data <- localizeErrors(ES,data)
data <- data.frame(data$adapt)
head(data)

new <- data.frame(lapply(data, function(x) if(is.logical(x)) { 
  return(as.character(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)

new%>%
  filter_all(any_vars(str_detect(.,pattern = "TRUE")))
