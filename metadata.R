## to make metadata file into textfile ##

require(readxl)
require(dplyr)
require(stringr)
## open metadata into R
read_data <- function(filepath){
  readxl::read_xlsx(filepath)
}

data<- read_data("C:/Users/ADMIN/Desktop/metadata.xlsx")
data

data <-read_xlsx("C:/Users/ADMIN/Desktop/metadata.xlsx")
data
data2 <- read_xlsx("C:/Users/ADMIN/Desktop/data.xlsx")
data2
names(data)
v1 <- c(names(data))
v1

v2 <- select(data, Label)
v2
testing <-rep(v2)
testing
complete.cases(testing)
na.omit(testing)

v1
v2
identical(v1,v2)
?rep
metadata <- read_xlsx("C:/Users/ADMIN/Desktop/연구과제리스트/EntityAttributeInformation.xlsx")
metadata

## writing edit rules in a textfile 

# only label, nullable, min and max are considered 
# descriptions should also be considered if needed

metadata$Label
metadata$Nullable


# checking null
# filter where nullable says no 
nullList <- select(filter(data, Nullble == "NO"), Label)
nullList <- data %>% filter(Nullble == "NO") %>% select(Label)
nullList <- rep(nullList)
nullList

# checking null in the data

for (item in nullList){
  selected<- data2 %>%
    select(item)
}
isnull<-data.frame(is.na(selected))
isnull%>%
  filter_all(any_vars(str_detect(.,pattern = "TRUE")))

data
data2
