## ALL THE REQUIRED PACKAGES HERE
require(editrules)
require(stringr)
require(dplyr)

## READING FILE 
people <- read.csv("C:/Users/ADMIN/Desktop/people.txt")

## col names
colnames(people)


testing<-is.na(people)
testing
class(testing)
testing<- data.frame(testing)
testing<- data.frame(lapply(testing, function(x) if(is.logical(x)) { 
  return(as.character(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)
testing%>%
  filter_all(any_vars(str_detect(.,pattern = "TRUE")))

people %>%
  mutate(rownum = 1:n())
# using editset
ES <- editset(c("age >= 0", "age<= 150"))
# using violatededits
ve <- violatedEdits(ES,people)

ve <- data.frame(ve)
# change to data.frame 
class(ve)

# checking what class every element of the table is 
sapply(ve,class)

## changing all the variable to character 
# filtering out the rows that contain TRUE (which means that the row has an inconsistency)
df_2 <- data.frame(lapply(ve, function(x) if(is.logical(x)) { 
  return(as.character(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)

# verifying that the variables have correctly converted to character 
sapply(df_2,class)


## FILTERING out the rows that have TRUE 
ve %>%
  filter_all(any_vars(str_detect(., pattern = "TRUE")))


# using localizeErrors
le <- localizeErrors(ES,people)
le <- le$adapt
le<- data.frame(le)

df_2 <- data.frame(lapply(le, function(x) if(is.logical(x)) { 
  return(as.character(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)

sapply(df_2, class)
le

le%>%
  filter_all(any_vars(str_detect(., pattern = "TRUE")))


####################################################################################
# using editfile 

EF <- editfile("C:/Users/ADMIN/Desktop/rules.txt")
EF

ve<-data.frame(violatedEdits(EF,people))


df_2 <- data.frame(lapply(ve, function(x) if(is.logical(x)) { 
  return(as.character(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)

ve %>%
  filter_all(any_vars(str_detect(., pattern = "TRUE")))
