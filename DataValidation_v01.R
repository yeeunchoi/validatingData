## ALL THE REQUIRED PACKAGES HERE ##
require(editrules)
require(stringr)
require(dplyr)
require(readxl)

### READING FILE ###
## IF the file is excel
data <-read_xlsx("file path") # if each sheet has to be examined, use "sheet = n " argument
## IF the file csv
data <- read.csv("file path")

data <- readxl::read_xlsx("C:/Users/ADMIN/Desktop/metadata.xlsx")
data

## Assign id number for recognition of each row
data %>% 
  mutate(rownum = 1:n())

### COLUMN VALIDATION ###


### CHECKING NULL ###
data %>%
  is.na(data) %>%
  filter_all(any_vars(str_detect(.,pattern="FALSE")))
                      
### DETECING INCONSISTENCIES ###
# OPEN EDITFILE
EF <- editfile("file path")

# violatedERRORS : telling which rules have been violated by which data value
data <- violatedEdits(EF,data)
data <- data.frame(data)

# localizeERRORS : telling the exact location where inconsistencies happen in the data table
data <- localizeErrors(EF,data)
data <- data.frame(data$adapt)

# FUNCTION to change logical elements to character
newdata <- data.frame(lapply(data, function(x) if(is.logical(x)) { 
  return(as.character(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)

# Filtering out the rows that contain inconsistencies 
newdata %>%
  filter_all(any_vars(str_detect(., pattern = "TRUE")))