# Testing 

data<- read_data("C:/Users/ADMIN/Desktop/testdata.xlsx")
metadata <- read_meta("C:/Users/ADMIN/Desktop/testmetadata.xlsx")

metadata
datalist<-data_list(data)
metalist <-meta_list(metadata,"Label")
datalist  
metalist
class(datalist)
class(metalist)
names(metadata)
dplyr::select(metadata,Label)
all(datalist, metalist)
ismatching(datalist, metalist)

nullList<-null_list(metadata, "Nullble", "Label")
nullList

  #nullList <- 
  metadata %>% 
  filter(Nullble == "no") %>% select(Label)
nullList<- rep(nullList)
nullList
