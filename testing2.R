
data<-readxl::read_xlsx("testdata.xlsx")
meta<-readxl::read_xlsx("testmetadata.xlsx")
meta<-meta[-1,]

heading <- c(names(data))
labels <-unlist(c(dplyr::select(meta,Label)),use.names=FALSE)
identical(heading,labels)

nullList<-meta %>% 
  filter(Nullble == "no") %>%
  select(Label)
nullList<-rep(nullList)

for (item in nullList){
  selected<- data %>%
    dplyr::select(item)
}
isnull<-data.frame(is.na(selected))
isnull
isnull%>%
  dplyr::filter_all(any_vars(stringr::str_detect(.,pattern = "TRUE")))

class(isnull)
