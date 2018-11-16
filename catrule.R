catrule <- as.data.frame(na.omit(select(metadata, Label, description)))
catrule

description<- as.list(unlist(strsplit((as.character(select(catrule, description))),",")))
description
t<- c()

for (item in description){
  t <- append(t,item)
}

t<- data.frame(t)

t

class(t)

for (item in t){
  a<- append(a, paste(item, sep = ','))
}

for (item in description){
  t <- append(t,item)
}
t
class(t)
?paste0
select(catrule[1,], description)

for (i in 1:nrow(catrule)){
  # select row  
  d<- as.list(unlist(strsplit(as.character(select(catrule[i,], description)),",")))
  for (item in d){
    # join every item
    cat("(", item)
    # text file writable line generating 
  }
  # save line to the rule text file 
}