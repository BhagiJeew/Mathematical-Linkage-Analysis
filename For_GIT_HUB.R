
Seperated_by_Unique_Item<-Sample_file %>% 
  arrange(Unique_Item,desc(Proportionated.Variable.Value)) %>% 
  group_by(Unique_Item) %>%
  nest() 


##Inserting the for loop to calculate the seperate R_square values
for (k in 1:200) { ## This k is the no.of Unique Items
  
  
  Seperated_by_Unique_Item[[2]][[k]]$Rsqure_C<-NA
  
  d<-nrow(Seperated_by_Unique_Item[[2]][[k]])
  
  for (i in 1:d) {
    
    Seperated_by_Unique_Item[[2]][[k]][i,12]<-summary(lm(Seperated_by_Unique_Item[[2]][[k]]$Proportionated.Variable.Value~Rsq[1:d,i+1]))$r.squared
    
  }
  
}


##Identifying the significant variables values using the maximum R_Squared value.

MaxR<-as.data.frame(Seperated_by_Unique_Item[[1]])

MaxR$Max_R<-NA

for (k in 1:200) {
  d<-which.max(Seperated_by_Unique_Item[[2]][[k]]$..2)
  MaxR[k,2]<-d
}

