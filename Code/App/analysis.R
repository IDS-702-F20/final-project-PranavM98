library(arm)
library(pROC)
library(e1071)
library(caret)
library(nnet)
library(knitr)



library(MASS)
library(ggplot2)


data_123<-function(new_data,label){

new_data<-data.frame(new_data)
print(new_data)
mdata<-read.csv('/Users/pranavmanjunath/Desktop/Duke/702/Final Project_Git/final-project-PranavM98/Data/cleaned_data.csv')
mdata<-subset(mdata, select=-c(Type,ID,Ref_Track,URL_features))


mdata$Duration_ms<-mdata$Duration_ms/1000
names(mdata)[names(mdata) == "Duration(seconds)"] <- "Duration_seconds"
mdata$Duration_ms<-as.integer(mdata$Duration_ms)
mdata<-mdata[!(mdata$Name=="Mana"),]


lista<-names(which(table(mdata$Overall_Genre)>2000))

subset_data<-mdata[mdata$Overall_Genre %in% lista,]


library(tidyr)
library(dplyr)
subset_data<-subset_data %>% drop_na()

subset_data$Overall_Genre<-as.factor(subset_data$Overall_Genre)

subset_data$Tempo<-as.integer(subset_data$Tempo)
print(dim(subset_data)[1])
subset_data<-subset_data %>% distinct()
print(dim(subset_data)[1])


file=paste("/Users/pranavmanjunath/Desktop/Duke/702/Final Project_Git/final-project-PranavM98/Data",label,sep='/')
filename=paste(file,'csv',sep=".")
p<-read.csv(filename)
print(p)
p1<-p
p <- subset(p, select = -c(Name, Uri, Genre,Overall_Genre,Liveness,X.1,X) )

p1 <- subset(p1, select = -c(Genre,Overall_Genre,Liveness,X.1,X) )
print(colnames(p))
print("P")
print(length(p))
print(length(new_data))

newdf <- rbind(p, new_data)

# Make sure that "my_matrix" is indeed a "matrix".
v <- as.numeric(newdf[dim(newdf)[1],])
my_matrix <- as.matrix(head(newdf,n=dim(newdf)[1]-1))
M <- my_matrix
cosSim <- ( M %*% v ) / sqrt( sum(v*v) * rowSums(M*M) )

sort_cos<-cosSim[order(-cosSim),]
final<-data.frame(sort_cos[1:10])

Name=c()
URI=c()
Cosine_Similarity=c()

print("O")
print(newdf)

count=0
for (i in rownames(final)){
  count=count+1
  Name<-append(Name,p1[i,]$Name)
  URI<-append(URI,p1[i,]$Uri)
  Cosine_Similarity<-append(Cosine_Similarity,round(final$sort_cos.1.10[count],10))

}
print("GE")
reccomendation<-data.frame(cbind(Name,URI,Cosine_Similarity))
print("REE")
print(reccomendation)
return (reccomendation)
}
