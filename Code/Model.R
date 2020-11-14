library(arm)
library(pROC)
library(e1071)
library(caret)
library(nnet)
library(knitr)



library(MASS)
library(ggplot2)

mdata<-read.csv('./App/cleaned_data.csv')
mdata<-subset(mdata, select=-c(Type,ID,Ref_Track,URL_features))

#mdata$Overall_Genre<-as.factor(mdata$Overall_Genre)

mdata$Duration_ms<-mdata$Duration_ms/1000
names(mdata)[names(mdata) == "Duration(seconds)"] <- "Duration_seconds"
mdata$Duration_ms<-as.integer(mdata$Duration_ms)

mdata<-mdata[!(mdata$Name=="Mana"),]


lista<-names(which(table(mdata$Overall_Genre)>2000))

subset_data<-mdata[mdata$Overall_Genre %in% lista,]

subset_data$Overall_Genre[subset_data$Overall_Genre=='House']<-'Electronic'
subset_data$Overall_Genre[subset_data$Overall_Genre=='Techno']<-'Electronic'




library(tidyr)  
subset_data<-subset_data %>% drop_na()


subset_data$Tempo<-as.integer(subset_data$Tempo)

############

subset_data<-read.csv('updated_genre.csv')
subset_data$Overall_Genre<-as.factor(subset_data$Overall_Genre)

##########
library(caret)
library(gbm)


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  #classProbs = TRUE,
  ## repeated one times
  repeats = 1)

gbmGrid <-  expand.grid(interaction.depth = 2,
                        n.trees = 300,
                        shrinkage = 0.15,
                        n.minobsinnode = 10)

gbmFit <- train(Overall_Genre ~ Key + Mode + Danceability + Energy + Loudness+Speechness+
                  Acousticness+Instrumentalness+Valence+Tempo + time_signature +
                  Duration_ms, data = dTrain,
                method = "gbm",
                trControl = fitControl,
                verbose = TRUE,
                tuneGrid = gbmGrid)






ars_boost <-  gbm(Overall_Genre ~ Key + Mode + Danceability + Energy + Loudness+Speechness+
                    Acousticness+Instrumentalness+Valence+Tempo + time_signature +
                    Duration_ms,data=dTrain,
                  n.trees=500, interaction.depth=2)


summary(ars_boost)

## Confusion matrix


pred_prob_boost <- predict(ars_boost,dTest,n.trees=150,type="response")
labels = colnames(pred_prob_boost)[apply(pred_prob_boost, 1, which.max)]

Conf_boost <- confusionMatrix(as.factor(labels),
                              as.factor(dTest$Overall_Genre))

multiclass.roc(subset_data$Overall_Genre,as.factor(labels),plot=T,print.thres="best",
               legacy.axes=T,print.auc =T,col="red3")

summary(
  ars_boost, 
  cBars = 14,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
cboost_list=c()
for (i in 1:length(pred_prob_boost)[1]){
  
  cboost_list<-append(cboost_list,match(max(pred_prob_boost[i,]),pred_prob_boost[i,]))
  
}
Conf_boost$table
Conf_boost$overall["Accuracy"]
#much better accuracy although we probably over fit.
#use out-of-sample RMSE or cross validation using average RMSE
Conf_boost$byClass[c("Sensitivity","Specificity")]

## ROC curve
roc(arsenic$switch,pred_prob_boost,
    plot=T,print.thres="best",legacy.axes=T,print.auc =T,col="red3")
#much better AUC. Again, we may have overfit! 


saveRDS(ars_boost, "final_model_boost.rds")

ew_data<-data.frame(
  Danceability = c (0.563), 
  Energy = c (0.648), 
  Key = c (9), 
  Loudness = c (-13.441), 
  Mode = c (1),
  Speechness = c (0.0284), 
  Acousticness = c (0.529),
  Instrumentalness= c(0.882),
  Valence = c (0.685), 
  Tempo = c (80), 
  Duration_ms = c (273)
)

ew_data$Key<-as.integer(ew_data$Key)
ew_data$Mode<-as.integer(ew_data$Mode)
ew_data$Tempo<-as.integer(ew_data$Tempo)
ew_data$Duration_ms<-as.integer(ew_data$Duration_ms)

a<-predict(ars_boost,ew_data)
a



########### XG BOOST ##################


install.packages("xgboost")
library(xgboost)


bst <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 1)

#################Cosine Similarity########################
as.numeric(subset_data[dim(subset_data)[1],])
##

p<-read.csv('./App/cleaned_data.csv')

p <- subset(p, select = -c(Name,time_signature, Genre,Overall_Genre,Liveness,URL_features,
                           ID, Uri, Ref_Track,Type))

newdf <- rbind(p, ew_data)

# Make sure that "my_matrix" is indeed a "matrix".
v <- as.numeric(newdf[dim(newdf)[1],])
my_matrix <- as.matrix(head(newdf,n=dim(newdf)[1]-1))
M <- my_matrix
cosSim <- ( M %*% v ) / sqrt( sum(v*v) * rowSums(M*M) )

sort_cos<-cosSim[order(-cosSim),] 
final<-data.frame(sort_cos[1:10])

rownames(final)
count=0
for (i in rownames(final)){
  count=count+1
  print(final$sort_cos.1.10[count])
}
reccomendation <- data.frame(Name= character(), Uri= character() )
Name=c()
URI=c()

for (i in rownames(final)){
  Name<-append(name,subset_data[i,]$Name)
  URI<-append(uri,subset_data[i,]$Uri)
  
}
reccomendation<-data.frame(cbind(name,uri))






#############
library(dplyr)

lop<-c("Punk","Rap","Rock","Techno")

for (i in lop){
  a<-subset_data %>%
    filter(Overall_Genre==i)
  text1<-paste("ars_boost",i, sep="_")
  text1 <-  gbm(factor(Genre) ~ Key + Mode + Danceability + Energy + Loudness+Speechness+
                  Acousticness+Instrumentalness+Valence+Tempo + Duration_ms,data=a,n.trees=5000, interaction.depth=2)
  text2<-paste(i,"boost.rds",sep="_")
  saveRDS(text1, text2)
}##############

library(tidyverse)
library(caret)
library(nnet)

training.samples <- subset_data$Overall_Genre %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- subset_data[training.samples, ]
test.data <- subset_data[-training.samples, ]


model <- nnet::multinom(Overall_Genre ~ Key + Mode + Danceability + Energy + Loudness+Speechness+
                          Acousticness+Instrumentalness+Valence+Tempo + time_signature +
                          Duration_ms, data = train.data)


predicted.classes <- model %>% predict(test.data)
head(predicted.classes)

mean(predicted.classes == test.data$Overall_Genre)
################



library(caret)
library(gbm)
set.seed(3456)
trainIndex <- createDataPartition(subset_data$Overall_Genre, p = .7, 
                                  list = FALSE, 
                                  times = 1)


dTrain <- subset_data[ trainIndex,]
dTest  <- subset_data[-trainIndex,]

ars_boost_train <-  gbm(Overall_Genre ~ Key + Mode + Danceability + Energy + Loudness+Speechness+
                    Acousticness+Instrumentalness+Valence+Tempo + time_signature +
                    Duration_ms,data=dTrain,
                  n.trees=500, interaction.depth=2)



pred_prob_boost1 <- predict(ars_boost_train,dTest,n.trees=500,type="response")
labels1 = colnames(pred_prob_boost1)[apply(pred_prob_boost1, 1, which.max)]

Conf_boost1 <- confusionMatrix(as.factor(labels1),
                              as.factor(dTest$Overall_Genre))


saveRDS(ars_boost_train, "final_model_boost_train.rds")


ew_data<-data.frame(
  Danceability = numeric(), 
  Energy = numeric(), 
  Key = integer(), 
  Loudness = numeric(),
  Mode = int(),
  Speechness = numeric(), 
  Acousticness = numeric(),
  Instrumentalness= numeric(),
  Valence = numeric(), 
  Tempo = integer(),
  Duration_ms = integer()
)

for (i in (0:1000)){
  ew_data<-data.frame(
    Danceability = numeric(), 
    Energy = numeric(), 
    Key = integer(), 
    Loudness = numeric(),
    Mode = integer(),
    Speechness = numeric(), 
    Acousticness = numeric(),
    Instrumentalness= numeric(),
    Valence = numeric(), 
    Tempo = integer(),
    Duration_ms = integer(),
    time_signature = integer()
  )
ew_data$Danceability<-f[i,'Danceability']
ew_data$Energy<-f[i,'Energy']
ew_data$Key<-f[i,'Key']
ew_data$Loudness<-f[i,'Loudness']
ew_data$Mode<-f[i,'Mode']
ew_data$Speechness<-f[i,'Speechness']
ew_data$Acousticness<-f[i,'Acousticness']
ew_data$Instrumentalness<-f[i,'Instrumentalness']
ew_data$Valence<-f[i,'Valence']
ew_data$Tempo<-f[i,'Tempo']
ew_data$Duration_ms<-f[i,'Duration_ms']
ew_data$time_signature<-f[i,'time_signature']
print(ew_data)
pred_prob_boost1 <- predict(ars_boost_train,ew_data,n.trees=500,type="response")
labels1 = colnames(pred_prob_boost1)[apply(pred_prob_boost1, 1, which.max)]
print(labels1)
}



#################

folkrds <- readRDS("./Models/Folk_boost.rds")
a<-predict(folkrds,ew_data)
labels12 = colnames(a)[apply(a, 1, which.max)]
labels43
