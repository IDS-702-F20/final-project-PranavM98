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

lista[1]
#library(dplyr)
#subset_data<- mdata %>%
#  filter(Overall_Genre=="Blues" | Overall_Genre=="Classical"|
#           Overall_Genre=="Folk" | Overall_Genre=="Hiphop"|
#           Overall_Genre=="House" | Overall_Genre=="Indie"|
#           Overall_Genre=="Jazz" | Overall_Genre=="Metal"|
#           Overall_Genre=="Pop" | Overall_Genre=="R&B" |
#            Overall_Genre=="Rock"| Overall_Genre=="Punk" |
#            Overall_Genre=="Soundtrack"| Overall_Genre=="EDM"
          
#           )

library(tidyr)  
subset_data<-subset_data %>% drop_na()

subset_data$Overall_Genre<-as.factor(subset_data$Overall_Genre)

subset_data$Tempo<-as.integer(subset_data$Tempo)

library(dplyr)

subset_data<-read.csv('updated_genre.csv')
subset_data$Overall_Genre<-as.factor(subset_data$Overall_Genre)


lista<-unique(subset_data$Overall_Genre)
for (i in lista){
 
    a<-subset_data %>% filter(Overall_Genre==i)
    file=paste(i,'csv',sep=".")
    f=paste('./Data',file,sep='/')
    write.csv(a,file)
}

subset_data$Danceability<-subset_data$Danceability*100
subset_data$Energy<-subset_data$Energy*100
subset_data$Loudness<-subset_data$Loudness*100
subset_data$Acousticness<-subset_data$Acousticness*100
subset_data$Speechness<-subset_data$Speechness*100
subset_data$Instrumentalness<-subset_data$Instrumentalness*100
subset_data$Liveness<-subset_data$Liveness*100
subset_data$Valence<-subset_data$Valence*100

subset_data$log_e<-log(subset_data$Energy+0.01)
subset_data$log_dance<-log(subset_data$Danceability+0.01)



ggplot(subset_data,aes(x=log_e)) +
  geom_histogram(binwidth=1)+
  scale_fill_brewer(palette="Reds") +
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ Overall_Genre)

#subset_data<-subset_data[!subset_data$Overall_Genre=='Country']

subset_data<-music
# Model Building
trainIndex <- createDataPartition(subset_data$Overall_Genre, p = .8, 
                                  list = FALSE, 
                                  times = 1)


dTrain <- subset_data[ trainIndex,]
dTest  <- subset_data[-trainIndex,]

model1 <- multinom(Overall_Genre ~ Key + Mode + Danceability + Energy + Loudness+Speechness+
Acousticness+Instrumentalness+Valence+time_signature+Tempo,data=dTrain)

predicted_scores <- predict (model1, dTest, "probs")

predicted_class <- predict (model1, dTest)


confusionMatrix(predicted_class,dTest$Overall_Genre)

output1 <- summary(model1)
z_value <- output1$coefficients/output1$standard.errors
p_value <- (1 - pnorm(abs(z_value), 0, 1))*2 
#we are using two-tailed z test, that is, a normal approximation
full_summary1 <- lapply(c(2:4),function(x) rbind(output1$coefficients[as.character(x),],
                                                 output1$standard.errors[as.character(x),],
                                                 z_value[as.character(x),],
                                                 p_value[as.character(x),]))
kable(lapply(full_summary1,function(x) {rownames(x) <- c("Coefficient","Std. Errors","z-value","p-value"); x}))
#too many p-values to check simulataneously, let's use deviance test instead



summary(model1)

rawres <- residuals(model1,"resp")

#binned residual plots
binnedplot(x=fitted(model1),y=rawres,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
###########
sub<-which(subset_data[subset_data$Energy==0])


###########
predprobs <- fitted(model1) 

predprobs[1,]
clist=c()
for (i in 1:dim(predprobs)[1]){
  
  clist<-append(clist,match(max(predprobs[i,]),predprobs[i,]))
  
  }
  
subset_data$genre_class[subset_data$Overall_Genre=='Ambient']<-1
subset_data$genre_class[subset_data$Overall_Genre=='Electronic']<-2
subset_data$genre_class[subset_data$Overall_Genre=='Folk']<-3
subset_data$genre_class[subset_data$Overall_Genre=='Funk']<-4
subset_data$genre_class[subset_data$Overall_Genre=='Hiphop']<-5
subset_data$genre_class[subset_data$Overall_Genre=='House']<-6
subset_data$genre_class[subset_data$Overall_Genre=='Indie']<-7
subset_data$genre_class[subset_data$Overall_Genre=='Jazz']<-8
subset_data$genre_class[subset_data$Overall_Genre=='Metal']<-9
subset_data$genre_class[subset_data$Overall_Genre=='Pop']<-10
subset_data$genre_class[subset_data$Overall_Genre=='Punk']<-11
subset_data$genre_class[subset_data$Overall_Genre=='Rap']<-12
subset_data$genre_class[subset_data$Overall_Genre=='Rock']<-13
subset_data$genre_class[subset_data$Overall_Genre=='Techno']<-14

a<-confusionMatrix(factor(clist), factor(subset_data$genre_class))

a1<-a$byClass

 ###### Diagnostics
####diagnostics comparing average raw residuals across bins based on predictor values
#for viewcat = 1:  create a raw residual using only the first column of the predicted probabilities
rawresid1 <- (subset_data$Overall_Genre == 'Ambient') -  predprobs[,1]
#for viewcat = 2:  create a raw residual using only the second column of the predicted probabilities
rawresid2 <- (subset_data$Overall_Genre == 'Country') -  predprobs[,2]
#for viewcat = 3:  create a raw residual using only the third column of the predicted probabilities
rawresid3 <- (subset_data$Overall_Genre == 'Electronic') -  predprobs[,3]
#for viewcat = 4:  create a raw residual using only the fourth column of the predicted probabilities
rawresid4 <- (subset_data$Overall_Genre == 'Folk') -  predprobs[,4]
rawresid5 <- (subset_data$Overall_Genre == 'Funk') -  predprobs[,5]
rawresid6 <- (subset_data$Overall_Genre == 'Hiphop') -  predprobs[,6]
rawresid7 <- (subset_data$Overall_Genre == 'House') -  predprobs[,7]
rawresid8 <- (subset_data$Overall_Genre == 'Indie') -  predprobs[,8]
rawresid9 <- (subset_data$Overall_Genre == 'Jazz') -  predprobs[,9]
rawresid10 <- (subset_data$Overall_Genre == 'Metal') -  predprobs[,10]
rawresid11 <- (subset_data$Overall_Genre == 'Pop') -  predprobs[,11]
rawresid12 <- (subset_data$Overall_Genre == 'Punk') -  predprobs[,12]
rawresid13 <- (subset_data$Overall_Genre == 'Rap') -  predprobs[,13]
rawresid14 <- (subset_data$Overall_Genre == 'Rock') -  predprobs[,14]
rawresid15 <- (subset_data$Overall_Genre == 'Techno') -  predprobs[,15]


##can do binned plots for continuous variables
#make a 2 by 2 graphical display

#Danceability
par(mfcol = c(2,2))
binnedplot(subset_data$log_dance, rawresid1, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")

binnedplot(subset_data$log_dance, rawresid2, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$log_dance, rawresid3, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$log_dance, rawresid4, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")

par(mfcol = c(2,2))
binnedplot(subset_data$log_dance, rawresid5, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$log_dance, rawresid6, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$log_dance, rawresid7, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$log_dance, rawresid8, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$log_dance, rawresid9, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$log_dance, rawresid10, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$log_dance, rawresid11, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$log_dance, rawresid12, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$log_dance, rawresid13, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$log_dance, rawresid14, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$log_dance, rawresid15, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")



#Energy
par(mfcol = c(2,2))
binnedplot(subset_data$log_e, rawresid1, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$log_e, rawresid2, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$log_e, rawresid3, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$log_e, rawresid4, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$log_e, rawresid5, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$log_e, rawresid6, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$log_e, rawresid7, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$log_e, rawresid8, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
#U Shaped Curve
binnedplot(subset_data$log_e, rawresid9, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")

binnedplot(subset_data$log_e, rawresid10, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")

binnedplot(subset_data$log_e, rawresid11, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$log_e, rawresid12, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,1))
binnedplot(subset_data$log_e, rawresid13, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$log_e, rawresid14, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")


#Loudness
par(mfcol = c(2,2))
binnedplot(subset_data$Loudness, rawresid1, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Loudness, rawresid2, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Loudness, rawresid3, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Loudness, rawresid4, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Loudness, rawresid5, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Loudness, rawresid6, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Loudness, rawresid7, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Loudness, rawresid8, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
#U Shaped Curve
binnedplot(subset_data$Loudness, rawresid9, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Loudness, rawresid10, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Loudness, rawresid11, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Loudness, rawresid12, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Loudness, rawresid13, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Loudness, rawresid14, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Loudness, rawresid15, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")


#Speechness
par(mfcol = c(2,2))
binnedplot(subset_data$Speechness, rawresid1, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Speechness, rawresid2, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Speechness, rawresid3, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Speechness, rawresid4, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Speechness, rawresid5, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Speechness, rawresid6, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Speechness, rawresid7, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Speechness, rawresid8, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))

binnedplot(subset_data$Speechness, rawresid9, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Speechness, rawresid10, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Speechness, rawresid11, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Speechness, rawresid12, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Speechness, rawresid13, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Speechness, rawresid14, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Speechness, rawresid15, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")



#Acousticness
par(mfcol = c(2,2))
binnedplot(subset_data$Acousticness, rawresid1, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Acousticness, rawresid2, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Acousticness, rawresid3, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Acousticness, rawresid4, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Acousticness, rawresid5, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Acousticness, rawresid6, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Acousticness, rawresid7, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Acousticness, rawresid8, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))

binnedplot(subset_data$Acousticness, rawresid9, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Acousticness, rawresid10, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Acousticness, rawresid11, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Acousticness, rawresid12, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Acousticness, rawresid13, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Acousticness, rawresid14, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Acousticness, rawresid15, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")

#Instrumentalness
par(mfcol = c(2,2))
binnedplot(subset_data$Instrumentalness, rawresid1, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Instrumentalness, rawresid2, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Instrumentalness, rawresid3, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Instrumentalness, rawresid4, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Instrumentalness, rawresid5, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Instrumentalness, rawresid6, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Instrumentalness, rawresid7, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Instrumentalness, rawresid8, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))

binnedplot(subset_data$Instrumentalness, rawresid9, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Instrumentalness, rawresid10, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Instrumentalness, rawresid11, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(subset_data$Instrumentalness, rawresid12, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(subset_data$Instrumentalness, rawresid13, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(subset_data$Instrumentalness, rawresid14, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(subset_data$Instrumentalness, rawresid15, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")


###################################
install.packages("party")
install.packages("randomForest")
library(party)
library(randomForest)


rf_output <- randomForest(Overall_Genre ~ Key + Mode + Danceability + Energy + Loudness+Speechness+
                                Acousticness+Instrumentalness+Valence+Tempo + Duration_ms, 
                              data = subset_data)

varImpPlot(rf_output)
importance(rf_output)

## Confusion matrix
Conf_mat_rf <- confusionMatrix(predict(rf_output,type="response"),
                               as.factor(subset_data$Overall_Genre),positive = "1")
Conf_mat_rf$table #compare to Conf_mat$table
Conf_mat_rf$overall["Accuracy"]
Conf_mat_rf$byClass[c("Sensitivity","Specificity")]
ew_data<-data.frame(
  Danceability = c (0.563), 
  Energy = c (0.648), 
  Loudness = c (-13.441), 
  Acousticness = c (0.529),
  Instrumentalness= c(0.882),
  Speechness = c (0.0284), 
  Valence = c (0.685), 
  Tempo = c (80), 
  Duration_ms = c (273), 
  Key = c (9), 
  Mode = c (1),
  time_signature= c(4))
a<-predict(rf_output,ew_data)
a
## ROC curve

print(output.forest)
print(importance(output.forest,type = 2)) 



saveRDS(rf_output, "final_model.rds")

# later...

# load the model
super_model <- readRDS("final_model.rds")
print(super_model)
# make a predictions on "new data" using the final model
final_predictions <- predict(super_model,)
confusionMatrix(final_predictions, validation$Class)


###################################

library(caret)
subset_data<-subset(subset_data, select=-c(Name,Genre,Uri,Liveness))

train.index <- createDataPartition(subset_data$Overall_Genre, p = .7, list = FALSE)
train <- subset_data[ train.index,]
test  <- subset_data[-train.index,]

###### Boosting



require(verification)
?roc.area

k=10

n=floor(nrow(train)/k)

err.vect = rep( NA,k)

i=1
for(i in 1:k){
s1= ((i-1)* n+1)
s2 = (i*n)
subset=s1:s2

cv.train=train[-subset,]
cv.test=train[subset,]

ars_boost <-  gbm(Overall_Genre ~ Key + Mode + Danceability + Energy + Loudness+Speechness+
                    Acousticness+Instrumentalness+Valence+Tempo + time_signature +
                    Duration_ms,data=cv.train, shrinkage =0.005,
                  n.trees=100, interaction.depth=2)

prediction = predict(ars_boost, cv.test[,c(1:12)],n.trees=500)

err.vect[i]= roc.area(cv.test[,c(13)], prediction)$A
print(paste("AUC for fold", i, ":", err.vect[i]))
}

print(paste("Average AUC:", mean(err.vect)))


##########

library(gbm)
ars_boost <-  gbm(Overall_Genre ~ Key + Mode + Danceability + Energy + Loudness+Speechness+
                    Acousticness+Instrumentalness+Valence+Tempo + time_signature +
                    Duration_ms,data=subset_data,
                  n.trees=150, interaction.depth=2)


summary(ars_boost)

## Confusion matrix


pred_prob_boost <- predict(ars_boost,subset_data,n.trees=5000,type="response")
labels = colnames(pred_prob_boost)[apply(pred_prob_boost, 1, which.max)]

Conf_boost <- confusionMatrix(as.factor(labels),
                              as.factor(subset_data$Overall_Genre))


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

#########################################
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
}




#################

folkrds <- readRDS("./Models/Folk_boost.rds")
a<-predict(folkrds,ew_data)
labels12 = colnames(a)[apply(a, 1, which.max)]
labels43
