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

subset_data$Danceabilityc<-subset_data$Danceability-mean(subset_data$Danceability)
subset_data$Energyc<-subset_data$Energy-mean(subset_data$Energy)
subset_data$Loudnessc<-subset_data$Loudness-mean(subset_data$Loudness)
subset_data$Acousticnessc<-subset_data$Acousticness-mean(subset_data$Acousticness)
subset_data$Speechnessc<-subset_data$Speechness-mean(subset_data$Speechness)
subset_data$Instrumentalnessc<-subset_data$Instrumentalness-mean(subset_data$Instrumentalness)
subset_data$Livenessc<-subset_data$Liveness-mean(subset_data$Liveness)
subset_data$Valencec<-subset_data$Valence-mean(subset_data$Valence)



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

model1 <- multinom(Overall_Genre ~ (Danceability + Acousticness) * ( Key + Mode + Danceability + Energy + Loudness+Speechness+
                                                                  Acousticness+Instrumentalness+Valence+time_signature+Tempo),data=dTrain)

model12 <- multinom(Overall_Genre ~  Key + Mode + Danceabilityc + Energyc + Loudnessc+Speechnessc+Duration_ms+
                                                                       Acousticnessc+Instrumentalnessc+Valencec+time_signature+Tempo,data=dTrain)


anova(model1, model12, test = "Chisq")


predicted_scores <- predict (model12, dTest, "probs")

predicted_class <- predict (model12, dTest)


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
predprobs <- fitted(model12) 

predprobs[1,]
clist=c()
for (i in 1:dim(predprobs)[1]){
  
  clist<-append(clist,match(max(predprobs[i,]),predprobs[i,]))
  
  }
  
dTrain$genre_class[dTrain$Overall_Genre=='Ambient']<-1
dTrain$genre_class[dTrain$Overall_Genre=='Electronic']<-2
dTrain$genre_class[dTrain$Overall_Genre=='Folk']<-3
dTrain$genre_class[dTrain$Overall_Genre=='Hiphop']<-4
dTrain$genre_class[dTrain$Overall_Genre=='Jazz']<-5
dTrain$genre_class[dTrain$Overall_Genre=='Metal']<-6
dTrain$genre_class[dTrain$Overall_Genre=='Pop']<-7
dTrain$genre_class[dTrain$Overall_Genre=='Rock']<-8


a<-confusionMatrix(factor(clist), factor(dTrain$genre_class))

a1<-a$byClass

 ###### Diagnostics
####diagnostics comparing average raw residuals across bins based on predictor values
#for viewcat = 1:  create a raw residual using only the first column of the predicted probabilities
rawresid1 <- (dTrain$Overall_Genre == 'Ambient') -  predprobs[,1]
#for viewcat = 2:  create a raw residual using only the second column of the predicted probabilities
#for viewcat = 3:  create a raw residual using only the third column of the predicted probabilities
rawresid2 <- (dTrain$Overall_Genre == 'Electronic') -  predprobs[,2]
#for viewcat = 4:  create a raw residual using only the fourth column of the predicted probabilities
rawresid3 <- (dTrain$Overall_Genre == 'Folk') -  predprobs[,3]
rawresid4 <- (dTrain$Overall_Genre == 'Hiphop') -  predprobs[,4]
rawresid5 <- (dTrain$Overall_Genre == 'Jazz') -  predprobs[,5]
rawresid6 <- (dTrain$Overall_Genre == 'Metal') -  predprobs[,6]
rawresid7 <- (dTrain$Overall_Genre == 'Pop') -  predprobs[,7]
rawresid8 <- (dTrain$Overall_Genre == 'Rock') -  predprobs[,8]





dTrain$genre_class<-factor(dTrain$genre_class)
par(mfrow=c(2,2))
roc(dTrain$genre_class==1,predprobs[,1],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="red3",percent=T,main="Ambient")
roc(dTrain$genre_class==2,predprobs[,2],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="gray3",percent=T,main="Electronic")
roc(dTrain$genre_class==3,predprobs[,3],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="green3",percent=T,main="Folk")
roc(dTrain$genre_class==4,predprobs[,4],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="blue3",percent=T,main="Hip-Hop")
par(mfrow=c(2,2))
roc(dTrain$genre_class==5,predprobs[,5],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="red3",percent=T,main="Jazz")
roc(dTrain$genre_class==6,predprobs[,6],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="gray3",percent=T,main="Metal")
roc(dTrain$genre_class==7,predprobs[,7],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="green3",percent=T,main="Pop")
roc(dTrain$genre_class==8,predprobs[,8],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="blue3",percent=T,main="Rock")


multiclass.roc(dTrain$Overall_Genre,predprobs,plot=T,print.thres="best",legacy.axes=T,print.auc =T,col="red3",percent=T)

##can do binned plots for continuous variables
#make a 2 by 2 graphical display

#Danceability
par(mfcol = c(2,2))
binnedplot(dTrain$Danceability, rawresid1, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: Genre = Ambient")
binnedplot(dTrain$Danceability, rawresid5, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: Genre = Jazz")
binnedplot(dTrain$Acousticness, rawresid6, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: Genre = Metal")
binnedplot(dTrain$Energy, rawresid6, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: Genre = Metal")



binnedplot(dTrain$Danceability, rawresid2, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: Genre = Electronic")
binnedplot(dTrain$Danceability, rawresid3, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: Genre = Folk")
binnedplot(dTrain$Danceability, rawresid4, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: Genre = Hiphop")

par(mfcol = c(2,2))
binnedplot(dTrain$Danceability, rawresid5, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Danceability, rawresid6, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Danceability, rawresid7, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Danceability, rawresid8, xlab = "Danceability", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")



#Energy
par(mfcol = c(2,2))
binnedplot(dTrain$Energy, rawresid1, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Energy, rawresid2, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Energy, rawresid3, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Energy, rawresid4, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(dTrain$Energy, rawresid5, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Energy, rawresid6, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Energy, rawresid7, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Energy, rawresid8, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
#U Shaped Curve
binnedplot(dTrain$log_e, rawresid9, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")

binnedplot(dTrain$log_e, rawresid10, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")

binnedplot(dTrain$log_e, rawresid11, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$log_e, rawresid12, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,1))
binnedplot(dTrain$log_e, rawresid13, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$log_e, rawresid14, xlab = "Energy", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")


#Loudness
par(mfcol = c(2,2))
binnedplot(dTrain$Loudness, rawresid1, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Loudness, rawresid2, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Loudness, rawresid3, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Loudness, rawresid4, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(dTrain$Loudness, rawresid5, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Loudness, rawresid6, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Loudness, rawresid7, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Loudness, rawresid8, xlab = "Loudness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))


#Speechness
par(mfcol = c(2,2))
binnedplot(dTrain$Speechness, rawresid1, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Speechness, rawresid2, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Speechness, rawresid3, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Speechness, rawresid4, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(dTrain$Speechness, rawresid5, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Speechness, rawresid6, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Speechness, rawresid7, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Speechness, rawresid8, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))

binnedplot(dTrain$Speechness, rawresid9, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Speechness, rawresid10, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Speechness, rawresid11, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Speechness, rawresid12, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(dTrain$Speechness, rawresid13, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Speechness, rawresid14, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Speechness, rawresid15, xlab = "Speechness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")



#Acousticness
par(mfcol = c(2,2))
binnedplot(dTrain$Acousticness, rawresid1, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Acousticness, rawresid2, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Acousticness, rawresid3, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Acousticness, rawresid4, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(dTrain$Acousticness, rawresid5, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Acousticness, rawresid6, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Acousticness, rawresid7, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Acousticness, rawresid8, xlab = "Acousticness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")

#Instrumentalness
par(mfcol = c(2,2))
binnedplot(dTrain$Instrumentalness, rawresid1, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Instrumentalness, rawresid2, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Instrumentalness, rawresid3, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Instrumentalness, rawresid4, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(dTrain$Instrumentalness, rawresid5, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Instrumentalness, rawresid6, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Instrumentalness, rawresid7, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Instrumentalness, rawresid8, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))

binnedplot(dTrain$Instrumentalness, rawresid9, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Instrumentalness, rawresid10, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Instrumentalness, rawresid11, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(dTrain$Instrumentalness, rawresid12, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")
par(mfcol = c(2,2))
binnedplot(dTrain$Instrumentalness, rawresid13, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(dTrain$Instrumentalness, rawresid14, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(dTrain$Instrumentalness, rawresid15, xlab = "Instrumentalness", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")


###################################
install.packages("party")
install.packages("randomForest")
library(party)
library(randomForest)


rf_output <- randomForest(Overall_Genre ~ Key + Mode + Danceability + Energy + Loudness+Speechness+
                                Acousticness+Instrumentalness+Valence+Tempo + Duration_ms, 
                              data = dTrain)

varImpPlot(rf_output)
importance(rf_output)

## Confusion matrix
Conf_mat_rf <- confusionMatrix(predict(rf_output,type="response"),
                               as.factor(dTrain$Overall_Genre),positive = "1")
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
