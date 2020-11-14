music<-read.csv('./App/cleaned_data.csv')
library(dplyr)
music<-music %>% distinct()
write.csv(music,'cleaned_data.csv', row.names = FALSE)
music<-subset(music, select=-c(Type,ID,Uri,Ref_Track,URL_features))


music$Duration_ms<-music$Duration_ms/1000
names(music)[names(music) == "Duration_ms"] <- "Duration(seconds)"
mdata<-read.csv('./App/cleaned_data.csv')
mdata<-subset(mdata, select=-c(Type,ID,Ref_Track,URL_features))

#mdata$Overall_Genre<-as.factor(mdata$Overall_Genre)

mdata$Duration_ms<-mdata$Duration_ms/1000
names(mdata)[names(mdata) == "Duration(seconds)"] <- "Duration_seconds"
mdata$Duration_ms<-as.integer(mdata$Duration_ms)

mdata<-mdata[!(mdata$Name=="Mana"),]


lista<-names(which(table(mdata$Overall_Genre)>2000))

mdata<-mdata[mdata$Overall_Genre %in% lista,]

subset_data<-mdata

subset_data<-subset_data[!(subset_data$Genre=="funkrock"),]

subset_data$Overall_Genre[subset_data$Overall_Genre=='House']<-'Electronic'
subset_data$Overall_Genre[subset_data$Overall_Genre=='Techno']<-'Electronic'


lista<-names(which(table(subset_data$Overall_Genre)>2000))

subset_data<-subset_data[subset_data$Overall_Genre %in% lista,]

subset_data$Overall_Genre<-as.factor(subset_data$Overall_Genre)

write.csv(subset_data,'updated_genre.csv')



music<-read.csv('updated_genre.csv')
music$Overall_Genre<-as.factor(music$Overall_Genre)


ggplot(data=music,aes(x=Danceability,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Energy,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Key,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Loudness,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Mode,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=time_signature,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Acousticness,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Instrumentalness,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Liveness,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Valence,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Tempo,y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Duration(seconds),y=Overall_Genre,color=Overall_Genre)) +geom_boxplot()

music <- subset_data %>%
  filter(Overall_Genre=='Folk')
ggplot(data=music,aes(x=Danceability,y=Genre,color=Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Energy,y=Genre,color=Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Key,y=Genre,color=Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Loudness,y=Genre,color=Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Mode,y=Genre,color=Genre)) +geom_boxplot()
ggplot(data=music,aes(x=time_signature,y=Genre,color=Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Acousticness,y=Genre,color=Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Instrumentalness,y=Genre,color=Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Liveness,y=Genre,color=Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Valence,y=Genre,color=Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Tempo,y=Genre,color=Genre)) +geom_boxplot()
ggplot(data=music,aes(x=Duration(seconds),y=Genre,color=Genre)) +geom_boxplot()
