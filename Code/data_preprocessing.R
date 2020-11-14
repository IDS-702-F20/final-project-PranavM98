
library(dplyr)
library(ggplot2)

data1<-read.csv('data-2.csv')
data2 <- read.delim("~/Desktop/Duke/702/Final Project/songDb.tsv", comment.char="#")

data2$Tempo<-as.numeric(data2$Tempo)
properties<-data.frame("Variable"=c("Name","Danceability"  ,   "Energy"    ,       "Key"       ,       "Loudness",
                             "Mode"     ,        "Speechness"      , "Acousticness"  ,   "Instrumentalness" ,
                             "Liveness"    ,     "Valence"      ,    "Tempo"       ,  "Duration_ms"   ,   "Time_Signature" ,  "Genre"),
                "Description"=c("Song Name",
                                "Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. ",
                                "Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity.",
                                "The estimated overall key of the track.",
                                "The overall loudness of a track in decibels (dB).",
                                "Mode indicates the modality (major or minor) of a track.",
                                "Speechiness detects the presence of spoken words in a track.",
                                "A confidence measure from 0.0 to 1.0 of whether the track is acoustic.",
                                "Predicts whether a track contains no vocals.",
                                "Detects the presence of an audience in the recording.",
                                "A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track.",
                                "The overall estimated tempo of a track in beats per minute (BPM).",
                                "The duration of the track in milliseconds.",
                                "An estimated overall time signature of a track.",
                                "Genre of the Song"))


####### Implementing Merging Year on ID

merged<-merge(x=data2,y=data1,by.x="ID",by.y="id",all.x=TRUE)



library(tidyr)
merged<-merged %>% drop_na()



#####


#NEW DATA has genres that have atleast 100 songs
#new_data<-data2[data2$Genre %in% names(table(data2$Genre))[table(data2$Genre)>=100],]
new_data<-data2

#DIFFERENT TYPES OF METAL SONGS
metalist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('metal', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    metalist<-append(metalist,new_data[i,'Genre'])
    }
}
metalist<-unique(metalist)

Metal<-data2[data2$Genre %in% metalist,]

Metal$index<-c(1:dim(Metal)[1])
Metal$Genre<-factor(Metal$Genre)


#Groove - Trash



### CLASSICAL


classicalist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('classical', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    classicalist<-append(classicalist,new_data[i,'Genre'])

  }
}

classicalist<-append(classicalist,"orchestralperformance")
classicalist<-unique(classicalist)


Classical<-data2[data2$Genre %in% classicalist,]
Classical$index<-c(1:dim(Classical)[1])
Classical$Genre<-factor(Classical$Genre)


### Pop


poplist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('pop', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    poplist<-append(poplist,new_data[i,'Genre'])
  }
}
poplist<-unique(poplist)
poplist<-append(poplist,'skyroom')
Pop<-data2[data2$Genre %in% poplist,]
#Classical$index<-c(1:dim(Classical)[1])
Pop$Genre<-factor(Pop$Genre)




### Folk


folklist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('folk', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    folklist<-append(folklist,new_data[i,'Genre'])
  }
}
folklist<-unique(folklist)

Folk<-data2[data2$Genre %in% folklist,]
#Classical$index<-c(1:dim(Classical)[1])
Folk$Genre<-factor(Folk$Genre)

### Rock

rocklist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('rock', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    rocklist<-append(rocklist,new_data[i,'Genre'])
  }
}
rocklist<-unique(rocklist)

Rock<-data2[data2$Genre %in% rocklist,]
#Classical$index<-c(1:dim(Classical)[1])
Rock$Genre<-factor(Rock$Genre)



### Indie

indielist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('indie', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    indielist<-append(indielist,new_data[i,'Genre'])
  }
}
indielist<-unique(indielist)

Indie<-data2[data2$Genre %in% indielist,]
#Classical$index<-c(1:dim(Classical)[1])
Indie$Genre<-factor(Indie$Genre)



### Hiphop


hiphoplist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('hiphop', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    hiphoplist<-append(hiphoplist,new_data[i,'Genre'])
  }
}
hiphoplist<-unique(hiphoplist)
HipHop<-data2[data2$Genre %in% hiphoplist,]
#Classical$index<-c(1:dim(Classical)[1])
HipHop$Genre<-factor(HipHop$Genre)



### R&B


rblist=c()


for (i in (1:dim(new_data)[1])){
  flag<-regexpr('r&b', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    rblist<-append(rblist,new_data[i,'Genre'])
  }
}

rblist<-unique(rblist)
RB<-data2[data2$Genre %in% rblist,]
#Classical$index<-c(1:dim(Classical)[1])
RB$Genre<-factor(RB$Genre)




### Jazz
jazzlist=c()
for (i in (1:dim(new_data)[1])){
  flag<-regexpr('jazz', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    jazzlist<-append(jazzlist,new_data[i,'Genre'])
  }
}
jazzlist<-unique(jazzlist)


Jazz<-data2[data2$Genre %in% jazzlist,]
#Classical$index<-c(1:dim(Classical)[1])
Jazz$Genre<-factor(Jazz$Genre)

### Blues
blueslist=c()
for (i in (1:dim(new_data)[1])){
  flag<-regexpr('blues', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    blueslist<-append(blueslist,new_data[i,'Genre'])
  }
}
blueslist<-unique(blueslist)


Blues<-data2[data2$Genre %in% blueslist,]
#Classical$index<-c(1:dim(Classical)[1])
Blues$Genre<-factor(Blues$Genre)



### House

houselist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('house', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    houselist<-append(houselist,new_data[i,'Genre'])
  }
}
houselist<-unique(houselist)


House<-data2[data2$Genre %in% houselist,]
#Classical$index<-c(1:dim(Classical)[1])
House$Genre<-factor(House$Genre)


######Dubstep 

dubsteplist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('dubstep', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    dubsteplist<-append(dubsteplist,new_data[i,'Genre'])
  }
}
dubsteplist<-unique(dubsteplist)


Dubstep<-data2[data2$Genre %in% dubsteplist,]
#Classical$index<-c(1:dim(Classical)[1])
Dubstep$Genre<-factor(Dubstep$Genre)


###### Punk

punklist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('punk', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    punklist<-append(punklist,new_data[i,'Genre'])
  }
}
punklist<-unique(punklist)


Punk<-data2[data2$Genre %in% punklist,]
#Classical$index<-c(1:dim(Classical)[1])
Punk$Genre<-factor(Punk$Genre)



###### EDM

edmlist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('edm', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    edmlist<-append(edmlist,new_data[i,'Genre'])
  }
}
edmlist<-unique(edmlist)


EDM<-data2[data2$Genre %in% edmlist,]
#Classical$index<-c(1:dim(Classical)[1])
EDM$Genre<-factor(EDM$Genre)

###### Country

countrylist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('country', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    countrylist<-append(countrylist,new_data[i,'Genre'])
  }
}
countrylist<-unique(countrylist)
countrylist<-append(countrylist,'alternativeamericana')

Country<-data2[data2$Genre %in% countrylist,]
#Classical$index<-c(1:dim(Classical)[1])
Country$Genre<-factor(Country$Genre)


#### Soundtrack


soundtracklist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('soundtrack', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    soundtracklist<-append(soundtracklist,new_data[i,'Genre'])
  }
}
soundtracklist<-unique(soundtracklist)


Soundtrack<-data2[data2$Genre %in% soundtracklist,]
#Classical$index<-c(1:dim(Classical)[1])
Soundtrack$Genre<-factor(Soundtrack$Genre)


####### Techno

technolist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('techno', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    technolist<-append(technolist,new_data[i,'Genre'])
  }
}
technolist<-unique(technolist)


Techno<-data2[data2$Genre %in% technolist,]
#Classical$index<-c(1:dim(Classical)[1])
Techno$Genre<-factor(Techno$Genre)

####### Rap

raplist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('rap', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    raplist<-append(raplist,new_data[i,'Genre'])
  }
}
raplist<-unique(raplist)


Rap<-data2[data2$Genre %in% raplist,]
#Classical$index<-c(1:dim(Classical)[1])
Rap$Genre<-factor(Rap$Genre)


## Renaissance

renaissancelist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('renaissance', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    renaissancelist<-append(renaissancelist,new_data[i,'Genre'])
  }
}
renaissancelist<-unique(renaissancelist)

Renaissance<-data2[data2$Genre %in% renaissancelist,]
#Classical$index<-c(1:dim(Classical)[1])
Renaissance$Genre<-factor(Renaissance$Genre)


## acappella

acappellalist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('acappella', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    acappellalist<-append(acappellalist,new_data[i,'Genre'])
  }
}
acappellalist<-unique(acappellalist)

Acappella<-data2[data2$Genre %in% acappellalist,]
#Classical$index<-c(1:dim(Classical)[1])
Acappella$Genre<-factor(Acappella$Genre)

## reggae


reggaelist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('reggae', new_data[i,'Genre'], ignore.case ="True")
  if (flag!=-1){
    reggaelist<-append(reggaelist,new_data[i,'Genre'])
  }
}
reggaelist<-unique(reggaelist)

Reggae<-data2[data2$Genre %in% reggaelist,]
#Classical$index<-c(1:dim(Classical)[1])
Reggae$Genre<-factor(Reggae$Genre)

## Electronic

electroniclist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('electronic', new_data[i,'Genre'], ignore.case ="True")
  flag1<-regexpr('electro', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1 | flag1!=-1){
    electroniclist<-append(electroniclist,new_data[i,'Genre'])
  }
}
electroniclist<-unique(electroniclist)


Electronic<-data2[data2$Genre %in% electroniclist,]
#Classical$index<-c(1:dim(Classical)[1])
Electronic$Genre<-factor(Electronic$Genre)

## Worship


worshiplist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('worship', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    worshiplist<-append(worshiplist,new_data[i,'Genre'])
  }
}
worshiplist<-unique(worshiplist)

Worship<-data2[data2$Genre %in% worshiplist,]
#Classical$index<-c(1:dim(Classical)[1])
Worship$Genre<-factor(Worship$Genre)


## Funk


funklist=c()





for (i in (1:dim(new_data)[1])){
  flag<-regexpr('funk', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    funklist<-append(funklist,new_data[i,'Genre'])
  }
}
funklist<-unique(funklist)
funklist<-append(funklist,'go-go')

Funk<-data2[data2$Genre %in% funklist,]
#Classical$index<-c(1:dim(Classical)[1])
Funk$Genre<-factor(Funk$Genre)




## Dance


dancelist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('dance', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    dancelist<-append(dancelist,new_data[i,'Genre'])
  }
}
dancelist<-unique(dancelist)

Dance<-data2[data2$Genre %in% dancelist,]
#Classical$index<-c(1:dim(Classical)[1])
Dance$Genre<-factor(Dance$Genre)



## Reading


readinglist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('read', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    readinglist<-append(readinglist,new_data[i,'Genre'])
  }
}
readinglist<-unique(readinglist)

Reading<-data2[data2$Genre %in% readinglist,]
#Classical$index<-c(1:dim(Classical)[1])
Reading$Genre<-factor(Reading$Genre)


## Beats


beatslist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('beat', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    beatslist<-append(beatslist,new_data[i,'Genre'])
  }
}
beatslist<-unique(beatslist)

Beats<-data2[data2$Genre %in% beatslist,]
#Classical$index<-c(1:dim(Classical)[1])
Beats$Genre<-factor(Beats$Genre)



## Gospel


gospelist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('gospel$', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    gospelist<-append(gospelist,new_data[i,'Genre'])
  }
}
gospelist<-unique(gospelist)

Gospel<-data2[data2$Genre %in% gospelist,]
#Classical$index<-c(1:dim(Classical)[1])
Gospel$Genre<-factor(Gospel$Genre)




## Early Music

earlymusiclist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('earlymusic', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    earlymusiclist<-append(earlymusiclist,new_data[i,'Genre'])
  }
}
earlymusiclist<-unique(earlymusiclist)

EarlyMusic<-data2[data2$Genre %in% earlymusiclist,]
#Classical$index<-c(1:dim(Classical)[1])
EarlyMusic$Genre<-factor(EarlyMusic$Genre)


## Salsa

salsalist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('salsa', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    salsalist<-append(salsalist,new_data[i,'Genre'])
  }
}
salsalist<-unique(salsalist)

Salsa<-data2[data2$Genre %in% salsalist,]
#Classical$index<-c(1:dim(Classical)[1])
Salsa$Genre<-factor(Salsa$Genre)



## Choir

choirlist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('choir', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    choirlist<-append(choirlist,new_data[i,'Genre'])
  }
}
choirlist<-unique(choirlist)

Choir<-data2[data2$Genre %in% choirlist,]
#Classical$index<-c(1:dim(Classical)[1])
Choir$Genre<-factor(Choir$Genre)


## Trance

trancelist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('trance', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    trancelist<-append(trancelist,new_data[i,'Genre'])
  }
}
trancelist<-unique(trancelist)

Trance<-data2[data2$Genre %in% trancelist,]
#Classical$index<-c(1:dim(Classical)[1])
Trance$Genre<-factor(Trance$Genre)


## Ambient

ambientlist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('ambient', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    ambientlist<-append(ambientlist,new_data[i,'Genre'])
  }
}

ambientlist<-unique(ambientlist)
ambientlist<-append(ambientlist,'deepchill-out')
Ambient<-data2[data2$Genre %in% ambientlist,]
#Classical$index<-c(1:dim(Classical)[1])
Ambient$Genre<-factor(Ambient$Genre)

## Emo

emolist=c()

for (i in (1:dim(new_data)[1])){
  flag<-regexpr('emo$', new_data[i,'Genre'], ignore.case ="True")
  
  if (flag!=-1){
    emolist<-append(emolist,new_data[i,'Genre'])
  }
}

emolist<-unique(emolist)

Emo<-data2[data2$Genre %in% emolist,]
#Classical$index<-c(1:dim(Classical)[1])
Emo$Genre<-factor(Emo$Genre)


########### Common Genres


##Combined

indierocklist<-c("belgianindierock","indiegaragerock","deepindierock",     
                 "indieemorock","brazilianindierock","britishindierock",  
                 "danishpoprock")


IndieRock<-data2[data2$Genre %in% indierocklist,]
#Classical$index<-c(1:dim(Classical)[1])
IndieRock$Genre<-factor(IndieRock$Genre)

indiepoplist<-c("swedishindiepop","danishindiepop","italianindiepop","indiepoptimism")

IndiePop<-data2[data2$Genre %in% indiepoplist,]
#Classical$index<-c(1:dim(Classical)[1])
IndiePop$Genre<-factor(IndiePop$Genre)

bluesrocklist<-c("blues-rock","psychedelicblues-rock")

BluesRock<-data2[data2$Genre %in% bluesrocklist,]
#Classical$index<-c(1:dim(Classical)[1])
BluesRock$Genre<-factor(BluesRock$Genre)

poprocklist<-c('danishpoprock')

PopRock<-data2[data2$Genre %in% poprocklist,]
#Classical$index<-c(1:dim(Classical)[1])
PopRock$Genre<-factor(PopRock$Genre)

folkpoplist<-c("frenchfolkpop")

FolkPop<-data2[data2$Genre %in% folkpoplist,]
#Classical$index<-c(1:dim(Classical)[1])
FolkPop$Genre<-factor(FolkPop$Genre)

indiefolklist<-c("australianindiefolk")

IndieFolk<-data2[data2$Genre %in% indiefolklist,]
#Classical$index<-c(1:dim(Classical)[1])
IndieFolk$Genre<-factor(IndieFolk$Genre)

indielist<-indielist[!indielist %in% indierocklist]
indielist<-indielist[!indielist %in% indiepoplist]
rocklist<-rocklist[!rocklist %in% indierocklist]
poplist<-poplist[!poplist %in% indiepoplist]

blueslist<-blueslist[!blueslist %in% bluesrocklist]
rocklist<-rocklist[!rocklist %in% bluesrocklist ]

poplist <-poplist[!poplist %in% poprock]
rocklist <-rocklist[!rocklist %in% poprock]

folklist <-folklist[!folklist %in% folkpop]
poplist <-poplist[!poplist %in% folkpop]

folklist <-folklist[!folklist %in% indiefolk]
indielist<-indielist[!indielist %in% indiefolk]




Genres_list=c('blueslist','classicalist','folklist','hiphoplist','houselist',
              'indielist','jazzlist','metalist','poplist','rblist','rocklist',
              'punklist','dubsteplist','soundtracklist','edmlist','countrylist',
              'technolist','raplist','electroniclist','indierocklist','indiepoplist',
              'bluesrocklist','poprocklist','folkpoplist','indiefolklist','renaissancelist','acappellalist',
              'reggaelist','worshiplist','funklist','dancelist','readinglist','beatslist',
              'gospelist','earlymusiclist','salsalist','choirlist','trancelist',
              'ambientlist','emolist')


Genres_list=c(blueslist,classicalist,folklist,hiphoplist,houselist,
              indielist,jazzlist,metalist,poplist,rblist,rocklist,
              punklist,dubsteplist,soundtracklist,edmlist,countrylist,
              technolist,raplist,electroniclist,indierocklist,indiepoplist,
              bluesrocklist,poprocklist,folkpoplist,indiefolklist,renaissancelist,
              acappellalist,reggaelist,worshiplist,funklist,dancelist,readinglist,
              beatslist,gospelist,earlymusiclist,salsalist,choirlist,trancelist,
              ambientlist)

#print(intersect(poplist,folklist))

gl=c()
for (i in Genres_list){
  gl<-append(gl,i)
}



test<-data2[!data2$Genre %in% gl,]




##TEsting Plots
all<-Rap
ggplot(aes(x=Genre,y=Danceability,color=Genre),data=all)+geom_boxplot()
ggplot(aes(x=Genre,y=Energy,color=Genre),data=all)+geom_boxplot()
ggplot(aes(x=Genre,y=Loudness,color=Genre),data=all)+geom_boxplot()
ggplot(aes(x=Genre,y=Speechness,color=Genre),data=all)+geom_boxplot()
ggplot(aes(x=Genre,y=Instrumentalness,color=Genre),data=all)+geom_boxplot()
ggplot(aes(x=Genre,y=Tempo,color=Genre),data=all)+geom_boxplot()
ggplot(aes(x=Genre,y=Duration_ms,color=Genre),data=all)+geom_boxplot()


###

Genres_list=c('blueslist','classicalist','folklist','hiphoplist','houselist',
              'indielist','jazzlist','metalist','poplist','rblist','rocklist',
              'punklist','dubsteplist','soundtracklist','edmlist','countrylist',
              'technolist','raplist','electroniclist','indierocklist','indiepoplist',
              'bluesrocklist','poprocklist','folkpoplist','indiefolklist','renaissancelist','acappellalist',
              'reggaelist','worshiplist','funklist','dancelist','readinglist','beatslist',
              'gospelist','earlymusiclist','salsalist','choirlist','trancelist',
              'ambientlist','emolist')


data2$Overall_Genre[data2$Genre %in% blueslist]<-"Blues"
data2$Overall_Genre[data2$Genre %in% classicalist]<-"Classical"
data2$Overall_Genre[data2$Genre %in% folklist]<-"Folk"
data2$Overall_Genre[data2$Genre %in% hiphoplist]<-"Hiphop"
data2$Overall_Genre[data2$Genre %in% houselist]<-"House"

data2$Overall_Genre[data2$Genre %in% indielist]<-"Indie"
data2$Overall_Genre[data2$Genre %in% jazzlist]<-"Jazz"
data2$Overall_Genre[data2$Genre %in% metalist]<-"Metal"
data2$Overall_Genre[data2$Genre %in% poplist]<-"Pop"
data2$Overall_Genre[data2$Genre %in% rblist]<-"R&B"
data2$Overall_Genre[data2$Genre %in% rocklist]<-"Rock"

data2$Overall_Genre[data2$Genre %in% punklist]<-"Punk"
data2$Overall_Genre[data2$Genre %in% dubsteplist]<-"Dubstep"
data2$Overall_Genre[data2$Genre %in% soundtracklist]<-"Soundtrack"
data2$Overall_Genre[data2$Genre %in% edmlist]<-"EDM"
data2$Overall_Genre[data2$Genre %in% countrylist]<-"Country"

data2$Overall_Genre[data2$Genre %in% technolist]<-"Techno"
data2$Overall_Genre[data2$Genre %in% raplist]<-"Rap"
data2$Overall_Genre[data2$Genre %in% electroniclist]<-"Electronic"
data2$Overall_Genre[data2$Genre %in% indierocklist]<-"IndieRock"
data2$Overall_Genre[data2$Genre %in% indiepoplist]<-"IndiePop"

data2$Overall_Genre[data2$Genre %in% bluesrocklist]<-"BluesRock"
data2$Overall_Genre[data2$Genre %in% poprocklist]<-"PopRock"
data2$Overall_Genre[data2$Genre %in% folkpoplist]<-"FolkPop"
data2$Overall_Genre[data2$Genre %in% indiefolklist]<-"IndieFolk"
data2$Overall_Genre[data2$Genre %in% renaissancelist]<-"Renaissance"
data2$Overall_Genre[data2$Genre %in% acappellalist]<-"Acappella"

data2$Overall_Genre[data2$Genre %in% reggaelist]<-"Reggae"
data2$Overall_Genre[data2$Genre %in% worshiplist]<-"Worship"
data2$Overall_Genre[data2$Genre %in% funklist]<-"Funk"
data2$Overall_Genre[data2$Genre %in% dancelist]<-"Dance"
data2$Overall_Genre[data2$Genre %in% readinglist]<-"Reading"
data2$Overall_Genre[data2$Genre %in% beatslist]<-"Beats"

data2$Overall_Genre[data2$Genre %in% gospelist]<-"Gospel"
data2$Overall_Genre[data2$Genre %in% earlymusiclist]<-"EarlyMusic"
data2$Overall_Genre[data2$Genre %in% salsalist]<-"Salsa"
data2$Overall_Genre[data2$Genre %in% choirlist]<-"Choir"
data2$Overall_Genre[data2$Genre %in% trancelist]<-"Trance"
data2$Overall_Genre[data2$Genre %in% ambientlist]<-"Ambient"
data2$Overall_Genre[data2$Genre %in% emolist]<-"Emo"

final<-data2
final<-na.omit(final)


write.csv(final,"cleaned_data.csv", row.names = FALSE)
library(dplyr)
library(tidyr)

ggplot(aes(x=Overall_Genre,y=Danceability,color=Overall_Genre),data=all)+geom_boxplot()
ggplot(aes(x=Overall_Genre,y=Energy,color=Overall_Genre),data=all)+geom_boxplot()
ggplot(aes(x=Overall_Genre,y=Loudness,color=Overall_Genre),data=all)+geom_boxplot()
ggplot(aes(x=Overall_Genre,y=Speechness,color=Overall_Genre),data=all)+geom_boxplot()
ggplot(aes(x=Overall_Genre,y=Instrumentalness,color=Overall_Genre),data=all)+geom_boxplot()
ggplot(aes(x=Overall_Genre,y=Tempo,color=Overall_Genre),data=all)+geom_boxplot()
ggplot(aes(x=Overall_Genre,y=Duration_ms,color=Overall_Genre),data=all)+geom_boxplot()

