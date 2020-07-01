

rm(list=ls())

setwd("F:/R")
getwd()

FinalProject="cis8392-selva-project-flickr"

devtools::install_github("cloudyr/RoogleVision")
devtools::install_github("cloudyr/googleComputeEngineR")
library(tidyverse)
library(bigQueryR)
library(googleCloudStorageR)
library(googleLanguageR)
library(RoogleVision)
library(googleComputeEngineR)

library(googleCloudStorageR)
Sys.setenv("GCS_AUTH_FILE" = "gcp-service-account-key_finalproject.json")
options("googleAuthR.scopes.selected" =
          "https://www.googleapis.com/auth/cloud-platform")
proj_id = googleAuthR::gar_set_client("gcp-auth_finalproject.json")
gcs_auth("gcp-service-account-key_finalproject.json")
buckets <- gcs_list_buckets(proj_id)
buckets

gcs_global_bucket("cis8392-selva-project-flickr")

objects <- gcs_list_objects(bucket="cis8392-selva-project-flickr") # manually specify bucket
objects <- gcs_list_objects() # from the default bucket
class(objects)
objects$name
objects %>% as_tibble() # pretty print


nlp_result <- gl_nlp(text)
str(nlp_result, max.level = 2)
glimpse(nlp_result$sentences[[1]]) #View(nlp_result$sentences[[1]])##
nlp_result$sentences[[1]]
glimpse(nlp_result$tokens[[1]]) #View(nlp_result$tokens[[1]])

library(googleLanguageR)
text <- read_file("F:/R/welcomenote_finalproject.txt")
gl_talk(text, gender = "FEMALE", languageCode = "en-US")

saveRDS("F:/R/WelcomeNote.wav",file='wave.rds')
x=readRDS(file="wave.rds")
x
###########################################################Label detection
#LABEL DETECTION

rm(list=ls())

setwd("F:/R")
getwd()
Sys.setenv("GAR_CLIENT_JSON" = "gcp-service-account-key_finalproject.json")

library(tidyverse)
library(leaflet)
library(jsonlite)
library(EBImage)
library(googleAuthR)
library(RoogleVision)
options("googleAuthR.scopes.selected" =
          "https://www.googleapis.com/auth/cloud-platform")
gar_set_client("gcp-auth_finalproject.json")
gcs_auth("gcp-service-account-key_finalproject.json")


l <- getGoogleVisionResponse(
  imagePath=paste0("F:/R/Flickr_Images/pop/pop.1.jpg"), numResults=5,
  feature="LABEL_DETECTION")
l

l$description[1]
l$description[2]
l$description[3]
l$description[4]
l$description[5]




df.captions = data.frame( "photoid"="pop1.jpg",
                          "c1"=l$description[1],
                          "c2"=l$description[2],
                          "c3"=l$description[3],
                          "c4"=l$description[4],
                          "c5"=l$description[5],stringsAsFactors = FALSE)
df.captions
#str(df.captions)
len=length(list.files("F:/R/Flickr_Images/pop"))
len
for (i in 1:5000){
      l <- getGoogleVisionResponse(
        imagePath=paste0("F:/R/Flickr_Images/pop/pop.",i,".jpg"), numResults=5,
        feature="LABEL_DETECTION")

      df.captions=rbind(df.captions,c(paste0("pop",i,".jpg"),
                        l$description[1],
                        l$description[2],
                        l$description[3],
                        l$description[4],
                        l$description[5]))
            
      }

df.captions
saveRDS(df.captions,file='labels.rds')
x=readRDS(file="labels.rds")
x$c1

write.csv(df.captions,file='df.captions5k.csv')

library(tidytext)
df.captions$c1
library(wordcloud)
pal <- brewer.pal(8,"Dark2")

with(wordcloud(df.captions$c1,random.order = FALSE,max.words = 500,colors = pal))




###########################################################
#Face Detection

f <- getGoogleVisionResponse(
  imagePath=paste0("F:/R/Flickr_Images/pop/pop.29.jpg"), numResults=5,
  feature="FACE_DETECTION")
f
f$detectionConfidence
!(is.null(f))
#for(fa in 1: length(f$boundingPoly$vertices)){
#  a=f$boundingPoly$vertices[[f]]$x
#  b=f$boundingPoly$vertices[[f]]$y
#  polygon(x=a,y=b,border = "green",lwd=5)
#}

colnames(f)
f[7:15]

n=length(colnames(f))
n
f$detectionConfidence
condition=(!(is.null(f)))
df.faces=""

if(condition){
df.faces = data.frame("photoid"="pop.29.jpg",
  detectionConfidence=f$detectionConfidence,
  landmarkingConfidence=f$landmarkingConfidence,
  joyLikelihood=f$joyLikelihood,
  sorrowLikelihood=f$sorrowLikelihood,
  angerLikelihood=f$angerLikelihood,
  surpriseLikelihood=f$surpriseLikelihood,
  underExposedLikelihood=f$underExposedLikelihood,
  blurredLikelihood=f$blurredLikelihood,
  headwearLikelihood=f$headwearLikelihood,
  stringsAsFactors = FALSE)
}
  
  

df.faces

str(df.faces)
condition=FALSE
for(i in 30:500){
  f <- getGoogleVisionResponse(
    imagePath=paste0("F:/R/Flickr_Images/pop/pop.",i,".jpg"), numResults=5,
    feature="FACE_DETECTION")
  
  condition=(!(is.null(f)))
  
  if(!(is.null(f$detectionConfidence))){df.faces<-rbind(df.faces,c(paste0("pop",i,".jpg"),
               f$detectionConfidence,
               f$landmarkingConfidence,
               f$joyLikelihood,
               f$sorrowLikelihood,
               f$angerLikelihood,
               f$surpriseLikelihood,
               f$underExposedLikelihood,
               f$blurredLikelihood,
               f$headwearLikelihood))
  condition=FALSE}
  }
  
df.faces
saveRDS(df.faces,file='faces.rds')
y=readRDS(file="faces.rds")
y


write.csv(df.faces,file='df.faces.csv')
#######################################################################

###############################


###########################################################Label detection
#LABEL DETECTION - Unpopular

rm(list=ls())

setwd("F:/R")
getwd()
Sys.setenv("GAR_CLIENT_JSON" = "gcp-service-account-key_finalproject.json")

library(tidyverse)
library(leaflet)
library(jsonlite)
library(EBImage)
library(googleAuthR)
library(RoogleVision)
options("googleAuthR.scopes.selected" =
          "https://www.googleapis.com/auth/cloud-platform")
gar_set_client("gcp-auth_finalproject.json")
gcs_auth("gcp-service-account-key_finalproject.json")


l <- getGoogleVisionResponse(
  imagePath=paste0("F:/R/Flickr_Images/unpop/unpop.1.jpg"), numResults=5,
  feature="LABEL_DETECTION")
l
#plot(l)
l$description[1]
l$description[2]
l$description[3]
l$description[4]
l$description[5]



df.captions.un = data.frame( "photoid"="unpop1.jpg",
                          "c1"=l$description[1],
                          "c2"=l$description[2],
                          "c3"=l$description[3],
                          "c4"=l$description[4],
                          "c5"=l$description[5],stringsAsFactors = FALSE)
df.captions.un
#str(df.captions)
len=length(list.files("F:/R/Flickr_Images/unpop"))
len
for (i in 1:5000){
  l <- getGoogleVisionResponse(
    imagePath=paste0("F:/R/Flickr_Images/unpop/unpop.",i,".jpg"), numResults=5,
    feature="LABEL_DETECTION")
  
  df.captions.un=rbind(df.captions.un,c(paste0("unpop",i,".jpg"),
                                  l$description[1],
                                  l$description[2],
                                  l$description[3],
                                  l$description[4],
                                  l$description[5]))
  
}

df.captions.un
saveRDS(df.captions.un,file='labels.un.rds')
x1=readRDS(file="labels.un.rds")
x1$c1

write.csv(df.captions.un,file='df.captions5k.un.csv')

library(tidytext)

library(wordcloud)
pal <- brewer.pal(8,"Dark2")

with(wordcloud(df.captions.un$c1,random.order = FALSE,max.words = 500,colors = pal))
with(wordcloud(df.captions.un$c2,random.order = FALSE,max.words = 500,colors = pal))
with(wordcloud(df.captions.un$c3,random.order = FALSE,max.words = 500,colors = pal))
with(wordcloud(df.captions.un$c4,random.order = FALSE,max.words = 500,colors = pal))
with(wordcloud(df.captions.un$c5,random.order = FALSE,max.words = 500,colors = pal))



###########################################################
#Face Detection - unpopular

f <- getGoogleVisionResponse(
  imagePath=paste0("F:/R/Flickr_Images/unpop/unpop.1.jpg"), numResults=5,
  feature="FACE_DETECTION")
f
f$detectionConfidence
!(is.null(f))
#for(fa in 1: length(f$boundingPoly$vertices)){
#  a=f$boundingPoly$vertices[[f]]$x
#  b=f$boundingPoly$vertices[[f]]$y
#  polygon(x=a,y=b,border = "green",lwd=5)
#}
colnames(f)
f[7:15]

n=length(colnames(f))
n
f$detectionConfidence
condition=(!(is.null(f)))
df.faces.un=""

if(condition){
  df.faces.un = data.frame("photoid"="unpop.1.jpg",
                        detectionConfidence=f$detectionConfidence,
                        landmarkingConfidence=f$landmarkingConfidence,
                        joyLikelihood=f$joyLikelihood,
                        sorrowLikelihood=f$sorrowLikelihood,
                        angerLikelihood=f$angerLikelihood,
                        surpriseLikelihood=f$surpriseLikelihood,
                        underExposedLikelihood=f$underExposedLikelihood,
                        blurredLikelihood=f$blurredLikelihood,
                        headwearLikelihood=f$headwearLikelihood,
                        stringsAsFactors = FALSE)
}



df.faces.un

str(df.faces.un)
condition=FALSE
for(i in 2:5){
  f <- getGoogleVisionResponse(
    imagePath=paste0("F:/R/Flickr_Images/unpop/unpop.",i,".jpg"), numResults=5,
    feature="FACE_DETECTION")
  
  condition=(!(is.null(f)))
  
  if(!(is.null(f$detectionConfidence))){df.faces.un<-rbind(df.faces.un,c(paste0("un.pop",i,".jpg"),
                                                                   f$detectionConfidence,
                                                                   f$landmarkingConfidence,
                                                                   f$joyLikelihood,
                                                                   f$sorrowLikelihood,
                                                                   f$angerLikelihood,
                                                                   f$surpriseLikelihood,
                                                                   f$underExposedLikelihood,
                                                                   f$blurredLikelihood,
                                                                   f$headwearLikelihood))
  condition=FALSE}
}

df.faces.un
saveRDS(df.faces.un,file='faces.rds')
y=readRDS(file="faces.un.rds")
y


write.csv(df.faces,file='df.faces.csv')


#include till this point. the next one is for sample, trying.. don't submit those until finished.

###################################################

#Experimental - don't include for project YET..... work in progress.

#label
l <- getGoogleVisionResponse(
  imagePath=paste0("F:/R/Flickr_Images/pop/pop.1.jpg"), numResults=5,
  feature="LABEL_DETECTION")
l

tag1=l$description[1]
tag2=l$description[2]
tag3=l$description[3]
tag4=l$description[4]
tag5=l$description[5]

getwd()
library("jpeg")
jj <- readJPEG("Flickr_Images/pop/pop.1.jpg",native=TRUE)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(jj,0,0,1,1)
paste0("Possible tags for image 1 are :",tag1," ",tag2," ",tag3," ",tag4," ",tag5)

#LANDMARK detection
l2 <- getGoogleVisionResponse(
  imagePath=paste0("Flickr_Images/pop/taj-bro.jpg"), numResults=5,
  feature="LANDMARK_DETECTION")
l2
jj <- readJPEG("Flickr_Images/pop/taj-bro.jpg",native=TRUE)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(jj,0,0,1,1)
l <- getGoogleVisionResponse(
  imagePath=paste0("Flickr_Images/pop/taj-bro.jpg"), numResults=5,
  feature="LABEL_DETECTION")
l

tag1=l$description[1]
tag2=l$description[2]
tag3=l$description[3]
tag4=l$description[4]
tag5=l$description[5]

paste0("Possible tags for image 1 are :",tag1," ",tag2," ",tag3," ",tag4," ",tag5)


#Face detection
f <- getGoogleVisionResponse(
  imagePath=paste0("Flickr_Images/pop/pop.544.jpg"), numResults=5,
  feature="FACE_DETECTION")
f
f$detectionConfidence
!(is.null(f))
condition=(!(is.null(f)))
df1.faces=""

if(condition){
  df1.faces = data.frame("photoid"="pop.544.jpg",
                        detectionConfidence=f$detectionConfidence,
                        landmarkingConfidence=f$landmarkingConfidence,
                        joyLikelihood=f$joyLikelihood,
                        sorrowLikelihood=f$sorrowLikelihood,
                        angerLikelihood=f$angerLikelihood,
                        surpriseLikelihood=f$surpriseLikelihood,
                        underExposedLikelihood=f$underExposedLikelihood,
                        blurredLikelihood=f$blurredLikelihood,
                        headwearLikelihood=f$headwearLikelihood,
                        stringsAsFactors = FALSE)
}


jj <- readJPEG("Flickr_Images/pop/pop.544.jpg",native=TRUE)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(jj,0,0,1,1)

df1.faces
##
df1.faces