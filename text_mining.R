# Natural Language Processing
#importing the data
install.packages("tm")
library(tm)
install.packages("dplyr")
library(dplyr)
library(textstem)
library(ggplot2)
install.packages("SnowballC")
library(SnowballC)
install.packages("quanteda") 
library(quanteda)
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
maldives <- read.csv("maldives_hotel_reviews.csv",header=T)
names(maldives)
summary(maldives)
summary(maldives$Hotel.Name)

HolidayInn<-subset(maldives,
                   Hotel.Name=="Holiday Inn Resort Kandooma Maldives")
Shangri<-subset(maldives,
                   Hotel.Name=="Shangri-La's Villingili Resort and Spa Maldives")
Adaaran<-subset(maldives,
                Hotel.Name=="Adaaran Select Meedhupparu")
Amari<-subset(maldives,
                Hotel.Name=="Amari Havodda Maldives")
Cinnamon<-subset(maldives,
              Hotel.Name=="Cinnamon Dhonveli Maldives")
FourSeasons<-subset(maldives,
                 Hotel.Name=="Four Seasons Resort Maldives at Landaa Giraavaru")

#Inspect the review column in the datasets
head(HolidayInn$Review)
head(Shangri$Review)
head(Adaaran$Review)
head(Amari$Review)
head(Cinnamon$Review)
head(FourSeasons$Review)

#Create text vectors
review_HolidayInn<-HolidayInn$Review
review_Shangri<-Shangri$Review
review_Adaaran<-Adaaran$Review
review_Amari<-Amari$Review
review_Cinnamon<-Cinnamon$Review
review_FourSeasons<-FourSeasons$Review

#Convert all text to lower case
review_HolidayInn<-tolower(review_HolidayInn)
review_Shangri<-tolower(review_Shangri)
review_Adaaran<-tolower(review_Adaaran)
review_Amari<-tolower(review_Amari)
review_Cinnamon<-tolower(review_Cinnamon)
review_FourSeasons<-tolower(review_FourSeasons)                        
                        
#Remove links from the reviews
review_HolidayInn <- gsub("http\\S+\\s*", "", review_HolidayInn)
review_Shangri <- gsub("http\\S+\\s*", "", review_Shangri)                       
review_Adaaran <- gsub("http\\S+\\s*", "", review_Adaaran)                         
review_Amari <- gsub("http\\S+\\s*", "", review_Amari)                         
review_Cinnamon <- gsub("http\\S+\\s*", "", review_Cinnamon)                       
review_FourSeasons <- gsub("http\\S+\\s*", "", review_FourSeasons)                       
                      
#Remove punctuation from the reviews
review_HolidayInn <- gsub("[[:punct:]]", "", review_HolidayInn)
review_Shangri <- gsub("[[:punct:]]", "", review_Shangri)                       
review_Adaaran <- gsub("[[:punct:]]", "", review_Adaaran)                         
review_Amari <- gsub("[[:punct:]]", "", review_Amari)                         
review_Cinnamon <- gsub("[[:punct:]]", "", review_Cinnamon)                       
review_FourSeasons <- gsub("[[:punct:]]", "", review_FourSeasons)                       

#Remove digits from the reviews
review_HolidayInn <- gsub("[[:digit:]]", "", review_HolidayInn)                        
review_Shangri <- gsub("[[:digit:]]", "", review_Shangri)                       
review_Adaaran <- gsub("[[:digit:]]", "", review_Adaaran)                         
review_Amari <- gsub("[[:digit:]]", "", review_Amari)                         
review_Cinnamon <- gsub("[[:digit:]]", "", review_Cinnamon)                       
review_FourSeasons <- gsub("[[:digit:]]", "", review_FourSeasons)                              

#Remove leading blank spaces at the beginning from the reviews
review_HolidayInn <- gsub("^ ", "", review_HolidayInn)
review_Shangri <- gsub("^ ", "", review_Shangri)                       
review_Adaaran <- gsub("^ ", "", review_Adaaran)                         
review_Amari <- gsub("^ ", "", review_Amari)                         
review_Cinnamon <- gsub("^ ", "", review_Cinnamon)                       
review_FourSeasons <- gsub("^ ", "", review_FourSeasons)                              

#Remove blank spaces at the end from the reviews
review_HolidayInn <- gsub(" $", "", review_HolidayInn)
review_Shangri <- gsub(" $", "", review_Shangri)                       
review_Adaaran <- gsub(" $", "", review_Adaaran)                         
review_Amari <- gsub(" $", "", review_Amari)                         
review_Cinnamon <- gsub(" $", "", review_Cinnamon)                       
review_FourSeasons <- gsub(" $", "", review_FourSeasons)                         

#Remove "hotel", "room", "maldives" and "resort"word from the reviews
review_HolidayInn <- gsub(" hotel", "", review_HolidayInn)
review_Shangri <- gsub(" hotel", "", review_Shangri)                       
review_Adaaran <- gsub(" hotel", "", review_Adaaran)                         
review_Amari <- gsub(" hotel", "", review_Amari)                         
review_Cinnamon <- gsub(" hotel", "", review_Cinnamon)                       
review_FourSeasons <- gsub(" hotel", "", review_FourSeasons)

review_HolidayInn <- gsub(" room", "", review_HolidayInn)
review_Shangri <- gsub(" room", "", review_Shangri)                       
review_Adaaran <- gsub(" room", "", review_Adaaran) 
review_Amari <- gsub(" room", "", review_Amari)                         
review_Cinnamon <- gsub(" room", "", review_Cinnamon)                       
review_FourSeasons <- gsub(" room", "", review_FourSeasons)

review_HolidayInn <- gsub(" maldiv", "", review_HolidayInn)
review_Shangri <- gsub(" maldiv", "", review_Shangri)                       
review_Adaaran <- gsub(" maldiv", "", review_Adaaran) 
review_Amari <- gsub(" maldiv", "", review_Amari)                         
review_Cinnamon <- gsub(" maldiv", "", review_Cinnamon)                       
review_FourSeasons <- gsub(" maldiv", "", review_FourSeasons)

#Inspect the vectors after cleaning
head(review_HolidayInn)                      
head(review_Shangri) 
head(review_Adaaran)
head(review_Amari)
head(review_Cinnamon)
head(review_FourSeasons)

#Converting the text vectors to corpus
corpus_HolidayInn <- Corpus(VectorSource(review_HolidayInn))
corpus_Shangri <- Corpus(VectorSource(review_Shangri))
corpus_Adaaran <- Corpus(VectorSource(review_Adaaran))
corpus_Amari <- Corpus(VectorSource(review_Amari))                      
corpus_Cinnamon <- Corpus(VectorSource(review_Cinnamon))                      
corpus_FourSeasons <- Corpus(VectorSource(review_FourSeasons))                    

corpus_HolidayInn
corpus_Shangri
corpus_Adaaran
corpus_Amari
corpus_Cinnamon
corpus_FourSeasons

#Clean up corpus by removing stop words and Whitespace
corpus_HolidayInn <- tm_map(corpus_HolidayInn, removeWords,stopwords("english"))
corpus_HolidayInn <- tm_map(corpus_HolidayInn, stripWhitespace)
inspect(corpus_HolidayInn)

corpus_Shangri <- tm_map(corpus_Shangri, removeWords,stopwords("english"))
corpus_Shangri <- tm_map(corpus_Shangri, stripWhitespace)
inspect(corpus_HolidayInn)

corpus_Adaaran <- tm_map(corpus_Adaaran, removeWords,stopwords("english"))
corpus_Adaaran <- tm_map(corpus_Adaaran, stripWhitespace)
inspect(corpus_HolidayInn)

corpus_Amari <- tm_map(corpus_Amari, removeWords,stopwords("english"))
corpus_Amari <- tm_map(corpus_Amari, stripWhitespace)
inspect(corpus_HolidayInn)

corpus_Cinnamon <- tm_map(corpus_Cinnamon, removeWords,stopwords("english"))
corpus_Cinnamon <- tm_map(corpus_Cinnamon, stripWhitespace)
inspect(corpus_HolidayInn)

corpus_FourSeasons <- tm_map(corpus_FourSeasons, removeWords,stopwords("english"))
corpus_FourSeasons <- tm_map(corpus_FourSeasons, stripWhitespace)
inspect(corpus_HolidayInn)

#K-means clustering
library(cluster) #load cluster package
install.packages("factoextra")
library(factoextra) #load factoextra package

#Create document-term matrix
Adaaran_dtm <- DocumentTermMatrix(
  corpus_Adaaran,
  control = list(minWordLength=c(3,Inf),
                 bounds = list(global = c(40, Inf)))
)

Adaaran_dtm
Adaaran_dtm2 <- as.matrix(Adaaran_dtm)

head(Adaaran_dtm2)
Adaaran_dist <- dist(t(Adaaran_dtm2), method="euclidian")
kfit <- kmeans(Adaaran_dist, 3)
Adaaran_dist
kfit
fviz_cluster(kfit,Adaaran_dist)

#Create document-term matrix
HolidayInn_dtm <- DocumentTermMatrix(
  corpus_HolidayInn,
  control = list(minWordLength=c(3,Inf),
                 bounds = list(global = c(40, Inf)))
)

HolidayInn_dtm
HolidayInn_dtm2 <- as.matrix(HolidayInn_dtm)

head(HolidayInn_dtm2)
HolidayInn_dist <- dist(t(HolidayInn_dtm2), method="euclidian")
kfit <- kmeans(HolidayInn_dist, 3)
HolidayInn_dist
kfit
fviz_cluster(kfit,HolidayInn_dist)

#Create document-term matrix
Amari_dtm <- DocumentTermMatrix(
  corpus_Amari,
  control = list(minWordLength=c(3,Inf),
                 bounds = list(global = c(40, Inf)))
)

Amari_dtm
Amari_dtm2 <- as.matrix(Amari_dtm)

head(Amari_dtm2)
Amari_dist <- dist(t(Amari_dtm2), method="euclidian")
kfit <- kmeans(Amari_dist, 3)
Amari_dist
kfit
fviz_cluster(kfit,Amari_dist)

#Create document-term matrix
Cinnamon_dtm <- DocumentTermMatrix(
  corpus_Cinnamon,
  control = list(minWordLength=c(3,Inf),
                 bounds = list(global = c(40, Inf)))
)

Cinnamon_dtm
Cinnamon_dtm2 <- as.matrix(Cinnamon_dtm)
head(Cinnamon_dtm2)
Cinnamon_dist <- dist(t(Cinnamon_dtm2), method="euclidian")
kfit <- kmeans(Cinnamon_dist, 3)
Cinnamon_dist
kfit
fviz_cluster(kfit,Cinnamon_dist)

#Create document-term matrix
FourSeasons_dtm <- DocumentTermMatrix(
  corpus_FourSeasons,
  control = list(minWordLength=c(3,Inf),
                 bounds = list(global = c(40, Inf)))
)

FourSeasons_dtm
FourSeasons_dtm2 <- as.matrix(FourSeasons_dtm)
head(FourSeasons_dtm2)
FourSeasons_dist <- dist(t(FourSeasons_dtm2), method="euclidian")
kfit <- kmeans(FourSeasons_dist, 3)
FourSeasons_dist
kfit
fviz_cluster(kfit,FourSeasons_dist)

#Create document-term matrix
Shangri_dtm <- DocumentTermMatrix(
  corpus_Shangri,
  control = list(minWordLength=c(3,Inf),
                 bounds = list(global = c(40, Inf)))
)

Shangri_dtm
Shangri_dtm2 <- as.matrix(Shangri_dtm)
head(Shangri_dtm2)
Shangri_dist <- dist(t(Shangri_dtm2), method="euclidian")
kfit <- kmeans(Shangri_dist, 3)
Shangri_dist
kfit
fviz_cluster(kfit,Shangri_dist)

#Stem the words to their root of all reviews present in the corpus
stem_corpus_HolidayInn <- tm_map(corpus_HolidayInn, stemDocument)
stem_corpus_Shangri <- tm_map(corpus_Shangri, stemDocument)
stem_corpus_Adaaran  <- tm_map(corpus_Adaaran , stemDocument)
stem_corpus_Amari <- tm_map(corpus_Amari, stemDocument)
stem_corpus_Cinnamon <- tm_map(corpus_Cinnamon, stemDocument)
stem_corpus_FourSeasons<- tm_map(corpus_FourSeasons, stemDocument)

#Load the positive and negative lexicon data
positive_lexicon <- read.csv("positive-lexicon.txt")
negative_lexicon <- read.csv("negative-lexicon.txt")

#Inspect lexicons
head(positive_lexicon)
tail(positive_lexicon)
head(negative_lexicon)
tail(negative_lexicon)

#Adaaran hotel
sentiment <- function(stem_corpus_Adaaran)
{
  #generate wordclouds
  wordcloud(stem_corpus_Adaaran,
            min.freq = 3,
            colors=brewer.pal(5, "Dark2"),
            random.color = TRUE,
            max.words = 50)
  
  #Calculating the count of total positive and negative words in each review
  
  #Create variables and vectors
  total_pos_count <- 0
  total_neg_count <- 0
  pos_count_vector <- c()
  neg_count_vector <- c()
  #Calculate the size of the corpus
  size <- length(stem_corpus_Adaaran)
  for(i in 1:size)
  {
    #All the words in current review
    corpus_wordsAdaaran<- list(strsplit(stem_corpus_Adaaran[[i]]$content, split = " "))
    #positive words in current review
    
    pos_count <-length(intersect(unlist(corpus_wordsAdaaran), unlist(positive_lexicon)))
    
    #negative words in current review
    neg_count <- length(intersect(unlist(corpus_wordsAdaaran), unlist(negative_lexicon)))
    
    total_pos_count <- total_pos_count + pos_count ## overall positive count
    total_neg_count <- total_neg_count + neg_count ## overall negative count
    
  }
  #Calculating overall percentage of positive and negative words of all the reviews
  total_pos_count ## overall positive count
  total_neg_count ## overall negative count
  total_count <- total_pos_count + total_neg_count
  overall_positive_percentage <- (total_pos_count*100)/total_count
  overall_negative_percentage <- (total_neg_count*100)/total_count
  overall_positive_percentage ## overall positive percentage
  #Create a dataframe with all the positive and negative reviews
  df<-data.frame(Review_Type=c("Postive","Negitive"),
                 Count=c(total_pos_count ,total_neg_count ))
  print(df) #Print
  overall_positive_percentage<-paste("Percentage of Positive Reviews:",
                                     round(overall_positive_percentage,2),"%")
  return(overall_positive_percentage)
}

sentiment(stem_corpus_Adaaran)

#HolidayInn hotel
sentiment <- function(stem_corpus_HolidayInn)
{
  #generate wordclouds
  wordcloud(stem_corpus_HolidayInn,
            min.freq = 3,
            colors=brewer.pal(8, "Dark2"),
            random.color = TRUE,
            max.words = 50)
  
  #Calculating the count of total positive and negative words in each review
  
  #Create variables and vectors
  total_pos_count <- 0
  total_neg_count <- 0
  pos_count_vector <- c()
  neg_count_vector <- c()
  #Calculate the size of the corpus
  size <- length(stem_corpus_HolidayInn)
  for(i in 1:size)
  {
    #All the words in current review
    corpus_wordsHolidayInn<- list(strsplit(stem_corpus_HolidayInn[[i]]$content, split = " "))
    #positive words in current review
    
    pos_count <-length(intersect(unlist(corpus_wordsHolidayInn), unlist(positive_lexicon)))
    
    #negative words in current review
    neg_count <- length(intersect(unlist(corpus_wordsHolidayInn), unlist(negative_lexicon)))
    
    total_pos_count <- total_pos_count + pos_count ## overall positive count
    total_neg_count <- total_neg_count + neg_count ## overall negative count
    
  }
  #Calculating overall percentage of positive and negative words of all the reviews
  total_pos_count ## overall positive count
  total_neg_count ## overall negative count
  total_count <- total_pos_count + total_neg_count
  overall_positive_percentage <- (total_pos_count*100)/total_count
  overall_negative_percentage <- (total_neg_count*100)/total_count
  overall_positive_percentage ## overall positive percentage
  #Create a dataframe with all the positive and negative reviews
  df<-data.frame(Review_Type=c("Postive","Negitive"),
                 Count=c(total_pos_count ,total_neg_count ))
  print(df) #Print
  overall_positive_percentage<-paste("Percentage of Positive Reviews:",
                                     round(overall_positive_percentage,2),"%")
  return(overall_positive_percentage)
}

  sentiment(stem_corpus_HolidayInn)  

  #Shangri Hotel
  sentiment <- function(stem_corpus_Shangri)
  {
    #generate wordclouds
    wordcloud(stem_corpus_Shangri,
              min.freq = 3,
              colors=brewer.pal(8, "Dark2"),
              random.color = TRUE,
              max.words = 50)
    
    #Calculating the count of total positive and negative words in each review
    #Create variables and vectors
    total_pos_count <- 0
    total_neg_count <- 0
    pos_count_vector <- c()
    neg_count_vector <- c()
    #Calculate the size of the corpus
    size <- length(stem_corpus_Shangri)
    for(i in 1:size)
    {
      #All the words in current review
      corpus_wordsShangri<- list(strsplit(stem_corpus_Shangri[[i]]$content, split = " "))
      #positive words in current review
      
      pos_count <-length(intersect(unlist(corpus_wordsShangri), unlist(positive_lexicon)))
      
      #negative words in current review
      neg_count <- length(intersect(unlist(corpus_wordsShangri), unlist(negative_lexicon)))
      
      total_pos_count <- total_pos_count + pos_count ## overall positive count
      total_neg_count <- total_neg_count + neg_count ## overall negative count
      
    }
    #Calculating overall percentage of positive and negative words of all the reviews
    total_pos_count ## overall positive count
    total_neg_count ## overall negative count
    total_count <- total_pos_count + total_neg_count
    overall_positive_percentage <- (total_pos_count*100)/total_count
    overall_negative_percentage <- (total_neg_count*100)/total_count
    overall_positive_percentage ## overall positive percentage
    #Create a dataframe with all the positive and negative reviews
    df<-data.frame(Review_Type=c("Postive","Negitive"),
                   Count=c(total_pos_count ,total_neg_count ))
    print(df) #Print
    overall_positive_percentage<-paste("Percentage of Positive Reviews:",
                                       round(overall_positive_percentage,2),"%")
    return(overall_positive_percentage)
  }
  
  sentiment(stem_corpus_Shangri)

  #Amari hotel
  sentiment <- function(stem_corpus_Amari)
  {
    #generate wordclouds
    wordcloud(stem_corpus_Amari,
              min.freq = 3,
              colors=brewer.pal(8, "Dark2"),
              random.color = TRUE,
              max.words = 50)
    
    #Calculating the count of total positive and negative words in each review
    #Create variables and vectors
    total_pos_count <- 0
    total_neg_count <- 0
    pos_count_vector <- c()
    neg_count_vector <- c()
    #Calculate the size of the corpus
    size <- length(stem_corpus_Amari)
    for(i in 1:size)
    {
      #All the words in current review
      corpus_wordsAmari<- list(strsplit(stem_corpus_Amari[[i]]$content, split = " "))
      #positive words in current review
      
      pos_count <-length(intersect(unlist(corpus_wordsAmari), unlist(positive_lexicon)))
      
      #negative words in current review
      neg_count <- length(intersect(unlist(corpus_wordsAmari), unlist(negative_lexicon)))
      
      total_pos_count <- total_pos_count + pos_count ## overall positive count
      total_neg_count <- total_neg_count + neg_count ## overall negative count
      
    }
    #Calculating overall percentage of positive and negative words of all the reviews
    total_pos_count ## overall positive count
    total_neg_count ## overall negative count
    total_count <- total_pos_count + total_neg_count
    overall_positive_percentage <- (total_pos_count*100)/total_count
    overall_negative_percentage <- (total_neg_count*100)/total_count
    overall_positive_percentage ## overall positive percentage
    #Create a dataframe with all the positive and negative reviews
    df<-data.frame(Review_Type=c("Postive","Negitive"),
                   Count=c(total_pos_count ,total_neg_count ))
    print(df) #Print
    overall_positive_percentage<-paste("Percentage of Positive Reviews:",
                                       round(overall_positive_percentage,2),"%")
    return(overall_positive_percentage)
  }
  
  sentiment(stem_corpus_Amari)

  #Cinnamon Hotel
  sentiment <- function(stem_corpus_Cinnamon)
  {
    #generate wordclouds
    wordcloud(stem_corpus_Cinnamon,
              min.freq = 3,
              colors=brewer.pal(8, "Dark2"),
              random.color = TRUE,
              max.words = 50)
    
    #Calculating the count of total positive and negative words in each review
    
    #Create variables and vectors
    total_pos_count <- 0
    total_neg_count <- 0
    pos_count_vector <- c()
    neg_count_vector <- c()
    #Calculate the size of the corpus
    size <- length(stem_corpus_Cinnamon)
    for(i in 1:size)
    {
      #All the words in current review
      corpus_wordsCinnamon<- list(strsplit(stem_corpus_Cinnamon[[i]]$content, split = " "))
      #positive words in current review
      
      pos_count <-length(intersect(unlist(corpus_wordsCinnamon), unlist(positive_lexicon)))
      
      #negative words in current review
      neg_count <- length(intersect(unlist(corpus_wordsCinnamon), unlist(negative_lexicon)))
      
      total_pos_count <- total_pos_count + pos_count ## overall positive count
      total_neg_count <- total_neg_count + neg_count ## overall negative count
      
    }
    #Calculating overall percentage of positive and negative words of all the reviews
    total_pos_count ## overall positive count
    total_neg_count ## overall negative count
    total_count <- total_pos_count + total_neg_count
    overall_positive_percentage <- (total_pos_count*100)/total_count
    overall_negative_percentage <- (total_neg_count*100)/total_count
    overall_positive_percentage ## overall positive percentage
    #Create a dataframe with all the positive and negative reviews
    df<-data.frame(Review_Type=c("Postive","Negitive"),
                   Count=c(total_pos_count ,total_neg_count ))
    print(df) #Print
    overall_positive_percentage<-paste("Percentage of Positive Reviews:",
                                       round(overall_positive_percentage,2),"%")
    return(overall_positive_percentage)
  }
  
  sentiment(stem_corpus_Cinnamon)

  #FourSeason Hotel  
  sentiment <- function(stem_corpus_FourSeasons)
  {
    #generate wordclouds
    wordcloud(stem_corpus_FourSeasons,
              min.freq = 3,
              colors=brewer.pal(8, "Dark2"),
              random.color = TRUE,
              max.words = 50)
    
    #Calculating the count of total positive and negative words in each review
    #Create variables and vectors
    total_pos_count <- 0
    total_neg_count <- 0
    pos_count_vector <- c()
    neg_count_vector <- c()
    #Calculate the size of the corpus
    size <- length(stem_corpus_FourSeasons)
    for(i in 1:size)
    {
      #All the words in current review
      corpus_wordsFourseasons<- list(strsplit(stem_corpus_FourSeasons[[i]]$content, split = " "))
      #positive words in current review
      
      pos_count <-length(intersect(unlist(corpus_wordsFourseasons), unlist(positive_lexicon)))
      
      #negative words in current review
      neg_count <- length(intersect(unlist(corpus_wordsFourseasons), unlist(negative_lexicon)))
      
      total_pos_count <- total_pos_count + pos_count ## overall positive count
      total_neg_count <- total_neg_count + neg_count ## overall negative count
      
    }
    #Calculating overall percentage of positive and negative words of all the reviews
    total_pos_count ## overall positive count
    total_neg_count ## overall negative count
    total_count <- total_pos_count + total_neg_count
    overall_positive_percentage <- (total_pos_count*100)/total_count
    overall_negative_percentage <- (total_neg_count*100)/total_count
    overall_positive_percentage ## overall positive percentage
    #Create a dataframe with all the positive and negative reviews
    df<-data.frame(Review_Type=c("Postive","Negitive"),
                   Count=c(total_pos_count ,total_neg_count ))
    print(df) #Print
    overall_positive_percentage<-paste("Percentage of Positive Reviews:",
                                       round(overall_positive_percentage,2),"%")
    return(overall_positive_percentage)
  }
  
  sentiment(stem_corpus_FourSeasons)

  #missing value
  sapply(maldives, function(x) sum(is.na(x)))

  

  
  
  
  
  
  #read file
  
  #tm package provides a framework for text mining applications
  install.packages("tm")
  library(tm)
  
  maldives<- readLines("maldives_hotel_reviews.csv")
  names(maldives)
  summary(maldives)
  
  HolidayInn<-subset(maldives,
                     Hotel.Name=="Holiday Inn Resort Kandooma Maldives")
  
  Shangri<-subset(maldives,
                  Hotel.Name=="Shangri-La's Villingili Resort and Spa Maldives")
  
  Adaaran<-subset(maldives,
                  Hotel.Name=="Adaaran Select Meedhupparu")
  
  Amari<-subset(maldives,
                Hotel.Name=="Amari Havodda Maldives")
  
  Cinnamon<-subset(maldives,
                   Hotel.Name=="Cinnamon Dhonveli Maldives")
  
  FourSeasons<-subset(maldives,
                      Hotel.Name=="Four Seasons Resort Maldives at Landaa Giraavaru")
  
  
  #converting the text file to corpus
  #Corpus is collections of documents containing (natural language)
  #text. Corpus is the main structure that tm uses for storing and
  #manipulating text documents.
  mycorpus <- Corpus(VectorSource(maldives))
  mycorpus
  inspect(mycorpus[1])
  inspect(mycorpus[2])
  inspect(mycorpus[3])
  inspect(mycorpus[8])
  mycorpus[8]
  #tm_map function can be used as interface to apply
  #transformation functions to corpus.
  mycorpus <- tm_map(mycorpus,tolower)
  inspect(mycorpus[8])
  getTransformations()
  mycorpus <-tm_map(mycorpus,removePunctuation)
  inspect(mycorpus[8])
  mycorpus <-tm_map(mycorpus,removeNumbers)
  stopwords("en")
  dataclean <-tm_map(mycorpus,removeWords,stopwords("english"))
  inspect(dataclean[8])
  dataclean <- tm_map(dataclean,stripWhitespace)
  inspect(dataclean[8])
  dtm <- TermDocumentMatrix(dataclean,
                            control = list(minWordLength=c(1,Inf))
  )
  dtm
  
  #findFreqTerms function can be used to find frequent terms in a
  #document-term or term-document matrix.
  findFreqTerms(dtm,lowfreq = 2)
  termFrequency <- rowSums(as.matrix(dtm))
  termFrequency 

  termFrequency <- subset(termFrequency,termFrequency>=15)
  termFrequency
  
  barplot(termFrequency,las=2,col=rainbow(20))

  install.packages("wordcloud") #install "wordcloud"
  library(wordcloud) #load "wordcloud"
  
  wordfreq <-sort(termFrequency,decreasing = TRUE)
  wordfreq
  
  wordcloud(words = names(wordfreq),
            freq=wordfreq,max.words=100,
            min.freq = 5,
            random.order = F)
  
  wordcloud(words = names(wordfreq),
            freq=wordfreq,max.words=100,
            min.freq = 5,
            random.order = F,
            colors = rainbow(20))
  
 # Word distribution
  #We now see the distribution of the 50 most frequent words in a
 # barplot.
  #We now see the distribution of the 50 most frequent words in a
 # barplot.
  barplot(wordfreq[1:50],
          xlab = "term",
          ylab = "frequency",
          las=2,
          col=heat.colors(50))
  
  
  
  
  