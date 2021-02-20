#Load library
library(tm)

#Set directory containing documents
dir_docs<- "C:/Capstone/final/"

#Set working directory to dir_docs
setwd(dir_docs)

#Load in documents into a corpus
docs<- VCorpus(DirSource(directory = dir_docs), readerControl=list(readPlain, language="en", load=TRUE))

#Show metadata of first document
meta(docs[[1]])

#Remove all non-ascii characters
for(i in 1:length(docs)){
docs[[i]][[1]] <- gsub("[^\x01-\x7F]","",docs[[i]][[1]])
}

#Remove punctuation
docs <- tm_map(docs,removePunctuation)

#Convert to lower case
docs <- tm_map(docs, content_transformer(tolower))

#Remove stopwords
#docs <- tm_map(docs, removeWords, stopwords("english"))


#Load the profanity vector from an adjacent directory
dir_profanity <- "C:/Capstone/profanity"
setwd(dir_profanity)
bad_words <- read.csv(paste0(dir_profanity,"/profanity_list.csv"),header=FALSE, stringsAsFactors = FALSE)
profanity_vector <- bad_words$V1


#Run a loop to clear all the profanity.  This loop takes about 10 minutes to run on my PC.  Pretty long time.
for (i in 1:length(docs)){
  for (j in 1:length(profanity_vector)){
    docs[[i]][[1]] <- gsub(profanity_vector[j],"",docs[[i]][[1]])
  }
}

#Get rid of excess whitespace in all documents
docs<- tm_map(docs, stripWhitespace)


dir_clean_docs<- "C:/Capstone/clean2/"  
#Go to the directory to store the cleaned documents
setwd(dir_clean_docs)


#Write the clean documents to separate files so I don't have to re-process later.
write(docs[[1]][[1]], "en_US_blogs_clean.txt", sep="\n")
write(docs[[2]][[1]], "en_US_news_clean.txt", sep="\n")
write(docs[[3]][[1]], "en_US_twitter_clean.txt", sep="\n")
