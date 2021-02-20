#Exploratory Analysis of N-gram text data.


#Load library
library(tm)
library(ggplot2)
library(ngram)


#Go to the directory to store the cleaned documents
dir_clean_docs<- "C:/Capstone/clean2/"  
setwd(dir_clean_docs)

#Load in documents into a corpus
clean_docs<- VCorpus(DirSource(directory = dir_clean_docs), 
readerControl=list(readPlain, language="en", load=TRUE))

#Create a matrix of terms in the documents
dtm <- DocumentTermMatrix(clean_docs, control=list(tolower=FALSE))

#Inspect the document term Matrix
inspect(dtm)


#Determine the number of words in each document
nwords <- rowSums(as.matrix(dtm))

#Create a data frame to store number of words, characters and lines in each document.
doc_summary <- data.frame(matrix(ncol =3, nrow = length(clean_docs)))

#Character vector of document names
doc_names <- names(nwords) 

#Put row names as documents
rownames(doc_summary) <- doc_names

#Set up column names for word counts, character counts and line counts.
colnames(doc_summary) <- c("Word Count", "Character Count", "Line Count")

#Add word counts to the data frame.
words_col <- unname(nwords)
doc_summary$`Word Count` <- words_col

# Add the number of lines in each document to the data frame.
lines_col <- c(length(clean_docs[[1]][[1]]),length(clean_docs[[2]][[1]]),
               length(clean_docs[[3]][[1]]))
doc_summary$`Line Count` <- lines_col

#Add the number of characters to the data frame.
char_col <- c(sum(nchar(clean_docs[[1]][[1]])),sum(nchar(clean_docs[[2]][[1]])),
              sum(nchar(clean_docs[[3]][[1]])))

doc_summary$`Character Count` <- char_col

#Sort the word data.
word_matrix <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)

word_freq_data <- data.frame(word = names(word_matrix),freq=word_matrix)

rownames(word_freq_data) <- seq(1, nrow(word_freq_data),1)

word_freq_data$word <- as.character(word_freq_data$word)



#Plot the 25 most common words
word_freq_plot <- ggplot(data=word_freq_data[1:25,], aes(reorder(word,-freq), freq)) +
               geom_bar(stat="identity", color="darkgreen", fill="cyan") + 
  labs(title = "Total Single Word Counts",x = "Word",y= "Frequency") + 
  theme(text = element_text(size=16),axis.text.x = element_text(angle = 60, hjust = 1))



save(word_freq_data, file="C:/Capstone/OneGram.RData")


#Develop an Ngram tokenizer and use it to create a data frame of 2-grams seen in the documents
NgramTokenizer <- function(document, nval){
str <- concatenate(document, collapse = " ",rm.space = FALSE)

n_gram <- ngram_asweka(str, min =nval , max = nval, sep = " ")

n_gram_data <- table(n_gram)

n_gram_data <- as.data.frame(n_gram_data)

n_gram_data <- n_gram_data[order(n_gram_data$Freq, decreasing=T),]

rownames(n_gram_data) <- seq(1, nrow(n_gram_data),1)

return(n_gram_data)
}


#Develop a tokenizator for unigrams.
OnegramTokenizer <- function(document, direc)
{
  dir_docs<- paste0("C:/Capstone/",direc)  
  setwd(dir_docs)  
  single_one<- VCorpus(DirSource(directory = dir_docs), 
            readerControl=list(readPlain, language="en", load=TRUE))
  dtm_single <- DocumentTermMatrix(single_one, control=list(tolower=FALSE))
  
  word_matrix_single <- sort(colSums(as.matrix(dtm_single)), decreasing = TRUE)
  
  word_freq_sing_data <- data.frame(word = names(word_matrix_single),freq=word_matrix_single)
  
  rownames(word_freq_sing_data) <- seq(1, nrow(word_freq_sing_data),1)
  
  word_freq_sing_data$word <- as.character(word_freq_sing_data$word)
  
  return(word_freq_sing_data)
}

save_data <- function(data, filename){
save(data, file=paste0("C:/Capstone/",filename))
}

#n_gram_data <- n_gram_data[n_gram_data$Freq!=5,]
one_grams_blogs <- OnegramTokenizer(clean_docs[[1]][[1]],"blogs/")
one_grams_news <- OnegramTokenizer(clean_docs[[2]][[1]],"news/")
one_grams_twitter <- OnegramTokenizer(clean_docs[[3]][[1]],"twitter/")

save_data(one_grams_blogs,"OneGram_blogs.RData")
save_data(one_grams_news,"OneGram_news.RData")
save_data(one_grams_twitter,"OneGram_twitter.RData")


two_grams_blogs <- NgramTokenizer(clean_docs[[1]][[1]],2)
two_grams_news <- NgramTokenizer(clean_docs[[2]][[1]],2)
two_grams_twitter <- NgramTokenizer(clean_docs[[3]][[1]],2)


save_data(two_grams_blogs,"TwoGram_blogs.RData")
save_data(two_grams_news,"TwoGram_news.RData")
save_data(two_grams_twitter,"TwoGram_twitter.RData")


three_grams_blogs <- NgramTokenizer(clean_docs[[1]][[1]],3)
three_grams_news <- NgramTokenizer(clean_docs[[2]][[1]],3)
three_grams_twitter <- NgramTokenizer(clean_docs[[3]][[1]],3)

save_data(three_grams_blogs,"ThreeGram_blogs.RData")
save_data(three_grams_news,"ThreeGram_news.RData")
save_data(three_grams_twitter,"ThreeGram_twitter.RData")



four_grams_blogs <- NgramTokenizer(clean_docs[[1]][[1]],4)
four_grams_news <- NgramTokenizer(clean_docs[[2]][[1]],4)
four_grams_twitter <- NgramTokenizer(clean_docs[[3]][[1]],4)

save_data(four_grams_blogs,"FourGram_blogs.RData")
save_data(four_grams_news,"FourGram_news.RData")
save_data(four_grams_twitter,"FourGram_twitter.RData")


main_folder <-"C:/Capstone/"

load_files <- function(filename){
  main_folder <-"C:/Capstone/"
 load(file=paste0(main_folder,filename))
 }

data_cutoff <- function(data, cutoff){
data <- data[data[,2]>=cutoff, ]
return(data)
}

load(file=paste0(main_folder,"OneGram_blogs.RData"))
one_grams_blogs <- data
rm(data)
one_grams_blogs <- data_cutoff(one_grams_blogs,10)

save_data(one_grams_blogs,"OneGram_blogs_reduced.RData")

save_data(word_freq_data,"OneGram_reduced.RData")

write.csv(four_grams_blog, "C:/Capstone/OneGram_blogs_reduced.csv")
four_grams_blog <- read.csv("C:/Capstone/OneGram_blogs_reduced.csv")
