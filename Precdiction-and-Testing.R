#Load the necessary libraries.
library(tm)
library(ngram)
library(SnowballC)
library(dplyr)


#Create a subset of clean documents that are smaller and write to separate directories.
dir_clean_docs<- "C:/Capstone/clean2/"
setwd(dir_clean_docs)


clean_docs<- VCorpus(DirSource(directory = dir_clean_docs),
          readerControl=list(readPlain, language="en", load=TRUE))
 nlines <- 200000


write(clean_docs[[1]][[1]][1:nlines], "C:/Capstone/blogs_smaller/en_US_blogs_clean_smaller.txt", sep="\n")
write(clean_docs[[2]][[1]], "C:/Capstone/news_smaller/en_US_news_clean_smaller.txt", sep="\n")
write(clean_docs[[3]][[1]][1:nlines], "C:/Capstone/twitter_smaller/en_US_twitter_clean_smaller.txt", sep="\n")



#Function to tokenize the documents.
NgramTokenizer <- function(document, nval){
str <- concatenate(document, collapse = " ",rm.space = FALSE)

n_gram <- ngram_asweka(str, min =nval , max = nval, sep = " ")

n_gram_data <- table(n_gram)

n_gram_data <- as.data.frame(n_gram_data)

n_gram_data <- n_gram_data[order(n_gram_data$Freq, decreasing=T),]

rownames(n_gram_data) <- seq(1, nrow(n_gram_data),1)

n_gram_data[,1] <- as.character(n_gram_data[,1])

return(n_gram_data)
}



#Make the n-gram tables smaller.  Get rid of all frequency equal to 1 n-grams.
#Use a function to do this.
data_cutoff <- function(data, cutoff){
  data <- data[data[,2]>=cutoff, ]
  return(data)
}




#Load each smaller document as separate corpuses.
dir_clean_docs<- "C:/Capstone/blogs_smaller/"
setwd(dir_clean_docs)

clean_blog_smaller<- VCorpus(DirSource(directory = dir_clean_docs),
                      readerControl=list(readPlain, language="en", load=TRUE))

dir_clean_docs<- "C:/Capstone/news_smaller/"
setwd(dir_clean_docs)

clean_news_smaller<- VCorpus(DirSource(directory = dir_clean_docs),
                     readerControl=list(readPlain, language="en", load=TRUE))


dir_clean_docs<- "C:/Capstone/twitter_smaller/"
setwd(dir_clean_docs)

clean_twitter_smaller<- VCorpus(DirSource(directory = dir_clean_docs),
                 readerControl=list(readPlain, language="en", load=TRUE))


#Generate one, two, three and four grams from all documents.
one_grams_blogs <- NgramTokenizer(clean_blog_smaller[[1]][[1]],1)
one_grams_news <- NgramTokenizer(clean_news_smaller[[1]][[1]],1)
one_grams_twitter <- NgramTokenizer(clean_twitter_smaller[[1]][[1]],1)


two_grams_blogs <- NgramTokenizer(clean_blog_smaller[[1]][[1]],2)
two_grams_news <- NgramTokenizer(clean_news_smaller[[1]][[1]],2)
two_grams_twitter <- NgramTokenizer(clean_twitter_smaller[[1]][[1]],2)

three_grams_blogs <- NgramTokenizer(clean_blog_smaller[[1]][[1]],3)
three_grams_news <- NgramTokenizer(clean_news_smaller[[1]][[1]],3)
three_grams_twitter <- NgramTokenizer(clean_twitter_smaller[[1]][[1]],3)


four_grams_blogs <- NgramTokenizer(clean_blog_smaller[[1]][[1]],4)
four_grams_news <- NgramTokenizer(clean_news_smaller[[1]][[1]],4)
four_grams_twitter <- NgramTokenizer(clean_twitter_smaller[[1]][[1]],4)




#Write the Ngrams to separate CSV files so they can be easily loaded later.
write.csv(one_grams_blogs, "C:/Capstone/OneGram_blogs_smaller.csv", row.names = FALSE)
write.csv(one_grams_news, "C:/Capstone/OneGram_news_smaller.csv", row.names = FALSE)
write.csv(one_grams_twitter, "C:/Capstone/OneGram_twitter_smaller.csv", row.names = FALSE)
#
write.csv(two_grams_blogs, "C:/Capstone/TwoGram_blogs_smaller.csv", row.names = FALSE)
write.csv(two_grams_news, "C:/Capstone/TwoGram_news_smaller.csv", row.names = FALSE)
write.csv(two_grams_twitter, "C:/Capstone/TwoGram_twitter_smaller.csv", row.names = FALSE)
#
write.csv(three_grams_blogs, "C:/Capstone/ThreeGram_blogs_smaller.csv", row.names = FALSE)
write.csv(three_grams_news, "C:/Capstone/ThreeGram_news_smaller.csv", row.names = FALSE)
write.csv(three_grams_twitter, "C:/Capstone/ThreeGram_twitter_smaller.csv", row.names = FALSE)

write.csv(four_grams_blogs, "C:/Capstone/FourGram_blogs_smaller.csv", row.names = FALSE)
write.csv(four_grams_news, "C:/Capstone/FourGram_news_smaller.csv", row.names = FALSE)
write.csv(four_grams_twitter, "C:/Capstone/FourGram_twitter_smaller.csv", row.names = FALSE)





# #Load up all the n-gram data.
one_grams_blogs <- read.csv("C:/Capstone/OneGram_blogs_smaller.csv", stringsAsFactors = F)
one_grams_news <- read.csv("C:/Capstone/OneGram_news_smaller.csv",stringsAsFactors = F)
one_grams_twitter <- read.csv("C:/Capstone/OneGram_twitter_smaller.csv",stringsAsFactors = F)

two_grams_blogs <- read.csv("C:/Capstone/TwoGram_blogs_smaller.csv",stringsAsFactors = F)
two_grams_news <- read.csv("C:/Capstone/TwoGram_news_smaller.csv",stringsAsFactors = F)
two_grams_twitter <- read.csv("C:/Capstone/TWoGram_twitter_smaller.csv",stringsAsFactors = F)

three_grams_blogs <- read.csv("C:/Capstone/ThreeGram_blogs_smaller.csv",stringsAsFactors = F)
three_grams_news <- read.csv("C:/Capstone/ThreeGram_news_smaller.csv",stringsAsFactors = F)
three_grams_twitter <- read.csv("C:/Capstone/ThreeGram_twitter_smaller.csv",stringsAsFactors = F)

four_grams_blogs <- read.csv("C:/Capstone/FourGram_blogs_smaller.csv",stringsAsFactors = F)
four_grams_news <- read.csv("C:/Capstone/FourGram_news_smaller.csv",stringsAsFactors = F)
four_grams_twitter <- read.csv("C:/Capstone/FourGram_twitter_smaller.csv",stringsAsFactors = F)
















#Reduce the size of the 2, 3 and 4-gram tables.
two_grams_blogs <- data_cutoff(two_grams_blogs,2)
two_grams_news <- data_cutoff(two_grams_news,2)
two_grams_twitter <- data_cutoff(two_grams_twitter,2)

three_grams_blogs <- data_cutoff(three_grams_blogs,2)
three_grams_news <- data_cutoff(three_grams_news,2)
three_grams_twitter <- data_cutoff(three_grams_twitter,2)

four_grams_blogs <- data_cutoff(four_grams_blogs,2)
four_grams_news <- data_cutoff(four_grams_news,2)
four_grams_twitter <- data_cutoff(four_grams_twitter,2)



#Write these cutoff n-gram tables to csv files.
write.csv(two_grams_blogs, "C:/Capstone/TwoGram_blogs_smaller.csv", row.names = FALSE)
write.csv(two_grams_news, "C:/Capstone/TwoGram_news_smaller.csv", row.names = FALSE)
write.csv(two_grams_twitter, "C:/Capstone/TwoGram_twitter_smaller.csv", row.names = FALSE)
#
write.csv(three_grams_blogs, "C:/Capstone/ThreeGram_blogs_smaller.csv", row.names = FALSE)
write.csv(three_grams_news, "C:/Capstone/ThreeGram_news_smaller.csv", row.names = FALSE)
write.csv(three_grams_twitter, "C:/Capstone/ThreeGram_twitter_smaller.csv", row.names = FALSE)

write.csv(four_grams_blogs, "C:/Capstone/FourGram_blogs_smaller.csv", row.names = FALSE)
write.csv(four_grams_news, "C:/Capstone/FourGram_news_smaller.csv", row.names = FALSE)
write.csv(four_grams_twitter, "C:/Capstone/FourGram_twitter_smaller.csv", row.names = FALSE)



#Read them in again and time it.  Should take less time.  Only takes 6 seconds!
one_grams_blogs <- read.csv("C:/Capstone/OneGram_blogs_smaller.csv", stringsAsFactors = F)
one_grams_news <- read.csv("C:/Capstone/OneGram_news_smaller.csv",stringsAsFactors = F)
one_grams_twitter <- read.csv("C:/Capstone/OneGram_twitter_smaller.csv",stringsAsFactors = F)

two_grams_blogs <- read.csv("C:/Capstone/TwoGram_blogs_smaller.csv",stringsAsFactors = F)
two_grams_news <- read.csv("C:/Capstone/TwoGram_news_smaller.csv",stringsAsFactors = F)
two_grams_twitter <- read.csv("C:/Capstone/TWoGram_twitter_smaller.csv",stringsAsFactors = F)

three_grams_blogs <- read.csv("C:/Capstone/ThreeGram_blogs_smaller.csv",stringsAsFactors = F)
three_grams_news <- read.csv("C:/Capstone/ThreeGram_news_smaller.csv",stringsAsFactors = F)
three_grams_twitter <- read.csv("C:/Capstone/ThreeGram_twitter_smaller.csv",stringsAsFactors = F)

four_grams_blogs <- read.csv("C:/Capstone/FourGram_blogs_smaller.csv",stringsAsFactors = F)
four_grams_news <- read.csv("C:/Capstone/FourGram_news_smaller.csv",stringsAsFactors = F)
four_grams_twitter <- read.csv("C:/Capstone/FourGram_twitter_smaller.csv",stringsAsFactors = F)







#Combine n-grams from all sources together, write them as CSV files.
#Can load these combined ngram CSV files later.
one_grams <- rbind(one_grams_blogs, one_grams_news, one_grams_twitter)
one_grams <- aggregate(Freq~n_gram, one_grams, sum)
one_grams <- arrange(one_grams, desc(Freq))
write.csv(one_grams, "C:/Capstone/OneGrams.csv", row.names = FALSE)
#
two_grams <- rbind(two_grams_blogs, two_grams_news, two_grams_twitter)
two_grams <- aggregate(Freq~n_gram, two_grams, sum)
two_grams <- arrange(two_grams, desc(Freq))
write.csv(two_grams, "C:/Capstone/TwoGrams.csv", row.names = FALSE)
#
three_grams <- rbind(three_grams_blogs, three_grams_news, three_grams_twitter)
three_grams <- aggregate(Freq~n_gram, three_grams, sum)
three_grams <- arrange(three_grams, desc(Freq))
write.csv(three_grams, "C:/Capstone/ThreeGrams.csv", row.names = FALSE)

one_grams <- read.csv("C:/Capstone/OneGrams.csv", stringsAsFactors = F)
two_grams <- read.csv("C:/Capstone/TwoGrams.csv",stringsAsFactors = F)
three_grams <- read.csv("C:/Capstone/ThreeGrams.csv",stringsAsFactors = F)





#Function to add maximum likelihood column to unigram frequency tables
add_MLs <- function(frequency_table){

frequency_table <- frequency_table %>% mutate(p = frequency_table$Freq/sum(frequency_table$Freq))

return(frequency_table)

}



#Basic bigram Katz Backoff model.  Test it with an input word.
input_word <- c("decoction")
test_word<- paste("^", input_word, " ", sep="")


#Function for bigram Katz backoff.
bigram_backoff <- function(input_word, unigrams, bigrams){

#Add starting metacharacter and space to input word.
test_word <- paste("^", input_word, " ", sep="")

#Check if the input word can be found in any of the bigrams.
if(length(grep(test_word, bigrams$n_gram)) > 0){

#Find a sub bigram containing entries that only start with the entered input word.
sub_bigrams <- bigrams[grep(test_word, bigrams[,1]),]

#Add a column to the two_gram subset containing the discounted counts.
sub_bigrams_discounted <- sub_bigrams %>%
  mutate(discounted = sub_bigrams$Freq - 0.5)


#Add a column to get the discounted maximum likelihoods for each bigram in the subset
sub_bigrams_discounted <- sub_bigrams_discounted %>%
mutate(q = sub_bigrams_discounted$discounted/sum(sub_bigrams_discounted$Freq))

#Calculate leftover probability mass
alpha_bi <- 1 - sum(sub_bigrams_discounted$q)

#Select the top 5 most likely words following the input word.  Show probabilities
q_bo_bi <- subset(sub_bigrams_discounted, select = c("n_gram","q"))
q_bo_bi$n_gram <- substring(q_bo_bi$n_gram,nchar(test_word))

} else {

      #Get total probability in unigram list.
      total_probability <- sum(unigrams$p)
      alpha_bi <- 1

      #Add a column containing normalized probabilities to unigram list.
      #This is the discounted probability
      unigrams <- unigrams %>% mutate(q = alpha_bi*unigrams$p/total_probability)

      #Return these unigrams, which are best guess for the next word if nothing
      #was found in the bigram list.
      q_bo_bi<- subset(unigrams, select = c("n_gram","q"))
}

return(q_bo_bi)
}


#Run two tests of the bigram Katz backoff function.
test1 <- bigram_backoff("of",one_grams_blogs, two_grams_blogs)
print(test1[1:10,])

test2 <- bigram_backoff("intracranial",one_grams_blogs, two_grams_blogs)
print(test2[1:10,])


#Record start time of code chunk execution here.
start_time <- Sys.time()


##MODEL 1:  Katz backoff trigram model.##

#Read them in again and time it.  Should take less time.  Only takes 6 seconds!
one_grams_blogs <- read.csv("C:/Capstone/OneGram_blogs_smaller.csv", stringsAsFactors = F)
one_grams_news <- read.csv("C:/Capstone/OneGram_news_smaller.csv",stringsAsFactors = F)
one_grams_twitter <- read.csv("C:/Capstone/OneGram_twitter_smaller.csv",stringsAsFactors = F)

two_grams_blogs <- read.csv("C:/Capstone/TwoGram_blogs_smaller.csv",stringsAsFactors = F)
two_grams_news <- read.csv("C:/Capstone/TwoGram_news_smaller.csv",stringsAsFactors = F)
two_grams_twitter <- read.csv("C:/Capstone/TWoGram_twitter_smaller.csv",stringsAsFactors = F)

three_grams_blogs <- read.csv("C:/Capstone/ThreeGram_blogs_smaller.csv",stringsAsFactors = F)
three_grams_news <- read.csv("C:/Capstone/ThreeGram_news_smaller.csv",stringsAsFactors = F)
three_grams_twitter <- read.csv("C:/Capstone/ThreeGram_twitter_smaller.csv",stringsAsFactors = F)

four_grams_blogs <- read.csv("C:/Capstone/FourGram_blogs_smaller.csv",stringsAsFactors = F)
four_grams_news <- read.csv("C:/Capstone/FourGram_news_smaller.csv",stringsAsFactors = F)
four_grams_twitter <- read.csv("C:/Capstone/FourGram_twitter_smaller.csv",stringsAsFactors = F)



#Function to add maximum likelihood column to unigram frequency tables
add_MLs <- function(frequency_table){
  
  frequency_table <- frequency_table %>% mutate(p = frequency_table$Freq/sum(frequency_table$Freq))
  
  return(frequency_table)
  
}



#Now do the Katz backoff trigram model.

#Generate the trigram Katz backoff model function..
trigram_backoff <- function(input_phrase, one_grams, two_grams, three_grams){
 
#Modify input phrase and also add probabilities to unigram list.  
test_phrase <- paste("^", input_phrase, " ", sep="")
one_grams <- add_MLs(one_grams)

#Check if there are any 3-grams that start with the test phrase.  
if(length(grep(test_phrase, three_grams$n_gram)) > 0){
  
      #If so, return a subset of three-grams that start with the test phrase.
      sub_three_grams<- three_grams[grep(test_phrase, three_grams[,1]),]
      
      #Add a column of discounted frequency counts for trigrams that start
      #with the test phrase.
      sub_three_grams_discounted <- sub_three_grams %>% 
            mutate(discounted = sub_three_grams$Freq - 0.5)
      
      #Add another column that provides the discounted probability in this subset
      #of 3-grams.
      sub_three_grams_discounted <- sub_three_grams_discounted %>% 
            mutate(q = sub_three_grams_discounted$discounted/sum(sub_three_grams_discounted$Freq))
      
      #Calculate the remaining probablity mass available for guessing new words.
      alpha_tri <- 1 - sum(sub_three_grams_discounted$q)
      
      #Create a table of just the n-grams starting with the input phrase and the discounted
      #probabilities only.
      q_bo_tri <- subset(sub_three_grams_discounted, select = c("n_gram","q"))
      
      #Show only the predicted word and not the entire trigram in this table.
      q_bo_tri$n_gram <- substring(q_bo_tri$n_gram,nchar(test_phrase))
} else {
            #No words follow input phrase in trigrams, so full probablity mass available for 
            #guessing.
            alpha_tri <- 1
            
            #Break up the input phrase and get only the previous word closest to the 
            #word being guessed.
            split_ngram <- strsplit(input_phrase, " ")   
            previous_word <- split_ngram[[1]][2]
            previous_word <- paste("^",previous_word, " ", sep="")
        
            #Now look at the bigram table.  See if any bigrams start with this 
            #previous word.  Create a subset of bigrams for which this is true.
            #Include discounted counts and probabilities in the table and calculate
            #alpha again.  Return only the predicted words within the bigrams.
            if(length(grep(previous_word, two_grams$n_gram)) > 0){
                  sub_two_grams <- two_grams[grep(previous_word, two_grams[,1]),]
                  sub_two_grams_discounted <- sub_two_grams %>% 
                          mutate(discounted = sub_two_grams$Freq - 0.5)
                  sub_two_grams_discounted <- sub_two_grams_discounted %>% 
                          mutate(q = sub_two_grams_discounted$discounted/sum(sub_two_grams_discounted$Freq))
                  alpha_bi <- 1 - sum(sub_two_grams_discounted$q)
                  q_bo_bi <- subset(sub_two_grams_discounted, select = c("n_gram","q"))
                  q_bo_bi$n_gram <- substring(q_bo_bi$n_gram,nchar(previous_word))
            } else {
                      
                      #Nothing in the bigrams either that start with test word.  Full alpha.
                      alpha_bi <- 1
                      total_probability <- sum(one_grams$p)
                      
                      #Adjust the one-gram probabilities, normalize to probability sum.
                      one_grams <- one_grams %>% mutate(q = alpha_bi*one_grams$p/total_probability) 
                      
                      #Can now only return the one grams with the highest probability of being the 
                      #following word.
                      q_bo_bi<- subset(one_grams, select = c("n_gram","q"))
            
            } 
               
            #Must also include code to calculate the summation term in the trigram Katz model
            #If previous word part of the bigram list, then repeat the above steps
            #to calculate the bigram discounted probabilities and then add them all up.
            #Alpha must be recacluated.  Must also find which unigrams are a part of this
            #group.  Adjust the unigram probabilities based on this and new alpha accordingly.
            #Suum these fractional probabilities for the cases where words do follow with the 
            # with the probablities of the words that don't follow the previous word to get
            #the total.  If no words at all follow the previous word, then just add
            #the unigram probabilities.  Return the final trigram probability.
            if(length(grep(previous_word, two_grams$n_gram)) > 0){      
            sub_two_grams <- two_grams[grep(previous_word, two_grams[,1]),]
            sub_two_grams_discounted <- sub_two_grams %>% 
              mutate(discounted = sub_two_grams$Freq - 0.5)
            sub_two_grams_discounted <- sub_two_grams_discounted %>% 
              mutate(q = sub_two_grams_discounted$discounted/sum(sub_two_grams_discounted$Freq))
            q_bo_bi_sum_if_word_follows <- sum(sub_two_grams_discounted$q)
            alpha_bi <- 1 - sum(sub_two_grams_discounted$q)
            
            q_bo_bi <- subset(sub_two_grams_discounted, select = c("n_gram","q"))
            q_bo_bi$n_gram <- substring(q_bo_bi$n_gram,nchar(previous_word))
            
            uni_remaining_words <- one_grams %>% filter(!(one_grams$n_gram %in% q_bo_bi$n_gram))
            uni_remaining_words <- uni_remaining_words %>% mutate(frac_q = alpha_bi*uni_remaining_words$p/sum(uni_remaining_words$p))  
            
            q_bo_bi_sum_if_no_word_follows <- sum(uni_remaining_words$frac_q)
            q_bo_bi_sum <- q_bo_bi_sum_if_word_follows + q_bo_bi_sum_if_no_word_follows
            }  else {
                    
              q_bo_bi_sum <- sum(one_grams$p/sum(one_grams$p))
              
              
                    }
            q_bo_tri <- q_bo_bi
            q_bo_tri$q <- alpha_tri*q_bo_tri$q/q_bo_bi_sum 

            
}

return(q_bo_tri)  
  
}




#Test the trigram backoff function with a few examples and print out some of the predicted
#results.
input_phrase <- c("the yellow")
test_blogs <- trigram_backoff(input_phrase, one_grams_blogs, two_grams_blogs, three_grams_blogs)
test_news <- trigram_backoff(input_phrase, one_grams_news, two_grams_news, three_grams_news)
test_twitter <- trigram_backoff(input_phrase, one_grams_twitter, two_grams_twitter, three_grams_twitter)

print(test_blogs[1:10,1])
print(test_news[1:10,1])
print(test_twitter[1:10,1])




#Can also try entering some quiz words so the prediction model can guess with following word
#is the correct word.  Filter out the ngrams to show only the quiz words if they can be found.
#Print out the quiz word guesses.
quiz_words <- c("referees", "players", "defense", "crowd")
quiz_words_blogs <- test_blogs %>% filter((test_blogs$n_gram %in% quiz_words))
quiz_words_news <- test_news %>% filter((test_news$n_gram %in% quiz_words))
quiz_words_twitter <- test_twitter %>% filter((test_twitter$n_gram %in% quiz_words))

print(quiz_words_blogs)
print(quiz_words_news)
print(quiz_words_twitter)

#Record the end time of the code chunk run.
end_time <- Sys.time()

#Record the total run time of the code run.
end_time - start_time










##MODEL 2:  Stupid backoff algorithm.


#Record code execution start time.
start_time <- Sys.time()


#Read them in again and time it.  Should take less time.  Only takes 6 seconds!
one_grams_blogs <- read.csv("C:/Capstone/OneGram_blogs_smaller.csv", stringsAsFactors = F)
one_grams_news <- read.csv("C:/Capstone/OneGram_news_smaller.csv",stringsAsFactors = F)
one_grams_twitter <- read.csv("C:/Capstone/OneGram_twitter_smaller.csv",stringsAsFactors = F)

two_grams_blogs <- read.csv("C:/Capstone/TwoGram_blogs_smaller.csv",stringsAsFactors = F)
two_grams_news <- read.csv("C:/Capstone/TwoGram_news_smaller.csv",stringsAsFactors = F)
two_grams_twitter <- read.csv("C:/Capstone/TWoGram_twitter_smaller.csv",stringsAsFactors = F)

three_grams_blogs <- read.csv("C:/Capstone/ThreeGram_blogs_smaller.csv",stringsAsFactors = F)
three_grams_news <- read.csv("C:/Capstone/ThreeGram_news_smaller.csv",stringsAsFactors = F)
three_grams_twitter <- read.csv("C:/Capstone/ThreeGram_twitter_smaller.csv",stringsAsFactors = F)

four_grams_blogs <- read.csv("C:/Capstone/FourGram_blogs_smaller.csv",stringsAsFactors = F)
four_grams_news <- read.csv("C:/Capstone/FourGram_news_smaller.csv",stringsAsFactors = F)
four_grams_twitter <- read.csv("C:/Capstone/FourGram_twitter_smaller.csv",stringsAsFactors = F)



#Function to add maximum likelihood column to unigram frequency tables
add_MLs <- function(frequency_table){
  
  frequency_table <- frequency_table %>% mutate(p = frequency_table$Freq/sum(frequency_table$Freq))
  
  return(frequency_table)
  
}




#Develop stupid backoff models for 2,3  and 4-grams.  This is quite a bit easier and can
#include 4 grams as well fairly easily.  I have written functions to make word predictions
#after at least one, two or three words typed.  
stupid_backoff_4gram <- function(input_phrase, one_grams, two_grams, three_grams, four_grams){
    
    #Adjust the test phrase with a starting character and add probabilities to one-grams.
    test_phrase <- paste("^", input_phrase, " ", sep="")
    one_grams <- add_MLs(one_grams)
    
    #Are any of the three word phases found in the 4-grams?
    if(length(grep(test_phrase, four_grams$n_gram)) > 0){
      
        #Get the four grams that include the 3-word starting phrase and add probability
        #Only show the predicted words and store them in this subset.
        sub_four_grams<- four_grams[grep(test_phrase, four_grams[,1]),]
        sub_four_grams<- sub_four_grams %>% 
          mutate(p = sub_four_grams$Freq/sum(sub_four_grams$Freq))
        p_fourgram <- subset(sub_four_grams, select = c("n_gram","p"))
        p_fourgram$n_gram <- substring(p_fourgram$n_gram,nchar(test_phrase))
        final_results <- p_fourgram
    } else {
        
        #Split the 3 word phase to show only the last two words of the phrase.
        split_3wordphrase <- strsplit(input_phrase, " ")   
        two_word_phrase <- c(split_3wordphrase[[1]][2], split_3wordphrase[[1]][3]) 
        two_word_phrase <- concatenate(two_word_phrase, collapse = " ",rm.space = FALSE)
        two_word_phrase <- paste("^", two_word_phrase, " ", sep="")
        
        #Is the two word phrase part of the trigram list?
        if(length(grep(two_word_phrase, three_grams$n_gram)) > 0){
          
          #If so, get subset of three grams countaining the two-word phrase.  
          #Add probability columns, with 0.4 factor multiplier.
          #Show the results of the words which follow the two-word phrase.
          sub_three_grams <- three_grams[grep(two_word_phrase, three_grams[,1]),]
          sub_three_grams<- sub_three_grams %>% 
            mutate(p = 0.4*sub_three_grams$Freq/sum(sub_three_grams$Freq))
          p_threegram <- subset(sub_three_grams, select = c("n_gram", "p"))
          p_threegram$n_gram <- substring(p_threegram$n_gram, nchar(two_word_phrase))
          final_results <- p_threegram
        } else {
            
            #split the two word phase and show only the previous word.
            split_2wordphrase <- strsplit(input_phrase, " ")
            previous_word <- split_2wordphrase[[1]][2]
            previous_word <- paste("^",previous_word," ",sep="")
          
            #If bigrams are available that start with this previous word
            #Then show this subset of birgrams.  Add corrected probability
            #column.  Can generate predicted word results now.
            if(length(grep(previous_word, two_grams$n_gram)) > 0){
            sub_two_grams <- two_grams[grep(previous_word, two_grams[,1]),]
            sub_two_grams <- sub_two_grams %>% 
              mutate(p = 0.4*sub_two_grams$Freq/sum(sub_two_grams$Freq))
            p_twogram <- subset(sub_two_grams, select = c("n_gram", "p"))
            p_twogram$n_gram <- substring(p_twogram$n_gram, nchar(previous_word))
            final_results <- p_twogram
            } else {
              
              #No words follow any of the previous one, two, or 3-word phrases
              #Must guess from just the unigrams and use the high probability
              #unigrams as the prediction.
              p_onegrams <- one_grams
              p_onegrams<- subset(p_one_grams, select = c("n_gram","p"))
              final_results <- p_onegrams
            
            
            }
          
  
        } 
    }
    return(final_results)
}



#Repeat everything as above, but do it only for two-word phrases and up to
#trigrams only.  Create a function for this.
stupid_backoff_3gram <- function(input_phrase, one_grams, two_grams, three_grams){
  
  
  test_phrase <- paste("^", input_phrase, " ", sep="")
  one_grams <- add_MLs(one_grams)
  
    
    if(length(grep(test_phrase, three_grams$n_gram)) > 0){
      sub_three_grams <- three_grams[grep(test_phrase, three_grams[,1]),]
      sub_three_grams<- sub_three_grams %>% 
        mutate(p = 0.4*sub_three_grams$Freq/sum(sub_three_grams$Freq))
      p_threegram <- subset(sub_three_grams, select = c("n_gram", "p"))
      p_threegram$n_gram <- substring(p_threegram$n_gram, nchar(test_phrase))
      final_results <- p_threegram
    } else {
      
      split_2wordphrase <- strsplit(input_phrase, " ")
      previous_word <- split_2wordphrase[[1]][2]
      previous_word <- paste("^",previous_word," ",sep="")
      
      if(length(grep(previous_word, two_grams$n_gram)) > 0){
        sub_two_grams <- two_grams[grep(previous_word, two_grams[,1]),]
        sub_two_grams <- sub_two_grams %>% 
          mutate(p = 0.4*sub_two_grams$Freq/sum(sub_two_grams$Freq))
        p_twogram <- subset(sub_two_grams, select = c("n_gram", "p"))
        p_twogram$n_gram <- substring(p_twogram$n_gram, nchar(previous_word))
        final_results <- p_twogram
      } else {
        
        p_onegrams <- one_grams
        p_onegrams<- subset(p_one_grams, select = c("n_gram","p"))
        final_results <- p_onegrams
        
        
      }
      
      
    }
  return(final_results)
  }




#Repeat as above, but create a function to predict a word following
#just one word.
stupid_backoff_2gram <- function(input_phrase, one_grams, two_grams){
  
  
  test_phrase <- paste("^", input_phrase, " ", sep="")
  one_grams <- add_MLs(one_grams)
  
    
    if(length(grep(test_phrase, two_grams$n_gram)) > 0){
      sub_two_grams <- two_grams[grep(test_phrase, two_grams[,1]),]
      sub_two_grams <- sub_two_grams %>% 
        mutate(p = 0.4*sub_two_grams$Freq/sum(sub_two_grams$Freq))
      p_twogram <- subset(sub_two_grams, select = c("n_gram", "p"))
      p_twogram$n_gram <- substring(p_twogram$n_gram, nchar(test_phrase))
      final_results <- p_twogram
    } else {
      
      p_onegrams <- one_grams
      p_onegrams<- subset(p_one_grams, select = c("n_gram","p"))
      final_results <- p_onegrams
      
      
    }
  return(final_results)
    
  }
  



#If the user inputs a phrase longer than three words, just take the last three words
#of the input phrase.  
input_phrase <- c("the yellow")

if(length(input_phrase)>0){
  num_words <- wordcount(input_phrase, sep = " ", count.function = sum)
} else {
  num_words <- 0
}

if(num_words > 3){
  input_phrase <- strsplit(input_phrase," ")
  input_phrase <- input_phrase[[1]][(num_words-2):num_words]
  input_phrase <- concatenate(input_phrase, collapse = " ",rm.space = FALSE)
} 

#Get the number of words in the input phrase.
if(num_words >0){
  word_count <- wordcount(input_phrase, sep=" ",count.function = sum)
}else{
  word_count <- 0
}


#If three words in phrase, use the 4-gram stupid backoff model.
if(word_count == 3){
  test_backoff_blogs <- stupid_backoff_4gram(input_phrase, one_grams_blogs, two_grams_blogs, three_grams_blogs, four_grams_blogs) 
  test_backoff_news <- stupid_backoff_4gram(input_phrase, one_grams_news, two_grams_news, three_grams_news, four_grams_news) 
  test_backoff_twitter <- stupid_backoff_4gram(input_phrase, one_grams_twitter, two_grams_twitter, three_grams_twitter, four_grams_twitter)

#If two words in input phrase, use the 3-gram stupid backoff model.
} else if(word_count == 2){
  test_backoff_blogs <- stupid_backoff_3gram(input_phrase, one_grams_blogs, two_grams_blogs, three_grams_blogs) 
  test_backoff_news <- stupid_backoff_3gram(input_phrase, one_grams_news, two_grams_news, three_grams_news) 
  test_backoff_twitter <- stupid_backoff_3gram(input_phrase, one_grams_twitter, two_grams_twitter, three_grams_twitter)

#If only one word in input phrase, use just the bigram stupid backoff model.    
} else if(word_count == 1){
  test_backoff_blogs <- stupid_backoff_2gram(input_phrase, one_grams_blogs, two_grams_blogs, three_grams_blogs) 
  test_backoff_news <- stupid_backoff_2gram(input_phrase, one_grams_news, two_grams_news, three_grams_news) 
  test_backoff_twitter <- stupid_backoff_2gram(input_phrase, one_grams_twitter, two_grams_twitter)

#If no words entered, ask the user to enter at least one word.    
} else{
  if(word_count == 0){
  print("Please enter at least one word:")
  }
}


#Print the results of the predicted words using the stupid backoff algorithm.

print(test_backoff_blogs[1:10,1])
print(test_backoff_news[1:10,1])
print(test_backoff_twitter[1:10,1])


#Record the end time of this code chunk and then calculate the elapsed time for running the code.
end_time <- Sys.time()

#Get the code chunk execution time.
end_time - start_time





#Perplexity calculations.  The perplexity does go down with n-gram models of 
#increasing n.
perplexity <- function(n_grams){
n_grams_perp <- n_grams %>% 
  mutate(p_inv_log =log2((n_grams$Freq/sum(n_grams$Freq))))
prob_sum <- sum(n_grams_perp$p_inv_log)/nrow(n_grams_perp)
perplexity_result <- 2^(-prob_sum)
return(perplexity_result)
}

one_grams_blogs_perp <- perplexity(one_grams_blogs)
one_grams_news_perp <- perplexity(one_grams_news)
one_grams_twitter_perp <- perplexity(one_grams_twitter)

two_grams_blogs_perp <- perplexity(two_grams_blogs)
two_grams_news_perp <- perplexity(two_grams_news)
two_grams_twitter_perp <- perplexity(two_grams_twitter)

three_grams_blogs_perp <- perplexity(three_grams_blogs)
three_grams_news_perp <- perplexity(three_grams_news)
three_grams_twitter_perp <- perplexity(three_grams_twitter)

four_grams_blogs_perp <- perplexity(four_grams_blogs)
four_grams_news_perp <- perplexity(four_grams_news)
four_grams_twitter_perp <- perplexity(four_grams_twitter)



#Start investigating linear interpolation model.
#Create the held-out dataset first.
write(clean_docs[[1]][[1]][300000:340000], "C:/Capstone/blogs_interpolation/en_US_blogs_clean_inter.txt", sep="\n")
write(clean_docs[[2]][[1]][20000:40000], "C:/Capstone/news_interpolation/en_US_news_clean_inter.txt", sep="\n")
write(clean_docs[[3]][[1]][400000:440000], "C:/Capstone/twitter_interpolation/en_US_twitter_clean_inter.txt", sep="\n")



#Load each held_out_document as separate corpuses.
dir_clean_int<- "C:/Capstone/blogs_interpolation/"
setwd(dir_clean_int)

clean_blog_int<- VCorpus(DirSource(directory = dir_clean_int),
                             readerControl=list(readPlain, language="en", load=TRUE))

dir_clean_int<- "C:/Capstone/news_interpolation/"
setwd(dir_clean_int)

clean_news_int<- VCorpus(DirSource(directory = dir_clean_int),
                             readerControl=list(readPlain, language="en", load=TRUE))


dir_clean_int<- "C:/Capstone/twitter_interpolation/"
setwd(dir_clean_int)

clean_twitter_int<- VCorpus(DirSource(directory = dir_clean_int),
                                readerControl=list(readPlain, language="en", load=TRUE))



#Generate one, two, three and four grams from all documents in held out set for interpolation.
one_grams_blogs_ho <- NgramTokenizer(clean_blog_int[[1]][[1]],1)
one_grams_news_ho <- NgramTokenizer(clean_news_int[[1]][[1]],1)
one_grams_twitter_ho <- NgramTokenizer(clean_twitter_int[[1]][[1]],1)
# 
# 
two_grams_blogs_ho <- NgramTokenizer(clean_blog_int[[1]][[1]],2)
two_grams_news_ho <- NgramTokenizer(clean_news_int[[1]][[1]],2)
two_grams_twitter_ho <- NgramTokenizer(clean_twitter_int[[1]][[1]],2)
# 
three_grams_blogs_ho <- NgramTokenizer(clean_blog_int[[1]][[1]],3)
three_grams_news_ho <- NgramTokenizer(clean_news_int[[1]][[1]],3)
three_grams_twitter_ho <- NgramTokenizer(clean_twitter_int[[1]][[1]],3)
# 
# 
four_grams_blogs_ho <- NgramTokenizer(clean_blog_int[[1]][[1]],4)
four_grams_news_ho <- NgramTokenizer(clean_news_int[[1]][[1]],4)
four_grams_twitter_ho <- NgramTokenizer(clean_twitter_int[[1]][[1]],4)



#Reduce the size of the 2, 3 and 4-gram tables.
two_grams_blogs_ho <- data_cutoff(two_grams_blogs_ho,2)
two_grams_news_ho <- data_cutoff(two_grams_news_ho,2)
two_grams_twitter_ho <- data_cutoff(two_grams_twitter_ho,2)
# 
three_grams_blogs_ho <- data_cutoff(three_grams_blogs_ho,2)
three_grams_news_ho <- data_cutoff(three_grams_news_ho,2)
three_grams_twitter_ho <- data_cutoff(three_grams_twitter_ho,2)
# 
four_grams_blogs_ho <- data_cutoff(four_grams_blogs_ho,2)
four_grams_news_ho <- data_cutoff(four_grams_news_ho,2)
four_grams_twitter_ho <- data_cutoff(four_grams_twitter_ho,2)



#Write these cutoff n-gram tables to csv files.  
#Write the Ngrams to separate CSV files so they can be easily loaded later.
write.csv(one_grams_blogs_ho, "C:/Capstone/OneGram_blogs_inter.csv", row.names = FALSE)
write.csv(one_grams_news_ho, "C:/Capstone/OneGram_news_inter.csv", row.names = FALSE)
write.csv(one_grams_twitter_ho, "C:/Capstone/OneGram_twitter_inter.csv", row.names = FALSE)
# 
write.csv(two_grams_blogs_ho, "C:/Capstone/TwoGram_blogs_inter.csv", row.names = FALSE)
write.csv(two_grams_news_ho, "C:/Capstone/TwoGram_news_inter.csv", row.names = FALSE)
write.csv(two_grams_twitter_ho, "C:/Capstone/TwoGram_twitter_inter.csv", row.names = FALSE)
# 
write.csv(three_grams_blogs_ho, "C:/Capstone/ThreeGram_blogs_inter.csv", row.names = FALSE)
write.csv(three_grams_news_ho, "C:/Capstone/ThreeGram_news_inter.csv", row.names = FALSE)
write.csv(three_grams_twitter_ho, "C:/Capstone/ThreeGram_twitter_inter.csv", row.names = FALSE)

write.csv(four_grams_blogs_ho, "C:/Capstone/FourGram_blogs_inter.csv", row.names = FALSE)
write.csv(four_grams_news_ho, "C:/Capstone/FourGram_news_inter.csv", row.names = FALSE)
write.csv(four_grams_twitter_ho, "C:/Capstone/FourGram_twitter_inter.csv", row.names = FALSE)
# 
# 
# 
#Read them in again and time it.  Should take less time.  Only takes 6 seconds!
one_grams_blogs_ho <- read.csv("C:/Capstone/OneGram_blogs_inter.csv", stringsAsFactors = F)
one_grams_news_ho <- read.csv("C:/Capstone/OneGram_news_inter.csv",stringsAsFactors = F)
one_grams_twitter_ho <- read.csv("C:/Capstone/OneGram_twitter_inter.csv",stringsAsFactors = F)

two_grams_blogs_ho <- read.csv("C:/Capstone/TwoGram_blogs_inter.csv",stringsAsFactors = F)
two_grams_news_ho <- read.csv("C:/Capstone/TwoGram_news_inter.csv",stringsAsFactors = F)
two_grams_twitter_ho <- read.csv("C:/Capstone/TWoGram_twitter_inter.csv",stringsAsFactors = F)

three_grams_blogs_ho <- read.csv("C:/Capstone/ThreeGram_blogs_inter.csv",stringsAsFactors = F)
three_grams_news_ho <- read.csv("C:/Capstone/ThreeGram_news_inter.csv",stringsAsFactors = F)
three_grams_twitter_ho <- read.csv("C:/Capstone/ThreeGram_twitter_inter.csv",stringsAsFactors = F)

four_grams_blogs_ho <- read.csv("C:/Capstone/FourGram_blogs_inter.csv",stringsAsFactors = F)
four_grams_news_ho <- read.csv("C:/Capstone/FourGram_news_inter.csv",stringsAsFactors = F)
four_grams_twitter_ho <- read.csv("C:/Capstone/FourGram_twitter_inter.csv",stringsAsFactors = F)









##MODEL 3:  Linear interpolation model.

start_time <- Sys.time()


#Read them in again and time it.  Should take less time.  Only takes 6 seconds!
one_grams_blogs <- read.csv("C:/Capstone/OneGram_blogs_smaller.csv", stringsAsFactors = F)
one_grams_news <- read.csv("C:/Capstone/OneGram_news_smaller.csv",stringsAsFactors = F)
one_grams_twitter <- read.csv("C:/Capstone/OneGram_twitter_smaller.csv",stringsAsFactors = F)

two_grams_blogs <- read.csv("C:/Capstone/TwoGram_blogs_smaller.csv",stringsAsFactors = F)
two_grams_news <- read.csv("C:/Capstone/TwoGram_news_smaller.csv",stringsAsFactors = F)
two_grams_twitter <- read.csv("C:/Capstone/TWoGram_twitter_smaller.csv",stringsAsFactors = F)

three_grams_blogs <- read.csv("C:/Capstone/ThreeGram_blogs_smaller.csv",stringsAsFactors = F)
three_grams_news <- read.csv("C:/Capstone/ThreeGram_news_smaller.csv",stringsAsFactors = F)
three_grams_twitter <- read.csv("C:/Capstone/ThreeGram_twitter_smaller.csv",stringsAsFactors = F)

four_grams_blogs <- read.csv("C:/Capstone/FourGram_blogs_smaller.csv",stringsAsFactors = F)
four_grams_news <- read.csv("C:/Capstone/FourGram_news_smaller.csv",stringsAsFactors = F)
four_grams_twitter <- read.csv("C:/Capstone/FourGram_twitter_smaller.csv",stringsAsFactors = F)



#Function to add maximum likelihood column to unigram frequency tables
add_MLs <- function(frequency_table){
  
  frequency_table <- frequency_table %>% mutate(p = frequency_table$Freq/sum(frequency_table$Freq))
  
  return(frequency_table)
  
}

#Add maximum likelihoods to each frequency table.
one_grams_blogs <- add_MLs(one_grams_blogs)
one_grams_news <- add_MLs(one_grams_news)
one_grams_twitter <- add_MLs(one_grams_twitter)

two_grams_blogs <- add_MLs(two_grams_blogs)
two_grams_news <- add_MLs(two_grams_news)
two_grams_twitter <- add_MLs(two_grams_twitter)

three_grams_blogs <- add_MLs(three_grams_blogs)
three_grams_news <- add_MLs(three_grams_news)
three_grams_twitter <- add_MLs(three_grams_twitter)

four_grams_blogs <- add_MLs(four_grams_blogs)
four_grams_news <- add_MLs(four_grams_news)
four_grams_twitter <- add_MLs(four_grams_twitter)







#Break up higher order n-grams into smaller n-gram columns.
ngram_subsets <- function(four_grams, three_grams, two_grams,one_grams){
  #Split all trigrams into individual words
  four_gram_split<- strsplit(four_grams$n_gram, " ") 


  third_last_word <- sapply(four_gram_split, "[[", 2)

  #Create a character vector of only the second word in each trigram.
  second_last_word <- sapply(four_gram_split, "[[", 3)

  #Create a character vector of only the last word in each trigram
  last_word <- sapply(four_gram_split, "[[", 4)

  #Create a vector of bigrams that are the last two words of each trigram.
  sub_three_grams<- paste(third_last_word, second_last_word, last_word, sep=" ")

  sub_bigrams <- paste(second_last_word, last_word, sep=" ")

 
  #Create a data frame of the bigrams from the dataset that are actually part of the last two
  #words of the trigrams in the trigram data frame.  Make sure to match the order that they are found 
  #in the trigram dataframe as well.
  three_grams_included <- three_grams %>% filter(three_grams$n_gram %in% sub_three_grams)
  three_grams_included <- three_grams_included[order(match(three_grams_included$n_gram, sub_three_grams)),]
    
  two_grams_included <- two_grams %>% filter(two_grams$n_gram %in% sub_bigrams)
  two_grams_included <- two_grams_included[order(match(two_grams_included$n_gram, sub_bigrams)),]

  one_grams_included <- one_grams %>% filter(one_grams$n_gram %in% last_word)
  one_grams_included <- one_grams_included[order(match(one_grams_included$n_gram, last_word)),]

  
  four_grams <- four_grams %>% mutate(three_grams = sub_three_grams)
  four_grams <- four_grams %>% mutate(two_grams = sub_bigrams)
  four_grams <- four_grams %>% mutate(one_grams = last_word)
  
  prob_three_grams <- three_grams_included$p[match(four_grams$three_grams, three_grams_included$n_gram)]
  prob_two_grams <- two_grams_included$p[match(four_grams$two_grams, two_grams_included$n_gram)]
  prob_one_grams <- one_grams_included$p[match(four_grams$one_grams, one_grams_included$n_gram)]
  
  four_grams <- four_grams %>% mutate(p_threegrams = prob_three_grams)
  four_grams <- four_grams %>% mutate(p_twograms = prob_two_grams)
  four_grams <- four_grams %>% mutate(p_onegrams = prob_one_grams)
  
return(four_grams)  
}


#Break up three grams into smaller n-grams.
threegram_subsets <- function(three_grams, two_grams,one_grams){
  #Split all trigrams into individual words
  three_gram_split<- strsplit(three_grams$n_gram, " ") 
  
  
  #Create a character vector of only the second word in each trigram.
  second_last_word <- sapply(three_gram_split, "[[", 2)
  
  #Create a character vector of only the last word in each trigram
  last_word <- sapply(three_gram_split, "[[", 3)
  
  
  sub_bigrams <- paste(second_last_word, last_word, sep=" ")
  
  
  #Create a data frame of the bigrams from the dataset that are actually part of the last two
  #words of the trigrams in the trigram data frame.  Make sure to match the order that they are found 
  #in the trigram dataframe as well.
  two_grams_included <- two_grams %>% filter(two_grams$n_gram %in% sub_bigrams)
  two_grams_included <- two_grams_included[order(match(two_grams_included$n_gram, sub_bigrams)),]
  
  one_grams_included <- one_grams %>% filter(one_grams$n_gram %in% last_word)
  one_grams_included <- one_grams_included[order(match(one_grams_included$n_gram, last_word)),]
  
  
  three_grams <- three_grams %>% mutate(two_grams = sub_bigrams)
  three_grams <- three_grams %>% mutate(one_grams = last_word)
  
  prob_two_grams <- two_grams_included$p[match(three_grams$two_grams, two_grams_included$n_gram)]
  prob_one_grams <- one_grams_included$p[match(three_grams$one_grams, one_grams_included$n_gram)]
  
  three_grams <- three_grams %>% mutate(p_twograms = prob_two_grams)
  three_grams <- three_grams %>% mutate(p_onegrams = prob_one_grams)
  
  return(three_grams)  
}


#Break up two garms into individual words and show the last word.
twogram_subsets <- function(two_grams,one_grams){
  #Split all trigrams into individual words
  two_gram_split<- strsplit(two_grams$n_gram, " ") 
  
  
  last_word <- sapply(two_gram_split, "[[", 2)
  
  
  #Create a data frame of the bigrams from the dataset that are actually part of the last two
  #words of the trigrams in the trigram data frame.  Make sure to match the order that they are found 
  #in the trigram dataframe as well.
  
  one_grams_included <- one_grams %>% filter(one_grams$n_gram %in% last_word)
  one_grams_included <- one_grams_included[order(match(one_grams_included$n_gram, last_word)),]
  
  
  two_grams <- two_grams %>% mutate(one_grams = last_word)
  
  prob_one_grams <- one_grams_included$p[match(two_grams$one_grams, one_grams_included$n_gram)]
  
  two_grams <- two_grams %>% mutate(p_onegrams = prob_one_grams)
  
  return(two_grams)  
}




#Add probability vectors to the datsets.
four_grams_blogs_p <- ngram_subsets(four_grams_blogs, three_grams_blogs, two_grams_blogs, one_grams_blogs)
four_grams_news_p <- ngram_subsets(four_grams_news, three_grams_news, two_grams_news, one_grams_news)
four_grams_twitter_p <- ngram_subsets(four_grams_twitter, three_grams_twitter, two_grams_twitter, one_grams_twitter)


three_grams_blogs_p <- threegram_subsets(three_grams_blogs, two_grams_blogs, one_grams_blogs)
three_grams_news_p <- threegram_subsets(three_grams_news, two_grams_news, one_grams_news)
three_grams_twitter_p <- threegram_subsets(three_grams_twitter, two_grams_twitter, one_grams_twitter)


two_grams_blogs_p <- twogram_subsets(two_grams_blogs, one_grams_blogs)
two_grams_news_p <- twogram_subsets(two_grams_news, one_grams_news)
two_grams_twitter_p <- twogram_subsets(two_grams_twitter, one_grams_twitter)






#Function to calculate the interpolated probability for each n-gram.
interpolation_fourgrams <- function(four_grams, lambda1,lambda2,lambda3,lambda4){

  four_grams$interpolated_p <- lambda1*four_grams$p + lambda2*four_grams$p_threegrams 
  + lambda3*four_grams$p_twograms + lambda4*four_grams$p_onegrams
  four_grams <- four_grams[order(four_grams$interpolated_p, decreasing=T),]

  return(four_grams)  
}



#Function to calculate the interpolated probability for each n-gram.
interpolation_threegrams <- function(three_grams, lambda1,lambda2,lambda3){
  
  three_grams$interpolated_p <- lambda1*three_grams$p + lambda2*three_grams$p_twograms 
  + lambda3*three_grams$p_onegrams
  three_grams <- three_grams[order(three_grams$interpolated_p, decreasing=T),]
  
  return(three_grams)  
}




#Function to calculate the interpolated probability for each n-gram.
interpolation_twograms <- function(two_grams, lambda1,lambda2){
  
  two_grams$interpolated_p <- lambda1*two_grams$p + lambda2*two_grams$p_onegrams 
  two_grams <- two_grams[order(two_grams$interpolated_p, decreasing=T),]
  
  return(two_grams)  
}




#Set lambda values for the interpolation.
lambda1_4gram <- 0.25
lambda2_4gram <- 0.25
lambda3_4gram <- 0.25
lambda4_4gram <- 0.25


lambda1_3gram <- 1/3
lambda2_3gram <- 1/3
lambda3_3gram <- 1/3


lambda1_2gram <- 0.5
lambda2_2gram <- 0.5



#Use the interpolation function to do the interpolation on each n-gram dataset.
four_grams_blogs_li <- interpolation_fourgrams(four_grams_blogs_p, lambda1_4gram, lambda2_4gram, lambda3_4gram, lambda4_4gram)
four_grams_news_li <- interpolation_fourgrams(four_grams_news_p, lambda1_4gram, lambda2_4gram, lambda3_4gram, lambda4_4gram)
four_grams_twitter_li <- interpolation_fourgrams(four_grams_twitter_p, lambda1_4gram, lambda2_4gram, lambda3_4gram, lambda4_4gram)


three_grams_blogs_li <- interpolation_threegrams(three_grams_blogs_p, lambda1_3gram, lambda2_3gram, lambda3_3gram)
three_grams_news_li <- interpolation_threegrams(three_grams_news_p, lambda1_3gram, lambda2_3gram, lambda3_3gram)
three_grams_twitter_li <- interpolation_threegrams(three_grams_twitter_p, lambda1_3gram, lambda2_3gram, lambda3_3gram)


two_grams_blogs_li <- interpolation_twograms(two_grams_blogs_p, lambda1_2gram, lambda2_2gram)
two_grams_news_li <- interpolation_twograms(two_grams_news_p, lambda1_2gram, lambda2_2gram)
two_grams_twitter_li <- interpolation_twograms(two_grams_twitter_p, lambda1_2gram, lambda2_2gram)




#Now we need to make predictions with the linear interpolation model.  
input_phrase <- c("the yellow")



#Create the prediction function for the interpolation model.
prediction <- function(input_phrase, four_grams_li, three_grams_li, two_grams_li, one_grams){
  
  #Get the word count from the input phrase and truncate it to 3 words if the phrase was longer
  #than three words.
  if(length(input_phrase)>0){
  num_words <- wordcount(input_phrase, sep = " ", count.function = sum)
  } else
  {
    num_words <- 0
  }
  
  if(num_words > 3){
    input_phrase <- strsplit(input_phrase," ")
    input_phrase <- input_phrase[[1]][(num_words-2):num_words]
    input_phrase <- concatenate(input_phrase, collapse = " ",rm.space = FALSE)
   } 
  
  #Get the number of words in the input phrase.
  if(num_words >0){
  word_count <- wordcount(input_phrase, sep=" ",count.function = sum)
  } else{
    word_count <- 0
  }
  
  test_phrase <- paste("^", input_phrase, " ", sep="")  
  
  
  #check if you are predicting the 4th word from an input trigram.
if(word_count ==3){
if(length(grep(test_phrase, four_grams_li$n_gram)) > 0)
{

#Get the top 10 best guesses if the trigram is found in the list of 4grams.  
sub_four_grams<- four_grams_li[grep(test_phrase, four_grams_li[,1]),] 
results_fourgram <- subset(sub_four_grams, select = c("n_gram","interpolated_p"))
results_fourgram$n_gram <- substring(results_fourgram$n_gram,nchar(test_phrase))
final_results <- results_fourgram
print(final_results[1:10,1])
}else{
  
  #Make the 3 word phrase into a two word phrase.
  split_3wordphrase <- strsplit(input_phrase, " ")   
  two_word_phrase <- c(split_3wordphrase[[1]][2], split_3wordphrase[[1]][3]) 
  two_word_phrase <- concatenate(two_word_phrase, collapse = " ",rm.space = FALSE)
  two_word_phrase <- paste("^", two_word_phrase, " ", sep="")
  
  #See in which trigrams the two word phrase shows up and get the trigrams that
  #meet this criterion.
  if(length(grep(two_word_phrase, three_grams_li$n_gram)) > 0)
  {
    sub_three_grams<- three_grams_li[grep(two_word_phrase, three_grams_li[,1]),] 
    results_threegram <- subset(sub_three_grams, select = c("n_gram","interpolated_p"))
    results_threegram$n_gram <- substring(results_threegram$n_gram,nchar(two_word_phrase))
    final_results <- results_threegram
    print(final_results[1:10,1])
  }else{
    
      #Make two word phrase into a single word.
      split_3wordphrase <- strsplit(input_phrase, " ")
      previous_word <- split_3wordphrase[[1]][3]
      previous_word <- paste("^",previous_word," ",sep="")
      
      #Check to see if the input word is in any of the bigrams.  Return subset
      #of bigrams in which this is the case.
      if(length(grep(previous_word, two_grams_li$n_gram)) > 0)
      {
        sub_two_grams<- two_grams_li[grep(previous_word, two_grams_li[,1]),] 
        results_twogram <- subset(sub_two_grams, select = c("n_gram","interpolated_p"))
        results_twogram$n_gram <- substring(results_twogram$n_gram,nchar(previous_word))
        final_results <- results_twogram
        print(final_results[1:10,1])
      } else{
             
            results_onegram <- subset(one_grams, select = c("n_gram", "p"))
            final_results <- results_onegram
            print(final_results[1:10,1])
        
            }
      }
}
  
  
  #Repeat the above commands if only a bigram is input.
} else if(word_count==2){
  
  if(length(grep(test_phrase, three_grams_li$n_gram)) > 0)
  {
    sub_three_grams <- three_grams_li[grep(test_phrase, three_grams_li[,1]),] 
    results_threegram <- subset(sub_three_grams, select = c("n_gram","interpolated_p"))
    results_threegram$n_gram <- substring(results_threegram$n_gram,nchar(test_phrase))
    final_results <- results_threegram
    print(final_results[1:10,1])
  }else{
    
    #Make two word phrase into a single word.
    split_2wordphrase <- strsplit(input_phrase, " ")
    previous_word <- split_2wordphrase[[1]][2]
    previous_word <- paste("^",previous_word," ",sep="")
    
    if(length(grep(previous_word, two_grams_li$n_gram)) > 0)
    {
      sub_two_grams<- two_grams_li[grep(previous_word, two_grams_li[,1]),] 
      results_twogram <- subset(sub_two_grams, select = c("n_gram","interpolated_p"))
      results_twogram$n_gram <- substring(results_twogram$n_gram,nchar(previous_word))
      final_results <- results_twogram
      print(final_results[1:10,1])
    } else{
      
      results_onegram <- subset(one_grams, select = c("n_gram", "p"))
      final_results <- results_onegram
      print(final_results[1:10,1])
      
    }
  }
  
  #Repeat the above approach if only a single word is entered.
} else if(word_count == 1){
  
  if(length(grep(test_phrase, two_grams_li$n_gram)) > 0)
  {
    sub_two_grams<- two_grams_li[grep(test_phrase, two_grams_li[,1]),] 
    results_twogram <- subset(sub_two_grams, select = c("n_gram","interpolated_p"))
    results_twogram$n_gram <- substring(results_twogram$n_gram,nchar(test_phrase))
    final_results <- results_twogram
    print(final_results[1:10,1])
  } else{
    
    results_onegram <- subset(one_grams, select = c("n_gram", "p"))
    final_results <- results_onegram
    print(final_results[1:10,1])
    
  }
  
} else{
  
  
  #Ask the user to enter at least one word if they didn't type anything in the first time.
  if(word_count == 0){
    print("Please enter at least one word:")
  }
  
}
}  
  
#Now call the linear interpolation prediction for blogs, news and tweets.
prediction(input_phrase, four_grams_blogs_li, three_grams_blogs_li, two_grams_blogs_li, one_grams_blogs)  
prediction(input_phrase, four_grams_news_li, three_grams_news_li, two_grams_news_li, one_grams_news)
prediction(input_phrase, four_grams_twitter_li, three_grams_twitter_li, two_grams_twitter_li, one_grams_twitter) 


#Record end execution times and the total execution time for the linear interpolation prediction.
end_time <- Sys.time()

end_time - start_time
