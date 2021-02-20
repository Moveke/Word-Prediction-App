#Load the necessary libraries.
suppressWarnings(library(tm))
suppressWarnings(library(ngram))
suppressWarnings(library(dplyr))
suppressWarnings(library(shiny))

options(shiny.maxRequestSize = 30*1024^2)

#Read in all the text files needed for the app word prediction.
one_grams_blogs <- read.csv("onegram_blogs_smaller.csv", stringsAsFactors = F)
one_grams_news <- read.csv("onegram_news_smaller.csv",stringsAsFactors = F)
one_grams_twitter <- read.csv("onegram_twitter_smaller.csv",stringsAsFactors = F)

two_grams_blogs <- read.csv("twogram_blogs_smaller.csv",stringsAsFactors = F)
two_grams_news <- read.csv("twogram_news_smaller.csv",stringsAsFactors = F)
two_grams_twitter <- read.csv("twogram_twitter_smaller.csv",stringsAsFactors = F)

three_grams_blogs <- read.csv("threegram_blogs_smaller.csv",stringsAsFactors = F)
three_grams_news <- read.csv("threegram_news_smaller.csv",stringsAsFactors = F)
three_grams_twitter <- read.csv("threegram_twitter_smaller.csv",stringsAsFactors = F)

four_grams_blogs <- read.csv("fourgram_blogs_smaller.csv",stringsAsFactors = F)
four_grams_news <- read.csv("fourgram_news_smaller.csv",stringsAsFactors = F)
four_grams_twitter <- read.csv("fourgram_twitter_smaller.csv",stringsAsFactors = F)


#Function to add maximum likelihood column to unigram frequency tables
add_MLs <- function(frequency_table){
  
  frequency_table <- frequency_table %>% mutate(p = frequency_table$Freq/sum(frequency_table$Freq))
  
  return(frequency_table)
  
}


#Add the maximum likelihood column to each unigram table.
one_grams_blogs <- add_MLs(one_grams_blogs)
one_grams_news <- add_MLs(one_grams_news)
one_grams_twitter <- add_MLs(one_grams_twitter)






#Stupid backoff algorithm function for 4-grams. 
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
        p_onegrams<- subset(p_onegrams, select = c("n_gram","p"))
        final_results <- p_onegrams
        
        
      }
      
      
    } 
  }
  return(final_results)
}





#Stupid backoff algorithm function for trigrams.
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
      p_onegrams<- subset(p_onegrams, select = c("n_gram","p"))
      final_results <- p_onegrams
      
      
    }
    
    
  }
  return(final_results)
}



#Stupid backoff algorithm function for bigrams.
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
    p_onegrams<- subset(p_onegrams, select = c("n_gram","p"))
    final_results <- p_onegrams
    
    
  }
  return(final_results)
  
}






#Function to predict the next word following the previous one, two or three input words.
#Makes use of the stupid backoff algorithm.
Prediction <- function(input_phrase, one_grams,two_grams,three_grams,four_grams){

#Count the number of words in the input phrase  
if(length(input_phrase)>0){
  num_words <- wordcount(input_phrase, sep = " ", count.function = sum)
} else {
  num_words <- 0
}

#If the number of words in the input phrase is larger than 3, cut it down to the last
#three words.
if(num_words > 3){
  input_phrase <- strsplit(input_phrase," ")
  input_phrase <- input_phrase[[1]][(num_words-2):num_words]
  input_phrase <- concatenate(input_phrase, collapse = " ",rm.space = FALSE)
} 

#Get the number of words in the modified input phrase.
if(num_words >0){
  word_count <- wordcount(input_phrase, sep=" ",count.function = sum)
}else{
  word_count <- 0
}


#If three words in phrase, use the 4-gram stupid backoff model to predict the
#most likely word..
if(word_count == 3){
  predicted_word <- stupid_backoff_4gram(input_phrase, one_grams, two_grams, three_grams, four_grams) 
  predicted_word <-predicted_word[1,1]
  

  
  #If two words in input phrase, use the 3-gram stupid backoff model to predict the 
  #most likely word.
} else if(word_count == 2){
  predicted_word <- stupid_backoff_3gram(input_phrase, one_grams, two_grams, three_grams) 
  predicted_word <-predicted_word[1,1]
  
 
  
  #If only one word in input phrase, use just the bigram stupid backoff model to predict
  #the most likely word.
} else if(word_count == 1){
  predicted_word <- stupid_backoff_2gram(input_phrase, one_grams, two_grams) 
  predicted_word <-predicted_word[1,1]
  
  #If no words entered, ask the user to enter at least one word.    
} else{
  if(word_count == 0){
    predicted_word <- c("Please enter at least one word:")
  }
}


#Return the results of the predicted words.
return(predicted_word)

}
