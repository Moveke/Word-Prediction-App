# build-ngram-frequencies.R
# author: Amine Agrane
# date: 15 Feb, 2021
# Description: Prepare n-gram frequencies

library(tm)
library(dplyr)
library(stringi)
library(stringr)
library(quanteda)
library(data.table)

# ------------------------------------------------------------------------------
# Prepare environment
# ------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
setwd("/Users/bnpp/Desktop/coursera-data-science-capstone-master/shiny-app")

# ------------------------------------------------------------------------------
# Load the training data
# ------------------------------------------------------------------------------

# blogs
con <- file("./data/en_US/en_US.blogs.txt", open = "r")
blogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

# news
con <- file("./data/en_US/en_US.news.txt", open = "r")
news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

# twitter
con <- file("./data/en_US/en_US.twitter.txt", open = "r")
twitter <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

print("Loaded training data")
print(paste0("Number of lines per file (blogs):   ", format(length(blogs), big.mark = ",")))
print(paste0("Number of lines per file (news):    ", format(length(news), big.mark = ",")))
print(paste0("Number of lines per file (twitter): ", format(length(twitter), big.mark = ",")))
print(paste0("Number of lines per file (total):   ", format(length(blogs) + length(news) + length(twitter), big.mark = ",")))


# ------------------------------------------------------------------------------
# Prepare the data
# ------------------------------------------------------------------------------

# set seed for reproducability
set.seed(42)

# assign sample size
# Merging the sample datasets into one, trainMerged is a vector of sentences
sampleData <- c(sample(blogs, length(blogs) * 0.2), sample(news, length(news) * 0.2), sample(twitter, length(twitter) * 0.2))


# get number of lines and words from the sample data set
sampleDataLines <- length(sampleData)
sampleDataWords <- sum(stri_count_words(sampleData))
print("Create sample data set")
print(paste0("Number of lines:  ", format(sampleDataLines, big.mark = ",")))
print(paste0("Number of words: ", format(sampleDataWords, big.mark = ",")))


# ------------------------------------------------------------------------------
# Clean the data
# ------------------------------------------------------------------------------

# load bad words file
con <- file("./data/bad-words.txt", open = "r")
profanity <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
profanity <- iconv(profanity, "latin1", "ASCII", sub = "")
close(con)

# convert text to lowercase
sampleData <- tolower(sampleData)

# remove URL, email addresses, Twitter handles and hash tags
sampleData <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", sampleData, ignore.case = FALSE, perl = TRUE)
sampleData <- gsub("\\S+[@]\\S+", "", sampleData, ignore.case = FALSE, perl = TRUE)
sampleData <- gsub("@[^\\s]+", "", sampleData, ignore.case = FALSE, perl = TRUE)
sampleData <- gsub("#[^\\s]+", "", sampleData, ignore.case = FALSE, perl = TRUE)

# remove ordinal numbers
sampleData <- gsub("[0-9](?:st|nd|rd|th)", "", sampleData, ignore.case = FALSE, perl = TRUE)

# remove profane words
sampleData <- removeWords(sampleData, profanity)

# remove punctuation
sampleData <- gsub("[^\\p{L}'\\s]+", "", sampleData, ignore.case = FALSE, perl = TRUE)

# remove punctuation (leaving ')
sampleData <- gsub("[.\\-!]", " ", sampleData, ignore.case = FALSE, perl = TRUE)

# trim leading and trailing whitespace
sampleData <- gsub("^\\s+|\\s+$", "", sampleData)
sampleData <- stripWhitespace(sampleData)

# write sample data set to disk
sampleDataFileName <- "data/en_US.sample.txt"
con <- file(sampleDataFileName, open = "w")
writeLines(sampleData, con)
close(con)

# remove variables no longer needed to free up memory
rm(badWordsURL, badWordsFile, con, sampleDataFileName, profanity)

# ------------------------------------------------------------------------------
# Build corpus
# ------------------------------------------------------------------------------

corpus <- corpus(sampleData)

# ------------------------------------------------------------------------------
# Build n-gram frequencies
# ------------------------------------------------------------------------------

getTopThree <- function(corpus) {
    first <- !duplicated(corpus$token)
    balance <- corpus[!first,]
    first <- corpus[first,]
    second <- !duplicated(balance$token)
    balance2 <- balance[!second,]
    second <- balance[second,]
    third <- !duplicated(balance2$token)
    third <- balance2[third,]
    return(rbind(first, second, third))
}

# Generate a token frequency dataframe. Do not remove stemwords because they are
# possible candidates for next word prediction.
tokenFrequency <- function(corpus, n = 1, rem_stopw = NULL) {
    corpus <- dfm(corpus, ngrams = n)
    corpus <- colSums(corpus)
    total <- sum(corpus)
    corpus <- data.frame(names(corpus),
                         corpus,
                         row.names = NULL,
                         check.rows = FALSE,
                         check.names = FALSE,
                         stringsAsFactors = FALSE
    )
    colnames(corpus) <- c("token", "n")
    corpus <- mutate(corpus, token = gsub("_", " ", token))
    corpus <- mutate(corpus, percent = corpus$n / total)
    if (n > 1) {
        corpus$outcome <- word(corpus$token, -1)
        corpus$token <- word(string = corpus$token, start = 1, end = n - 1, sep = fixed(" "))
    }
    setorder(corpus, -n)
    corpus <- getTopThree(corpus)
    return(corpus)
}

# get top 3 words to initiate the next word prediction app
startWord <- word(corpus$documents$texts, 1)  # get first word for each document
startWord <- tokenFrequency(startWord, n = 1, NULL)  # determine most popular start words
startWordPrediction <- startWord$token[1:3]  # select top 3 words to start word prediction app
saveRDS(startWordPrediction, "./data/start-word-prediction2.RData")

# bigram
bigram <- tokenFrequency(corpus, n = 2, NULL)
saveRDS(bigram, "./data/bigram2.RData")
remove(bigram)

# trigram
trigram <- tokenFrequency(corpus, n = 3, NULL)
trigram <- trigram %>% filter(n > 1)
saveRDS(trigram, "./data/trigram2.RData")
remove(trigram)

# quadgram
quadgram <- tokenFrequency(corpus, n = 4, NULL)
quadgram <- quadgram %>% filter(n > 1)
saveRDS(quadgram, "./data/quadgram2.RData")
remove(quadgram)
