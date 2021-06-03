# server.R
# Author: Maroje Raguž
# Date: 01-Jul-2021
# Description: Shiny Server, Coursera Data Science Capstone Final Project
# GitHub: https://github.com/MRHub13/DSS-Capstone-Project

# ... loading training Data, created by `¸The_Script.R`
uni_words <- readRDS("./ngram_match/app_data/uni_grams_fast.rds")
bi_words <- readRDS("./ngram_match/app_data/bi_grams_fast.rds")
tri_words  <- readRDS("./ngram_match/app_data/tri_grams_fast.rds")
quad_words <- readRDS("./ngram_match/app_data/quad_grams_fast.rds")
quint_words <- readRDS("./ngram_match/app_data/quint_grams_fast.rds")
sext_words <- readRDS("./ngram_match/app_data/sext_grams_fast.rds")

# ... creating 'n-gram' matching functions

unigram <- function(input_words){
    num <- length(input_words)
    filter(uni_words, 
           word1==input_words[num]) %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 1)) %>%
        as.character() -> out
    ifelse(out =="character(0)", "?", return(out))
}

bigram <- function(input_words){
    num <- length(input_words)
    filter(bi_words, 
           word1==input_words[num]) %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 2)) %>%
        as.character() -> out
    ifelse(out =="character(0)", "?", return(out))
}

trigram <- function(input_words){
    num <- length(input_words)
    filter(tri_words, 
           word1==input_words[num-1], 
           word2==input_words[num])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 3)) %>%
        as.character() -> out
    ifelse(out=="character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
    num <- length(input_words)
    filter(quad_words, 
           word1==input_words[num-2], 
           word2==input_words[num-1], 
           word3==input_words[num])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 4)) %>%
        as.character() -> out
    ifelse(out=="character(0)", trigram(input_words), return(out))
}

quintgram <- function(input_words){
    num <- length(input_words)
    filter(quint_words, 
           word1==input_words[num-3], 
           word2==input_words[num-2], 
           word3==input_words[num-1], 
           word4==input_words[num])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 5)) %>%
        as.character() -> out
    ifelse(out=="character(0)", quadgram(input_words), return(out))
}

sextgram <- function(input_words){
    num <- length(input_words)
    filter(sext_words, 
           word1==input_words[num-4],
           word2==input_words[num-3], 
           word3==input_words[num-2], 
           word4==input_words[num-1], 
           word5==input_words[num])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 6)) %>%
        as.character() -> out
    ifelse(out=="character(0)", quintgram(input_words), return(out))
}

# ... create user input ... and Data Cleaning Function; Calls the matching functions
ngrams <- function(input){
    # ... creating a dataframe
    input <- data_frame(text = input)
    # ... cleaning the input
    replace_reg <- "[^[:alpha:][:space:]]*"
    input <- input %>%
        mutate(text = str_replace_all(text, replace_reg, ""))
    # ... word counting, separating words, lower case
    input_count <- str_count(input, boundary("word"))
    input_words <- unlist(str_split(input, boundary("word")))
    input_words <- tolower(input_words)
    # ... calling the matching functions
    out <- ifelse(input_count == 0, "Please input a word or phrase",
                  ifelse(input_count == 5, sextgram(input_words),
                         ifelse(input_count == 4, quintgram(input_words),
                                ifelse(input_count == 3, quadgram(input_words),
                                       ifelse(input_count == 2, trigram(input_words),
                                              ifelse(input_count == 1, bigram(input_words), unigram(input_words)))))))
    
    # ... output
    return(out)
}