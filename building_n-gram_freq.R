# building_n-gram_freq.R
# Author: Maroje Raguž
# Date: 01-Jul-2021
# Description: Preparing n-gram frequencies
# GitHub: https://github.com/MRHub13/DSS-Capstone-Project


# ... initial setup
start <- Sys.time()

library(doParallel)
n_cores <- detectCores() - 1  
registerDoParallel(n_cores,cores=n_cores)



suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
  library(dplyr)
})

# ... downloading & unzipping the data
trainURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
trainDataFile <- "data/Coursera-SwiftKey.zip"
if (!file.exists('data')) {
  dir.create('data')
}
if (!file.exists("data/final/en_US")) {
  tempFile <- tempfile()
  download.file(trainURL, tempFile)
  unzip(tempFile, exdir = "data")
  unlink(tempFile)
}

# ... loading & reading the data
blogs_file   <- "./data/final/en_US/en_US.blogs.txt"
news_file    <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"  

blogs   <- readLines(blogs_file, skipNul = TRUE)
news    <- readLines(news_file,  skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE)

# ... creating dataframes
blogs   <- data_frame(text = blogs)
news    <- data_frame(text = news)
twitter <- data_frame(text = twitter)

# ... sampling the data
set.seed(999)
sample_pct <- 0.30
blogs_sample <- blogs %>%
  sample_n(., nrow(blogs)*sample_pct)
news_sample <- news %>%
  sample_n(., nrow(news)*sample_pct)
twitter_sample <- twitter %>%
  sample_n(., nrow(twitter)*sample_pct)

# ... creating repository sample
repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter")) 
repo_sample$source <- as.factor(repo_sample$source)

# ... memory cleanig
rm(list = c("blogs", "blogs_file", "blogs_sample","news", "news_file",     
            "news_sample", "sample_pct", "twitter","twitter_file", 
            "twitter_sample"))

# ... creating filters: non-alphanumeric's, url's, repeated letters(+3x)
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  

# ... cleaning the sample ... 
    # ... cleaning is separted from tidying so `unnest_tokens` function can be 
    # ... used for words, and ngrams
clean_sample <-  repo_sample %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

rm(list = c("repo_sample"))

# ... creating 'n-grams'

# .. unigrams
unigram_repo <- clean_sample  %>%
  unnest_tokens(unigram, text, token = "ngrams", n = 1)


# .. bigrams
bigram_repo <- clean_sample  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# .. trigrams
trigram_repo <- clean_sample  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

# ... quadgrams
quadgram_repo <- clean_sample  %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4)

# ... quintgrams
quintgram_repo <- clean_sample  %>%
  unnest_tokens(quintgram, text, token = "ngrams", n = 5)

# ... sextgrams 
sextgram_repo <- clean_sample  %>%
  unnest_tokens(sextgram, text, token = "ngrams", n = 6)


# ... reducing n-grams 

# ... unigrams
unigram_cover <- unigram_repo %>%
  count(unigram) %>%
  na.exclude %>%
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("unigram_repo"))

# ... bigrams
bigram_cover <- bigram_repo %>%
  count(bigram) %>%
  na.exclude %>%
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("bigram_repo"))

# .. trigrams
trigram_cover <- trigram_repo %>%
  count(trigram) %>%
  na.exclude %>%
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("trigram_repo"))

# .. quadgrams
quadgram_cover <- quadgram_repo %>%
  count(quadgram) %>%
  na.exclude %>%
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("quadgram_repo"))

# .. quintgrams
quintgram_cover <- quintgram_repo %>%
  count(quintgram) %>%  
  na.exclude %>%
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("quintgram_repo"))

# ... sextgrams
sextgram_cover <- sextgram_repo %>%
  count(sextgram) %>%  
  na.exclude %>% 
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("sextgram_repo"))

# ... visualisation of 'n-grams' 

disty <- data_frame(ngram = c(rep("unigrams",  nrow(unigram_cover)),
                              rep("bigrams",  nrow(bigram_cover)),
                              rep("trigrams",  nrow(trigram_cover)),
                              rep("quadgrams", nrow(quadgram_cover)),
                              rep("quintgrams", nrow(quintgram_cover)),
                              rep("sextgrams",  nrow(sextgram_cover))),
                    number = c(unigram_cover$n,bigram_cover$n,  trigram_cover$n, 
                               quadgram_cover$n, quintgram_cover$n,
                               sextgram_cover$n))
disty
disty$ngram <- as.factor(disty$ngram)
ggplot(data = disty, aes(y = number, x = reorder(ngram, -number))) +
  geom_boxplot() + scale_y_log10() +
  xlab("ngram")
ggsave("./ngram_match/www/ngrams.png")

sextgram_cover %>%
  top_n(10, n) %>%
  mutate(sextgram = reorder(sextgram, n)) %>%
  ggplot(aes(sextgram, n)) +
  geom_bar( stat = "Identity" , fill = I("green"))+
  xlab(NULL) +
  coord_flip() +
  ggtitle("sextgrams")
ggsave("./ngram_match/www/sextgrams.png")

quintgram_cover %>%
  top_n(10, n) %>%
  mutate(quintgram = reorder(quintgram, n)) %>%
  ggplot(aes(quintgram, n)) +
  geom_bar( stat = "Identity" , fill = I("green"))+
  xlab(NULL) +
  coord_flip() +
  ggtitle("quintgrams")
ggsave("./ngram_match/www/quintgrams.png")

quadgram_cover %>%
  top_n(10, n) %>%
  mutate(quadgram = reorder(quadgram, n)) %>%
  ggplot(aes(quadgram, n)) +
  geom_bar( stat = "Identity" , fill = I("green"))+
  xlab(NULL) +
  coord_flip() +
  ggtitle("quadgrams")
ggsave("./ngram_match/www/quadgrams.png")

trigram_cover %>%
  top_n(10, n) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_bar( stat = "Identity" , fill = I("green"))+
  xlab(NULL) +
  coord_flip() +
  ggtitle("trigrams")
ggsave("./ngram_match/www/trigrams.png")

bigram_cover %>%
  top_n(10, n) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_bar( stat = "Identity" , fill = I("green"))+
  xlab(NULL) +
  coord_flip() +
  ggtitle("bigrams")
ggsave("./ngram_match/www/bigrams.png")

unigram_cover %>%
  top_n(10, n) %>%
  mutate(unigram = reorder(unigram, n)) %>%
  ggplot(aes(unigram, n)) +
  geom_bar( stat = "Identity" , fill = I("green"))+
  xlab(NULL) +
  coord_flip() +
  ggtitle("unigrams")
ggsave("./ngram_match/www/unigrams.png")

# ... words separating

uni_words <- unigram_cover %>%
  separate(unigram, c("word1"), sep = " ")
uni_words

bi_words <- bigram_cover %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bi_words

tri_words <- trigram_cover %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
tri_words

quad_words <- quadgram_cover %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
quad_words

quint_words <- quintgram_cover %>%
  separate(quintgram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
quint_words

sext_words <- sextgram_cover %>%
  separate(sextgram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
sext_words


# ... saving the data for the Shiny App
saveRDS(uni_words, "./ngram_match/app_data/uni_grams_fast.rds")
saveRDS(bi_words, "./ngram_match/app_data/bi_grams_fast.rds")
saveRDS(tri_words, "./ngram_match/app_data/tri_grams_fast.rds")
saveRDS(quad_words,"./ngram_match/app_data/quad_grams_fast.rds")
saveRDS(quint_words,"./ngram_match/app_data/quint_grams_fast.rds")
saveRDS(sext_words,"./ngram_match/app_data/sext_grams_fast.rds")
#' 
#' -------------
#'  
#' ## Session info
sessionInfo()

end <- Sys.time()
elapsed <- end - start
elapsed
