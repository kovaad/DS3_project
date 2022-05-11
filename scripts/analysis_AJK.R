##############################
##      Data Science 3      ##
##   Kovács Ádám József   ##
###########################

# preparatory steps -------------------------------------------------------

#clear environment
rm(list = ls())

#load packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(dplyr,tidyverse, quanteda, quanteda.textstats, ggrepel,text2vec, topicmodels,ggfortify,ggwordcloud)

#check out custom theme
source("theme_adam.R")

#read in data
df <- read_csv("data/full_df_correct.csv")


#sentiment analysis
hu_stop_word <- read_csv("data/stopwords-hu.csv")

positive_words <- read_csv("data/PrecoSenti/PrecoPos.csv") %>%
  mutate(sentiment=1)

negative_words <- read_csv("data/PrecoSenti/PrecoNeg.csv") %>%
  mutate(sentiment=-1)

hungarian_sentiment <- rbind(positive_words, negative_words)


