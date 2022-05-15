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
pacman::p_load(dplyr,tidyverse, stringr, lubridate, tidytext, HunMineR, quanteda)# quanteda.textstats, ggrepel,text2vec, topicmodels,ggfortify,ggwordcloud)

#check out custom theme
source("theme_adam.R")

#read in data
df <- read_csv("../data/full_df_correct.csv")

# descriptives ------------------------------------------------------------

#create target variable of before and after the elections
#rename body for text
#remove any unnecessary whitespces
df <- df |> 
  mutate(
    label = ifelse(dates < ymd("2022-4-3"), "before", "after") 
  ) |> 
  rename(text = body) |> 
  mutate(
    text = stringr::str_trim(text),
    text = stringr::str_squish(text)
  )

#create tidy tokens dataframe using tidytext
tokens <- df |> 
  unnest_tokens(word, text)

#count tokens by article
tok_count <- tokens |> 
  group_by(dates,links) |> 
  summarise( 
    sum_tokens = n()
  )

#add to dataframe this information
df <- left_join(df, tok_count[, names(tok_count) != "dates"], by = "links")

#look at summary of tokens
summary(df$sum_tokens)

#remove those that contain less than 10 words
df <- subset(df, !(sum_tokens <= 10))

#create chart of number of articles by news portal
count_by_portal <- df |> group_by(name) |> summarise(count = n()) |> arrange(desc(count))

ggplot(count_by_portal, aes(reorder(name,-count), count)) +
  geom_bar(stat = "identity") +
  labs(
    y = "Number of articles",
    x = NULL
  ) +
  geom_text(data=count_by_portal,aes(label=count,y=count),vjust=-0.5) +
  theme_adam() 

#get number of articles and average length of articles by date
overtime <- toc_count %>%
  group_by(dates) %>% 
  summarise( 
    n_articles = n(),
    avg_tokens = mean(sum_tokens)
  )


#plot number of articles in dataset over time
arrows <- 
  tibble(
    x1 = c(ymd("2022-2-8"), ymd("2022-4-15")),
    x2 =  c(ymd("2022-2-24"),ymd("2022-4-3")),
    y1 = c(55, 45), 
    y2 = c(65, 48)
  )

ggplot(overtime, aes(dates, n_articles)) +
  geom_line() +
  labs(
    y = "Cikkek száma",
    x = NULL
  ) + 
  geom_vline(xintercept=ymd("2022-2-24"), linetype="dashed", 
             color = "red", size=1) + 
  geom_vline(xintercept=ymd("2022-4-3"), linetype="dashed", 
             color = "red", size=1) +
  ggplot2::annotate("text", x = ymd("2022-2-8"), y = 52, label = "Russian invasion of \n Ukraine") +
  ggplot2::annotate("text", x = ymd("2022-4-19"), y = 48, label = "Elections/\nBucha massacre") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = -0.3) +
  theme_adam()



#plot of evolution of length of articles
arrows <- 
  tibble(
    x1 = c(ymd("2022-2-3"), ymd("2022-4-23")),
    x2 =  c(ymd("2022-2-24"),ymd("2022-4-3")),
    y1 = c(650, 200), 
    y2 = c(700, 250)
  )

ggplot(overtime, aes(dates, avg_tokens)) +
  geom_line() +
  labs(
    y = "Average length of articles",
    x = NULL
  ) + 
  geom_vline(xintercept=ymd("2022-2-24"), linetype="dashed", 
             color = "red", size=1) + 
  geom_vline(xintercept=ymd("2022-4-3"), linetype="dashed", 
             color = "red", size=1) +
  ggplot2::annotate("text", x = ymd("2022-2-2"), y = 620, label = "Russian invasion of \n Ukraine") +
  ggplot2::annotate("text", x = ymd("2022-4-23"), y = 230, label = "Elections/\nBucha massacre") +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = -0.3) +
  theme_adam()


# data preprocessing ------------------------------------------------------

#load stopwords
custom_stopwords <- HunMineR::data_stopwords_extra

#create cleaner function
cleaner <- function(text) {
  
  #remove punctuations, numbers, make it lower case, remove unnecessary white spaces
  text <- stringr::str_remove_all(string = text, pattern = "[:punct:]") 
  text <- stringr::str_remove_all(string = text, pattern = "[:digit:]") 
  text <- stringr::str_to_lower(text)
  text <- stringr::str_trim(text) 
  text <- stringr::str_squish(text)
  
  # tokenize, filter out stopwords, drop those with less than 3 characters
  tokens <- unlist(strsplit(text, "\\s+"))
  tokens <- tokens[!(tokens %in% quanteda::stopwords("hungarian"))]
  tokens <- tokens[!(tokens %in% custom_stopwords)]
  tokens <- tokens[length(tokens) >= 3]
  
  # get back processed text
  clean_text <- paste(tokens, collapse = " ")
  
  return(clean_text)
}

#apply function
df$clean_text <- pblapply(df$text, cleaner)


# word frequencies --------------------------------------------------------

#create tidy tokens dataframe using tidytext from tokens before designated date
tokens_before <- df |> 
  filter(label == "before") |> 
  unnest_tokens(word, clean_text)

#count tokens by article
tok_count_before <- tokens_before |> 
  count(word, sort = TRUE) |> 
  top_n(10) |> 
  mutate(group = "before")

#create tidy tokens dataframe using tidytext
tokens_after <- df |> 
  filter(label == "after") |> 
  unnest_tokens(word, clean_text)

#count tokens by article
tok_count_after <- tokens_after |> 
  count(word, sort = TRUE) |> 
  top_n(10) |> 
  mutate(group = "after")

freq <- bind_rows(tok_count_after,tok_count_before)

#before after word frequency
freq %>% 
  ggplot(aes(x = tidytext::reorder_within(x=word, 
                                          by=n, 
                                          within=group), 
             y = n)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL,
       y = "Frequency") +
  facet_wrap(~group, scales = "free") +
  tidytext::scale_x_reordered() +
  theme_adam()


# wordcloud ---------------------------------------------------------------

#count tokens by article 2
tok_count_before2 <- tokens_before |> 
  count(word, sort = TRUE) |> 
  top_n(40) |> 
  mutate(group = "before")

#count tokens by article 2
tok_count_after2 <- tokens_after |> 
  count(word, sort = TRUE) |> 
  top_n(40) |> 
  mutate(group = "after")

freq2 <- bind_rows(tok_count_after2,tok_count_before2)

#wordcloud
freq2|> 
  ggplot(aes(label = word, size = n, colour = group)) +
  scale_size_area(max_size = 10) +
  geom_text_wordcloud(show.legend = TRUE) +
  theme_minimal()


#sentiment analysis
hu_stop_word <- read_csv("data/stopwords-hu.csv")

positive_words <- read_csv("data/PrecoSenti/PrecoPos.csv") %>%
  mutate(sentiment=1)

negative_words <- read_csv("data/PrecoSenti/PrecoNeg.csv") %>%
  mutate(sentiment=-1)

hungarian_sentiment <- rbind(positive_words, negative_words)


