#################################################################
#Author: Tomasz Hachaj, 2017
#Email: tomekhachaj@o2.pl
#################################################################
#Generate bigrams graph in gdf format (see https://gephi.org/)
#################################################################
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

#remove short words, mensions and stop words from bigrams
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!grepl('@',word1), !grepl('@',word2)) %>%
    filter(nchar(word1) > 3, 
           nchar(word2) > 3) %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

#plot a graph
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = 1), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}
#read data
#change path to the file!
dd <- read.csv("e:\\aa\\actress_timeline_1000000.csv", encoding="UTF-8")
#dd <- read.csv("e:\\aa\\writer_timeline_1000000.csv", encoding="UTF-8")
#dd <- read.csv("e:\\aa\\basketball_timeline_1000000.csv", encoding="UTF-8")
ddH <-as.data.frame(dd[!grepl('^RT', dd$text,ignore.case = TRUE),])
ddH[] <- lapply(ddH, as.character)
colnames(ddH) <- 'text'

ddH <-as.data.frame(dd[!grepl('http', dd$text),])
ddH[] <- lapply(ddH, as.character)
colnames(ddH) <- 'text'

library(stringr)

kjv_bigrams <- ddH %>%
  count_bigrams()


#plot graph with treshold 100 on edges wieghts
kjv_bigrams %>%
  filter(n > 100,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()


wordsNames <- (unique(c(kjv_bigrams$word1, kjv_bigrams$word2)))
wordsNames <- gsub(",", ".", wordsNames)
kjv_bigrams$word1 <- gsub(",", ".", kjv_bigrams$word1)
kjv_bigrams$word2 <- gsub(",", ".", kjv_bigrams$word2)


#save to file in gdf format
#change path to the file!
sink("e:\\aa\\actress_output_1000000.gdf")
cat("nodedef>name VARCHAR,label VARCHAR\n")
for (a in 1:length(wordsNames))
{
  #remove digits, _ and dots
  if (!str_detect(wordsNames[a], "\\d"))
    if (!str_detect(wordsNames[a], "_"))
      if (!str_detect(wordsNames[a], "\\."))
      {
        cat(paste(a, wordsNames[a], sep = ","))
        cat("\n")
      }
}


cat("edgedef>node1 VARCHAR,node2 VARCHAR, weight DOUBLE\n")
for (a in 1:length(kjv_bigrams$word1))
{
  if (!str_detect(kjv_bigrams$word1[a], "\\d") && !str_detect(kjv_bigrams$word2[a], "\\d"))
    if (!str_detect(kjv_bigrams$word1[a], "_") && !str_detect(kjv_bigrams$word2[a], "_"))
      if (!str_detect(kjv_bigrams$word1[a], "\\.") && !str_detect(kjv_bigrams$word2[a], "\\."))
  {
    cat(match(kjv_bigrams$word1[a],wordsNames))
    cat(",")
    cat(match(kjv_bigrams$word2[a],wordsNames))
    cat(",")
    cat(kjv_bigrams$n[a])
    cat("\n")
  }
}

sink()
#now open file in Gephi
