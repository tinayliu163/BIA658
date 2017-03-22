
# Visualize the weighted graph so that the size of the nodes correspond to the population of the states

install.packages('tidytext')
library(readr)
library(dplyr)
library(tidytext)
library(igraph)

song_lyrics = read_csv(file = "/Users/admin/Downloads/billboard_lyrics_1964-2015.csv")
names(song_lyrics)
states = read_csv("http://www.census.gov/popest/data/national/totals/2015/files/NST-EST2015-alldata.csv")[-(1:5), ]
state_names = tolower(states$NAME)

artist_lyrics = song_lyrics %>% group_by(Artist) %>% summarise(Lyrics= paste(Lyrics, collapse=" "))
artist_unigram = artist_lyrics %>% unnest_tokens(tokens, Lyrics, to_lower = TRUE)
artist_bigram = artist_lyrics %>% unnest_tokens(tokens, Lyrics, to_lower = TRUE, token = "ngrams", n = 2)
artist_tokens = rbind(artist_unigram, artist_bigram)
artist_tokens = artist_tokens %>% filter(tokens %in% state_names)

create_adj_list = function(df){
  # Input: a dataframe with a column "tokens"   
  # Output: all possible 2-combinations (sorted) of the unique tokens
  unique_tokens = unique(df$tokens)
  adj_list = data.frame()
  if(length(unique_tokens) >= 2) {
    all_combins = t(combn(unique_tokens, 2))
    all_combins = t(apply(all_combins, 1, sort))
    adj_list = data.frame(all_combins, stringsAsFactors = FALSE)
  }
  return(adj_list)
}

adj_list = artist_tokens %>% group_by(Artist) %>% do(create_adj_list(.))
adj_list_weighted = data.frame(adj_list) %>% group_by(X1, X2) %>% summarise(weight = n())

state_graph_weighted = graph.data.frame(adj_list_weighted[, c("X1", "X2")], directed = FALSE)
state_names_in_graph = data.frame(state = as.vector(V(state_graph_weighted)$name), stringsAsFactors = F)

states$NAME = tolower(states$NAME)
state_names_in_graph = state_names_in_graph %>% left_join(states, by = c("state" = "NAME"))

plot(state_graph_weighted, layout=layout.fruchterman.reingold, edge.width=E(state_graph_weighted)$weight, vertex.size = state_names_in_graph$POPESTIMATE2015/5000000)
