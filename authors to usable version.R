library(tidyverse)
options(scipen = 999)
# Step 1: Read-in data
pediatric_obesity <- read_csv("/Users/bnbh_imac/Desktop/Bjarni/R/NetworkViz/Adolescent Obesity Articles/csv-PediatricO-set (1).csv") %>%
  filter(str_detect(`First Author`, 'GBD 2015 Obesity Collaborators') == F)
#Step 2: separate individual authors with the PMID of the article
pmid_authors_sep <- pediatric_obesity %>% select(PMID, Authors) %>%
  separate_rows(., Authors, sep = ",") %>%
  rename_all(tolower) %>%
  mutate(author = str_trim(authors, side = "left")) %>% 
  select(-authors) 
#
all_authors <- pmid_authors_sep %>%
  mutate(author = str_remove_all(author, "\\."))
#
tictoc::tic()
author_pairs <- all_authors %>% 
  left_join(., all_authors, by = "pmid") %>%
  distinct() %>%
  filter(author.x != author.y) %>%
  group_by(author.x, author.y) %>%
  summarise(articles_together = n()) %>%
  mutate(ax_ay = str_c(author.x, author.y, sep = ", "))
#
#-------
nodes <- all_authors %>%
  select(-pmid) %>%
  rowid_to_column(var = "id")
#
edges <- author_pairs %>%
  ungroup %>%
  select(ax_ay) %>%
  rownames_to_column(var = "key") %>%
  separate_rows(ax_ay, sep = ", ") %>%
  distinct %>%
  arrange(key, ax_ay) %>%
  group_by(key) %>%
  summarise(ax_ay = toString(ax_ay)) %>%
  select(-key) %>%
  distinct %>%
  left_join(., author_pairs %>% ungroup %>% select(ax_ay, articles_together)) %>%
  separate(., ax_ay, sep = ", ", into = c("author_a", "author_b")) %>%
  left_join(nodes, by = c("author_a" = "author")) %>%
  rename(from = id) %>%
  left_join(., nodes, by = c("author_b" = "author")) %>%
  rename(to = id) %>%
  distinct() %>%
  select(from, to, weight = articles_together)
#--------
library(igraph)
#
authors_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
authors_igraph
#
#plot(authors_igraph)
# Too many
#-------
nodes_lim <- author_pairs %>% ungroup %>%
  filter(articles_together > 2) %>%
  select(author = author.x) %>%
  bind_rows(., author_pairs %>% ungroup %>%
              filter(articles_together > 2) %>%
              select(author = author.y)) %>%
  rowid_to_column(var = "id")
#
edges_lim <- author_pairs %>%
  ungroup %>%
  filter(articles_together > 2) %>%
  select(ax_ay) %>%
  rownames_to_column(var = "key") %>%
  separate_rows(ax_ay, sep = ", ") %>%
  distinct %>%
  arrange(key, ax_ay) %>%
  group_by(key) %>%
  summarise(ax_ay = toString(ax_ay)) %>%
  select(-key) %>%
  distinct %>%
  left_join(., author_pairs %>% ungroup %>% select(ax_ay, articles_together)) %>%
  separate(., ax_ay, sep = ", ", into = c("author_a", "author_b")) %>%
  left_join(nodes_lim, by = c("author_a" = "author")) %>%
  rename(from = id) %>%
  left_join(., nodes_lim, by = c("author_b" = "author")) %>%
  rename(to = id) %>%
  distinct() %>%
  select(from, to, weight = articles_together)
#
authors_igraph_lim<- graph_from_data_frame(d = edges_lim, vertices = nodes_lim, directed = TRUE)
#
plot(authors_igraph_lim)
#
library(tidygraph)
library(ggraph)
#
authors_tidy <- tbl_graph(nodes = nodes_lim, edges = edges_lim, directed = TRUE)
#
ggraph(authors_tidy) +
  geom_edge_link() +
  geom_node_point()
