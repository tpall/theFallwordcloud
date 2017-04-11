library(tidyverse)
library(rvest) # Simple web scraping for R
library(magrittr)
library(stringr)
# library(tm) # A framework for text mining applications within R
# library(wordcloud) # Word Clouds. Pretty ...

# Download lyrics
discography <- data_frame(releases= c("albums","singles"), 
                          url = list("http://thefall.org/discography/albums.html",
                               "http://thefall.org/discography/singles.html"))
discography %<>% mutate(html = map(url, read_html))
discography %<>% mutate(nodes = map(html, html_nodes, "a"))
discography %<>% mutate(attr = map(nodes, html_attr, "href"))
lyrics_paths <- discography %>% 
  unnest(attr) %>% 
  filter(str_detect(attr, "data")) %>% 
  mutate(url = file.path("http://thefall.org/discography", attr),
         html = map(url, read_html))

lyrics <- lyrics_paths %>% mutate(nodes = map(html, html_nodes, "[class=more]"),
                                  text = map(nodes, html_text, trim=TRUE),
                                  songs = map(html, html_nodes, "b"),
                                  songs = map(songs, html_text, trim=TRUE))

lyrics %<>% mutate(titles = map_chr(songs, 3),
                  songs = map(songs, ~.x[str_detect(.x, "^[0-9]{2}")]))

# Keep only releases with lyrics
lyrics %<>% 
  select(releases, titles, songs, text) %>% 
  filter(map_int(songs,length)==map_int(text, length)) %>% 
  unnest

# Unnest tokens
library(tidytext)
lyrics %<>% 
  unnest(text) %>% 
  unnest_tokens(lines, text, token="lines")

lyrics_words <- lyrics %>% unnest_tokens(words, lines)
