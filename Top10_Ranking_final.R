## 0. Libraries
library(ggplot2)
theme_set(theme_bw())  

## 1. Top 10 negative and positive words 

#Top 10 Lexicon_spanish
df_lex <- quijote_es_tidy %>%  
  inner_join(lexicon, by = c("word" = "palabra")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 

#Top 10 Bing
df_bin <- quijote_en_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() 

#Top 10 Affin
df_afi <- quijote_en_tidy %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, score, sort = TRUE) %>%
  mutate(score=ifelse(score > 0, "positive", "negative")) %>%
  group_by(score) %>%
  top_n(10) %>%
  ungroup()


#Top 10 nrc
nrc_np <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative" | sentiment == "positive" )

#Top 10 NRC
df_nrc <- quijote_en_tidy %>%
  inner_join(nrc_np) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup()

# 1.1 Data preparation
colnames(df_lex)[2]<-"sentiment"
colnames(df_afi)[2]<-"sentiment"




# 2. Diverging bars for each ranking
# 2.1 #Colors from Viridis Palette
A_col <- "#3F4788ff"
B_col <- "#FDE725FF"

# 2.2. Lexicon - Diverging Barcharts
df_lex$w_type <- ifelse(df_lex$sentiment == "neg", "below", "above")  # above / below avg flag
df_lex$n <- ifelse(df_lex$sentiment == "neg", df_lex$n*(-1), df_lex$n)
df_lex <- df_lex[order(df_lex$n), ]  # sort
df_lex$word <- factor(df_lex$word, levels = df_lex$word)  # convert to factor to retain sorted order in plot.

graph_lex <- ggplot(df_lex, aes(x=df_lex$word, y=df_lex$n )) + 
  geom_bar(stat='identity', aes(fill=w_type), width=.5)  +
  scale_fill_manual(guide = FALSE, 
                    values = c("above"=A_col, "below"=B_col)) + 
  labs(title="Lexicon in Spanish", 
       subtitle= "Top 10 positive & negative words") + 
  coord_flip() +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# 2.3. Lexicon - Diverging Barcharts
df_bin$w_type <- ifelse(df_bin$sentiment == "negative", "below", "above")  # above / below avg flag
df_bin$n <- ifelse(df_bin$sentiment == "negative", df_bin$n*(-1), df_bin$n)
df_bin <- df_bin[order(df_bin$n), ]  # sort
df_bin$word <- factor(df_bin$word, levels = df_bin$word)  # convert to factor to retain sorted order in plot.



# Diverging Barcharts 
graph_bin <- ggplot(df_bin, aes(x=df_bin$word, y=df_bin$n )) + 
  geom_bar(stat='identity', aes(fill=w_type), width=.5)  +
  scale_fill_manual(guide = FALSE, 
                    values = c("above"=A_col, "below"=B_col)) + 
  labs(title="Bing et al.", 
       subtitle= "Top 10 positive & negative words") + 
  coord_flip() +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# 2.4. AFFIN - Diverging Barcharts
df_afi$w_type <- ifelse(df_afi$sentiment == "negative", "below", "above")  # above / below avg flag
df_afi$n <- ifelse(df_afi$sentiment == "negative", df_afi$n*(-1), df_afi$n)
df_afi <- df_afi[order(df_afi$n), ]  # sort
df_afi$word <- factor(df_afi$word, levels = df_afi$word)  # convert to factor to retain sorted order in plot.



# Diverging Barcharts 
graph_afi <- ggplot(df_afi, aes(x=df_afi$word, y=df_afi$n )) + 
  geom_bar(stat='identity', aes(fill=w_type), width=.5)  +
  scale_fill_manual(guide = FALSE, 
                    values = c("above"=A_col, "below"=B_col)) + 
  labs(title="AFFIN", 
       subtitle= "Top 10 positive & negative words based on score") + 
  coord_flip() +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# 2.4. NRC - Diverging Barcharts
df_nrc$w_type <- ifelse(df_nrc$sentiment == "negative", "below", "above")  # above / below avg flag
df_nrc$n <- ifelse(df_nrc$sentiment == "negative", df_nrc$n*(-1), df_nrc$n)
df_nrc <- df_nrc[order(df_nrc$n), ]  # sort
df_nrc$word <- factor(df_nrc$word, levels = df_nrc$word)  # convert to factor to retain sorted order in plot.

head(df_nrc,3)

# Diverging Barcharts 
graph_nrc <- ggplot(df_nrc, aes(x=df_nrc$word, y=df_nrc$n )) + 
  geom_bar(stat='identity', aes(fill=w_type), width=.5)  +
  scale_fill_manual(guide = FALSE, 
                    values = c("above"=A_col, "below"=B_col)) + 
  labs(title="NRC", 
       subtitle= "Top 10 positive & negative words") + 
  coord_flip() +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

## 3. Using gridExtra Library to create a better arrange around so many graphs.
library(gridExtra)
grid.arrange(graph_lex, graph_bin, graph_afi, graph_nrc)
