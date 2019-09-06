##### AUTHORS: KAIKE WESLEY AND LUCAS MASCARENHAS

##### WORK DIRECTORY
# ! DEFINE YOUR WORKSPACE HERE
#setwd("~/Harbinger/UFBA/2019.1/ENG438/Trabalhos")

##### LIBRARY
library(quanteda)
library(stopwords)
library(wordcloud2)
library(tidytext)
library(tidyverse)
library(lexiconPT)
library(Matrix)
library(irlba)
library(tm)
library(SnowballC)
library(dplyr)

##### OPTIONS
quanteda_options("threads" = 3)
set.seed(651)

##### IMPORT & VARIABLES

# BOLSONARO DATASET
db <- read.csv('bolsonaro_tweets.csv', sep = ',', encoding="UTF-8")
db$link <- NULL
db$likes <- NULL
db$retweets <- NULL

# LEMMATIZATION DICTIONARY
ld <- read.csv('lemmatization-pt.txt', sep = '\t', header = T, encoding="UTF-8", stringsAsFactors = F)
colnames(ld) <- c('lemma','word')

# LEXICON PT DICTIONARY
lp <- lexiconPT::oplexicon_v3.0
colnames(lp)[1] <- "word"

# STOPWORDS AND NON-EASY-PATTERN-TO-FIND
custom_stop_words <- c(stopwords::stopwords(language = "pt", source = "stopwords-iso"),'q','d','p','ñ','c','pictwittercom','ex','x','twitter')

##### PRE-PROCESSING :: CLEANING DATA 

# REMOVE EMOTICONS AND ODD CHARACTERS, BUT KEEPING ACCENTS
db$text <- sapply(db$text,function(row) iconv(row, "UTF-8", "ISO_8859-1", sub=""))

# REMOVE EMPTY ROWS THAT DOESN'T HAVE ANY TEXT BECAUSE OF THE EMOJIS
db <- db[!db$text=="", ]
rownames(db) <- 1:nrow(db)

##### QUANTEDA CLEANING PRE-PROCESSING

# CREATE A CORPUS OBJECT
mc <- quanteda::corpus(db)

# CLEANING - CREATE A TOKEN OBJECT
mct <- quanteda::tokens(x = mc, what = 'fasterword',verbose = F,
                        remove_separators = T,
                        remove_twitter = F,
                        remove_symbols = T,
                        remove_numbers = T,
                        remove_hyphens = T,
                        remove_punct = T,
                        remove_url = T)

## CLEANING - TRANSFORM TOKEN TO DATAFRAME AND TAKE OUT REMAIN PUNCTUATION (BUG FROM QUANTEDA)
# 1 - CREATE A NEW COLUMN TOKEN LENGTH
db$textQ <- NA
nt <- length(mct)

# 2 - LOOP TO GET THE TOKEN, PASTE TOGETHER & ¨PUT IN THE DATAFRAME
for(i in 1:nt){
  indexador <- paste0('text',i,collapse = '')
  db$textQ[i] <- paste0(mct[[indexador]], collapse = ' ')
}

# 3 - FUNCTION TO TAKE OUT SPECIFICs PUNCT
## IF YOU SOLVE ... .. . CATASTROPHE PUT IT HERE
db$textQPF <- gsub("[][!$%()*,.:;<=>^?|~.{}]", "", db$textQ)
db$textQPF <- gsub('/', ' ', db$textQPF)
db$textQPF <- gsub('\"',' ',db$textQPF)
db$textQPF <- gsub('\'','',db$textQPF)

# 4 - REMOVE EMPTY ROWS THAT DOESN'T HAVE ANY TEXT THE PREVIOUS CLEANING (URLs most)
db <- db[!db$textQPF=="", ]
rownames(db) <- 1:nrow(db)
colnames(db) <- c('data','text_orig','text_q1','text')

# 5 - TRANSFORM INTO CORPUS AND THEN TOKEN AGAIN TO CONTINUE THE PROCESS
mc <- quanteda::corpus(db[,c('data','text')])
mct <- quanteda::tokens(x = mc, what = 'word', remove_twitter = F, remove_numbers = T)

# 6 - REMOVE AUX VARIABLES
rm(i, indexador, nt)

## CONTINUES...

# CLEANING - TRANSFORM ALL LETTERS TO LOWER CASE
mct <- quanteda::tokens_tolower(x = mct)

# TRANSFORM - COMPOUND SOME EXPRESSIONS
mct <- quanteda::tokens_compound(x = mct, pattern = quanteda::phrase(c('jair bolsonaro', 'são paulo', 'sao paulo')), concatenator = ' ')

# CLEANING - TAKE OUT STOPWORDS
mct <- quanteda::tokens_remove(x = mct, pattern = custom_stop_words)

# CLEANING - LEMMATIZATION
mct <- quanteda::tokens_replace(x = mct, pattern = ld$word, replacement = ld$lemma, case_insensitive = T, valuetype = "fixed")

# CREATE A DFM OBJECT
mcd <- quanteda::dfm(x = mct)

# DFM - TAKING A MINIMUM 10 FREQUENCIES WORDS (AVOID BROKEN WORDS)
mcd <- quanteda::dfm_trim(x = mcd, min_termfreq = 10)

# CREATE A FDM OBJECT BASED IN A COMPRESS DFM OBJECT (NECESSARY - HIGH DIMENSIONALITY)
mcf <- quanteda::fcm(x = mcd)

##### DATAFRAMES FOR PLOTS

# CREATE FOR WORD CLOUD PLOT
plt_frequency <- slam::col_sums(quanteda::convert(x = mcd, to = 'tm'))
plt_frequency <- plt_frequency[plt_frequency >= 20]
similarity_feat <- names(plt_frequency)
plt_frequency <- plt_frequency[plt_frequency >= 50]
plt_frequency <- data.frame(words = names(plt_frequency), freq = plt_frequency , row.names = NULL, stringsAsFactors = F)
colnames(plt_frequency) <- c('word','freq')

# TF-IDF INDEX
plt_tfidf <- quanteda::dfm_tfidf(x = mcd)

# CORRELATION SPARSE MATRIX
plt_similarity <- quanteda::textstat_simil(x = mcd, margin = "features",method = "cosine", selection = similarity_feat)
plt_similarity <- as.matrix(x = plt_similarity, diag = 1)
plt_similarity <- as(plt_similarity, "sparseMatrix")
plt_similarity <- Matrix(data = plt_similarity, sparse = T)

##### PCA SIMILARITY ANALYSIS

# CALCULATION
pca_analise <- irlba::prcomp_irlba(x = plt_similarity, n = 200)

# GENERATE NEW DATA
word_vectors <- pca_analise$x
rownames(word_vectors) <- rownames(plt_similarity)
dim(word_vectors)

# FUNCTION TO FIND HIGHER SIMILARITIES
search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  
  similarities %>%
    arrange(-similarity)    
}

# CHOSEN WORDs - presidente & brasil & economia
cWord1 <- search_synonyms(word_vectors, word_vectors["brasil",])
cWord2 <- search_synonyms(word_vectors, word_vectors["economia",])
cWord3 <- search_synonyms(word_vectors, word_vectors["presidente",])
sWord1 <- cWord1 %>% inner_join(lp, by = c("token" = "word"))
sWord2 <- cWord2 %>% inner_join(lp, by = c("token" = "word"))
sWord3 <- cWord3 %>% inner_join(lp, by = c("token" = "word"))

##### PLOTS

# SIMILATY - BRASIL
sWord1 %>% top_n(20, abs(similarity)) %>% 
  ggplot(aes(x = reorder(token, similarity), y = similarity, fill = as.factor(polarity))) +
  labs(title = 'Palavra Brasil',fill = "Polaridade", y = "Similaridade", x = "Palavras mais similares") + geom_col() + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank(), plot.title = element_text(hjust = 0.5))
ggsave("sim_brasil.png")

# SIMILATY - ECONOMIA
sWord2 %>% top_n(20, abs(similarity)) %>% 
  ggplot(aes(x = reorder(token, similarity), y = similarity, fill = as.factor(polarity))) +
  labs(title = 'Palavra Economia',fill = "Polaridade", y = "Similaridade", x = "Palavras mais similares") + geom_col() + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank(), plot.title = element_text(hjust = 0.5))
ggsave("sim_economia.png")

# SIMILATY - PRESIDENTE
#sWord3 %>% top_n(20, abs(similarity)) %>% 
#  ggplot(aes(x = reorder(token, similarity), y = similarity, fill = as.factor(polarity))) +
#  labs(title = 'Palavra Presidente',fill = "Polaridade", y = "Similaridade", x = "Palavras mais similares") + geom_col() + theme_minimal() +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.ticks.x = element_blank(), plot.title = element_text(hjust = 0.5))
#ggsave("sim_presidente.png")

# WORDCLOUD
png(filename="wordcloud.png")
wordcloud2(plt_frequency, color = "random-light", size = 1, backgroundColor = "black")
dev.off()

# NETWORK ANALYSIS - OVERALL
#principais <- names(quanteda::topfeatures(x = mcf,n = 20))
#mcf_principais <- quanteda::fcm_select(x = mcf, pattern = principais)
#size_net <- log(colSums(quanteda::dfm_select(x = mcf, pattern = principais)))

#png(filename="net_all.png")
#quanteda::textplot_network(x = mcf_principais, min_freq = 0.5, vertex_size = size_net/max(size_net)*3, edge_color = 'green')
#dev.off()

# NETWORK ANALYSIS - @USERS
user_dfm <- dfm_select(mcf, pattern = "@*")
topuser <- names(topfeatures(user_dfm, 20))
user_fcm <- fcm(user_dfm)
user_fcm <- fcm_select(user_fcm, pattern = topuser)
png(filename="net_user.png")
textplot_network(user_fcm, min_freq = 0.5, edge_color = "orange", edge_alpha = 0.8, edge_size = 5)
dev.off()
