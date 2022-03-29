library(twitteR)
library(readxl)
library(tm)
library(SnowballC)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(reshape2)
library(textdata)
library(cluster) 
library(wordcloud)
library(lexiconPT)
library(fpc)


key <- "d7SMO6K8Ik4M6Q84vcpEwjN3J"
secret <- "NTa6aj3ktNM4qFOo6zXflntRBuvysXGKb8ZHwiz9uvinT3Dy0Z"
token <- "32443967-WMJOgBVe2Nlj0jQAAyZnFHvnrz3H4gks5pIhB0Ee4"
secretToken <- "VjygbbhaF8o9Zx8u8TjwAXjs0EdhHvK7CS2Xa8GnkmXfw"
setup_twitter_oauth(key, secret, token, secretToken)

#hashtag, quantidade, idioma
coleta1 <- searchTwitter('#eleições2022', n=500, lang="pt-br")
coleta2 <- searchTwitter('#bolsonaro', n=500, lang="pt-br")
coleta3 <- searchTwitter('#sergiomoro', n=500, lang="pt-br")

#conversao das mensagens para data frame
df1 <- do.call("rbind", lapply(coleta1, as.data.frame))
df2 <- do.call("rbind", lapply(coleta2, as.data.frame))
df3 <- do.call("rbind", lapply(coleta3, as.data.frame))

#unificacação das 3 bases em um único conjunto
conjunto1 = rbind(df1,df3)
conjuntoTwitter = rbind(conjunto1,df2)

dim(conjuntoTwitter)

#gravar em excel ou txt
write.table(conjuntoTwitter , file="dadosTwitter.txt")

#leitura dos dados do Facebook
dadosFacebook <- read_excel("dadosFacebook.xls")
View(dadosFacebook)    
dim(dadosFacebook)


conjuntoTotal = rbind(conjuntoTwitter[1],dadosFacebook)

#gravar em excel ou txt
write.table(conjuntoTotal , file="conjuntoTotal.txt")

#codificacao
tweets <- sapply(conjuntoTotal$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

myCorpus <- VCorpus(VectorSource(tweets))
myCorpus2 <- tm_map(myCorpus, tolower)

# remove retweet entities
removeRT <- function(x) gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", x)
myCorpus3 <- tm_map(myCorpus2, removeRT)

removert <- function(x) gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)
myCorpus4 <- tm_map(myCorpus3, removert)

# remove at people
removePeople <- function(x) gsub("@\\w+", "", x)
myCorpus5 <- tm_map(myCorpus4, removePeople)

myCorpus6 <- tm_map(myCorpus5, removePunctuation)
myCorpus7 <- tm_map(myCorpus6, removeNumbers)

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus8 <- tm_map(myCorpus7, removeURL)

myStopwords <- c(stopwords(kind="pt"), "eleições2022", "bolsonaro", "sergiomoro")
myCorpus9 <- tm_map(myCorpus8, removeWords, myStopwords)

myStopwords2 <- c(stopwords(kind="pt"), "href")
myCorpus10 <- tm_map(myCorpus9, removeWords, myStopwords)

myCorpus11 <- tm_map(myCorpus9, stripWhitespace)
myCorpus12 <- tm_map(myCorpus11, PlainTextDocument)
myCorpus13 <- tm_map(myCorpus12, stemDocument)
inspect(myCorpus13[[1]])

#matriz termo x frequencia
myTdmReal <- TermDocumentMatrix(myCorpus13,control=list(wordLengths=c(3,Inf)))

#gravo a base de dados
write.table(myTdmReal , file="matrizTxF.txt")

#gero a matriz 
m <- as.matrix(myTdmReal)

#vejo a frequencia das palavras
freq <- rowSums(m)

#pego as palavras com frequencia maior que x
freq <- subset(freq, freq>=5)

#gero o grafico
barplot(freq, las=2, col=rainbow(25))

#criacao do conjunto do modelo
ap_td <- tidy(myTdmReal)
ap_td 

#verifico a classificação de cada termo como positivo ou negativo
ap_sentiments <- ap_td %>% 
	inner_join(get_sentiments("bing"), by = c(term = "word"))

#encontra os termos mais negativos
ap_negativo_bing <- ap_sentiments %>%
  count(term, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(sentiment)

wordcloud(ap_negativo_bing$term, ap_negativo_bing$negative, min.freq=1)

#encontra os termos mais positivos
ap_positivo_bing <-ap_sentiments %>%
  count(term, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = negative - positive) %>%
  arrange(sentiment)

wordcloud(ap_positivo_bing$term, ap_positivo_bing$positive, min.freq=1)

#gera o gráfico termos positivos x negativos
ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  filter(n >= 1) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Nível de sentimento")+
  xlab("Termo")

# Léxicos da língua portuguesa para análise de sentimentos
oplex <- lexiconPT::oplexicon_v3.0
sentilex <- lexiconPT::sentiLex_lem_PT02
dplyr::glimpse(oplex)
dplyr::glimpse(sentilex)


#K-Means = 3, 5, 7, 9
matrix.kmeans <- kmeans(myTdmReal, 7)
matrix.kmeans$size
str(matrix.kmeans)

#rodar um para cada cluster
docs1 <- which(matrix.kmeans$cluster ==4)
docs1 <- myTdmReal[docs1, ]
#head(docs1); length(docs1)
matrix <- as.matrix(docs1) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#um para cada conjunto
ap_td <- tidy(docs1)
ap_td %>% 
  left_join(oplex , by = "term") %>% 
  left_join(sentilex %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(term, polarity, lex_polarity) %>% 
  head(10)



#K-medoids = 3, 5, 7, 9
matrix.kmedoids <- pam(myTdmReal, 5)
matrix.kmedoids$clusinfo

#rodar um para cada cluster
docs1 <- which(matrix.kmedoids$clustering ==7)
docs1 <- myTdmReal[docs1, ]
#head(docs1); length(docs1)
matrix <- as.matrix(docs1) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#um para cada conjunto
ap_td <- tidy(docs1)
ap_td %>% 
  left_join(oplex , by = "term") %>% 
  left_join(sentilex %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(term, polarity, lex_polarity) %>% 
  head(10)



#K-medoids = 3, 5, 7, 9
matrix.kmedoids <- gmm(myTdmReal, 3)
matrix.kmedoids$clusinfo

#rodar um para cada cluster
docs1 <- which(matrix.kmedoids$clustering ==7)
docs1 <- myTdmReal[docs1, ]
#head(docs1); length(docs1)
matrix <- as.matrix(docs1) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#um para cada conjunto
ap_td <- tidy(docs1)
ap_td %>% 
  left_join(oplex , by = "term") %>% 
  left_join(sentilex %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(term, polarity, lex_polarity) %>% 
  head(10)



#DBscan
#gerar um para cada eps
dbscan.result <- dbscan(myTdmReal, 3)

#um para cada cluster
docs1 <- which(dbscan.result$cluster ==0)
docs1 <- myTdmReal[docs1, ]
#head(docs1); length(docs1)
matrix <- as.matrix(docs1) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#um para cada conjunto
ap_td <- tidy(docs1)
ap_td %>% 
  left_join(oplex , by = "term") %>% 
  left_join(sentilex %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(term, polarity, lex_polarity) %>% 
  head(10)