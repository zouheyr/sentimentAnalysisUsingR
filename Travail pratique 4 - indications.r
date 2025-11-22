#########################################
# Travail pratique 4 - individuel - 5%
#########################################
# Experimentation 4.1
# Installer les packages
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
# Charger les libraries
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

# Experimentation 4.2
# Lire le fichier texte ? partir de la machine locale, choisir le fichier de mani?re interactive
text <- readLines(file.choose())

# Exp?rimentation 4.3
# Charger les donn?es sous forme de corpus
TextDoc <- Corpus(VectorSource(text))

# Experimentation 4.4
#Remplacer "/", "@" et "|" par de l'espace
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

# Experimentation 4.5
# Convertir le texte en minuscules
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Supprimer des num?ros
TextDoc <- tm_map(TextDoc, removeNumbers)
# Supprimer les stopwords anglais courants
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Supprimez votre propre stopword
# sp?cifier vos stopwords comme vecteur de caract?res
TextDoc <- tm_map(TextDoc, removeWords, c("s", "company","team")) 
# Supprimer les ponctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# ?liminer les espaces blancs suppl?mentaires
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Saisie de texte - ce qui r?duit les mots ? leur forme racine
TextDoc <- tm_map(TextDoc, stemDocument)

# Experimentation 4.6
# Cr?er la matrice "Term-by-Document"
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)

# Experimentation 4.7
# Trier par valeur d?croissante de fr?quence
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Afficher les cinq mots les plus fr?quents
head(dtm_d, 5)

# Experimentation 4.8
# Tracer les mots les plus fr?quents
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

# Experimentation 4.9
# Generer un nuage de mots (word cloud)
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Experimentation 4.10
# Trouver des associations
findAssocs(TextDoc_dtm, terms = c("good","work","health"), corlimit = 0.25)
# Trouver des associations pour des mots qui se produisent au moins 50 fois
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.25)

# Experimentation 4.11
# score de sentiment r?gulier utilisant la fonction get_sentiment() et la m?thode de votre choix
# Veuillez noter que diff?rentes m?thodes peuvent avoir des ?chelles diff?rentes
syuzhet_vector <- get_sentiment(text, method="syuzhet")
# voir la premi?re ligne du vecteur
head(syuzhet_vector)
# voir les statistiques r?capitulatives du vecteur
summary(syuzhet_vector)

# Experimentation 4.12
# Methode: bing
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)
#Methode: affin
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

# # Experimentation 4.13
# Effectuer une analyse des sentiments du NRC pour retourner la base de donn?es 
# avec chaque ligne class?e comme l'une des suivantes?:
# emotions: col?re, anticipation, d?go?t, peur, joie, tristesse, surprise, confiance 
d<-get_nrc_sentiment(text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

# Experimentation 4.14
# Transposer
td<-data.frame(t(d))
# La fonction rowSums calcule les sommes des colonnes ? travers les lignes 
# pour chaque niveau d'une variable de regroupement.
td_new <- data.frame(rowSums(td[2:253]))
#Transformation et nettoyage
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

# Experimentation 4.15
#Plot 1 - nombre de mots associ?s ? chaque ?motion
quickplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment,ylab="count")+ggtitle("Survey sentiments")

# Experimentation 4.16
#Plot 2 - nombre de mots associ?s ? chaque ?motion, exprim? en pourcentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)

