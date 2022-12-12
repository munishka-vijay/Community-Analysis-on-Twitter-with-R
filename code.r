#importing tweets
library(rtweet)



# whatever name you assigned to your created app
appname <- "Community_Tweet_Analysis"

## api key (example below is not a real key)
key <- "kU3hCm0ebpIC4dMSJLYCjJPaz"

## api secret (example below is not a real key)
secret <- "PMidJ9rMEYr9lAlGE7LGe5rOsADOcAqjrObglhBNovtQRfQQAq"


access_token <- "1357922003114622976-3Fpmg34rTQ0x3DWJYQTbY4fBHKqeHA"

access_secret <- "tmSQW4QfNWHoISaN0QsUDCxPidIU9ePvFbcLKRD5XFAZE"


# create token named "twitter_token"
twitter_token <- create_token(
        app = appname,
        consumer_key = key,
        consumer_secret = secret,
        access_token = access_token,
        access_secret = access_secret)


tag = readline("Please Enter the Tweet You want to search")


# Read file

apple <- search_tweets(q = paste("#",tag,sep=""),n = 500)

# Build corpus
library(tm)

corp <- iconv(apple$text, to = "utf-8-mac")
corp <- Corpus(VectorSource(corp))

# Clean text
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
cleanS <- tm_map(corp, removeWords, stopwords('english'))
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanS <- tm_map(cleanS, content_transformer(removeURL))


tag1 = readLine("enter the similar words for the same")
tag2 = readLine("enter the similar words for the same")
tag3 = readLine("enter the similar words for the same")

cleanS <- tm_map(cleanS, removeWords, c(tag1,tag2, tag3))
cleanS <- tm_map(cleanS, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')
cleanS <- tm_map(cleanS, stripWhitespace)

# Term document matrix
termDM <- TermDocumentMatrix(cleanS)
termDM <- as.matrix(termDM)
termDM <- termDM[rowSums(termDM)>30,]
termDM[1:10,1:10]

# Network of terms
library(igraph)
termDM[termDM>1] <- 1
termM <- termDM %*% t(termDM)
termM[1:10,1:10]
g <- graph.adjacency(termM, weighted = T, mode = 'undirected')
g
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# Histogram of node degree
hist(V(g)$degree,
     breaks = 100,
     col = 'blue',
     main = 'Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')

# Network diagram

plot(g)
plot(g,
     vertex.color='blue',
     vertex.size = 4,
     vertex.label.dist = 1.5,
     vertex.label = NA)

# Community detection
comm <- cluster_edge_betweenness(g)
plot(comm, g)

prop <- cluster_label_prop(g)
plot(prop, g)

greed <- cluster_fast_greedy(as.undirected(g))
plot(greed, as.undirected(g))
