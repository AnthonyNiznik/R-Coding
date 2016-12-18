library(ggplot2)
library(dplyr)
library(rmarkdown)

path = getwd()
setwd(path)

#Read Book Rating Data and delete zeros
Book_Ratings = read.csv(file = "BX-Book-Ratings.csv", header = T, sep=",")
Book_Ratings = na.omit(Book_Ratings)
Book_Ratings <- Book_Ratings[-row(Book_Ratings)[Book_Ratings == 0],]

#Statistics On Book Rating
summary(Book_Ratings)

#Read Book Data
Books = read.csv(file = "BX-Books.csv", header = T, sep=",")

Books$X <- NULL
Books$X.1 <- NULL
Books$X.2<- NULL

#Remove Zero and Non-numeric Values From Year Of Publication
Books <- Books[-which(Books$Year.Of.Publication == "0" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2005" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2037" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2026" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2020" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2050" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2038" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2024" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2021" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2012" ), ]
Books <- Books[-which(Books$Year.Of.Publication == "2011" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2010" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2008" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2006" ), ] 
Books <- Books[-which(Books$Year.Of.Publication == "2030" ), ] 

Books <- Books[!is.na(as.numeric(as.character(Books$Year.Of.Publication))),]
Books = na.omit(Books)

#Merge Datasets
Book_Data <- merge(Books, Book_Ratings, sort = FALSE, by="ISBN")

Book_Data %>%
  group_by(ISBN) %>%
  summarise(Book.Rating.Average = mean(Book.Rating))  -> Ratings_Avg

Book_Data %>%
  group_by(ISBN) %>%
  summarise(Book.Rating.Median = median(Book.Rating))  -> Ratings_Avg2

Ratings_Avg3 <- merge(Ratings_Avg, Ratings_Avg2, sort = FALSE, by="ISBN")

Book_Data_Avg_Rating <- merge(Books, Ratings_Avg3, sort = FALSE, by="ISBN")


# Find Number of Ratings per Book

Book_Data %>%
  group_by(ISBN) %>%
  summarise(length(ISBN))  -> Number_Of_Ratings

#Final Data Set
Book_Data_Final <- merge(Book_Data_Avg_Rating, Number_Of_Ratings, sort = FALSE, by="ISBN")

#Rename Rating Count Column
colnames(Book_Data_Final)[8] <- "Number.Of.Ratings"

#Input Recommendation Column

Book_Data_Final$Recommend.Book <- ifelse(Book_Data_Final$Number.Of.Ratings >= 10, 0, ifelse(Book_Data_Final$Book.Rating.Average >= 8, 1, 0))

#Clustering
library(cluster)
set.seed(10)
Data_Cluster2=subset(Book_Data_Final,select=c(Year.Of.Publication, Book.Rating.Average, Book.Rating.Median, Number.Of.Ratings))
Data_Cluster3 <- sapply( Data_Cluster2, as.numeric )

DC1 = scale(Data_Cluster3)
DC2 = as.data.frame(DC1)

#Elbow Diagram
Num_Cluster <- DC2[, -3]
temp <- (nrow(Num_Cluster)-1)*sum(apply(Num_Cluster,2,var))
for (i in 2:10) 
{
  temp[i] <- sum(kmeans(Num_Cluster, centers=i)$withinss)
}


plot(1:10, temp, type="b", main = "Elbow Diagram", xlab="Number of Clusters",
     ylab="Within Cluster Sum Of Squares")

Book_Data_Final.km <- kmeans(DC2[, -3], 4)

names(Book_Data_Final.km)

Book_Data_Final.km$centers
#scale_colour_gradientn(colours = rainbow(8)) +

ggplot(Book_Data_Final, aes(x = Number.Of.Ratings, y = Year.Of.Publication)) +
  geom_point(aes(colour = Book_Data_Final.km$cluster)) +
  scale_colour_gradientn(colours = rainbow(7))+
  ggtitle("K-means Clustering Based On Median Book Rating") + 
  labs(colour="Cluster")

table1 <- table(Data_Cluster3[, 3], Book_Data_Final.km$cluster)
table1

x = 0
Sum_Num = 0
for (i in 1:19) 
{ x = max(table1[i,])/sum(table1[i,])
Sum_Num = Sum_Num + x}

Accuracy = Sum_Num/19
Accuracy

#Let's look at 3rd green dot in 1985 (Cluster 2)
Book_Data_Final.km$cluster[5937]
#We get 43 book ratings

#Let's try book with 42 book ratings (Cluster 2)
Book_Data_Final.km$cluster[15213]

#41 (Cluster 1)
Book_Data_Final.km$cluster[1021]

#40 (Cluster 1)
Book_Data_Final.km$cluster[566]

#Cluster 2 starts at 42 book ratings

Many_Ratings = subset(Book_Data_Final, Number.Of.Ratings >= 42)

#Text Analytics - Book Titles

library(tm)

#Books Rated over 41 times
corpus=Corpus(VectorSource(Many_Ratings$Book.Title))
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,PlainTextDocument)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,stripWhitespace)
corpus=tm_map(corpus,removeWords,c("paperback", "book", "novel", "play", "read", "reader"))
corpus=tm_map(corpus,removeWords,stopwords("english"))
corpus=tm_map(corpus,removeWords,stopwords("spanish"))
corpus=tm_map(corpus,removeWords,stopwords("german"))
library(SnowballC) 
corpus=tm_map(corpus,stemDocument)


dtm=DocumentTermMatrix(corpus)

#Creating Frequency of Terms
freq <- colSums(as.matrix(dtm))

#How many terms do we have?
length(freq)

#Ordered Based on Frequency
ord <- order(freq)

#Words with most frequency
freq[tail(ord)]

#Wordclouds
set.seed(1)
library(wordcloud)
wordcloud(names(freq), freq, min.freq=10, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

#Book Titles Hierarchal Clustering

#Remove Sparse terms
spdtm <- removeSparseTerms(dtm, .99)

ddistance <- dist(t(spdtm), method="euclidian")
fordendo <- hclust(d=ddistance, method="ward.D")
fordendo

#Dendogram
plot(fordendo, hang=-1) 
rect.hclust(fordendo, k=6, border="green")


#Recommended Book Authors

Recommended_Books = subset(Book_Data_Final, Recommend.Book == 1)

#Find number of recommended books author has written
Recommended_Books %>%
  group_by(Book.Author) %>%
  summarise(length(Book.Author))  -> Recommended_Authors

#Order books in descending order
Recommended_Authors <- Recommended_Authors[-which(Recommended_Authors$Book.Author == "Not Applicable (Na )" ), ]
Recommended_Authors <- Recommended_Authors[order(Recommended_Authors$`length(Book.Author)`,decreasing = T),]
Recommended_Authors
