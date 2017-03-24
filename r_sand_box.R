with_geo <- tweet_data4$geo!="b'NA"
View(with_geo)
length(with_geo)
geo_data <- tweet_data4[ which(tweet_data4$geo!="b'NA'" ), ]
coordinaties_data <- tweet_data4[ which(tweet_data4$created_at!="b'NA'" ), ]
head(tweet_data4)
bool_test <- coordinaties_data == geo_data
View(bool_test)

##to remove duplicates
a <- c(rep("A", 3), rep("B", 3), rep("C",2))
b <- c(1,1,2,4,1,1,2,2)
c <- c(1,2,3,4,1,2,3,4)
df <-data.frame(a,b,c)
View(df)

df <- df[!duplicated(df[-3]), ]

View(df)
