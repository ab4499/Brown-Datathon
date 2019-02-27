# Upload data
act <- read.csv("C:/Users/zkr2/Downloads/Events/BrownDatathon/TripAdvisorBrownDatathon/DataFromTripAdvisor/activity_data.csv", header = TRUE)
activity <- act
hot <- read.csv("C:/Users/zkr2/Downloads/Events/BrownDatathon/TripAdvisorBrownDatathon/DataFromTripAdvisor/hotel_data.csv", header = TRUE)
hotel <- hot

# K-fold clustering
#f_K_fold <- function(Nobs,K=10){
#  rs <- runif(Nobs)
#  id <- seq(Nobs)[order(rs)]
#  k <- as.integer(Nobs * seq(1, K-1) / K)
#  k <- matrix(c(0, rep(k, each=2), Nobs), ncol = 2, byrow = TRUE)
#  k[,1] <- k[,1]+1
#  l <- lapply(seq.int(K), function(x, k, d)
#    list(train=d[!(seq(d) %in% seq(k[x, 1],k[x, 2]))],
#         test=d[seq(k[x,1],k[x,2])]),
#    k=k,d=id)
#  return(l)
#}

library(tidyverse)
library(lubridate)
# Partition traning and test set
df <- left_join(act,hot,by="hotel_id")
df$date<-as_datetime(df$date)
training1<-filter(df, date<=as_datetime("2019-1-20"))
testing1<-filter(df, date>as_datetime("2019-1-20"))

# Generate the item-user matrix
training1$click <- ifelse(training1$user_action=="view", 0, 1)
t1.1 <- training1 %>% group_by(user_id, hotel_id) %>% summarise(click = sum(click))
t1.2 <- spread(t1.1, hotel_id, click)
rownames(t1.2) <- t1.2$user_id #user_id as rownames (each row is a user)
t1.2 <- t1.2[,-1] # get rid of user_id, get the user(row)-hotel(col) matrix

s.t1.2 <- t1.2[sample(nrow(t1.2), 1000), ]

# Item Cosine Similarity
library(lsa)
s.t1.2 <- as.matrix(s.t1.2)
H.SIM <- cosine(s.t1.2)

# make hotels in the test set no more than those in the training set
t.h.id <- unique(select(training1[1:100,], hotel_id))
testing1 <- left_join(t.h.id, testing1, by="hotel_id")

# test with the similarity matrix
a <- matrix(, nrow = 1, ncol=nrow(testing1))
for (i in 1:nrow(testing1)) {
  a[,i] <- head(rownames(H.SIM[order(H.SIM[testing1[i,5],], decreasing=TRUE),]),1)
}
b<-as.data.frame(t(a[1, 1:15]))
names(b)<-testing1[1:15,1]
