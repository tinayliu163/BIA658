
#****************** Modeling technique****************

# Code for Random Forest

install.packages("randomForest")
library(randomForest)
data = read.csv(file = "/Users/admin/Desktop/users_combined.csv")

s_d=sample(5300,2600)

train_data= data[s_d, ]
test_data=data[-s_d, ]

# Running the random forest model
random = randomForest(result ~ ., train_data, ntree=100)
random

predict_data=predict(random,train_Data)
predict_data

table(train_Data[,5], predict_data)

mean(train_Data[,5]==predict_data)

# Code for decision tree

install.packages("rpart.plot")
library(rpart.plot)

dim(train_data)

# Running the decision tree model
dtm<- rpart(result~., train_data, method="class")
dtm

rpart.plot(dtm,type=4,extra=101)
text(dtm)

## ************Data analysis******************
## part I : number of followers ; Location ; Hashtag

library(readr)
library(dplyr)
install.packages("plyr")
library(plyr)
install.packages("igraph")
library(igraph)

# ff means followers of fake account
ff = read.csv(file = "/Users/admin/Desktop/fake/followers.csv",header=TRUE, sep=",")
# tf means followers of true account
tf = read.csv(file = "/Users/admin/Desktop/true/followers.csv",header=TRUE, sep=",")


#number of followers#
a<-group_by(ff,target_id)
b<-summarise(a,length(target_id))
mean(b$`length(target_id)`)
c<-group_by(tf,target_id)
d<-summarise(c,length(target_id))
barplot(table(d$`length(target_id)`),col = blues9)
barplot(table(b$`length(target_id)`))
?barplot
mean(d$`length(target_id)`)
write.csv(table(d$`length(target_id)`),file = "/Users/admin/Downloads/Fake 3 good/Fake 3/1.csv")

#number of followers#

# ffr means friends of fake account
ffr = read.csv(file = "/Users/admin/Desktop/fake/friends.csv",header=TRUE, sep=",")
# tfr means friends of true account
tfr = read.csv(file = "/Users/admin/Desktop/true/friends.csv",header=TRUE, sep=",")

i<-group_by(ffr,source_id)
j<-summarise(i,length(source_id))
mean(j$`length(source_id)`)
k<-group_by(tfr,source_id)
l<-summarise(k,length(source_id))
mean(l$`length(source_id)`)

barplot(table(j$`length(source_id)`))
barplot(table(l$`length(source_id)`))

#percentage of geo-localized#

#fu means fake users
fu=read_csv(file = "/Users/admin/Desktop/fake/fakeusers.csv")
#tu means true users
tu=read_csv(file = "/Users/admin/Desktop/true/trueusers.csv")

e<-sum(is.na(fu$location))
f<-sum(is.na(tu$location))
e/nrow(fu)
f/nrow(tu)

#has used a hashtag #

#ft means tweets fake accounts posted
ft=read_csv(file = "/Users/admin/Desktop/fake/faketweets.csv")
#tt means tweets true accounts posted
tt=read_csv(file = "/Users/admin/Desktop/true/truetweets.csv")
g<-length(grep("#", ft$text))
h<-length(grep("#", tt$text))
g/nrow(ft)
h/nrow(tt)

#Part II : the ratio of friends to followers

account_followers = read.csv(file = "/Users/admin/Desktop/graph.csv")

plot(zxc, layout=layout.star,
     edge.width=E(zxc)$weight,
     edge.color="white",edge.arrow.size=10, edge.curved=.2,
     vertex.size=30, vertex.label.family="Arial Black" )

#Creating a gml file to import it to gephi and do visualization

write.graph(zxc, "graph.gml", format = "gml")
qwe<-plot(zxc,edge.width=E(zxc)$weight)
abc <- data.frame(account_followers)
zxc <- graph.data.frame(abc, directed = FALSE)
E(zxc)$weight = abc$weight


## ******************* Visualization **************

# randomly select 1500 samples and create network graph in Gephi

# randomly select samples from followers.csv

Random_ff = ff[sample(nrow(ff), 1500), ]
write.csv(Random_ff, file = "/Users/admin/Desktop/ff 1500.csv")

Random_tf = tf[sample(nrow(tf), 1500), ]
write.csv(Random_tf, file = "/Users/admin/Desktop/tf 1500.csv")

# randomly select samples from friends.csv

Random_ffr = ffr[sample(nrow(ffr), 1500), ]
write.csv(Random_ffr, file = "/Users/admin/Desktop/ffr 1500.csv")


Random_tfr = tfr[sample(nrow(tfr), 1500), ]
write.csv(Random_tfr, file = "/Users/admin/Desktop/tfr 1500.csv")




