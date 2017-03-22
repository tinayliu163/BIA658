# Question 1
Most_Popular_Baby_Names <- read.csv(file = "/Users/admin/Downloads/Most_Popular_Baby_Names.csv")
head(Most_Popular_Baby_Names)
df = data.frame(Most_Popular_Baby_Names)

# Using the names function to see names of the variables and which column of
# data to which they correspond
names(Most_Popular_Baby_Names)

# Subsetting variables
Baby_Names <- subset(Most_Popular_Baby_Names, BRTH_YR == 2013 | BRTH_YR == 2014)
rownames(Baby_Names) <- NULL
head(Baby_Names)

# Q(a). The total number of UNIQUE names in the dataset.
df <- data.frame(Baby_Names)
length(unique(df$Name))

# Q(b). Assuming that the ethnicity is non-overlapping, for each year calculate the total number of babies born for each
#       ethnicity in the dataset.
# The 2013 statistics should look like this:
# - 1 2013 ASIAN AND PACIFIC ISLANDER 9293
# - 2 2013 BLACK NON HISPANIC ????
# - 3 2013 HISPANIC ????
# - 4 2013 WHITE NON HISPANIC ????
library(dplyr)
group_by(df,BRTH_YR,ETHCTY) %>% 
  summarise(sum(Count))

# Q(c).the top 3 most popular male and female baby names for each ethnicity
dt <- group_by(df,ETHCTY,Gender,Name) %>%
  summarise(sum(Count)) %>%
  top_n(3)
View(dt)

#Question 2 plot the network

# install igraph
install.packages('igraph')
library(igraph)

# Create an undirected social network from the adjacency list from hwk2
# Here (1,2) is the first edge, and (2,3) is the second edge, etc.
g = graph(edges = c(1,2,2,3,2,4,3,5,4,5,5,6,4,7,5,7,5,8,7,8), n =8, directed = FALSE)
class(g)
g
is.directed(g)
summary(g)
# define some attributes for nodes
V(g)$name = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
summary(g)

# plot is a "generic function" in R
plot(g)

# centralization measure, use mode option for in/out degree
degree(g)
# use table to get a degree distribution
table(degree(g))

degree(g, normalized = TRUE)
betweenness(graph = g, normalized = FALSE)
betweenness(graph = g, normalized = TRUE)
centr_degree_tmax(g, normalized = TRUE)
# eigenvalue centrality
evcent(g)

centr_degree(g,normalized = FALSE)
centralization.degree(g,normalized = FALSE)
centralization.degree(g,normalized = TRUE)
centralization.degree.tmax(g,normalize(FALSE))
centr_degree(g,normalized = FALSE)$centralization
centr_degree(g,normalized = FALSE)$centralization %>%
  `/`(centr_degree_tmax(g))


