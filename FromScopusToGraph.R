library(readr)
data1 <- read_csv("~/Downloads/scopus (5).csv")
dim(data1)
data2 <- read_csv("~/Downloads/scopus (4).csv")
dim(data2)
data3 <- read_csv("~/Downloads/scopus (6).csv")
dim(data3)

dataI <- rbind(data1,data2)
dataII <- rbind(dataI,data3)
dim(dataII)

library(dplyr)
data <- dataII %>%
  distinct()
dim(data)

edgelist <- data.frame(Node1 = character(), Node2 = character(), stringsAsFactors = FALSE)

for (authors in data$Authors) {
  author_names <- strsplit(authors, ";\\s*")[[1]]
  
  if (length(author_names) > 1) {
    pairs <- combn(author_names, 2)
    edges <- data.frame(Node1 = pairs[1,], Node2 = pairs[2,], stringsAsFactors = FALSE)
    edgelist <- rbind(edgelist, edges)
  }
}

# ordine alfabetico
edgelist$From <- pmin(edgelist$Node1, edgelist$Node2)
edgelist$To   <- pmax(edgelist$Node1, edgelist$Node2)
edgelist$weight <- 1
edgelist

simplified_edgelist <- aggregate(weight ~ From + To, data = edgelist, FUN = sum)


library(igraph)
g <- graph_from_data_frame(simplified_edgelist, directed = F)
g
table(which_multiple(g))
plot(g,vertex.label.cex=0.5,vertex.size=1,edge.size=E(g)$weight*10)

