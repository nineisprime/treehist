##
## Code for Figure 7 in
## "Inference for the history of a randomly growing tree"
## by Harry Crane and Min Xu
##
## Analysis of the flu tree from Hens et al. (2012)
##
##
## Author: Min Xu
## Last updated: 3/02/21
##

library(igraph)
source("tree_tools.R")


## Reconstructed flu tree from Hens et al. (2012)
## Data is transcribed from Figure 3 in the Hens et al. paper

## 1 = C4
## 2 = C5
## 3 = C8
## 4 = C9
## 5 = C7
## 6 = C14
## 7 = C13
## 8 = C16
## 9 = C17
## 10 = C18
## 11 = C32
## 12 = C27
## 13 = C30
## 14 = C29
## 15 = C31
## 16 = C42
## 17 = C43
## 18 = C28
## 19 = C44
## 20 = C33
## 21 = C40
## 22 = C39
## 23 = C45
## 24 = C38
## 25 = C58
## 26 = C53
## 27 = C59
## 28 = C52
## 29 = C51
## 30 = C60
## 31 = C69
## 32 = C15
## 33 = C57


foo = list()
foo[[1]] = c(2, 4, 3, 5)
foo[[2]] = c(1, 7, 32, 6)
foo[[3]] = c(1)
foo[[4]] = c(1)
foo[[5]] = c(1, 11, 12)
foo[[6]] = c(2, 23, 22)
foo[[7]] = c(2, 10, 9, 8, 18)
foo[[8]] = c(7, 24)
foo[[9]] = c(7)
foo[[10]] = c(7)
foo[[11]] = c(5, 29)
foo[[12]] = c(5)
foo[[13]] = c(19, 21, 32)
foo[[14]] = c(32)
foo[[15]] = c(32)
foo[[16]] = c(32)
foo[[17]] = c(18)
foo[[18]] = c(7, 17)
foo[[19]] = c(13)
foo[[20]] = c(32)
foo[[21]] = c(13)
foo[[22]] = c(6, 28, 25)
foo[[23]] = c(6)
foo[[24]] = c(8, 26, 27, 33)
foo[[25]] = c(22)
foo[[26]] = c(24)
foo[[27]] = c(24)
foo[[28]] = c(22)
foo[[29]] = c(11, 30)
foo[[30]] = c(29, 31)
foo[[31]] = c(30)
foo[[32]] = c(2, 20, 13, 14, 15, 16)
foo[[33]] = c(24)


n = 33

epsilon = 0.2
epsilon2 = 0.05

probs = countAllHist(foo)

ord = order(probs, decreasing=TRUE)
conf = cumsum(sort(probs, decreasing=TRUE))

wts = round(probs, 4)
wts[ord[conf > 1-epsilon]] = NA
nodes = cbind(1:n, probs)

## convert tree to igraph object for plotting
links = matrix(0, (n-1), 2)
ix = 1
for (ii in 1:n){
   tmp = foo[[ii]]
   for (jj in 1:length(tmp)){
       a = min(c(ii, tmp[jj]))
       b = max(c(ii, tmp[jj]))
       if (b %in% links[links[, 1] == a, ]) next
       links[ix, 1] = a
       links[ix, 2] = b
       ix = ix + 1
   }
}
net <- graph_from_data_frame(d=links, vertices=nodes, directed=FALSE)

V(net)$color = "gray"
V(net)[ord[conf < 1 - epsilon2]]$color = "green"

V(net)$size = 7
V(net)[ord[conf < 1 - epsilon2]]$size = 7

V(net)[c(1)]$color = "gold"

l <- layout_with_fr(net, niter=90)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

## Uncomment to create "flu_tree.pdf" plot

## pdf("flu_tree.pdf")
## plot(net,
##      rescale=FALSE,
##      layout=l,
##      vertex.label=wts,
##      vertex.label.dist=1,
##      vertex.label.color="red",
##      vertex.label.cex=1.3,
##      vertex.label.family="Helvetica",
##      edge.width=1.5)
## dev.off()
