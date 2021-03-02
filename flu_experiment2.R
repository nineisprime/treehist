##
##
##
## Code for Table 5 in
## "Inference for the history of a randomly growing tree"
## by Harry Crane and Min Xu
##
## Analysis of the flu graph (not tree) from Hens et al. (2012)
##
##
## Author: Min Xu
## Last updated: 3/02/21
##

source("tree_tools.R")
library(igraph)


## Flu network from Hens et al. (2012)
## Data is transcribed from Figure 1 in the Hens et al. paper
foo = list()
foo[[1]] = c(2,3,4,5,12,13,32,21,15)
foo[[2]] = c(1,32,13,21,15,6,7)
foo[[3]] = c(1)
foo[[4]] = c(1)
foo[[5]] = c(1, 33, 10, 12, 11)
foo[[6]] = c(2, 23, 22, 10)
foo[[7]] = c(2, 8, 9, 10)
foo[[8]] = c(24, 33)
foo[[9]] = c(24, 33)
foo[[10]] = c(19)
foo[[11]] = c(29)
foo[[12]] = c(5)
foo[[13]] = c(21, 17, 19)
foo[[14]] = c(32)
foo[[15]] = c(1, 21)
foo[[16]] = c(32)
foo[[17]] = c()
foo[[18]] = c(17, 32, 7, 6)
foo[[19]] = c()
foo[[20]] = c()
foo[[21]] = c()
foo[[22]] = c(33, 28, 25)
foo[[23]] = c(6)
foo[[24]] = c(26, 27, 33)
foo[[25]] = c()
foo[[26]] = c(27)
foo[[27]] = c()
foo[[28]] = c(25)
foo[[29]] = c(30, 22, 13, 24)
foo[[30]] = c(31, 22, 29, 24)
foo[[31]] = c()
foo[[32]] = c(13, 20, 14, 21, 15, 16)
foo[[33]] = c(8)

## symmetrize the adjacency list representation
n = 33
for (i in 1:n){
    for (j in foo[[i]]){
        if (!(i %in% foo[[j]]))
            foo[[j]] = c(foo[[j]], i)
    }
}

## Known transmission edges from Hens et al. (2012)
## solid edges in Figure 1 of Hens et al. (2012)
core_edges = t(matrix(c(1, 5, 1, 3, 1, 4, 1, 2, 14, 32, 22, 28, 24, 26), 2, 7))

links = graph2Links(foo)

core_edges_num = which(links[, 1] %in% core_edges[, 1] & links[, 2] %in% core_edges[, 2])

nodes = 1:n
net <- graph_from_data_frame(d=links, vertices=nodes, directed=FALSE)
V(net)$color = "black"
V(net)$size = 1

## results of the experiment:
## "succ" is a vector of 0/1, equals 1 if conf set contains true root
## "sizes" is vector of integers, records size of the conf set

ntrial = 200
succ = rep(0, ntrial)
sizes = rep(0, ntrial)

epsilon = 0.15

for (ii in 1:ntrial){

    E(net)$wts = rnorm(length(E(net)))
    E(net)$wts[core_edges_num] = -1e6

    atree = mst(net, E(net)$wts)

    mytree = list()
    for (i in 1:n){
        mytree[[i]] = as.vector(neighbors(atree, i))
    }

    hist = countAllHist(mytree)
    ord = order(hist, decreasing=TRUE)
    tmp = cumsum(sort(hist, decreasing=TRUE))

    K = sum(tmp <= 1-epsilon)
    confset_sm = ord[1:K]

    sizes[ii] = K
    succ[ii] = (1 %in% confset_sm)
}


## Code for visualizing the flu network

l <- layout_with_fr(net, niter=100)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
V(net)$color="grey"
V(net)$size = 6
V(net)$color[1] = "gold"
E(net)$color = "black"

## Uncomment to create "flu_net.pdf"

## pdf("flu_net.pdf")
## plot(net,
##      rescale=FALSE,
##      vertex.label = rep(NA, n),
##      layout=l,
##      edge.width=.6)
## dev.off()
