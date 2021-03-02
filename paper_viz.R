
##
## Code for Figure 5 in
## "Inference for the history of a randomly growing tree"
## by Harry Crane and Min Xu
##
## Analysis of the flu graph (not tree) from Hens et al. (2012)
##
##
## Author: Min Xu
## Last updated: 3/02/21
##

library(igraph)
source("tree_tools.R")

n = 1000

## Select parameter of the simulated tree
atree = createPATree(n, alpha=1, beta=0)

hist = countAllHist(atree)

ord = order(hist, decreasing=TRUE)
tmp = cumsum(sort(hist, decreasing=TRUE))

epsilon = 0.05
K = sum(tmp <= 1-epsilon)

confset = ord[1:(K+1)]

epsilon = 0.15
K = sum(tmp <= 1-epsilon)
confset_sm = ord[1:(K+1)]


links = matrix(0, (n-1), 2)
ix = 1
for (ii in 1:n){
   tmp = atree[[ii]] ## tree name here
   for (jj in 1:length(tmp)){
       a = min(c(ii, tmp[jj]))
       b = max(c(ii, tmp[jj]))
       if (b %in% links[links[, 1] == a, ]) next
       links[ix, 1] = a
       links[ix, 2] = b
       ix = ix + 1
   }
}

nodes = 1:n
net <- graph_from_data_frame(d=links, vertices=nodes, directed=FALSE)
V(net)$color = "black"

V(net)[confset]$color = "cyan"
V(net)[confset_sm]$color = "green"

wts = rep(NA, n)
wts[confset_sm] = round(hist[confset_sm], 3)

V(net)$size = 1

V(net)[confset]$size = 3.5
V(net)[confset_sm]$size=4.5

V(net)[1]$size = 3.5
V(net)[c(1)]$color = "gold"

l <- layout_with_fr(net, niter=120)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

## pdf("paper_bigua.pdf")
## plot(net,
##      rescale=FALSE,
##      layout=l,
##      vertex.label=wts,
##      vertex.label.dist=2,
##      vertex.label.color="red",
##      vertex.label.cex=1,
##      vertex.label.family="Helvetica",
##      edge.width=.6,
##      edge.color="gray")
## dev.off()
