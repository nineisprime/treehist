##
## Generates Figure 5 in the paper
## "Inference on the history of a randomly
##  growing tree" by Harry Crane and Min Xu
##
##  produces two pdf in current working directory:
## "arrival_tree.pdf"
## "arrivalall.pdf"
##
## Author: Min Xu
## Last updated: 3/02/21
##

library(igraph)
library(ggplot2)
source("tree_tools.R")

n = 300
atree = createPATree(n, alpha=0, beta=1)

## Nodes to mark
a_node = 3
b_node = 200
c_node = 60


## Creating |E| x 2 matrix representing edges

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
V(net)$color = "grey"

V(net)$size = 2
V(net)[c(1)]$color="gold"
V(net)[1]$size = 6
V(net)[a_node]$size = 6
V(net)[a_node]$color= "red"
V(net)[b_node]$size = 6
V(net)[b_node]$color= "green"
V(net)[c_node]$size = 6
V(net)[c_node]$color= "blue"
wts = rep(NA, n)

l <- layout_with_fr(net, niter=120)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

## Uncomment to create plot

## pdf("arrival_tree.pdf")
## plot(net,
##      rescale=FALSE,
##      layout=l,
##      vertex.label=wts,
##      vertex.label.dist=1,
##      vertex.label.color="red",
##      vertex.label.cex=1,
##      vertex.label.family="Helvetica",
##      edge.width=.6,
##      edge.color="black")
## dev.off()


M = 500
probs = countAllHist(atree)
arriv = rep(0, M)
arriv2 = rep(0, M)
arriv3 = rep(0, M)
for (ii in 1:M){
    root = sample(1:n, 1, prob=probs)
    myhist = generateRandomTopoOrder(atree, root)
    arriv[ii] = which(myhist == a_node)
    arriv2[ii] = which(myhist == b_node)
    arriv3[ii] = which(myhist == c_node)
}


mydf = data.frame(
    node = c(rep("A", M), rep("B", M), rep("C",M)),
    arrival = c(arriv, arriv2, arriv3))

## pdf("arrivalall.pdf")
## ggplot(mydf, aes(x=arrival, color=node)) +
##   geom_histogram(fill="white", position="dodge", bins=80)+
##     theme(legend.position="top")+
##     theme_minimal()+
##     theme(text = element_text(size=20))
## dev.off()
