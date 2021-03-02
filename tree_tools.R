## The list-of-vector representation of a tree:
##
## Nodes are represented as integers
## A tree with n nodes is represented as a list of length n
## Each entry in the list is a set of nodes
##
## EX:
## length(mytree)   number of nodes
## mytree[u]      a vector of all children of node u


## Some examples:

bar = list()
bar[[1]] = c(2,3,4,5,6)
bar[[2]] = 1
bar[[3]] = 1
bar[[4]] = 1
bar[[5]] = 1
bar[[6]] = 1

mytree = list()
mytree[[1]] = c(2,3,4)
mytree[[2]] = c(1,5,6)
mytree[[3]] = c(1,7,8,9)
mytree[[4]] = c(1,10,11)
mytree[[5]] = c(2,12,13)
mytree[[6]] = c(2)
mytree[[7]] = c(3)
mytree[[8]] = c(3)
mytree[[9]] = c(3)
mytree[[10]] = c(4)
mytree[[11]] = c(4)
mytree[[12]] = c(5)
mytree[[13]] = c(5)


## Generate preferential attachment tree with attachment
## probability  beta * degree(j)^gamma + alpha
##
## INPUT: n -- size of the tree,
##        alpha, beta -- parameters
##
## OUTPUT: a tree with list-of-vectors representation

createPATree <- function(n, alpha=0, beta=1, gamma=1){

    mytree = list()

    mytree[[1]] = c(2)
    mytree[[2]] = c(1)

    degrees = rep(0, n)
    degrees[1] = 1
    degrees[2] = 1

    for (i in 3:n){
        ix = sample(1:(i-1), size=1, prob=(pmax(0, alpha + beta*degrees[1:(i-1)]^gamma)) )
        mytree[[ix]] = c(mytree[[ix]], i)
        mytree[[i]] = c(ix)
        degrees[i] = 1
        degrees[ix] = degrees[ix] + 1
    }

    return(mytree)
}

##
## INPUT: tree with list-of-vector representation
## OUTPUT: adjacency matrix corresponding to the tree
tree2Mat <- function(atree){
    library(Matrix)

    n = length(atree)
    out_mat = Matrix(0, n, n, sparse=TRUE)
    for (i in 1:n){
        out_mat[i, atree[[i]]] = 1
    }
    return(out_mat)
}



## not exact probability
getOrderProb <- function(mytree, node_seq, gamma=1){

    n = length(node_seq)

    degrees = rep(0, n)
    degrees[node_seq[1]] = 1
    degrees[node_seq[2]] = 1

    logp_sofar = 1

    for (ii in 3:n){

        cur_node = node_seq[ii]

        nbs = mytree[[cur_node]]

        prev = intersect(nbs, node_seq[1:(ii-1)])
        if (length(prev) != 1)
            print("ERROR!")

        logp_sofar = logp_sofar +
            log( (degrees[prev]^gamma) /sum(degrees^gamma) )

        degrees[prev] = degrees[prev] + 1
        degrees[cur_node] = 1

    }
    return(logp_sofar)
}

## INPUT: "prev_root" should be 0 by default
## OUTPUT: a vector of all nodes of mytree
generateRandomTopoOrder <- function(mytree, root, prev_root=0){
    children = mytree[[root]]
    if (prev_root != 0 && !(prev_root %in% children)) {
        print("Error")
    }

    if (length(children) == 1 && children[1] == prev_root)
        return( c(root) )

    children = setdiff(children, c(prev_root))

    sizes = c()
    results = list()
    for (ii in 1:length(children)){
        child = children[ii]

        res = generateRandomTopoOrder(mytree, child, root)
        results[[ii]] = res
        sizes = c(sizes, length(res))
    }

    tmp = runif(sum(sizes))
    rand_ord = order(tmp)


    my_out = rep(0, sum(sizes))

    start = 1
    for (ii in 1:length(children)){
        my_out[ sort(rand_ord[start:(start+sizes[ii]-1)]) ] = results[[ii]]
        start = start+sizes[ii]
    }

    return( c(root, my_out) )

}


## INPUT: "mytree" tree of size n, "root" integer
## OUTPUT: "subtree_sizes" array of size n, maps
##           each node to size of the subtree

global_subtree_sizes = list()
countSubtreeSizes <- function(mytree, root){
    n = length(mytree)
    global_subtree_sizes <<- list()
    countSubtreeSizesH(mytree, root, 0)
    subtree_sizes = global_subtree_sizes
    return(subtree_sizes)
}

countSubtreeSizesH <- function(mytree, root, prev){
    allchildren = mytree[[root]]
    counter = 1
    for (child in allchildren){
        if (child == prev)
            next
        else
            counter = counter + countSubtreeSizesH(mytree, child, root)
    }
    global_subtree_sizes[[root]] <<- counter
    return(counter)
}


## INPUT: mytree, root can be 1
## OUTPUT: a vector of conditional root probabilities
##            length is the size of the tree
countAllHist <- function(mytree, root=1){

    n = length(mytree)
    subtree_sizes = countSubtreeSizes(mytree, root)
    hist = rep(0, n)

    subtree_sizes = unlist(subtree_sizes)
    #print(subtree_sizes)

    S = c(root)
    T = c(root)

    hist[root] = - sum(log(subtree_sizes))
    while (length(S) > 0){
        cur_node = S[1]

        children = mytree[[cur_node]]
        for (child in children){
            if (child %in% T) next

            S[length(S) + 1] = child
            T[length(T) + 1] = child

            hist[child] = hist[cur_node] + log(subtree_sizes[child]/(n - subtree_sizes[child]))
        }

        S = S[-1]

    }

    hist = hist - max(hist) + 5
    hist = exp(hist)/sum(exp(hist))
    return(hist)
}

## INPUT: "graf"  adjacency list representation of a graph
##                nodes labeled [n]
##
## OUTPUT: n^2--by--2 matrix of all edges
##
##
graph2Links = function(graf){
    n = length(graf)

    links = matrix(0, n^2, 2)
    ix = 1
    for (ii in 1:n){
        tmp = graf[[ii]]
        for (jj in 1:length(tmp)){
            a = min(c(ii, tmp[jj]))
            b = max(c(ii, tmp[jj]))
            if (b %in% links[links[, 1] == a, ]) next
            links[ix, 1] = a
            links[ix, 2] = b
            ix = ix + 1
        }
    }

    links = links[1:(ix-1), ]
    return(links)
}
