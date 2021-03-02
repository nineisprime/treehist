##
##
##
## Reproduces all simulation results in
## "Inference on the history of a randomly
##  growing tree"  by Harry Crane and Min Xu
##
## Author: Min Xu
## Last updated: 3/02/21
##
##

source("tree_tools.R")

ntrials = 200

##
## (n, alpha, beta, gamma, 1-epsilon)
##
## gamma set to 1 in all cases
##

settings = list(
    c(10000, 0, 1, 1, 0.95),   ## Tab1,2,3
    c(10000, 1, 0, 1, 0.95),   ## Tab1,4,5
    c(10000, 8, 1, 1, 0.95),   ## Tab1
    c(10000, 8, -1, 1, 0.95),  ## Tab1
    c(5000, 0, 1, 1, 0.95),    ## Tab2,3
    c(20000, 0, 1, 1, 0.95),   ## Tab2,3
    c(10000, 0, 1, 1, 0.99),   ## Tab1,3
    c(10000, 0, 1, 1, 0.9),    ## Tab1,3
    c(10000, 1, 0, 1, 0.99),   ## Tab1,4,5
    c(10000, 1, 0, 1, 0.9),    ## Tab1,4,5
    c(100000, 0, 1, 1, 0.95)   ## Tab2
    )


## Results of simulations
## "covered" is a matrix of 0/1, equals 1
##  if conf set contains true root
##
## "sizes" is a matrix of integers,
##   giving size of the conf sets.

covered = matrix(0, length(settings), ntrials)
sizes = matrix(0, length(settings), ntrials)


max_prob = matrix(0, length(settings), ntrials)

for (ii in 1:length(settings)){

    for (it in 1:ntrials){
        print(c(ii, it))
        mysetting = settings[[ii]]

        n = mysetting[1]
        alpha = mysetting[2]
        beta = mysetting[3]
        gamma = mysetting[4]
        level = mysetting[5]

        atree = createPATree(n, alpha, beta, gamma)

        hist = countAllHist(atree)
        ord = order(hist, decreasing=TRUE)
        tmp = cumsum(sort(hist, decreasing=TRUE))
        K = sum(tmp <= level)

        confset = ord[1:K]

        max_prob[ii, it] = hist[ord[1]]
        covered[ii, it] = (1 %in% confset)
        sizes[ii, it] = K
    }
}

## average coverage and size across ntrials experiments
apply(covered, 1, mean)
apply(sizes, 1, mean)
