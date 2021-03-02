# treehist

## Description
Accompanying code for "Inference on the history of a randomly growing tree" by Harry Crane and Min Xu.

**COMING SOON:** a more general package that handles networks with cycles.

The main file is "tree_tools.R". 
Trees are represented as adjacency lists and converted to
igraph objects for plotting purposes.

In "tree_tools.R", the function "countAllHist(tree)" will return
an n-dimensional vector of the posterior root probabilities for a 
tree of n nodes. See other files for examples. 

## Requirement
igraph R package. 

## Usage
To run the files (e.g. paper_exp.R), download all files into one directory and use the command

  > source("paper_exp.R")

in an R console. 
