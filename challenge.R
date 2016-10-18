library(hash)

#constructor function for "trie" class: 
trie<- function(maxweight=-1, selfweight=-1, letterkey=NA, children=hash(), isword=FALSE){  #initialze the attributes of node
  a <- list(maxweight = maxweight,
               selfweight = selfweight,
               letterkey = letterkey,
               children = children,
               isword=isword)
  attr(a, "class") <- "trie"
  return(a)
}

##The name of the functions have two parts separated by a ”.” 
##where the prefix is the function name and the suffix is the name of a class.

#Insert:
#generic function:
insert <- function(x){
  UseMethod("insert")
}

insert.trie <- function(node,weight,word){
  if(is.null(word)){
    node$isword = TRUE
    node$selfweight = weight
  } else{
    letter<- unlist(strsplit(word, split = ""))
    if(!has.key(letter[1], node$children)){ #if current letter is not a key to children, create a new node
      node$children[[letter[1]]] = trie(letterkey=letter[1])   # assign key value pair to this current node
    }
    if(length(letter)>1){
      restletter <- paste(letter[2:length(letter)], collapse = "",sep="")
    } else{
      restletter <- NULL
    }
    node$children[[letter[1]]] = insert.trie(node$children[[letter[1]]],weight,restletter) 
  }
 return(node)
}

#find maximum weight
#generic function:
maximumweight <- function(x){
  UseMethod("maxweight")
}
maximumweight.trie <- function(node){
if(is.empty(node$children)){#if it is leaf node, it doesn't have children
  node$maxweight <- node$selfweight #if it is leaf node, max weight = self weight
  return(node)
} 
else {  #if it is not leaf node, get maximum weight by comparison in its children's weight
  weight <- c()  #creat an empty list first
  for ( key in keys(node$children)){   # find all keys of current node$children. 
                                        # loop through all of its children to get their max weight individually
    weight <- c(weight, maximumweight.trie(node$children[[key]])$maxweight) #list out all the weight  
  }
  node$maxweight <- max(weight)  #get the maximum weight 
  return(node)
}
}

read.terms <- function(textfile){
  #read file
  dtf <- read.csv(textfile, header = FALSE, as.is = TRUE, skip = 1,strip.white = TRUE, sep = "\t", col.names = c("weight", "word"))
  
  #initialize root, then insert every word to build trie
  root = trie()
  for (i in 1:nrow(dtf)){
    root = insert.trie(root,dtf[i,1],dtf[i,2])
  }
  #update the maxweights of every node
  root = maximumweight.trie(root)
  
} 
  

#autocomplete()

