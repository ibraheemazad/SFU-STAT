#recpart.R - lab 3 copied
##----------given function week04---------------------
new_node <- function(data,childl=NULL,childr=NULL){
  nn <- list(data=data,childl=childl,childr=childr)
  class(nn) <- "node"
  return(nn)
}

new_region <- function(coords=NULL,x,y){
  if(is.null(coords)) {
    coords <- sapply(x,range)
  }

  "for region need coordinate matrix, x and y"
  out <- list(coords=coords,x=x,y=y)
  class(out) <- "region"
  return(out)
}

split_points <- function(x){
  x <- sort(unique(x))
  x <- x[-length(x)]
  return(x)
}

split.region <- function(Region,variable,points){
  #copied from lab 4
  r1_ind <- (Region$x[,variable] <= points)
  c1 <- c2 <- Region$coords
  c1[2,variable] <- points; c2[1,variable] <- points
  Rl <- new_region(c1,Region$x[r1_ind,,drop=FALSE],Region$y[r1_ind])
  Rr <- new_region(c2,Region$x[!r1_ind,,drop=FALSE],Region$y[!r1_ind])
  return(list(Rl=Rl,Rr=Rr))

}

recpart_recursive <- function(node) {
  R <- node$data
  # stop recursion if region has a single data point
  if(length(R$y) == 1) { return(node) } # NB: was return(NULL)
  # else find a split that minimizes a LOF criterion
  # Initialize
  lof_best  <- Inf
  # Loop over variables and splits
  for(v in 1:ncol(R$x)){
    "split point - instead of looking at split point, split based on 0,1 for parent and child "
    tt <- split_points(R$x[,v]) # Exercise: write split_points()
    for(t in tt) {
      gdat <- data.frame(y=R$y,x=as.numeric(R$x[,v] <= t))
      "each split point checking lack of fit"
      lof <- LOF(y~.,gdat) # Exercise: write LOF()
      if(lof < lof_best) {
        lof_best <- lof
        childRs <- split.region(Region=R,variable=v,points=t) # Exercises: write split.region() --- Must match function definition
      }
    }
  }
  # Call self on best split
  node$childl <- recpart_recursive(new_node(childRs$Rl))
  node$childr <- recpart_recursive(new_node(childRs$Rr))
  return(node)
  "output is a chlild"
}


"this returns a tree object"
recpart <- function(x,y){
  init <- new_node(new_region(x=x,y=y))
  tree <- recpart_recursive(init)
  class(tree) <- c("tree",class(tree))
  return(tree)
}

##### week 04 exercises #############
# Write the function `LOF()` that returns the lack-of-fit criterion
# for a model. The function should take a model formula and
# data frame as input, pass these to `lm()` and return the
# residual sum of squares. "

LOF <- function(formula,dataframe){
  fitted_function <- lm(formula,dataframe)
  residual_sum_squares <- sum(resid(fitted_function)^2)

  return(residual_sum_squares)
}

################# 1.print region ###############################
#
# 1. Write a print method, print.region(), for a region data structure.
# Print whatever you find interesting or relevant about a region.
#
#######################################################################
print.region <- function(out){

  cat("coordinates of region")
  print(out$coords)

  cat("x:\n")
  print(out$x)

  cat("y:\n")
  print(out$y)

}
x <- data.frame(x1=c(-0.66,1.72,2.12,1.50,-0.04),x2=c(1.23,-0.66,1.07,-0.38,1.04))
y <- c(-0.38,0.3,0.67,-0.29,0.49)
coords <- sapply(x,range)
region_data <- list(coords=coords,x=x,y=y)
print.region(region_data)

######## 2. plot_regions.tree() ##########
# take a tree as input, do a scatterplot of the covariate data in the tree (tree$data$x) and then call
# plot_regions.node() on each of the child nodes of the tree.
###################
"DONE"
plot_regions.tree <- function (tree)
{
  plot(tree$data$x[,1],tree$data$x[,2],xlab="X1",ylab="X2")

  #plot the nodes
  plot_regions.node(tree$childl) #need to decide which produces node output***
  plot_regions.node(tree$childr) #need to decided which produces node output***
}

##################### 2. plot_regions.node ############################################
# ake a node as input.
# If the node is NULL, the function should just return without doing anything.
# If node is not NULL, the function should
#     # (i) use lines() to draw a box on the scatterplot with vertices given by the
#     #     coordinates matrix in the node's data
#     # (ii) call itself on the input node's two child nodes.
# Thus, plot_regions.node() recursively traverses the tree, plotting the regions of the partition.
#
#################################################################
"done"
plot_regions.node <- function (node)
{
  if(is.null(node) == TRUE)
  {
    return (NULL)
  }

  #need x and y node coordinates
  x <- node$data$coords[,1]
  y <- node$data$coords[,2]


  #i) use lines(x, y) - draws box with coordinates
  lines(c(x[1],x[2],x[2],x[1],x[1]),c(y[1],y[1],y[2],y[2],y[1]), col="red") #x1y1,x2y1,x2y2,x1y2,x1y1

  #ii) call on left and right node
  plot_regions.node(node$childl)
  plot_regions.node(node$childr)

}


#### 3.# Test:  ############
#set.seed(123); n <- 10
#x <- data.frame(x1=rnorm(n),x2=rnorm(n))
#y <- rnorm(n)
#
#mytree <- recpart(x,y) #need split points #LOF wrote these
#str(mytree)#
#mytree

"lines exaplined"
#mytree$data$x[,1]

#plot_regions(mytree) - showed in lecture
#plot_regions.node(mytree)
"output matches-100% right 2:50 pm"

#mytree$childl$childl
#mytree$childr$childr
