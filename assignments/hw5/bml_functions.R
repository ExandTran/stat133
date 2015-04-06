#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  m=
    matrix(
      sample(0:2,r*c,replace=TRUE,prob=c(1-p,.5*p,.5*p)),
      ncol=r,
      nrow=c
    )
  return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.
#blue(2) goes up
#red(1) goes right
blue.step.up <- function(m){
  n=1
  x=1
  while (n<=nrow(m)){
    while (x<=ncol(m)){
      if (m[n,x]==2){
        if (n==1){
          if (m[nrow(m),x]==0){
            m[nrow(m),x]=2
            m[n,x]=0
          }
        }else if (m[n-1,x]==0){
          m[n-1,x]=2
          m[n,x]=0
        }
      }
      x=x+1  
    }
    n=n+1
    x=1
  }
  return(m)
}

red.step.right <- function(m){
  x=ncol(m)
  n=1
  while (x>=1){
    while (n<=nrow(m)){
      if (x==ncol(m)){
        if (m[n,x]==1){
          if (m[n,1]==0){
            m[n,x]=0
            m[n,1]=1
          }
        }
      } else if (m[n,x]==1){
          if (m[n,x+1]==0){
            m[n,x]=0
            m[n,x+1]=1
          }
        }
      n=n+1
    }
    x=x-1
    n=1
  }
  return(m)
}

bml.step <- function(m){
  a=red.step.right(m)
  b=blue.step.up(a)
  if (identical(b,m)){
    grid.new=FALSE
  } else {
    grid.new=TRUE
  }
  return(list(b, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m=bml.init(r,c,p)
  lst=bml.step(m)
  k=0
  while (lst[2]==TRUE & k<=10000){
    lst=bml.step(lst[[1]])
    k=k+1
  }
  return(k)
}
