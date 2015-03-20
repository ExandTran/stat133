# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  m=matrix(
    sample(initial.doctors,n.doctors),
    ncol=n.days,
    nrow=n.doctors
  )
  for (i in 2:n.days){
    a=sample(1:n.doctors,2)
    b=m[a,i]
    if (1 %in% b & 0 %in% b){
      c=sample(0:1,1,prob=c(1-p,p))
      if (c==1){
        m[a[1],i:n.days]=1
        m[a[2],i:n.days]=1
      }
    }  
  }
  return(m)
}

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output


# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
initial.doctors=c(rep(1,100),rep(0,900))
n.doctors=1000
n.days=2500
test=list(
  sim.doctors(initial.doctors,n.doctors,n.days,.2),
  sim.doctors(initial.doctors,n.doctors,n.days,.4),
  sim.doctors(initial.doctors,n.doctors,n.days,.6),
  sim.doctors(initial.doctors,n.doctors,n.days,.8),
  sim.doctors(initial.doctors,n.doctors,n.days,1)
  )

plot(x=1:n.days,y=colSums(test[[1]]),type="l",col="red",main="Number of Doctors that Adopted the New Drug in 2500 Days",xlab="Number of Days",ylab="Number of Doctors")
lines(x=1:n.days,y=colSums(test[[2]]),col="blue")
lines(x=1:n.days,y=colSums(test[[3]]),col="green")
lines(x=1:n.days,y=colSums(test[[4]]),col="black")
lines(x=1:n.days,y=colSums(test[[5]]),col="orange")
