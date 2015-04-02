#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.
lst.simulations=c()
simulation <- function(r,c,p){
  n=1
  lst=c()
  while (n<200){
    a=bml.sim(r,c,p)
    if (a>9999){
      
    } else {
      lst[n]=a
      n=n+1
    }
  }
  return(mean(lst))
}
sim.density.study=
  data.frame(
    density=c(0.3,0.5,0.6,0.7,0.8,1.0),
    steps=c(
    simulation(10,10,.3),
    simulation(10,10,.5),
    simulation(10,10,.6),
    simulation(10,10,.7),
    simulation(10,10,.8),
    simulation(10,10,1.0)
    )
  )
#.3=0
#.5=1206.94
#.6=74.61
#.7=27.29
#.8=15.583
#1.0=1
sim.grid.size.study=
  data.frame(
    size=c(5,10,15,20,25),
    steps=c(
    simulation(5,5,.6),
    simulation(10,10,.6),
    simulation(15,15,.6),
    simulation(20,20,.6),
    simulation(25,25,.6)
    )
  )
    
#5=15.81
#10=78.15
#15=67.30
#20=80.31
#25=92.14
lst.simulations=list(sim.density.study,sim.grid.size.study)
plot(lst.simulations[[1]],pch=20,main="The Effect of Density on the Amount of Steps Before Gridlock")
plot(lst.simulations[[2]],pch=20,main="The Effect of Grid Size on the Amount of Steps Before Gridlock")

save(lst.simulations,file="simulation.rda")
