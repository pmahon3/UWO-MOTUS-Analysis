library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(HDInterval)
source("Inputs.R")
## Analyze coverage
NPOPS=75
burnin = 100

## init coverage results
muDelta.primeCoverage = 0
muMuDeltaCoverage = c(0,0)

for (pop in 1:NPOPS){
  
  # read in data
  samples = data.frame(readRDS(paste("./results/samples/population", toString(pop), ".rds", sep = "")))
  
  # Coverage
  
  simulatedParams = readRDS(paste("./results/data/simulatedParams", toString(pop), ".rds", sep = ""))

  # init coverage results

  delta.primeCoverage = 0
  muDeltaCoverage = c(0,0)
  deltaCoverage = c(0,0)
  
  for (bird in 1:nBirds) {
    # delta.prime coverage
    delta.primeStr = paste("delta.prime.", toString(bird), ".", sep="")  
    trueDelta.prime = simulatedParams$deltaPrime[bird]
    delta.primeHdi = hdi(samples[[delta.primeStr]][burnin:length(samples[[delta.primeStr]])], 0.95)  
    if (trueDelta.prime > delta.primeHdi[1] && trueDelta.prime < delta.primeHdi[2]){delta.primeCoverage = delta.primeCoverage + 1}
    
    # muDelta coverage
    for (i in 1:2) {
      muDeltaStr = paste("muDelta.", toString(bird), "..", toString(i), ".", sep="")    
      trueMuDelta = simulatedParams$muDelta[bird,i]  
      muDeltaHdi = hdi(samples[[muDeltaStr]][burnin:length(samples[[muDeltaStr]])], 0.95)
      if (trueMuDelta > muDeltaHdi[1] && trueMuDelta < muDeltaHdi[2]){muDeltaCoverage[i] = muDeltaCoverage[i] + 1}
    }
    for (day in 1:nDays) {
      # delta coverage    
      for (i in 1:2){ 
        deltaStr = paste("delta.", toString(bird), "..", toString(day), "..", toString(i), ".", sep="") 
        trueDelta = simulatedParams$delta[bird,day,i]
        deltaHdi = hdi(samples[[deltaStr]][burnin:length(samples[[deltaStr]])])
        if (trueDelta > deltaHdi[1] && trueDelta < deltaHdi[2]){deltaCoverage[i] = deltaCoverage[i] + 1}
      }
    }
  }
  
  # muDelta.prime coverage
  trueMuDelta.prime = trueParams$muDelta.prime
  muDelta.primeHdi = hdi(samples[['muDelta.prime']][burnin:length(samples[['muDelta.prime']])])  
#  print("muDelta.prime:")  
  if (trueMuDelta.prime > muDelta.primeHdi[1] && trueMuDelta.prime < muDelta.primeHdi[2]){
    muDelta.primeCoverage = muDelta.primeCoverage + 1
 #   print(1)  
  }
  else{
 #   print(0)
  }  
  
  # muMuDelta coverage
  tmp = c(0,0)
  for (i in 1:2) {
    muMuDeltaStr = paste("muMuDelta.", toString(i), ".", sep="")
    muMuDeltaHdi = hdi(samples[[muMuDeltaStr]][burnin:length(samples[[muMuDeltaStr]])], 0.95)
    if (trueParams$muMuDelta[i] > muMuDeltaHdi[1] && trueParams$muMuDelta[i] < muMuDeltaHdi[2]){
      muMuDeltaCoverage[i] = muMuDeltaCoverage[i] + 1
      tmp[i] = 1   
    }
  }
#  print("muMuDelta:")
#  print(tmp)

  
  
  # results output
  delta.primeCoverage = delta.primeCoverage/nBirds
  deltaCoverage = deltaCoverage/(nBirds*nDays)
  muDeltaCoverage = muDeltaCoverage/nBirds
  
  if (TRUE) {
    print(paste("Simulation", toString(pop), "Coverage", sep = " "))
    print("------------------------------------------")
    print(muDelta.primeHdi)
    print("delta.prime:")
    print(delta.primeCoverage)
    print("delta:")
    print(deltaCoverage)
    print("muDelta:")
    print(muDeltaCoverage)
    
    par(mfrow=c(2,1))
    plot(samples$muDelta.prime, type = "l")
    abline(a=trueParams$muDelta.prime, b=0, lty = 3, col = 'red')
    abline(a=muDelta.primeHdi[1], b = 0, lty = 2, col = 'blue')
    abline(a=muDelta.primeHdi[2], b = 0, lty = 2, col = 'blue')
    plot(samples$muMuDelta.2., type = "l")
    abline(a=trueParams$muMuDelta[2], b=0, lty = 3, col = 'red')
    abline(a=muMuDeltaHdi[1], b = 0, lty = 2, col = 'blue')
    abline(a=muMuDeltaHdi[2], b = 0, lty = 2, col = 'blue')
  }
}

# simulation run results
muMuDeltaCoverage = muMuDeltaCoverage/NPOPS
muDelta.primeCoverage = muDelta.primeCoverage/NPOPS

print(paste("Overall", "Results", sep = " "))
print("------------------------------------------")
print("muMuDelta:")
print(muMuDeltaCoverage)
print("muDelta.prime:")
print(muDelta.primeCoverage)
