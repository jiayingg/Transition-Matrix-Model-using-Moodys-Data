source("functions.R")

load("data/00 loan.RData")

zdata = rho.iteration(5, loan)

save(zdata, file = "data/01 zdata.RData")