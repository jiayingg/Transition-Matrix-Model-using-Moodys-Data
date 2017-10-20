source("functions.R")

load("data/00 macro.RData")
load("data/01 zdata.RData")

cl = makeCluster(detectCores())  # Detects the number of cores
registerDoParallel(cl)           # Registers the number of cores
getDoParWorkers()

# tight: Pass all tests
Model_3v = model.select(zdata$zlist, macro$x, 3, 0.1, "tight")
Model_2v = model.select(zdata$zlist, macro$x, 2, 0.1, "tight")
tight_3v = Sign.Check_3v(Model_3v)
tight_2v = Sign.Check_2v(Model_2v)

save(tight_3v, tight_2v, file = "data/04 final_model_tight.RData")

# loose: Pass only p-value <= tol
Model_3v = model.select(zdata$zlist, macro$x, 3, 0.1, "loose")
Model_2v = model.select(zdata$zlist, macro$x, 2, 0.1, "loose")
loose_3v = Sign.Check_3v(Model_3v)
loose_2v = Sign.Check_2v(Model_2v)

save(loose_3v, loose_2v, file = "data/04 final_model_loose.RData")