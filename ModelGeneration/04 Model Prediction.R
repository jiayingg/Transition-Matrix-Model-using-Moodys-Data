source("functions.R")

load("data/00 macro.RData")
load("data/01 zdata.RData")
load("data/04 final_model_loose.RData")
load("data/04 final_model_tight.RData")

z_final = data.frame()

z_final = rbind(z_final, cbind(pred(zdata$zlist, macro, loose_2v[2,]), Model = rep("loose 2v", 50)))
z_final = rbind(z_final, cbind(pred(zdata$zlist, macro, loose_3v[2,]), Model = rep("loose 3v", 50)))
z_final = rbind(z_final, cbind(pred(zdata$zlist, macro, tight_2v[1,]), Model = rep("tight 2v", 50)))
z_final = rbind(z_final, cbind(pred(zdata$zlist, macro, tight_3v[2,]), Model = rep("tight 3v", 50)))

save(z_final, file = "data/05 z_final.RData")

for(m in unique(z_final$Model)) {print(plot_z(z_final[z_final$Model == m, 1:3]))}