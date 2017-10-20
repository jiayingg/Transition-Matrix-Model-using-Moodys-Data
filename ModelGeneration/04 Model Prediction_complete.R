source("functions.R")

load("data/00 macro.RData")
load("data/01 zdata.RData")
load("data/04 final_model_loose.RData")
load("data/04 final_model_tight.RData")

z_final = data.frame()
for(i in 1:nrow(loose_2v)) {z_final = rbind(z_final, cbind(pred(zdata$zlist, macro, loose_2v[i,]), Model = rep(paste("loose 2v", i), 50)))}
# for(m in rev(unique(z_final$Model))) {print(plot_z(z_final[z_final$Model == m, 1:3], "Z Score"))}

# z_final = data.frame()
for(i in 1:nrow(loose_3v)) {z_final = rbind(z_final, cbind(pred(zdata$zlist, macro, loose_3v[i,]), Model = rep(paste("loose 3v", i), 50)))}
# for(m in rev(unique(z_final$Model))) {print(plot_z(z_final[z_final$Model == m, 1:3], "Z Score"))}

# z_final = data.frame()
for(i in 1:nrow(tight_2v)) {z_final = rbind(z_final, cbind(pred(zdata$zlist, macro, tight_2v[i,]), Model = rep(paste("tight 2v", i), 50)))}
# for(m in rev(unique(z_final$Model))) {print(plot_z(z_final[z_final$Model == m, 1:3], "Z Score"))}

# z_final = data.frame()
for(i in 1:nrow(tight_3v)) {z_final = rbind(z_final, cbind(pred(zdata$zlist, macro, tight_3v[i,]), Model = rep(paste("tight 3v", i), 50)))}
# for(m in rev(unique(z_final$Model))) {print(plot_z(z_final[z_final$Model == m, 1:3], "Z Score"))}

z_final_all = z_final
save(z_final_all, file = "data/05 z_final_all.RData")