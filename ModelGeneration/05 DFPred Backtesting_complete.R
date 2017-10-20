source("functions.R")

load("data/00 loan.RData")
load("data/01 zdata.RData")
load("data/05 z_final.RData")

total     = loan %>% group_by(Year) %>% summarise(Total   = sum(Cnt)) %>% select(Total)
df        = loan %>% group_by(Year) %>% summarize(Default = sum(Default_cnt)) %>% select(Default)
df_actual = (df/total) %>% unlist()

m = unique(z_final_all$Model)
prerate = matrix(loan$Cnt, byrow = FALSE, nrow = 7)[,19]
df = c()

for(m in unique(z_final_all$Model))
{
  df_bt = df.backtest(loan$Cnt, z_final_all[z_final_all$Model == m & z_final_all$scenario == "Predict", "value"], zdata$rho, data.prep(loan)$cml_cnt)
  df_b  = df.scenario(prerate , z_final_all[z_final_all$Model == m & z_final_all$scenario == "Base"   , "value"], zdata$rho, data.prep(loan)$cml_cnt)
  df_a  = df.scenario(prerate , z_final_all[z_final_all$Model == m & z_final_all$scenario == "Adverse", "value"], zdata$rho, data.prep(loan)$cml_cnt)
  df_s  = df.scenario(prerate , z_final_all[z_final_all$Model == m & z_final_all$scenario == "Severe" , "value"], zdata$rho, data.prep(loan)$cml_cnt)
  df = c(df, df_actual, df_bt, df_b, df_a, df_s)
}

df_final = z_final_all
df_final$value = df

save(df_final, file = "data/08 df_final_all.RData")

for(m in rev(unique(z_final_all$Model))) {print(plot_z(df_final[df_final$Model == m, 1:3], "Default Rate"))}