source("functions.R")

load("data/00 loan.RData")
load("data/01 zdata.RData")
load("data/05 z_final.RData")

total     = loan %>% group_by(Year) %>% summarise(Total   = sum(Cnt)) %>% select(Total)
df        = loan %>% group_by(Year) %>% summarize(Default = sum(Default_cnt)) %>% select(Default)
df_actual = (df/total) %>% unlist()

m = unique(z_final$Model)
prerate = matrix(loan$Cnt, byrow = FALSE, nrow = 7)[,19]
df = c()

for(m in unique(z_final$Model))
{
  df_bt = df.backtest(loan$Cnt, z_final[z_final$Model == m & z_final$scenario == "Predict", "value"], zdata$rho, data.prep(loan)$cml_cnt)
  df_b  = df.scenario(prerate , z_final[z_final$Model == m & z_final$scenario == "Base"   , "value"], zdata$rho, data.prep(loan)$cml_cnt)
  df_a  = df.scenario(prerate , z_final[z_final$Model == m & z_final$scenario == "Adverse", "value"], zdata$rho, data.prep(loan)$cml_cnt)
  df_s  = df.scenario(prerate , z_final[z_final$Model == m & z_final$scenario == "Severe" , "value"], zdata$rho, data.prep(loan)$cml_cnt)
  df = c(df, df_actual, df_bt, df_b, df_a, df_s)
}

df_final = z_final
df_final$value = df

save(df_final, file = "data/08 df_final.RData")