source("functions.R")

load("data/00 loan.RData")
load("data/01 zdata.RData")

df_bt = df.backtest(loan$Cnt, zdata$zlist, zdata$rho, data.prep(loan)$cml_cnt)

total   = loan %>% group_by(Year) %>% summarise(Total   = sum(Cnt)) %>% select(Total)
df      = loan %>% group_by(Year) %>% summarize(Default = sum(Default_cnt)) %>% select(Default)
df_back = data.frame(Year = 1998:2016, Actual = df/total, BackTest = df_bt)

save(df_back, file = "data/02 df_back.RData")