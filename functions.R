library(dplyr)
library(reshape2)
# library(parallel)
# library(doParallel)
# library(lmtest)
# library(car)
# library(tseries)

#====================================================================================#
# Transform raw time series data into list of matrices ==============================#
#====================================================================================#
matrix.list = function(matrix)
{
  mtx_list = list()
  mtx_length = ncol(matrix)-1 
  
  for (i in 1:(nrow(matrix)/mtx_length))
  {
    mtx_list[[i]] = matrix[(mtx_length*i-mtx_length + 1):(mtx_length*i),]
  }
  return(mtx_list)
}

#====================================================================================#
# Calculate cumulate prob and Average pd ============================================#
#====================================================================================#
data.prep = function(loan)
{
  count = loan %>% select(contains("_cnt"))
  rownames(count) = paste(loan$Year, loan$Rating)
  count_list = matrix.list(count)
  
  count_prop = loan %>% select(Aaa,Aa,A,Baa,Ba,B,Caa.C,Default)
  rownames(count_prop) = paste(loan$Year, loan$Rating)
  count_prop_list = matrix.list(count_prop)
  
  t = cbind(loan$Rating, count)
  colnames(t) = c("Rating","Aaa","Aa","A","Baa","Ba","B","Caa.C","Default")
  t$Rating = ordered(t$Rating, levels = c("Aaa","Aa","A","Baa","Ba","B","Caa.C"))
  count_avg = acast(melt(t, id = "Rating"), Rating ~ variable, fun.aggregate = sum, value.var = "value")
  count_avg_prop = prop.table(count_avg, 1)
  
  # Cummulate Probability
  cml_cnt = cbind(rep(0, nrow(count_avg_prop)), t(apply(count_avg_prop,1,cumsum)))
  cml_cnt[,ncol(count_avg_prop)+1] = 1
  cml_cnt[which(cml_cnt>1)] = 1
  
  # Constrain # 1.
  avgpd_cnt = matrix(c(count_avg_prop[,ncol(count_avg_prop)], 1.0), 
                     nrow(count_avg_prop), ncol(count_avg_prop), 
                     byrow = TRUE)
  
  return(data = list("count_prop_list" = count_prop_list, 
                     "count_list" = count_list, 
                     "cml_cnt" = cml_cnt, 
                     "avgpd_cnt" = avgpd_cnt))
}

#====================================================================================#
# G-to-g transition rate in quarter t ===============================================#
#====================================================================================#
delta.transition = function(rho, Zt, cml)
{
  bins_mtx = qnorm(1 - cml, mean = 0, sd = 1)
  delta = pnorm((bins_mtx[, -ncol(bins_mtx)] - sqrt(rho)*Zt)/sqrt(1 - rho), mean = 0, sd = 1, log = FALSE) - 
    pnorm((bins_mtx[, -1] - sqrt(rho)*Zt)/sqrt(1 - rho), mean = 0, sd = 1, log = FALSE)
  colnames(delta) = c(colnames(delta)[-1], "DF")
  return(delta)
  
  # test_delta = delta.transition(0, 0, cml)
  # --> should be the same as data$bal_avg_prop
}

#====================================================================================#
# 1. Least Square Error(LSE) to optimize ============================================#
# 2. Absolute Error(AE) to optimize =================================================#
# 3. Relative Error(RE) to optimize =================================================#
#====================================================================================#
optimize.problem = function(rho, Zt, observed, startingcount, cml, avgpd, error)
{
  # Constrains
  # 1. put more weights on ranks with higher default rate 
  
  # 2. put more weights on larger balance amount/counts
  startingcount2 = matrix(rowSums(startingcount), byrow = FALSE)
  
  stressed = delta.transition(rho, Zt, cml)
  
  # min_op = (observed - stressed)^2/(stressed * (1-stressed))
  # min_op[is.na(min_op)] = 0
  # min_op = sum(min_op)
  
  delta = stressed - observed
  min_op1 = sum((c(startingcount2)*avgpd*delta)^2)
  min_op2 = sum(abs(c(startingcount2)*avgpd*delta))
  min_op3 = abs(sum(c(startingcount2)*avgpd*delta))
  
  min_op = ifelse(error == 1, min_op1, ifelse(error == 2, min_op2, min_op3))
  
  return(min_op)
}

#====================================================================================#
# Finding z with a set of rho provided ==============================================#
#====================================================================================#
find.z = function(lower, upper, n_itr, prop_list, cnt_list, cml, avgpd, which_solution)
{
  var_list = c()
  mean_list = c()
  
  for (rho in seq(lower, upper, 1/10^n_itr))
  {
    print(paste("rho:", rho))
    z_list = c()
    min_obj = c()
    
    for (i in 1:length(prop_list))
    {
      OP = optimize(f              = optimize.problem,
                    rho            = rho,
                    observed       = prop_list[[i]],
                    startingcount  = balcnt_list[[i]],
                    cml            = cml,
                    avgpd          = avgpd,
                    error          = error,
                    interval       = c(-5, 5),
                    tol            = .Machine$double.eps^10)
      z_list[i] = OP$minimum
      min_obj[i] = OP$objective
    }
    
    var_list[length(var_list)+1] = var(z_list)
    mean_list[length(mean_list)+1] = mean(z_list)
    
  }
  
  index_1 = which.min(abs(var_list - 1))
  index_2 = ifelse(var_list[index_1] < 1, index_1 - 1, index_1 + 1)
  rho_left = lower + (ifelse(index_1 > index_2, index_2, index_1) - 1)*1/10^n_itr
  rho_right = lower + (ifelse(index_1 > index_2, index_1, index_2) - 1)*1/10^n_itr
  
  # return upper and lower bound value of next rho iteration
  bound = list(lower = rho_left, upper = rho_right, zlist = z_list)
  
  return(bound)
}

#====================================================================================#
# Improve the accuracy of rho digit by digit ========================================#
#====================================================================================#
rho.iteration = function(max_itr, seg, error = 3, weight = 1)
{
  # error = 1    Squared Error (SE)
  # error = 2    Absolute Error (AE)
  # error = 3    Relative Error (RE)
  # weight = 1   Weight on Counts
  # weight = 2   Weight on Balance
  
  data = data.prep2(seg)
  
  lower = 0
  upper = 0.99
  
  for(n_itr in 1:max_itr)
  {
    print(paste("Iteration No.", n_itr, "..............................."))
    
    if(weight == 1)
    {
      itr = find.z(lower, upper, n_itr, data$count_prop_list, data$count_list, data$cml_cnt, data$avgpd_cnt, error)
    } else if(weight == 2)
    {
      itr = find.z(lower, upper, n_itr, data$bal_prop_list,   data$bal_list,   data$cml_npb, data$avgpd_npb, error)
    }
    
    lower = itr$lower
    upper = itr$upper
  }
  
  rho = round(lower, max_itr-1)
  zlist = itr$zlist
  
  results = list("rho" = rho,
                 "zlist" = zlist)
  
  return(results)
}

#================================================================================================#
#-Model Generator Code---------------------------------------------------------------------------#
#================================================================================================#

model.select = function(y, x, n, tol, criterion)
{
  ## Make sure that no duplicated varaibles selected for the same model
  var = combn(colnames(x[,-1]),n) 
  var = t(var)
  
  var.prep = var %>% gsub(pattern = "_Ydf", replacement = "") %>%
    gsub(pattern = "_Ygr", replacement = "") %>%
    gsub(pattern = "lag0_", replacement = "") %>%
    gsub(pattern = "lag1_", replacement = "") 
  
  variable.data = var[apply(var.prep, 1, function(x) length(unique(x))) == n, ]
  
  w = foreach(k = 1:nrow(variable.data),.combine='rbind',.packages = c('lmtest','car','tseries')) %dopar%   
  {
    # Create linear model
    model.data = cbind(x[variable.data[k,]])
    model = lm(y~., data = model.data)
    summary = summary(model)
    
    # Extract the coefficients and signs
    coef = as.data.frame(summary$coefficients)
    coef$sign = "+"
    coef$sign[coef$Estimate <= 0] = "-"
    
    # Stationarity test for residuals (Augmented Dickey-Fuller)
    adf_pval = adf.test(summary$residuals)$p.val
    pp_pval = PP.test(summary$residuals)$p.val
    
    # Normality test for residuals
    sw_pval = shapiro.test(summary$residuals)$p.val
    
    # Hetereoscedasticity Test 
    bp_pval = bptest(model)$p.val
    
    # Multicollinearity test (Variance Inflation Factor)
    vif_x = vif(model)
    
    # Autocorrelation test (Durbin-Watson)
    dw_pval = dwtest(model,alternative="two.sided")$p.val
    # dw_pval = durbinWatsonTest(model,alternative="two.sided")$p
    
    result.combined=c(variable.data[k,],
                      summary$coefficients[,"Estimate"],
                      coef$sign,
                      summary$coefficients[,"Pr(>|t|)"],
                      summary$r.squared,
                      summary$adj.r.squared,
                      adf_pval,
                      pp_pval,
                      vif_x,
                      dw_pval,
                      sw_pval,
                      bp_pval)
    return(result.combined)
  }
  
  # Name the columns
  model.summary = data.frame(w)
  base       = c("Intercept", paste("var", 1:n, sep = ""))
  var_name   = paste(base[-1],"name",sep="_")
  coef_name  = paste(base,"coef",sep="_")
  sign_name  = paste(base,"sign",sep="_")
  pval_name  = paste(base,"pval",sep="_")
  vif_name   = paste(base[-1],"vif",sep="_")
  dw_name    = "DW_pval"
  adf_name   = "ADF_pval"
  pp_name    = "PP_pval"
  sw_name    = "SW_pval"
  bp_name    = "BP_pval"
  r2_name    = "Rsq"
  adjr2_name = "adj.Rsq" 
  final_name = c(var_name,
                 coef_name,
                 sign_name,
                 pval_name,
                 r2_name,
                 adjr2_name,
                 adf_name,
                 pp_name,
                 vif_name,
                 dw_name,
                 sw_name,
                 bp_name)
  colnames(model.summary) = final_name
  
  # Filtering
  filtered.columns = c(pval_name, adf_name, pp_name, vif_name, dw_name, sw_name, bp_name)[-1]
  data = lapply(model.summary[,filtered.columns], function(x) as.numeric(as.character(x)))
  data = as.data.frame(data)
  
  # p val < tolerance
  # adf   < tolerance
  # col.smaller = c(pval_name,adf_name)[-1]
  col.smaller = c(pval_name)[-1]
  
  VIF_max = apply(data[,vif_name],1,max)
  
  if(criterion == "tight")
  {
    keep = c(rowSums(data[,col.smaller] <= tol) == (n) &
               (data["PP_pval"] <= tol | data["ADF_pval"] <= tol) &
               data["DW_pval"] >= tol &
               data["SW_pval"] >= tol &
               data["BP_pval"] >= tol &
               VIF_max <= 10)
  } else
  {
    keep = c(rowSums(data[,col.smaller] <= tol) == (n))
  }
  
  model.filtered = model.summary[keep,] %>% arrange(desc(as.numeric(as.character(adj.Rsq))))
  return(model.filtered)
}

#================================================================================================#
#- Sign Check -----------------------------------------------------------------------------------#
#================================================================================================#

Sign.Check_3v = function(Model.Data)
{
  Model.Data = as.data.frame(Model.Data)
  
  Model.Data$SignCheck1=paste(Model.Data$var1_name,Model.Data$var1_sign)
  Model.Data$SignCheck2=paste(Model.Data$var2_name,Model.Data$var2_sign)
  Model.Data$SignCheck3=paste(Model.Data$var3_name,Model.Data$var3_sign)
  
  Model.Data$SignCheck1=gsub("_Ygr","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("_Ygr","",Model.Data$SignCheck2)
  Model.Data$SignCheck3=gsub("_Ygr","",Model.Data$SignCheck3)
  
  Model.Data$SignCheck1=gsub("_Ydf","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("_Ydf","",Model.Data$SignCheck2)
  Model.Data$SignCheck3=gsub("_Ydf","",Model.Data$SignCheck3)
  
  ####Second, remove the lag name
  Model.Data$SignCheck1=gsub("lag0_","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("lag0_","",Model.Data$SignCheck2)
  Model.Data$SignCheck3=gsub("lag0_","",Model.Data$SignCheck3)
  Model.Data$SignCheck1=gsub("lag1_","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("lag1_","",Model.Data$SignCheck2)
  Model.Data$SignCheck3=gsub("lag1_","",Model.Data$SignCheck3)
  
  CorrectSign = cbind("Unemp.Rt -",
                      "RGDP.Qgr +",
                      "CPI +",
                      "Trsy3M.Rt +",
                      "DJIA +",
                      "BBB.Spd -",
                      "CRE +",
                      "VIX -")
  WrongSign = cbind("Unemp.Rt +",
                    "RGDP.Qgr -",
                    "CPI -",
                    "Trsy3M.Rt -",
                    "DJIA -",
                    "BBB.Spd +",
                    "CRE -",
                    "VIX +")
  
  filter.Data = Model.Data %>% 
    filter(!(SignCheck1 %in% WrongSign)) %>% 
    filter(!(SignCheck2 %in% WrongSign)) %>% 
    filter((!(SignCheck3 %in% WrongSign))) %>% 
    select(-26,-27,-28)
  
  return(filter.Data)
}

Sign.Check_2v = function(Model.Data)
{
  Model.Data = as.data.frame(Model.Data)
  
  Model.Data$SignCheck1=paste(Model.Data$var1_name,Model.Data$var1_sign)
  Model.Data$SignCheck2=paste(Model.Data$var2_name,Model.Data$var2_sign)
  
  Model.Data$SignCheck1=gsub("_Ygr","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("_Ygr","",Model.Data$SignCheck2)
  
  Model.Data$SignCheck1=gsub("_Ydf","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("_Ydf","",Model.Data$SignCheck2)
  
  ####Second, remove the lag name
  Model.Data$SignCheck1=gsub("lag0_","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("lag0_","",Model.Data$SignCheck2)
  Model.Data$SignCheck1=gsub("lag1_","",Model.Data$SignCheck1)
  Model.Data$SignCheck2=gsub("lag1_","",Model.Data$SignCheck2)
  
  CorrectSign = cbind("Unemp.Rt -",
                      "RGDP.Qgr +",
                      "CPI +",
                      "Trsy3M.Rt +",
                      "DJIA +",
                      "BBB.Spd -",
                      "CRE +",
                      "VIX -")
  WrongSign = cbind("Unemp.Rt +",
                    "RGDP.Qgr -",
                    "CPI -",
                    "Trsy3M.Rt -",
                    "DJIA -",
                    "BBB.Spd +",
                    "CRE -",
                    "VIX +")
  
  filter.Data = Model.Data %>% 
    filter(!(SignCheck1 %in% WrongSign)) %>% 
    filter(!(SignCheck2 %in% WrongSign)) %>% 
    select(-21,-22)
  return(filter.Data)
}

#====================================================================================#
# Default Rate Back Test / Scenario  ================================================#
#====================================================================================#

df.backtest = function(balcnt, zlist, rho, cml)
{
  starting = matrix(balcnt, byrow = FALSE, nrow = 7)
  
  df_rt = c()
  
  for (i in 1:(length(zlist)))
  {
    t = delta.transition(rho, zlist[i], cml)
    ending = starting[,i] %*% t
    df_rt[i] = dplyr::last(ending)/sum(ending)
  }
  
  return(df_rt)
}

df.scenario = function(prerate, zlist, rho, cml)
{
  starting = prerate
  
  df_rt = c()
  
  for(i in 1:(length(zlist)))
  {
    t = delta.transition(rho, zlist[i], cml)
    ending = starting %*% t
    df_rt[i] = dplyr::last(ending)/sum(ending)
    starting = prop.table(ending[1:(length(ending) - 1)])
  }
  
  return(df_rt)
}

#====================================================================================#
# App  ==============================================================================#
#====================================================================================#

pred = function(actual, scenario, model)
{
  variable = model %>% select(contains("name")) %>% apply(2, FUN = as.character)
  coef = model %>% select(contains("coef")) %>% apply(2, FUN = as.numeric)
  
  Pred    = as.matrix(cbind(1, scenario$x[,variable])) %*% coef
  Base    = as.matrix(cbind(1, scenario$b[,variable])) %*% coef
  Adverse = as.matrix(cbind(1, scenario$a[,variable])) %*% coef
  Severe  = as.matrix(cbind(1, scenario$s[,variable])) %*% coef
  
  y = data.frame(Year = c(rep(1998:2016, 2), rep(2016:2019, 3)),
                 scenario = c(rep("Actual", 19), rep("Predict", 19), rep("Base", 4), rep("Adverse", 4), rep("Severe", 4)),
                 value = c(actual, Pred, Base, Adverse, Severe))
  
  y$scenario = ordered(as.factor(y$scenario), levels = c("Actual", "Predict", "Severe", "Adverse", "Base"))
  return(y)
}

# 03 mg
one_plot = function(data)
{
  # c("Aaa_Aa", "Aa_A", "A_Baa", "Baa_Ba", "Ba_B", "B_Caa.C", "Caa.C_DF")
  # c("Aaa_Aaa", "Aa_Aa", "A_A", "Baa_Baa", "Ba_Ba", "B_B", "Caa.C_Caa.C")
  
  p = plot_ly(data) %>%
    add_lines(x=~Year, 
              y=~round(value*100, 2), 
              color = I("#16a6b6"),
              line = list(shape = "spline"), 
              hoverinfo = "y+text+name",
              name=~gsub("_", "->", unique(variable)),
              text=~paste(Year)) %>%
    add_text(x = 2004, y = 108, text=~gsub("_", "->", unique(variable)), hoverinfo = "none", textfont = list(color = '#000000', size = 9)) %>%
    layout(
      xaxis = list(title = "", dtick = 4, tickfont = list(size = 9), tickangle = 30),
      yaxis = list(title = "", tickfont = list(size = 9), range = c(-5,120), dtick = 50, ticksuffix = "%", showgrid = FALSE),
      margin = list(r = 20, t = 20))
  
  return(p)
}

# 10 pred_z
plot_z = function(final, title)
{
  Pred = final %>% filter(scenario != "Actual") %>% as.data.frame()
  Actual = final %>% filter(scenario == "Actual") %>% as.data.frame()
  ticksuffix = ""
  
  if(title == "Default Rate") 
  {
    ticksuffix = "%"
    Pred$value = Pred$value * 100
    Actual$value = Actual$value * 100
  }
  
  max = max(Pred$value, Actual$value); min = min(Pred$value, Actual$value)
  
  p = plot_ly() %>% add_lines(data = Pred, 
                              x=~Year, 
                              y=~round(value, 4), 
                              line = list(shape = "spline"), 
                              hoverinfo = "y+text+name",
                              name=~scenario,
                              text=~Year,
                              color=~scenario, 
                              colors = c("black", "black", "red", "orange", "darkgreen")) %>%
    add_trace(data = Actual, 
              x=~Year, 
              y=~round(value, 4), 
              mode = "markers",
              hoverinfo = "y+text+name",
              name="Actual",
              text=~Year,
              marker = list(color = "#3c8dbc")) %>%
    layout(
      legend = list(orientation = 'h'),
      xaxis = list(title = "", dtick = 4, showgrid = FALSE),
      yaxis = list(title = title, showgrid = FALSE, ticksuffix = ticksuffix),
      shapes = list(type = "line", x0 = "2016", x1 = "2016", y0 = min, y1 = max, line = list(color = "gray", dash = "dash", width = 0.5)),
      hovermode = "x") %>% config(displayModeBar = FALSE)
  
  return(p)
}

fore_z = function(final, caption)
{
  data = final %>% 
    filter(Year >= 2017) %>% 
    dcast(Year~scenario, value.var = "value")
  
  data = data[,c("Year", "Base", "Adverse", "Severe")]
  
  data = datatable(data, 
                   caption = caption, 
                   rownames = FALSE, 
                   selection = list(mode = "single", target = "cell"),
                   options = list(searching = FALSE,
                                  paging = FALSE,
                                  ordering = FALSE,
                                  dom = 't'))
  if(caption == "Default Rate Prediction") data = data %>% formatPercentage(2:4, digits = 2)
  if(caption == "Transition Matrix - Z Score") data = data %>% formatRound(2:4, digits = 4)
  
  return(data)
}

v_ = function(model, n_v)
{
  if(n_v == 2) 
  {
    v1 = gsub("_",".",as.character(model[,1]))
    v2 = gsub("_",".",as.character(model[,2]))
    int = round(as.numeric(as.character(model[,3])),2)
    coe1 = round(as.numeric(as.character(model[,4])),2)
    coe2 = round(as.numeric(as.character(model[,5])),2)
    equation = paste("$$", int, "+", coe1, "\\times", v1, "+", coe2, "\\times", v2, "$$")
    adjr = paste("$$ Adjusted R^2:", round(as.numeric(as.character(model[,12])),2), "$$")
  }
  
  if(n_v == 3) 
  {
    v1 = gsub("_",".",as.character(model[,1]))
    v2 = gsub("_",".",as.character(model[,2]))
    v3 = gsub("_",".",as.character(model[,3]))
    int = round(as.numeric(as.character(model[,4])),2)
    coe1 = round(as.numeric(as.character(model[,5])),2)
    coe2 = round(as.numeric(as.character(model[,6])),2)
    coe3 = round(as.numeric(as.character(model[,7])),2)
    equation = paste("$$", int, "+", coe1, "\\times", v1, "+", coe2, "\\times", v2, "+", coe3, "\\times", v3, "$$")
    adjr = paste("$$ Adjusted R^2:", round(as.numeric(as.character(model[,16])),2), "$$")
  }
  
  return(list("equation" = equation, "adjr" = adjr))
}

mig.mtx = function(z_final, selected, model)
{
  row = selected[1] %>% as.numeric()
  col = selected[2] %>% as.numeric()
  
  year = 2016 + row
  sce = c("Base", "Adverse", "Severe")[col]
  z = z_final %>% filter(Year == year & scenario == sce & Model == model) %>% select(value) %>% as.numeric()
  rho = zdata$rho
  cml = data.prep(loan)$cml_cnt
  
  data = delta.transition(rho, z, cml)
  data = datatable(data, 
                   caption = "Transition Matrix (%)", 
                   options = list(searching = FALSE,
                                  paging = FALSE,
                                  ordering = FALSE,
                                  dom = 't')) %>% 
    formatStyle(0, fontWeight = "bold") %>%
    formatPercentage(colnames(data), digits = 2)
  return(data)
}