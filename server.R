library(shiny);library(shinydashboard);library(dplyr);library(plotly);
library(shinyWidgets);library(scales);library(tidyr);library(zoo);
library(reshape2);library(ggplot2);library(DT);library(rsconnect);
library(lubridate);library(gtable);library(viridis);library(grid);
library(rsconnect);library(googleVis)

function(input, output, session) {
  
  load("data/00 loan.RData")
  load("data/00 macro.RData")
  load("data/01 zdata.RData")
  load("data/02 df_back.RData")
  load("data/03 macro_desc.RData")
  load("data/04 final_model_loose.RData")
  load("data/04 final_model_tight.RData")
  load("data/05 z_final.RData")
  load("data/06 test_desc.RData")
  load("data/07 sign_desc.RData")
  load("data/08 df_final.RData")
  
  source("functions.R", local = TRUE)
  
  source("01 cnt.R", local = TRUE)
  source("02 df.R", local = TRUE)
  source("03 mg.R", local = TRUE)
  source("04 runoff.R", local = TRUE)
  source("05 zscore.R", local = TRUE)
  source("06 df_back.R", local = TRUE)
  source("07 z_macro.R", local = TRUE)
  source("08 v_select.R", local = TRUE)
  source("09 m_select.R", local = TRUE)
  source("10 pred_z.R", local = TRUE)
}