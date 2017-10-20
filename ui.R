library(shiny);library(shinydashboard);library(dplyr);library(plotly);
library(shinyWidgets);library(scales);library(tidyr);library(zoo);
library(reshape2);library(ggplot2);library(DT);library(rsconnect);
library(lubridate);library(gtable);library(viridis);library(grid);
library(rsconnect);library(googleVis)

# load("data/00 loan.rdata")
source("ui_sidebar.R", local = TRUE)
source("ui_body.R", local = TRUE)

dashboardPage(
  dashboardHeader(title = "Credit Transition Matrix", 
                  titleWidth = 240,
                  tags$li(a(href = 'https://github.com/jiayingg', 
                            icon("power-off"), 
                            title = "Back to GitHub"),
                          class = "dropdown")),
  sidebar,
  body
)