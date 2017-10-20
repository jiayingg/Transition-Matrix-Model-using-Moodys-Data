sidebar <- dashboardSidebar(
  
  width = 240,
  
  sidebarMenu(
    
    id = "tabs",
    
    menuItem("Exploratory Analysis", 
             icon = icon("dashboard"), 
             tabName = "explore", 
             startExpanded = TRUE,
             
             menuSubItem("Issuer Counts by Risk Rating", tabName = "cnt", selected = TRUE),
             menuSubItem("Default Rate by Risk Rating", tabName = "df"),
             menuSubItem("Credit Risk Migration", tabName = "mg"),
             menuSubItem("Run Off and New Issuers", tabName = "runoff")
    ),
    
    menuItem("Z Score Generation",
             icon = icon("calculator"),
             tabName = "zscore",
             startExpanded = TRUE,
             
             menuSubItem("Z Score", tabName = "zscore"),
             menuSubItem("Default Rate Backtesting", tabName = "df_back"),
             menuSubItem("Z Score vs. Macro", tabName = "z_macro")
    ),
    
    menuItem("Model Development",
             icon = icon("spinner"),
             tabName = "model", 
             startExpanded = TRUE,
             
             menuSubItem("Variable Selection", tabName = "v_select"),
             menuSubItem("Model Selection", tabName = "m_select")
    ),
    
    menuItem("Model Implementation",
             icon = icon("bar-chart"),
             tabName = "imple",
             startExpanded = TRUE,
             
             menuSubItem("2-Factor-Model (Loose)", tabName = "pred1"),
             menuSubItem("3-Factor-Model (Loose)", tabName = "pred2"),
             menuSubItem("2-Factor-Model (Strict)", tabName = "pred3"),
             menuSubItem("3-Factor-Model (Strict)", tabName = "pred4"),
             menuSubItem("Limitation and Improvement", tabName = "limit")),
    
    menuItem("Contact Author",
             icon = icon("user-circle"),
             tabName = "contact")
  )
)