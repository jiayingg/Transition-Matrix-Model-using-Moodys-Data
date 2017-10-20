body <- dashboardBody(
  
  includeCSS("style.css"),
  
  tabItems(
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "cnt",
      
      h4("Issuer Counts by Risk Rating"),
      
      p("Data transformation: Letter designations (such as AAA, B, CC) represent 
        the quality of an issuer. Moody assigns credit ratings as Aaa, Aa, A, Baa, 
        Ba, B, Caa, Ca, C, with WR as withdrawn. Here letter rating Cx's are 
        combined as Caa.C because segmented rating data before 2006 is not available. 
        Also, withdrawn issuers are excluded and the percentile is recalculated."),
      
      strong("Stacked Area Chart of Cumulative Counts/Percentage"),
      
      h5(" - Issuer counts BY risk rating in the beginning of the year"),
      
      fluidRow(column(width = 6, plotlyOutput("start1")),
               column(width = 6, plotlyOutput("start2"))),
      
      h5(" - Issuer counts BY risk rating at the end of the year"),
      
      fluidRow(column(width = 6, plotlyOutput("end1")),
               column(width = 6, plotlyOutput("end2"))),
      
      helpText("* Data source: ", 
               tags$a(href = "https://www.moodys.com/researchdocumentcontentpage.aspx?docid=PBC_1059749", 
                      "Moody's Annual Default Study"),
               "includes data of default, loss and rating transition experience of corporate bond, loan 
               and deposit issuers. This study covers financial institutions, corporates and regulated 
               utilities that have long-term debt ratings."),
      helpText("* Instruction: Hover on graphs to see detail. First row stands for cumulative numbers; 
               second stands for numbers of each rating.")
      ),
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "df",
      
      fluidRow(
        column(7,
               h4("Default Rate by Risk Rating"),
               plotlyOutput("df_rt", height = 600), br(), 
               p("The over-all default rate of Moody's-rated corporate issuers started to go up in 2016, 
                 sending the default tally to the highest level since 2014. By risk rating most of the 
                 default came from Caa.C and it was still going up in 2016. This fact can also be shown 
                 in the next tab: Credit Risk Migration.")),
        
        column(5, 
               dataTableOutput("df_tb"))
               )
        ),
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "mg",
      
      h4("Credit Risk Migration"), 
      
      p("This plot shows how rating migrated during 1998 - 2016. "),
      p("Here focusing more on the downgrade. Most issuers downgraded one risk rating. 
        Issuers rated in the lower rating categories have higher chances to be in default. 
        Most default came from rating Caa-C. Surprisingly in 2009 and 2011, a larger 
        portion of safer issuers(A's and B's) downgraded."),
      
      plotlyOutput("migrate", height = 660)
      ),
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "runoff",
      
      fluidRow(
        column(7,
               h4("Run Off and New Issuers"),
               
               fluidRow(align = "center",
                        awesomeRadio("run_rating",
                                     inline = TRUE,
                                     selected = "Aaa",
                                     label = NULL,
                                     choices = c("Aaa", "Aa", "A", "Baa", "Ba", "B", "Caa.C"),
                                     status = "primary")),
               
               plotlyOutput("run_off", height = 550),
               
               p("Notice the difference between number of issuers at the end of last year and 
                 in the beginning of this year. Negative difference means portfolio runoff and 
                 positive means new issuers enter the market."),
               # p(" Portfolio run-off is mainly caused by:"),
               # p("1. Loans are paid off at scheduled maturity dates."),
               # p("2. Borrowers prepay their loans."),
               # p("3. Borrowers declare charge-off."),
               p("From the plot we can see an increasing number of riskier issuers entered the 
                 market in recent years.")),
        
        column(5,
               dataTableOutput("run_tb"))
               )
      ),
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "zscore",
      
      h4("Z score"),
      
      fluidRow(
        column(9, 
               
               plotlyOutput("zplot"),
               helpText("* The z-score is able to capture the early 2000s recession. The recession 
                        affected the European Union during 2000 and 2001 and the United States in 
                        2002 and 2003. It also captures the United States housing bubble in 2006 
                        and the global financial crisis afterwards, which is considered by many 
                        economists to have been the worst financial crisis since the Great Depression 
                        of the 1930s."),
               strong("Methodology:"),
               p("The z-score model is a one - parameter representation of credit risk and transition 
                 matrices. We assume that ratings transition matrices result from the 'binning' of a 
                 standard normal random variable X that measures changes in creditworthiness. We 
                 further assume that X splits into two parts: (1) a (scaled) idiosyncratic component 
                 Y, unique to a borrower, and (2) a (scaled) systematic component Z, shared by all 
                 borrowers. We assume that Y and Z are unit normal random variables and mutually 
                 independent. The parameter rho(assumed positive) drives the correlation between Z 
                 and X; Z explains a fraction rho of the variance of X."),
               p("Broadly speaking, z-score measures the 'credit cycle', meaning the values of default 
                 rates and of endofperiod risk ratings not predicted (using historical average transition 
                 rates) by the initial mix of credit grades. In good years z-score will be positive, 
                 implying for each initial credit rating, a lower than average default rate and a higher 
                 than average ratio of upgrades to downgrades. In bad years, the reverse will be true."),
               p("Here I optimized rho and further optimized a series of z-score from the separate 
                 transition matrices tabulated each year by Moody's.")
               ),
        
        column(3, 
               
               fluidRow(valueBoxOutput("rho")),
               
               fluidRow(box(dataTableOutput("ztable"), 
                            title = "Z Score Table",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 12, height = 560)))
               )
      ),
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "df_back",
      
      h4("Default Rate Backtesting"),
      
      fluidRow(
        column(9, 
               
               plotlyOutput("backtest", height = 550),
               
               p("Conversely, given z-score of a certain year, transition matrix is easy to 
                 calculate. Here the backtesting default rate is derived from transition matrices. 
                 It is very close to actual value, meaning that the z-score model is able to 
                 capture the features of default events. In fact, my z-score optimization algorithm 
                 is designed this way -- optimize default rating as much as possible rather than 
                 the whole portfolio. For this purpose two weights are added into the optimization 
                 formula:"),
               p("1. Put more weights on higher default rate"),
               p("2. Put more weights on larger counts")),
        
        column(3, 
               
               fluidRow(valueBoxOutput("rho2")),
               
               fluidRow(box(dataTableOutput("dfback_tb"), 
                            title = "Z Score Transformed Default Rate",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 12, height = 560)))
        )
      ),
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "z_macro",
      
      h4("Z score vs. Macroeconomics Variable"),
      
      fluidRow(
        column(9, 
               
               plotlyOutput("zmacro", height = 550),
               helpText("* Data Source: Federal Reserve"),
               helpText("* Instrcution: Select a macroeconomic variable to see the trends. This 
                        facilitates variable selection process."), 
               p("The market often moves ahead of an upgrade or downgrade in anticipation of such 
                 an event. Combinations of a set of macroeconomic variables(sourced from the 
                 Federal Reserve) are used to predict dependent variable z-score. "),
               p("Some macro variables have the same trends as economic condition and some have 
                 the opposite. For example GDP growth indicates stronger economy, decreasing 
                 unemployment rate indicates stronger economy, rising volatility signals 
                 instability in equities markets, and so on. Models with unintuitive coefficient 
                 signs need to be filter out.")),
        
        column(3, 
               align = "center",
               
               selectInput(inputId = "macro", 
                           label = NULL,
                           selected = NULL,
                           choices = c("Unemployment Rate" = "lag0_Unemp.Rt",
                                       "BBB Corporate Rate" = "lag0_BBB.Rt",
                                       "Mortgage Rate" = "lag0_Mort.Rt",
                                       "Prime Rate" = "lag0_Prime.Rt",
                                       "Dow Jones Stock Market Index" = "lag0_DJIA",
                                       "Market Volatility Index (VIX)" = "lag0_VIX",
                                       "Real GDP Growth Rate" = "lag0_RGDP.Ygr",
                                       "Nominal GDP Growth Rate" = "lag0_NGDP.Ygr",
                                       "Nominal Disposable Income Growth Rate" = "lag0_NDI.Ygr",
                                       "Real Disposable Income Growth Rate" = "lag0_RDI.Ygr",
                                       "Consumer Price Index Rate" = "lag0_CPI.Ygr",
                                       "BBB Spread" = "lag0_BBB.Spd"
                           )),
               
               box(dataTableOutput("zmacrotable"), 
                   title = "Z Score vs Macro",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12, height = 560))
               )
               ),
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "v_select",
      
      h4("Variable Selection"),
      
      fluidRow(column(5,
                      p("Below is a list of variables that are used in the model. The variables selected are 
                        capable of indicating if economy is strong or weak. Allowing lags and different 
                        transformations, the final variables are highly correlated with predictors. Variable 
                        transformations are generally limited to year on year changes and growths in line 
                        with industry convention."),
                      column(width = 12, offset = 1,
                             p("1. Yearly difference (Ydf) for rate variables"),
                             p("2. Yearly growth rate (Ygr) for index variables"),
                             p("3. Lag: up to only one year lag for all raw/transformed variables")),
                      p("As part of the quantatitive aspect of determining macroeconomic variable pools, 
                        correlation analysis was performed to identify the level of sensitivity between
                        z score and macroeconomic variables. On the right is a correlation plot of all 
                        the variables. First row is a correlation plot of dependent variable z score with all 
                        independent variables. Although, there are variables that are highly correlation 
                        to z score, however, not all highly correlated variables are intuitive when it 
                        comes to interpreting how it impacts z score. With that said, no pre-selection 
                        is performed due to the limited number of variables/data points, meaning that 
                        all 2/3-variable-combinations will be taken into consideration."), br(),
                      htmlOutput("macro_desc")),
               
               column(7,
                      # dataTableOutput("cor_tb"),
                      plotlyOutput("cor_z", height = 65),
                      plotlyOutput("cor_plot", height = 720))
                      )
      ),
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "m_select",
      
      h4("Model Selection Criteria"),
      
      fluidRow(
        column(5,
               strong("1. P-value of variable coefficients"),
               p("The p-value tests the null hypothesis that the coefficient is equal to zero (no effect). 
                 A low p-value (< 0.1) indicates that we can reject the null hypothesis. In other words, a 
                 predictor that has a low p-value is likely to be a meaningful addition to the model because 
                 changes in the predictor's value are related to changes in the response variable."), br(),
               
               strong("2. Additional statistical testing"),
               p("Comprehensive testing of the standard linear regression assumptions was conducted to ensure 
                 validity and correct specification of the regression models. Each model was tested 
                 across six statistical criteria, namely goodness-of-fit, residual stationarity, collinearity, 
                 residual autocorrelation, residual normality, residual homoscedasticity."), br(),
               
               strong("3. Expected signs for variable coefficients"),
               p("Best subset selection approach was used for model selection process. All 3-variable-combinations 
                 and 2-variable-combinations has been tried. The variables and their respective signs need to be 
                 business intuitive and coefficients need to be reasonably large to produce sensitivity."), br(),
               
               strong("4. Ranking by adjusted R2"),
               p("Adjusted R2 were used as the initial ranking metrics. 
                 This metric was chosen because it represents the goodness of fit for the model against the 
                 historical data."), br(),
               
               p("Two sets of filtering criteria are used here: a loose one and a strict one. Only 4 models 
                 can pass all the statistical test (strict criteria). Due to the limited data points, we don't 
                 want to simply reject a model even if it cannot pass the tests. In stead we keep all the models
                 that p-value larger than 10% (loose criteria). ")
               ),
        
        column(7,
               helpText("Additional Statistical Testing", align = "center"),
               htmlOutput("test_desc"), br(),
               helpText("Expected Signs for Primary Variable Coefficients", align = "center"),
               htmlOutput("sign_desc"))
               )
               ),
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "pred1",
      
      h4("2-Factor-Model (Loose)"), 
      
      tabBox(width = "100%",
             
             tabPanel("Z Score",
                      fluidRow(column(6, 
                                      plotlyOutput("tmm1_plot", height = 480),
                                      uiOutput("equa1"), uiOutput("adjr1")), 
                               column(6, 
                                      dataTableOutput("tmm1_fore"), br(),
                                      helpText("* Click on the Z Score to display a corresponding Transition Matrix."), 
                                      dataTableOutput("tmm1_migrate")))),
             
             tabPanel("Default Rate",
                      fluidRow(column(6, 
                                      plotlyOutput("df1_plot", height = 480)), 
                               column(6, 
                                      dataTableOutput("df1_fore"))))
      )
      
    ),
    
    tabItem(
      tabName = "pred2",
      
      h4("2-Factor-Model (Loose)"), 
      
      tabBox(width = "100%",
             
             tabPanel("Z Score",
                      fluidRow(column(6, 
                                      plotlyOutput("tmm2_plot", height = 480),
                                      uiOutput("equa2"), uiOutput("adjr2")), 
                               column(6, 
                                      dataTableOutput("tmm2_fore"), br(),
                                      helpText("* Click on the Z Score to display a corresponding Transition Matrix."), 
                                      dataTableOutput("tmm2_migrate")))),
             
             tabPanel("Default Rate",
                      fluidRow(column(6, 
                                      plotlyOutput("df2_plot", height = 480)), 
                               column(6, 
                                      dataTableOutput("df2_fore"))))
      )
      
    ),
    
    tabItem(
      tabName = "pred3",
      
      h4("2-Factor-Model (Loose)"), 
      
      tabBox(width = "100%",
             
             tabPanel("Z Score",
                      fluidRow(column(6, 
                                      plotlyOutput("tmm3_plot", height = 480),
                                      uiOutput("equa3"), uiOutput("adjr3")), 
                               column(6, 
                                      dataTableOutput("tmm3_fore"), br(),
                                      helpText("* Click on the Z Score to display a corresponding Transition Matrix."), 
                                      dataTableOutput("tmm3_migrate")))),
             
             tabPanel("Default Rate",
                      fluidRow(column(6, 
                                      plotlyOutput("df3_plot", height = 480)), 
                               column(6, 
                                      dataTableOutput("df3_fore"))))
      )
      
    ),
    
    tabItem(
      tabName = "pred4",
      
      h4("2-Factor-Model (Loose)"), 
      
      tabBox(width = "100%",
             
             tabPanel("Z Score",
                      fluidRow(column(6, 
                                      plotlyOutput("tmm4_plot", height = 480),
                                      uiOutput("equa4"), uiOutput("adjr4")), 
                               column(6, 
                                      dataTableOutput("tmm4_fore"), br(),
                                      helpText("* Click on the Z Score to display a corresponding Transition Matrix."), 
                                      dataTableOutput("tmm4_migrate")))),
             
             tabPanel("Default Rate",
                      fluidRow(column(6, 
                                      plotlyOutput("df4_plot", height = 480)), 
                               column(6, 
                                      dataTableOutput("df4_fore"))))
      )
      
    ),
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "limit",
      
      h4("Model Limitation and Future Improvement [Draft]"), br(),
      
      column(6, offset = 3,
             
             p("'Model Implementation' tab shows the top 1 model from each criteria. Compare to historical data
               not all models perform reasonable prediction. In fact, all candidate models should be reviewed by 
               business experts. Top 1 model is not always intuitive from a business perspective. There are
               spaces for improvements for sure."), 
             
             strong("Data quality"),
             p("The model assumes that the initial data population is adequate for modeling purposes and that 
               the data elements have similar granularity. Data currently using is annual data from Moody's. 
               Compare to industry standard quarterly data, the data doesn't allow z score to capture 
               details on a finer granularity level and z score generation algorithm takes much longer time
               to find an optimize solution."), br(),
             
             strong("Measurement to rank top models"),
             p("The measure that currently used is adjusted R2. As the model is used for stress testing, we
               normally focus more on the default events and downgrade performance of severe adverse scenario.
               An alternative measurement is to compare the severe scenario to historical severe cases, in
               other words, the big crisis. The more it replicate the crisis period, the better the model."), br(),
             
             strong("Exit and new origination assumption"),
             p("The model assumes that number of issuers stay the same in the future 3 years. But as is shown in
               the 'Run Off and New Issuers' tab, more and more issuers especially lower rating issuers enter 
               the market in recent years, number of issuers should not be stay the same. As the proportion of 
               lower rating issuers gets higher, the propertion of downgrades should be higher correspondingly,
               default rate over all should also be higher."), br(),
             
             strong("ALLL process"),
             p("More ALLL process for loan data. Not applicable here.")
             )
             ),
    
    # ------------------------------------------------------------------------------------------------------------
    
    tabItem(
      tabName = "contact",
      
      column(12, 
             align = "center",
             
             img(src = "https://avatars2.githubusercontent.com/u/19355184?v=4&u=a4e092e0ceb613a2c4a111f57d8b620b17fcc602&s=400", class = "profile_pic"),
             h1("~ Contact Me ~", class = "contact_header"),
             p("Get in touch with me to get the ball rolling", class = "contact_text")),
      
      column(10, 
             offset = 1,
             
             column(3, 
                    align = "center",
                    
                    div(class = "circle", icon("phone-square", class = "fa-4x")), br(),
                    tags$a(class = "contact_how", "Phone"), br(),
                    tags$a(class = "contact_detail", "1-213-448-7579")), 
             
             column(3, 
                    align = "center",
                    
                    div(class = "circle", icon("envelope-square", class = "fa-4x")), br(),
                    tags$a(class = "contact_how", "Email"), br(),
                    tags$a(class = "contact_detail", "JiayingGu.Career@Gmail.com")),
             
             column(3, 
                    align = "center",
                    
                    div(class = "circle", icon("linkedin-square", class = "fa-4x")), br(),
                    tags$a(class="contact_how", href = "https://www.linkedin.com/in/JiayingGu/", "LinkedIn"), br(),
                    tags$a(class="contact_detail", href = "https://www.linkedin.com/in/JiayingGu/", "linkedin.com/in/JiayingGu")),
             
             column(3, 
                    align = "center",
                    
                    div(class = "circle",  icon("github-square", class = "fa-4x")), br(),
                    tags$a(class = "contact_how", href = "https://github.com/jiayingg?tab=repositories", "GitHub"), br(),
                    tags$a(class = "contact_detail", href = "https://github.com/jiayingg?tab=repositories", "github.com/JiayingG")
             )
      )
    )
             )
    )