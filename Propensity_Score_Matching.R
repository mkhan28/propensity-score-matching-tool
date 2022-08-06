# Copyright (C) 2021 Md Ayubur Rahman Khan, The University of Toledo - All Rights Reserved
# 
# This program is free software: you can redistribute it and/or modify it under the terms of 
# the GNU General Public License as published by the Free Software Foundation, either version 3 of 
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without 
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with this program. 
# If not, see <https://www.gnu.org/licenses/>


library(shiny)
#library(shinyjs)

# Define UI for data upload app ----
ui <- fluidPage(
    title = "Propensity Score Matching Tool",
    tabsetPanel(
        #useShinyjs(),
        
        ##################################################################################################################################################
        ################################################ Tab 1 : Input-1 #################################################################################
        ##################################################################################################################################################
        
        tabPanel("Upload Data", fluid = TRUE,
                 
                 
                 # App title ----
                 titlePanel(h3("Upload Student Data Set")),
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(width = 3,
                         
                         # Input: Select a file ----
                         fileInput("file1", "Choose CSV File",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         
                         # Horizontal line ----
                         tags$hr(),
                         
                         # Input: Checkbox if file has header ----
                         # checkboxInput("header1", "Header", TRUE),
                         
                         
                         # Horizontal line ----
                         tags$hr(),
                         
                         # Input: Select number of rows to display ----
                         radioButtons("disp1", "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head")
                         
                     ),
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9,
                         
                         # Output: Data file ----
                         tags$b(textOutput("out.1.1")),
                         tags$i(textOutput("out.1.2")),
                         tags$i(textOutput("out.1.3")),
                         tags$div(style="margin-bottom:50px"),  #----- adding margin between buttons --------#
                         tableOutput("out.1")
                     )
                 )
        ),
        
        
        
        ##################################################################################################################################################
        ################################################ Tab 2 - Missing Data ##############################################################################
        ##################################################################################################################################################
        
        tabPanel("Missing Data", fluid = TRUE,
                 
                 
                 # App title ----
                 titlePanel(h3("Missing Data Handling")),
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(width = 3, 
                         tags$b("Step-01:"),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         actionButton("BT1", "Check Missing Data"),
                         tags$div(style="margin-bottom:80px"),  #----- adding margin between buttons --------#
                         tags$hr(), # ----- Horizontal line ----#
                         
                         # condition for displaying the imputation panel
                         conditionalPanel(
                             condition = "output.id != 'Number of numeric variable having missing observation: 0' & input.BT1 == '1'",
                             
                         
                         tags$b("Step-02:"),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         selectInput("choose_action",
                                     label = "Select an option for handling missing data",
                                     multiple = FALSE,
                                     choices = c("",
                                                 "Apply mean imputation",
                                                 "Apply stochastic regression imputation")
                                     ),
                         
                         
                         conditionalPanel(
                             condition = "input.choose_action == 'Remove observations from data set'",
                             actionButton("BT1.1", "Remove Missing Observations")
                         ),
                         
                         conditionalPanel(
                             condition = "input.choose_action == 'Apply mean imputation'",
                             selectInput("input_var_miss_mean",
                                         label = "Select variable to impute",
                                         multiple = FALSE,
                                         choices = c("")
                             ),
                             actionButton("BT1.2", "Perform Mean Imputation")
                         ),
                         
                         conditionalPanel(
                             condition = "input.choose_action == 'Apply stochastic regression imputation'",
                             selectInput("input_var_miss",
                                         label = "Select variable to impute",
                                         multiple = FALSE,
                                         choices = c("")
                                         ),
                             selectInput("input_var_corr",
                                         label = "Select variable to correlate",
                                         multiple = FALSE,
                                         choices = c("")
                             ),
                             actionButton("BT2", "Check Correlation"),
                             tags$div(style="margin-bottom:10px"),  #----- adding margin between buttons --------#
                             tags$hr(), #------- Horizontal line --------#
                             
                             actionButton("BT3", "Perform Regression Imputation"),
                             tags$div(style="margin-bottom:80px"),  #----- adding margin between buttons --------#
                         )
                         )
                         
                     ),
                     
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9,
                         
                         # Output: Data file ----
                         fluidRow(align = "left",
                                  column(4, 
                                         tags$h4(tags$b(textOutput("out.3.1"))),
                                         tags$div(style="margin-bottom:25px"),  #----- adding margin between buttons --------#
                                         tableOutput("out.3"),
                                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                         #tags$i("Number of numeric variable having missing observation:"),
                                         tags$h4(textOutput("id")),
                                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                         tags$h4(textOutput("comment.2.1")),
                                         
                                         ),
                                         
                                  column(4, 
                                         tags$h4(tags$b(textOutput("out.4.1"))),
                                         plotOutput("out.4"),
                                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                         tags$h4(textOutput("comment.2.2")),
                                         tags$div(style="margin-bottom:15px")  #----- adding margin between buttons --------#
                                         ),
                                  column(4, 
                                         tags$h4(tags$b(textOutput("out.5.1"))),
                                         tags$div(style="margin-bottom:25px"),  #----- adding margin between buttons --------#
                                         tableOutput("out.5"),
                                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                         tags$h4(textOutput("comment.2.3.1")),
                                         tags$h4(textOutput("comment.2.3.2")),
                                         tags$div(style="margin-bottom:15px")  #----- adding margin between buttons --------#
                                         )
                         )
                         
                         
                     )
                 ),
                 
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     
                     sidebarPanel(width = 3,
                                  # condition for displaying the imputation panel
                                  conditionalPanel(
                                      condition = "output.id != 'Number of numeric variable having missing observation: 0' & input.BT1 == '1'",
                                      tags$b("Step-03:"),
                                      tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                      actionButton("BT4", "Check Validation"),
                                      tags$div(style="margin-bottom:700px")  #----- adding margin between buttons --------#
                                  )
                     ),
                                  
                    
                     
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9,
                         
                         tags$h4(tags$b(textOutput("out.6.1"))),
                         # Output: Data file ----
                         fluidRow(column(6, plotOutput("out.6")),
                                  column(6, plotOutput("out.8"))
                         ),
                         
                         fluidRow(tags$h4(textOutput("comment.2.4"))
                         ),
                         
                         fluidRow(column(6, plotOutput("out.7")),
                                  column(6, plotOutput("out.9"))
                         ),
                         
                         fluidRow(tags$h4(textOutput("comment.2.5"))
                         )
                         
                         
                     )
                 )
                 
        ),
        
        ##################################################################################################################################################
        ################################################ Tab 3 - Matching Criteria Selection #############################################################
        ##################################################################################################################################################
        
        tabPanel("Matching Criteria", fluid = TRUE,
                 
                 
                 # App title ----
                 titlePanel(h3("Matching Criteria Selection")),
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(width = 3,
                                  tags$b("Step-01:"),
                                  tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                  actionButton("BT5", "Run Variable Selection Process"),
                                  tags$i("(Execution may take some time..)"),
                                  tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                  tags$i("Note: It is an essential step to select variables that are significant in terms of treatment assignment. For this purpose, 
                                         a stepwise variable selection method (forward selection and backward elimination stepwise method) has been applied to see
                                         which variables are significantly contributing to the treatment assignment."),
                                  tags$div(style="margin-bottom:25px"),  #----- adding margin between buttons --------#
                                  tags$b("Optional Step:"),
                                  tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                  actionButton("BT5.1", "See Detail Statistical Process"),
                                  tags$i("(Execution may take some time..)"),
                                  tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                  tags$i("Note: This is an optional step to run. If you are interested to see how variable selection method worked for the data set, you 
                                         can run this process and it will show the steps ."),
                                  tags$div(style="margin-bottom:200px")  #----- adding margin between buttons --------#
                         
                     ),
                     
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9,
                         
                         # Output: Data file ----
                         tags$h4(tags$b(textOutput("out.10.0"))),
                         #tags$h4(tags$b("Statistically significant variables for treatment assignment:")),
                         verbatimTextOutput("out.10"),
                         tags$div(style="margin-bottom:25px"),  #----- adding margin between buttons --------#
                         verbatimTextOutput("out.10.1")
                         
                     )
                 ),
                 
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(width = 3,
                                  
                                  tags$b("Step-02:"),
                                  tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                  selectInput("SL1",
                                              label = "Select Variables for Matching",
                                              multiple = TRUE,
                                              choices = c("")
                                  ),
                                  tags$i("Note: There are no clear suggestions whether one should include all the variables in the final model (even non-significant). However, it is often
                                         suggested that the final model should include not only statistically significant variables (from the stepwise variable selection method), but also
                                         variables known to be associated with selection of the treatment group and outcome of the treatment."),
                                  
                                  tags$div(style="margin-bottom:100px"),  #----- adding margin between buttons --------#
                                  
                                  
                                  tags$b("Step-03:"),
                                  tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                  actionButton("BT6", "Validate Model"),
                                  tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                  tags$i("Note: There are several methods of estimating propensity scores, but, logistic regression is widely used. It is a good practice to run a 
                                  logistic regression based on the selected variables in order to validate the model fit. Then, a chi-square test based on residual deviance and the 
                                  degrees of freedom can show the model fitness. A high probability (P-value greater than 0.05) from the chi-square test 
                                  indicates that we have not missed any big source of data variation in our final model and the model fits the data sufficiently. Once validated, we 
                                  can proceed to the next step of propensity score matching, including these selected variables as matching criteria."),
                                  
                                  tags$div(style="margin-bottom:25px"),  #----- adding margin between buttons --------#
                                  tags$b("Optional Step:"),
                                  tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                                  actionButton("BT6.1", "See Statistical Summary of Logistic Regression"),
                                  tags$div(style="margin-bottom:100px")  #----- adding margin between buttons --------#
                                  
                                  
                     ),
                     
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9,
                               
                               # Output: Data file ----
                               tags$h4(tags$b(textOutput("out.11"))),
                               verbatimTextOutput("out.12"),
                               
                               tags$h4(tags$b(textOutput("out.14"))),
                               verbatimTextOutput("out.15"),
                               
                               tags$h4(tags$b(textOutput("out.13.1"))),
                               verbatimTextOutput("out.13")
                               
                               
                               
                     )
                 )
                 
        ),
        
        
        ##################################################################################################################################################
        ################################################ Tab 4 - Explore Imbalance ########################################################################
        ##################################################################################################################################################
        
        tabPanel("Explore Imbalance", fluid = TRUE,


                 # App title ----
                 titlePanel(h3("Explore Imbalance between Treatment and Control Groups")),

                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(width = 3,
                         
                         actionButton("BT8", "Exploration-1"),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         tags$i("Note: Before running the matching process using the estimated propensity scores,it is a good practice to determine if the two groups 
                                (treatment and control) are balanced. There are statistical tests that can be used to determine if the variables are balanced across groups. 
                                Here, a chi-square test is run to check if there is at least one variable in the selection model for which the two groups are different.
                                A statistically significant chi-square value (p-value << 0.05) indicates that at least one of the variables included in the model is creating
                                an imbalance between the two groups."),
                         tags$div(style="margin-bottom:15px"),  #----- adding margin between buttons --------#
                         tags$i("(Balance between two groups are checked for each individual variable. For a numerical variable (e.g., High School GPA),
                         two groups are balanced when they have similar distributions. For categorical variable (e.g., Ethnicity), two
                         groups are balanced when they have similar ratios for respective categories.)"),
                         
                         
                     ),
                     
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9,
                         
                         # Output: Data file ----
                         tags$h4(tags$b(textOutput("out.18"))),
                         verbatimTextOutput("out.19")
                     )
                 ),
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(width = 3,
                     
                         actionButton("BT9", "Exploration-2"),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         tags$i("Note: A graphical approach can also be used to assess the distribution similarity (balance) between treatment and control groups.
                                To determine the level of balance between two groups, we can look at the degree of overlap (also known as the common support region) between the two density
                                distributions. Balance is the best when there is a maximum common support region. Maximum common support region will be availed once the two distributions 
                                are so overlapped that we can not visually separate two colors."),
                     ),
                     
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9,
                         
                         # Output: Data file ----
                         tags$h4(tags$b(textOutput("out.20.1"))),
                         fluidRow(column(7, plotOutput("out.20")),
                                  column(5, )
                         )
                         
                     )
                 )

        ),
        
        ##################################################################################################################################################
        ################################################ Tab 5 - Run Matching ############################################################################
        ##################################################################################################################################################
        
        tabPanel("Run Matching", fluid = TRUE,
                 
                 
                 # App title ----
                 titlePanel(h3("Run Propensity Score Matching Process")),
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(width = 3,
                         
                         actionButton("BT10", "Run Matching"),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         tags$i("Note: In this step, matching algorithm is run using 'MatchIt' package in R and it provides summary tables that include means and standard deviations for 
                         the two groups both before and after the matching was completed. It also includes percent improvement. Finally, it provides a summary of the number of matched
                                individuals included in the final sample, and cases that were not matched."),
                         
                         
                     ),
                     
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9,
                         
                         # Output: Data file ----
                         #textOutput("out.21"),
                         tags$h4(tags$b(textOutput("out.22"))),
                         verbatimTextOutput("out.23")
                         
                         
                         
                         
                     )
                 )
                 
        ),
        
        ##################################################################################################################################################
        ################################################ Tab 6 - Validation ##############################################################################
        ##################################################################################################################################################
        
        tabPanel("Validation", fluid = TRUE,
                 
                 
                 # App title ----
                 titlePanel(h3("Validating Propensity Score Matching")),
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(width = 3,
                         
                         actionButton("BT12", "Validation-1"),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         tags$i("Note: It is now important to check if the groups are balanced, thus eliminating the initial selection bias. In the 'Explore Imbalance' tab, both 
                         statistical and graphical approaches were shown to determine the degree of imbalance. After the match has been conducted, both techniques are used again to 
                                determine that all the critical variables are balanced."),
                         
                         tags$div(style="margin-bottom:15px"),  #----- adding margin between buttons --------#
                         tags$i("Note: This time, a chi-square test is run again to check if there is at least one variable in the selection model for which the treatment and matched 
                         control groups are different. A chi-square test indicating no significance (p-value greater than 0.05) suggests equivalence between the treatment and matched 
                                control groups.")
                         
                         
                     ),
                     
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9,
                         
                         tags$h4(tags$b(textOutput("out.27"))),   #---- BT12 -----#
                         verbatimTextOutput("out.28")            #---- BT12 -----#
                         
                     )
                 ),
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(width = 3,
                         
                         actionButton("BT13", "Validation-2"),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         tags$i("Note: A graphical validation approach using jitter plot and density plot will help validate the matching process."),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         tags$i("Jitter plot will help get some idea about matched cases if they come from similar specific propensity-score region."),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         tags$i("Density plot will now tell us about the balance between treatment and matched pair group. Maximum level of balance will be achieved if 
                         distributions of these two groups are quite overlapped.")
                         
                     ),
                     
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9,
                         
                         # Output: Data file ----
                         #textOutput("out.24"),
                         
                         tags$h4(tags$b(textOutput("out.29"))),   #---- BT13 -----#
                         
                         fluidRow(column(5, plotOutput("out.30")),
                                  column(7, plotOutput("out.31"))
                         ),
                          
                         
                     )
                 ),
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(width = 3,
                         actionButton("BT15", "Validation-3"),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         tags$i("Note: Finally, a summary statistics for both the treatment group and the matched pair group will be displayed. It is important to observe how 
                         matching performed by looking at the balance of two groups for each individual variable."),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         tags$i("For a numeric/integer variable (e.g., ACT Score, High School GPA, Age etc.), two groups are balanced when they have similar 
                                distributions (e.g., mean, median, range etc.)."),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         tags$i("For a categorical variable (e.g., Gender, College, Ethnicity etc.), two groups are balanced when they have similar ratios for respective categories. If a categorial 
                         variable has several categories, it is important to balance those which have higher proportions in the treatment group"),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         tags$i("(As the number of variables increases, it becomes difficult to find good matches for subjects in the treatment group. If sufficient balance
                                can not be achieved with the selected set of variables, it is suggested that users try with a smaller set of variables that are more significantly 
                                associated with the likelihood of being in the treatment group)"),
                         tags$div(style="margin-bottom:25px")  #----- adding margin between buttons --------#
                     ),
                     
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9,
                         
                         # Output: Data file ----
                         #textOutput("out.24"),
                         
                         tags$h4(tags$b(textOutput("out.32"))),   #---- BT15 -----#
                         tags$h5(tags$b(textOutput("out.33.1"))),   #---- BT15 -----#
                         verbatimTextOutput("out.33"),            #---- BT15 -----#
                         tags$h5(tags$b(textOutput("out.34.1"))),   #---- BT15 -----#
                         verbatimTextOutput("out.34")             #---- BT15 -----#
                         
                     )
                 ),
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(width = 3,
                         tags$i("Click the following link to download the IDs of the matched pair group in a CSV file."),
                         tags$div(style="margin-bottom:5px"),  #----- adding margin between buttons --------#
                         downloadLink("downloadData", "Download matched students' ID")
                     ),
                     
                     
                     
                     # Main panel for displaying outputs ----
                     mainPanel(width = 9
                         
                         
                         
                     )
                 )
                 
        )
        
        
        
        )
    )


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------    SERVER    ----------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#



# Define server logic to read selected file ----
server <- function(input, output, session) {
    
    observe({
        
        ##################################################################################################################################################
        ################################################ Tab 1 : Input-1 #################################################################################
        ##################################################################################################################################################
        
        output$out.1 <- renderTable({
            
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, head of that data file by default,
            # or all rows if selected, will be shown.
            
            req(input$file1)
            
            # when reading semicolon separated files,
            # having a comma separator causes `read.csv` to error
            tryCatch(
                {
                    # df1 <- read.csv(input$file1$datapath, header = input$header1)
                    df1 <- read.csv(input$file1$datapath)
                },
                error = function(e) {
                    # return a safeError if a parsing error occurs
                    stop(safeError(e))
                }
            )
            
            if(input$disp1 == "head") {
                return(head(df1))
            }
            else {
                return(df1)
            }
            
        })
        
        getData1 <- reactive({
            
            inFile <- input$file1
            
            if (is.null(input$file1))
                return(NULL)
            
            # read.csv(inFile$datapath, header=input$header1, stringsAsFactors = T)
            read.csv(inFile$datapath, stringsAsFactors = T)
            
        })
        
        output$out.1.1 <- renderText({
            if (is.null(input$file1))
                return(NULL)
            paste("Number of Students: ", nrow(getData1()))
        })
        
        output$out.1.2 <- renderText({
            if (is.null(input$file1))
                return(NULL)
            x = getData1()
            paste("Program Participants/Treatment Recipients: ", nrow(x[x$TREATMENT == "Y",]))
        })
        
        output$out.1.3 <- renderText({
            if (is.null(input$file1))
                return(NULL)
            x = getData1()
            paste("Control Head Count: ", nrow(x[x$TREATMENT == "N",]))
        })
        
    
        ##################################################################################################################################################
        ################################################ Tab 2 - Imputation of Missing Data ##############################################################
        ##################################################################################################################################################
        
        # uploaded data set
        dat = reactive({
            dat = getData1()
            dat
        })
        
        # treatment data set
        dat.treat <- reactive({
            dat = dat()    #------ Total Population -------#
            dat[dat$TREATMENT == "Y", ]
            
        })
        
        # table with list of variables, their data type and number of missing observations
        dat.BT1 <- eventReactive(input$BT1,{
            if(is.null(input$BT1)){
                return()
            }
            
            dat.treat = dat.treat()[, -c(1,2)]
            
            #number of cases in treatment group with missing ACT score
            dat.BT1 = as.data.frame(cbind(Variable = names(dat.treat), Type = sapply(dat.treat, class), Count = apply(dat.treat, 2, function(x){sum(is.na(x))})))
            dat.BT1$Count = as.numeric(dat.BT1$Count)
            dat.BT1
        })
        
        # numeric variable(s) having missing observation
        var_miss <- reactive({
            dat.BT1 = dat.BT1()
            var_miss = dat.BT1$Variable[dat.BT1$Type %in% c("numeric", "integer") & dat.BT1$Count != 0]
            var_miss
        })
        
        # condition for hiding imputation panel
        output$id<-renderText({
            
            dat = getData1()
            dat.treat = dat[dat$TREATMENT == "Y", ]
            dat.treat = dat.treat[, -c(1,2)]
            
            #number of cases in treatment group with missing ACT score
            dat.BT1 = as.data.frame(cbind(Variable = names(dat.treat), Type = sapply(dat.treat, class), Count = apply(dat.treat, 2, function(x){sum(is.na(x))})))
            dat.BT1$Count = as.numeric(dat.BT1$Count)
            var_miss = dat.BT1$Variable[dat.BT1$Type %in% c("numeric", "integer") & dat.BT1$Count != 0]
            
            if (input$BT1 == 0)
                return(NULL)
            
            paste("Number of numeric variable having missing observation: ", length(var_miss), sep = "")
        })
        
        # other numeric variable(s) having no missing observation
        var_corr <- reactive({
            dat.BT1 = dat.BT1()
            var_corr = dat.BT1$Variable[dat.BT1$Type %in% c("numeric", "integer") & dat.BT1$Count == 0] 
            var_corr
        })
        
        # update input variables to select for stochastic imputation and correlation
        observeEvent(input$choose_action,{
            if(input$choose_action == "Apply stochastic regression imputation"){
                updateSelectInput(session, "input_var_miss", label = NULL, choices = c("", var_miss()), selected = NULL)
                updateSelectInput(session, "input_var_corr", label = NULL, choices = c("", var_corr()), selected = NULL)
            } else if(input$choose_action == "Apply mean imputation"){
                updateSelectInput(session, "input_var_miss_mean", label = NULL, choices = c("", var_miss()), selected = NULL)
            }
        })
        
        
        output$out.3.1 <- renderText({
            if (input$BT1 == 0)
                return(NULL)
            paste("Variables with missing value in treatment group: ")
        })
        
        output$out.3 <- renderTable({
            
            dat.BT1()
    
        })
        
        #-----------comment on missing value ----------------#
        output$comment.2.1 <- renderText({
            if (input$BT1 == 0)
                return(NULL)
            paste("Note: This tool can handle missing values in a single numeric/integer variable only. 
                  If the uploaded data set has multiple variables with missing observations or has missing value in a categorical variable, users are required to treat them before uploading 
                  (e.g., impute missing values, remove observations). If there is no missing value in the treatment group, please move to 'Matching Criteria' tab.")
        })
        
        
        
        output$out.4.1 <- renderText({
            if (input$BT2 == 0)
                return(NULL)
            
            if(input$choose_action == "Apply stochastic regression imputation"){
                paste("Correlation between numerical variables: ")
            } else if(input$choose_action == "Apply mean imputation"){
                
            }
        })
        
        
        # function for plotting correlation coefficient and scatter plot
        panel.cor <- reactive({
            
            
            function(x, y, digits=2, prefix="", cex.cor){
                usr <- par("usr"); on.exit(par(usr)) 
                par(usr = c(0, 1, 0, 1)) 
                r <- abs(cor(x, y)) 
                txt <- format(c(r, 0.123456789), digits=digits)[1] 
                txt <- paste(prefix, txt, sep="") 
                if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
                
                test <- cor.test(x,y) 
                # borrowed from printCoefmat
                Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                                 cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                 symbols = c("***", "**", "*", ".", " ")) 
                
                text(0.5, 0.5, txt, cex = cex * r) 
                text(.8, .8, Signif, cex=cex, col=2) 
            }
        
        
        })
        
        
        # data set having selected numeric variable to impute and selected numeric variable to predict for regression imputation
        dat.BT2 <- eventReactive(input$BT2,{
            var_miss = var_miss()
            if(length(var_miss) != 0){
                #correlation between selected numerical variables
                dat.treat = dat.treat()
                dat.treat[complete.cases(dat.treat), c(input$input_var_miss, input$input_var_corr)]
            } else {
                head(dat())
            }
        })
        
        
        output$out.4 <- renderPlot({
            var_miss = var_miss()
            if(length(var_miss) != 0){
                if(input$choose_action == "Apply stochastic regression imputation"){
                    pairs(dat.BT2(), lower.panel=panel.smooth, upper.panel=panel.cor())
                } else if(input$choose_action == "Apply mean imputation"){
                    
                }
            } else {
                NULL
            }
            
            
        })
        
        #-----------comment on correlation ----------------#
        output$comment.2.2 <- renderText({
            if (input$choose_action != "Apply stochastic regression imputation"){
                return(NULL)
            } else if (input$BT2 == 0){
                return(NULL)
            } else {
                
                paste("Note: There is no fixed rule for determining what size of correlation is considered strong, moderate or weak. 
                      Generally, correlation coefficients whose magnitude are above 0.5 are considered relatively strong, therefore, are suitable for applying regression imputation.")
            }
            
        })
        
        # 
        dat.imp = reactive({
            
            dat = dat()
            dat.treat = dat.treat()
            var_miss = var_miss()
            
            if(length(var_miss) != 0){
                if(input$choose_action == "Apply stochastic regression imputation"){
                    # perform stochastic imputation
                    m = input$input_var_miss           #------ index of covariate with missing value --------#
                    n = c(m, input$input_var_corr)   #------ indices of numerical covariates --------#
                    library(mice)
                    imp = mice(dat.treat[, n], method = "norm.nob", seed = 1, m = 1, print = FALSE)
                    dat.imp = complete(imp)
                    
                } else if(input$choose_action == "Apply mean imputation"){
                    # perform mean imputation
                    m = input$input_var_miss_mean           #------ index of covariate with missing value --------#
                    n = c(1:nrow(dat.treat))   #------ creating a dummy column as mice function needs matrix of dataframe as argument --------#
                    x = data.frame(dat.treat[, m], n)
                    colnames(x) = c(m, "SL")
                    library(mice)
                    imp = mice(x, method = "mean", print = FALSE)
                    # imp = dat.treat[, m]
                    # imp[is.na(imp)] = mean(imp, na.rm = T)
                    dat.imp = complete(imp)
                }
                
                
                
            } else {
                dat.imp = 1
            }
            
            
        })
        
        
        txt.BT3 <- eventReactive(input$BT3,{
            paste("Change in mean, variability & correlation due to imputation: ")
            
        })
        
        txt.BT1.2 <- eventReactive(input$BT1.2,{
            paste("Change in mean, variability due to imputation: ")
        })
        
        
        
        output$out.5.1 <- renderText({
            
            if(input$choose_action == "Apply stochastic regression imputation"){
                txt.BT3()
            } else if(input$choose_action == "Apply mean imputation"){
                txt.BT1.2()
            }
        })
        
        dat.BT3 <- eventReactive(input$BT3,{
            var_miss = var_miss()
            if(length(var_miss) != 0){
            
                dat.treat = dat.treat()
                dat.imp = dat.imp()
                
                # m = which(apply(dat.treat, 2, function(x){sum(is.na(x))}) != 0)           #------ index of covariate with missing value --------#
                # n = which(unlist(lapply(dat.treat, is.numeric))[1:ncol(dat.treat)-1])    #------ indices of numerical covariates --------#
                m = input$input_var_miss           #------ index of covariate with missing value --------#
                n = c(m, input$input_var_corr)   #------ indices of numerical covariates --------#
                
                #check mean, sd and correlation with GPA before and after the imputation
                mean = round(mean(dat.treat[, m], na.rm = T),2)
                sd = round(sd(dat.treat[, m], na.rm = T),2)
                cor.coef = round(cor(dat.treat[, n], use = "pair")[1,2],2)
                
                mean.imp = round(mean(dat.imp[, m], na.rm = T),2)
                sd.imp = round(sd(dat.imp[, m], na.rm = T),2)
                cor.coef.imp = round(cor(dat.imp, use = "pair")[1,2],2)
                
                data.frame(Metric = c("Mean", "Standard Dev.", "Correlation"), Before = rbind(mean, sd, cor.coef), After = rbind(mean.imp, sd.imp, cor.coef.imp))
            } else {
                data.frame(Metric = c("Mean", "Standard Dev."), Before = rbind(1, 2), After = rbind(1,2))
            }
            
        })
        
        dat.BT1.2 <- eventReactive(input$BT1.2,{
            var_miss = var_miss()
            if(length(var_miss) != 0){
                
                dat.treat = dat.treat()
                dat.imp = dat.imp()
                
                m = input$input_var_miss_mean           #------ index of covariate with missing value --------#
                # n = c(m, input$input_var_corr)   #------ indices of numerical covariates --------#
                
                #check mean, sd and correlation with GPA before and after the imputation
                mean = round(mean(dat.treat[, m], na.rm = T),2)
                sd = round(sd(dat.treat[, m], na.rm = T),2)
                # cor.coef = round(cor(dat.treat[, n], use = "pair")[1,2],2)
                
                mean.imp = round(mean(dat.imp[, m], na.rm = T),2)
                sd.imp = round(sd(dat.imp[, m], na.rm = T),2)
                # cor.coef.imp = round(cor(dat.imp, use = "pair")[1,2],2)
                
                data.frame(Metric = c("Mean", "Standard Dev."), Before = rbind(mean, sd), After = rbind(mean.imp, sd.imp))
                
            } else {
                data.frame(Metric = c("Mean", "Standard Dev."), Before = rbind(1, 2), After = rbind(1,2))
            }
        
            
        })
        
        
        output$out.5 <- renderTable({
            var_miss = var_miss()
            if(length(var_miss) != 0){
                    
                if(input$choose_action == "Apply stochastic regression imputation"){
                    dat.BT3()
                } else if(input$choose_action == "Apply mean imputation"){
                    dat.BT1.2()
                }
            } else {
                NULL
            }
            
        })
        
        #-----------comment on regression imputation effect ----------------#
        output$comment.2.3.1 <- renderText({
            if (input$choose_action != "Apply stochastic regression imputation"){
                return(NULL)
            } else if (input$BT3 == 0){
                return(NULL)
            } else {
                
                paste("Note: Stochastic regression imputation preserves both the average/mean and the variability of the observations. 
                This imputation is recommended when there is another variable in 
                  the data set that can sufficiently predict the missing values.")
            }
            
        })
        
        #-----------comment on mean imputation effect ----------------#
        output$comment.2.3.2 <- renderText({
            if (input$choose_action != "Apply mean imputation"){
                return(NULL)
            } else if (input$BT1.2 == 0){
                return(NULL)
            } else {
                
                paste("Note: Mean imputation preserves the average/mean, however, may reduce the variability of the observations. 
                  Mean imputation can be used when regression imputation is not justified (e.g., absence of other variable in 
                  the data set that can sufficiently predict the missing values)")
            }
            
        })
        
        #------------- revised data set with imputed values ---------------------#
        
        dat2 = reactive({    
            var_miss = var_miss()
            if(length(var_miss) != 0){
                
                dat = dat()
                dat.treat = dat.treat()
                dat.imp = dat.imp()
                dat.BT1 = dat.BT1()
                
                if(input$choose_action == "Apply stochastic regression imputation"){
                    m = input$input_var_miss           #------ index of covariate with missing value --------#
                } else if(input$choose_action == "Apply mean imputation"){
                    m = input$input_var_miss_mean           #------ index of covariate with missing value --------#
                }
                
                
                #replace missing ACT scores by imputed ones
                dat[is.na(dat[, m]) & dat$TREATMENT == "Y",][, m] = dat.imp[is.na(dat.treat[, m]), m]     ###previous chunks have to be run
                #remove incomplete records (having missing values) from control groups
                dat2 = dat[complete.cases(dat[, m]),]
                dat2$Treatment = 0
                dat2$Treatment[dat2$TREATMENT == "Y"] = 1
                dat2
                
                
            } else {
                
                dat2 = dat()
                dat2 = dat2[complete.cases(dat2),]
                dat2$Treatment = 0
                dat2$Treatment[dat2$TREATMENT == "Y"] = 1
                dat2
            }
            
            
            
            
            
            
        })
        
        #-----------------  Visual Validation --------------------------#
        
        output$out.6.1 <- renderText({
            if (input$BT4 == 0)
                return(NULL)
            paste("Visual inspection of imputed data set:")
        })
        
        plot1.BT4 <- eventReactive(input$BT4,{
            dat.treat = dat.treat()
            
            if(input$choose_action == "Apply stochastic regression imputation"){
                m = input$input_var_miss           #------ index of covariate with missing value --------#
            } else if(input$choose_action == "Apply mean imputation"){
                m = input$input_var_miss_mean           #------ index of covariate with missing value --------#
            }
            
            #x = names(which(apply(dat.treat, 2, function(x){sum(is.na(x))}) != 0))
            hist(dat.treat[,c(m)], breaks = 10, xlab = m, main = "Histogram before imputation", col = "light blue")
        })
        
        output$out.6 <- renderPlot({
            
            plot1.BT4()
            
        })
        
        
        plot2.BT4 <- eventReactive(input$BT4,{
            dat.treat = dat.treat()
            m = input$input_var_miss           #------ index of covariate with missing value --------#
            n = input$input_var_corr    #------ indices of numerical covariates --------#
            # y = names(which(apply(dat.treat, 2, function(x){sum(is.na(x))}) != 0))
            # x = names(n[!(n %in% m)])
            plot(dat.treat[, m], dat.treat[, n], col = "blue", xlab = m, ylab = n, pch = 19, cex = 1.5, main = "Scatter plot before imputation")
        })
        
        output$out.7 <- renderPlot({
            if(input$choose_action == "Apply stochastic regression imputation"){
                plot2.BT4()
            } 
            
            
        })
        
        
        plot3.BT4 <- eventReactive(input$BT4,{
            dat.treat = dat.treat()
            dat.imp = dat.imp()
            if(input$choose_action == "Apply stochastic regression imputation"){
                m = input$input_var_miss           #------ index of covariate with missing value --------#
            } else if(input$choose_action == "Apply mean imputation"){
                m = input$input_var_miss_mean           #------ index of covariate with missing value --------#
            }
            hist(dat.imp[, m], breaks = 10, xlab = m, main = "Histogram after imputation", col = "light blue")
        })
        
        output$out.8 <- renderPlot({
            
            plot3.BT4()
            
        })
        
        
        plot4.BT4 <- eventReactive(input$BT4,{
            dat.treat = dat.treat()
            dat.imp = dat.imp()
            m = input$input_var_miss           #------ index of covariate with missing value --------#
            n = input$input_var_corr    #------ indices of numerical covariates --------#
            # x = names(which(apply(dat.treat, 2, function(x){sum(is.na(x))}) != 0))
            # y = names(n[!(n %in% m)])
            col = rep("blue",nrow(dat.treat))
            col[is.na(dat.treat[, c(m)])] = "red"
            plot(dat.imp[, m], dat.imp[, n], col = col, xlab = m, ylab = n, pch = 19, cex = 1.5, main = "Scatter plot after imputation")
        })
        
        output$out.9 <- renderPlot({
            if(input$choose_action == "Apply stochastic regression imputation"){
                plot4.BT4()
            } 
            
            
        })
        
        
        
        #-----------comment on imputation effect - Histogram----------------#
        output$comment.2.4 <- renderText({
            if ((input$choose_action %in% c("Apply stochastic regression imputation", "Apply mean imputation")) & (input$BT4 != 0)){
                paste("Note: A good way to visually validate how imputation performs is to look at the histogram of the numeric variable before and after the imputation.
                      If the shape of histogram does not distort significantly, it is sufficient to say that the imputation worked well. ")
                
            } else {
                return(NULL)
            
            }
            
            
        })
        
       
        
        #-----------comment on regression imputation effect - Scatter plot ----------------#
        output$comment.2.5 <- renderText({
            if (input$choose_action != "Apply stochastic regression imputation"){
                return(NULL)
            } else if (input$BT4 == 0){
                return(NULL)
            } else {
                
                paste("Note: Another way to validate the stochastic regression imputation is to look at the scatter plot between imputed variable and the predictor variable. 
                      If there is a reasonably strong correlation between these two variables, imputed observations should be randomly scattered around the linear regression line.")
            }
            
        })
        
        
        ##################################################################################################################################################
        ################################################ Tab 3 - Matching Criteria Selection #############################################################
        ##################################################################################################################################################
        
        output$out.10.0 <- renderText({
            if (input$BT5 == 0){
                paste("")
            } else {
                paste("Statistically significant variables for treatment assignment: ")
            }
                
            
        })
        
        
        
        dat.BT5.1 <- eventReactive(input$BT5.1,{
            
            dat = dat2()      #-------- using revised (imputed) data set -------------#
            dat = dat[,-c(1,2)] #-------- Exclude id column ------------#
            
            #build null model with no covariate
            model.null = glm(Treatment ~ 1, data = dat, family = binomial())
            
            #build full model with all probable covariates
            model.full = glm(Treatment ~ ., data = dat, family = binomial())
            
            #run step wise model selection method
            step(model.null, scope = list(upper = model.full), direction = "both", test = "Chisq", data = dat)
            
        })
        
        dat.BT5 <- eventReactive(input$BT5,{
            dat = dat2()      #-------- using revised (imputed) data set -------------#
            dat = dat[,-c(1,2)] #-------- Exclude id column ------------#
            
            #build null model with no covariate
            model.null = glm(Treatment ~ 1, data = dat, family = binomial())
            
            #build full model with all probable covariates
            model.full = glm(Treatment ~ ., data = dat, family = binomial())
            
            #run step wise model selection method
            st = step(model.null, scope = list(upper = model.full), direction = "both", test = "Chisq", data = dat, trace = 0)
            names(st$model)[-c(1)]
        })
        
        output$out.10 <- renderPrint({
            
            dat.BT5()
            
        })
        
        output$out.10.1 <- renderPrint({
            
            dat.BT5.1()
            
        })
        
        
        observeEvent(input$BT1,{
            updateSelectInput(session, "SL1", label = NULL, choices = colnames(dat())[-c(1,2)], selected = NULL)
            
        })
        
        
        output$out.11 <- renderText({
            
            "Selected Variables:"
        })
        
        
        dat.SL1 <- reactive({
            
            input$SL1
            
        })
        
        output$out.12 <- renderPrint({
            if (length(input$SL1) == 0){
                paste("No variable selected")
            } else {
                dat.SL1()
            }
                
            
            
        })
        
        
        output$out.13.1 <- renderText({
            if (input$BT6.1 == 0){
                
            } else {
                paste("Summary of logistic regression model based on selected variables: ")
            }
                
            
        })
        
        dat.ps <- eventReactive(input$BT6,{
            
            dat = dat2()      #-------- using revised (imputed) data set -------------#
            
            f = as.formula(paste("Treatment ~ ", paste(input$SL1, collapse = " + " )))
            glm(f, data = dat, family = binomial())
            
        })
        
        
        dat.ps.1 <- eventReactive(input$BT6.1,{
            
            dat = dat2()      #-------- using revised (imputed) data set -------------#
            
            f = as.formula(paste("Treatment ~ ", paste(input$SL1, collapse = " + " )))
            glm(f, data = dat, family = binomial())
            
        })
        
        output$out.13 <- renderPrint({
            
            summary(dat.ps.1())
            
        })
        
        output$out.14 <- renderText({
            if (input$BT6 == 0)
                return(NULL)
            "Result of chi-square test:"
        })
        
        output$out.15 <- renderPrint({
            
            ps = dat.ps()
            paste("P-value: ", 1 - pchisq(ps$deviance, ps$df.residual))
            
        })
        
        
        ##################################################################################################################################################
        ################################################ Tab 4 - Data Exploration ########################################################################
        ##################################################################################################################################################
        
        output$out.16 <- renderText({
            if (input$BT7 == 0)
                return(NULL)
            "Standardized/ normalized difference (numerical covariates only):"
        })
        
        dat.BT7 <- eventReactive(input$BT7,{
            
            dat = dat2()      #-------- using revised (imputed) data set -------------#
            dat = dat[,-c(1,2)] #-------- Exclude id column ------------#
            dat = dat[,c(input$SL1, "Treatment")]
            
            n = which(unlist(lapply(dat, is.numeric))[1:ncol(dat)-1])    #------ indices of numerical covariates --------#
            
            #standardize difference
            ind = rep(0,length(dat$Treatment))
            ind[dat$Treatment == 1] = TRUE
            
            cov = dat[, c(n)]
            apply(cov, 2, function(x){100*(mean(x[ind]) - mean(x[!ind]))/(sqrt(0.5*(var(x[ind]) + var(x[!ind]))))})
            
        })
        
        output$out.17 <- renderPrint({
            
            abs(dat.BT7())
            
        })
        
        
        output$out.18 <- renderText({
            if (input$BT8 == 0)
                return(NULL)
            "Chi-square test to check imbalance between two groups:"
        })
        
        dat.BT8 <- eventReactive(input$BT8,{
            
            dat = dat2()      #-------- using revised (imputed) data set -------------#
            dat = dat[,-c(1,2)] #-------- Exclude id column ------------#
            dat = dat[,c(input$SL1, "Treatment")]
            
            #chi-square test
            library(RItools)
            
            xBalance(Treatment ~ ., data = dat, report = c("chisquare.test"))
            
        })
        
        output$out.19 <- renderPrint({
            
            dat.BT8()
            
        })
        
        
        output$out.20.1 <- renderText({
            if (input$BT9 == 0)
                return(NULL)
            "Distribution of propensity score before matching:"
        })
        
        plot.BT9 <- eventReactive(input$BT9,{
            
            dat = dat2()      #-------- using revised (imputed) data set -------------#
            dat = dat[,-c(1,2)] #-------- Exclude id column ------------#
            dat = dat[,c(input$SL1, "Treatment")]
            ps = dat.ps()
            
            #propensity scores predicted from the logistic regression
            dat$psvalue = predict(ps, type = "response")
            
            #plot distribution of estimated propensity scores for treatment and control groups
            dat$Group = "Treatment"
            dat$Group[dat$Treatment == 0] = "Control"
            
            library(ggplot2)
            ggplot(dat, aes(x = psvalue, fill = Group)) +
                geom_density(alpha = 0.5) +
                theme_grey() +
                labs(y = "Frequency",
                     x = "Propensity score",
                     fill = "Group")
            
            
        })
        
        output$out.20 <- renderPlot({
            
            plot.BT9()
            
        })
        
        ##################################################################################################################################################
        ################################################ Tab 5 - Run Matching ############################################################################
        ##################################################################################################################################################
        
        # output$out.21 <- renderText({"In this step, matching algorithm is run using `MatchIt` package and it provides summary tables 
        #     that include means and standard deviations for the two groups both before and after the matching was completed. It also 
        #     includes percent improvement. Finally, it provides a summary of the number of individuals included in the final sample, 
        #     and cases that were not matched."})
        # 
        txt.BT10 <- eventReactive(input$BT10,{
            
            c("Summary of Matching Process:")
            
        })
        
        output$out.22 <- renderText({
            txt.BT10()
        })
        
        
        mat <- reactive({
            
            dat = dat2()      #-------- using revised (imputed) data set -------------#
            # dat = dat[,-c(1)] #-------- Exclude id column ------------#
            # dat = dat[,c(input$SL1, "Treatment")]
            ps = dat.ps()
            
            #propensity scores predicted from the logistic regression
            dat$psvalue = predict(ps, type = "response")
            
            #run matching algorithm
            library(MatchIt)
            f = as.formula(paste("Treatment ~ ", paste(input$SL1, collapse = " + " )))
            matchit(f, data = dat, method = "nearest", ratio = 1)
            
        })
        
        
        dat.BT10 <- eventReactive(input$BT10,{
            
            summary(mat())
            
        })
        
        output$out.23 <- renderPrint({
            
            dat.BT10()
            
        })
        
        ##################################################################################################################################################
        ################################################ Tab 6 - Validation & Download ##############################################################################
        ##################################################################################################################################################
        
        output$out.24 <- renderText({"Finally, propensity score matching process is validated using both statistical test and data visualization"})
        
        txt.BT11 <- eventReactive(input$BT11,{
            
            c("Standardized/ normalized difference (numerical covariates only):")
            
        })
        
        output$out.25 <- renderText({
            txt.BT11()
        })
        
        
        dat.BT11 <- eventReactive(input$BT11,{
            
            library(MatchIt)
            dat.mat = match.data(mat())
            dat.treat = dat.treat()
            n = names(which(unlist(lapply(dat.treat[,-c(1)], is.numeric))[1:ncol(dat.treat)-1]))    #------ indices of numerical covariates --------#
            
            
            #standardize difference
            ind = rep(0,length(dat.mat$Treatment))
            ind[dat.mat$Treatment == 1] = TRUE
            
            cov = dat.mat[, n]
            std.diff = apply(cov, 2, function(x){100*(mean(x[ind]) - mean(x[!ind]))/(sqrt(0.5*(var(x[ind]) + var(x[!ind]))))})
            
            abs(std.diff)
            
        })
        
        output$out.26 <- renderPrint({
            
            dat.BT11()
            
        })
        
        
        txt.BT12 <- eventReactive(input$BT12,{
            
            c("Chi-square test to check imbalance between two groups:")
            
        })
        
        output$out.27 <- renderText({
            txt.BT12()
        })
        
        
        dat.BT12 <- eventReactive(input$BT12,{
            
            library(MatchIt)
            dat = match.data(mat())     
            dat = dat[,c(input$SL1, "Treatment")]
            
            #chi-square test
            library(RItools)
            
            xBalance(Treatment ~ ., data = dat, report = c("chisquare.test"))
            
            
        })
        
        output$out.28 <- renderPrint({
            
            dat.BT12()
            
        })
        
        
        
        txt.BT13 <- eventReactive(input$BT13,{
            
            c("Distribution of propensity scores after matching:")
            
        })
        
        output$out.29 <- renderText({
            txt.BT13()
        })
        
        
        
        plot.BT13 <- eventReactive(input$BT13,{
            
            #jitter plot
            plot(mat(), type = "jitter", col = "blue")
            
        })
        
        output$out.30 <- renderPlot({
            
            plot.BT13()
            
        })
        
        
        plot2.BT13 <- eventReactive(input$BT13,{
            
            library(MatchIt)
            dat.mat = match.data(mat()) 
            
            #plot distribution of estimated propensity scores for treatment and control groups
            dat.mat$Group = "Treatment"
            dat.mat$Group[dat.mat$Treatment == 0] = "Matched Control Pair"
            
            library(ggplot2)
            ggplot(dat.mat, aes(x = psvalue, fill = Group)) +
                geom_density(alpha = 0.5) +
                theme_grey() +
                labs(y = "Frequency",
                     x = "Propensity score",
                     fill = "Group")
            
            
            
        })
        
        output$out.31 <- renderPlot({
            
            plot2.BT13()
            
        })
        
        
        
        txt.BT15 <- eventReactive(input$BT15,{
            
            c("Summary statistics-")
            
        })
        
        output$out.32 <- renderText({
            txt.BT15()
        })
        
        output$out.33.1 <- renderText({
            if (input$BT15 == 0)
                return(NULL)
            "Treatment group:"
        })
        
        dat.BT15 <- eventReactive(input$BT15,{
            
            library(MatchIt)
            dat.mat = match.data(mat()) 
            
            #treatment group
            summary(dat.mat[dat.mat$Treatment == 1, input$SL1])
            
        })
        
        output$out.33 <- renderPrint({
            
            dat.BT15()
            
        })
        
        output$out.34.1 <- renderText({
            if (input$BT15 == 0)
                return(NULL)
            "Matched pair group:"
        })
        
        
        dat2.BT15 <- eventReactive(input$BT15,{
            
            library(MatchIt)
            dat.mat = match.data(mat()) 
            
            #matched pair group
            summary(dat.mat[dat.mat$Treatment == 0, input$SL1])
            
        })
        
        output$out.34 <- renderPrint({
            
            dat2.BT15()
            
        })
        
        
        dat.out <- reactive({
            library(MatchIt)
            dat.mat = match.data(mat())
            
            n = names(dat2())[1]
            data.frame(id = dat.mat[dat.mat$Treatment == 0, c(n)])
        })
        
        
        output$downloadData <- downloadHandler(
            
            filename = function(){"pair-group.csv"}, 
            content = function(fname){
                
                write.csv(dat.out(), fname)
            }
        )
        
        
        
    })
    
    
    
        
    
    
    
}

# Create Shiny app ----
shinyApp(ui, server)