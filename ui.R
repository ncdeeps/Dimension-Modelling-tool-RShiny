#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyLP)
library(summarytools)
library(dplyr)
library(visdat)
library(corrgram)
library(ggplot2)
library(GGally)
library(DataExplorer)
library(devtools)
library(scales)
library(RColorBrewer)
library(stats)
library(vegan)
library(shinycssloaders)
library(xray)
library(pls)



# Define UI for application that draws a histogram
{
    sidebar <- dashboardSidebar(
        hr(),
        sidebarMenu(id="tabs",
                    menuItem("Data", icon=icon("folder-plus"),
                             menuSubItem("Table Import",tabName = "table", icon=icon("table"), selected=TRUE),
                             menuSubItem("DataFrame", tabName = "dataframe", icon=icon("table")),
                             menuSubItem("Anomalies", tabName="Anamoly", icon=icon("exclamation-triangle")),
                             menuSubItem("Plot", tabName="plot", icon=icon("line-chart"))
                    ),
                    menuItem("Summary",tabName = "summary",icon = icon("book")),
                    menuItem("MDS",tabName = "MDS",icon=icon("play")),
                    menuItem("Non-Metric MDS",tabName = "NMDS",icon=icon("play")),
                    menuItem("PCA",icon=icon("play"),
                             menuSubItem("Compute PCA",tabName = "PCAcompute",icon=icon("table")),
                             menuSubItem("PCA Summary",tabName = "PCsumm",icon=icon("table")),
                             menuSubItem("PCA Plots",tabName = "PCAplot",icon=icon("line-chart"))
                    ),
                    menuItem("PLS", icon=icon("play"),
                             menuSubItem("Compute PLS",tabName = "PLScompute",icon=icon("table")),
                             menuSubItem("PLS Summary",tabName = "PLSsumm",icon=icon("table")),
                             menuSubItem("PLS Plots",tabName = "PLSplot",icon=icon("line-chart"))
                    ),
                    menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board"))
        )
    )
    body <- dashboardBody(
        tabItems(
            tabItem(tabName = "readme",
                    fluidPage(
                        fluidRow(
                            column(width=12,
                                   includeHTML("readme.html")
                            )
                        ),
                        fluidRow(
                            column(width=12,
                                   iframe(width = "560", height = "315",
                                          url_link = "https://www.youtube.com/embed/yLdOS6xyM_Q"))
                        )
                    )
            ),
            tabItem(tabName = "table",
                    box(width = NULL, status = "primary", solidHeader = TRUE, title="Table",
                        fileInput('datafile', 'Choose CSV file',
                                  accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                        br(),
                        box(width = NULL, status = "primary", solidHeader = TRUE, title="Dataset",
                            tableOutput("filetable"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                    )
            ),
            tabItem(tabName ="summary",
                    fluidRow(
                        column(width = 12,
                               verbatimTextOutput(outputId = "dfsumm")
                        )
                    )
            ),
            tabItem(tabName ="Anamoly",
                    fluidRow(
                        column(width = 12,
                               p("Analyzes all your columns for anomalies, whether they are NAs, Zeroes, Infinite, etc, and warns you if it detects variables with at least 80% of rows with those anomalies. It also warns you when all rows have the same value."),
                               verbatimTextOutput(outputId = "anomaly")
                        )
                    )
            ),
            tabItem(tabName ="dataframe",
                    box(width = NULL,
                        conditionalPanel(
                            condition="output.contents",
                            selectizeInput('var', label = "Variable to display", "",multiple= TRUE)
                        )),
                    br(),
                    DT::dataTableOutput('contents'),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
            ),
            tabItem(tabName = "MDS",
                    tabsetPanel(
                        tabPanel("DistanceMatrix",
                                 fluidRow(
                                     column(width = 8,
                                            p("The Distance matrix is calculated for method chosen in plot"),
                                            verbatimTextOutput(outputId = "MDS.dist"))
                                 )
                        ),
                        tabPanel("Plot",
                                 fluidRow(
                                     column(width = 12,
                                            withSpinner(plotOutput("MDS.data2d")))
                                 ),
                                 p("MDS projects n-dimensional data points to a (commonly) 2-dimensional space such that similar objects 
                                   in the n-dimensional space will be close together on the two dimensional plot"),
                                 sliderInput(inputId = "k", label = "Choose Dimension", min = 1, max = 2, step = 1, value = 2),
                                 radioButtons(inputId = 'distmethod',  
                                              label = 'Distance-method',
                                              choices = c('euclidean',
                                                          'maximum',
                                                          'manhattan',
                                                          'canberra',
                                                          'minkowski'
                                              ), 
                                              selected = 'euclidean')
                        )
                    )
            ),
            tabItem(tabName = "NMDS",
                    tabsetPanel(
                        tabPanel("DistanceMatrix",
                                 fluidRow(
                                     column(width = 8,
                                            p("The Distance matrix is calculated for method chosen in plot"),
                                            verbatimTextOutput(outputId = "NMDS.dist"))
                                 )
                        ),
                        tabPanel("Plot",
                                 fluidRow(
                                     column(width = 12,
                                            withSpinner(plotOutput("NMDS.data2d")))
                                 ),
                                 p("NMDS attempts to represent the pairwise dissimilarity between objects in a low-dimensional space. Any dissimilarity coefficient or 
                                   distance measure may be used to build the distance matrix used as input."),
                                 sliderInput(inputId = "k1", label = "Choose Dimension", min = 1, max = 2, step = 1, value = 2),
                                 radioButtons(inputId = 'distmethod1',  
                                              label = 'Distance-method',
                                              choices = c('euclidean',
                                                          'maximum',
                                                          'manhattan',
                                                          'canberra',
                                                          'minkowski'
                                              ), 
                                              selected = 'euclidean')
                        )
                    )
            ),
            tabItem(tabName = "plot",
                    fluidRow(
                        column(width = 6, 
                               selectInput("plottype","Plot Type:",
                                           list(MissingData = "MissingData", BoxPlot = "BoxPlot",CorrelationPlot = "CorrelationPlot",HomogeneityPlot ="HomogenietyPlot",RisingOrder = "RisingOrder",ScatterPlot = "ScatterPlot",Distributions = "Distributions"))
                        )
                    ),
                    fluidRow(
                        column(width = 10,align="center",
                               withSpinner(plotOutput('plot',width = "auto"))       
                        )
                    ),
                    conditionalPanel("input.plottype == 'ScatterPlot'",
                                     fluidRow(
                                         column(width = 10,
                                                selectInput('xcol', 'X Variable', ""),
                                                selectInput('ycol', 'Y Variable', "", selected = "")
                                         )
                                     ) 
                    ),
                    conditionalPanel(
                        condition="input.plottype == 'HomogenietyPlot'",
                        selectizeInput('hom', label = "Variable to display", "",multiple= TRUE)
                    ),
                    conditionalPanel(
                        condition="input.plottype == 'MissingData'",
                        radioButtons(inputId = 'clstr',  
                                     label = 'Cluster?',
                                     choices = c('TRUE',
                                                 'FALSE'
                                     ),
                                     selected = 'FALSE')
                    ),
                    conditionalPanel(
                        condition="input.plottype == 'CorrelationPlot'",
                        selectizeInput('corr', label = "Variable to display", "",multiple= TRUE)
                    ),
                    conditionalPanel(
                        condition="input.plottype == 'BoxPlot'",
                        selectizeInput('box', label = "Variable to display", "",multiple= TRUE),
                        checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
                        sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
                    ),
                    conditionalPanel(
                        condition="input.plottype == 'RisingOrder'",
                        selectizeInput('rise', label = "Variable to display", "",multiple= TRUE)
                    ),
                    conditionalPanel(
                        condition="input.plottype == 'Distributions'",
                        selectizeInput('distr', label = "Variable to display", "",multiple= TRUE,options = list(maxItems = 4)),
                        p("Tries to analyze the distribution of your variables, so you can understand how each variable is statistically structured"),
                        p("Only four variables are set to display at one instance")
                    )
            ),
            tabItem(tabName = "PCAcompute",
                    p("Choose the columns of your data to include in the PCA."),
                    p("Only columns containing numeric data are shown here because PCA doesn't work with non-numeric data."),
                    p("The PCA is automatically re-computed each time you change your selection."),
                    p("Observations (ie. rows) are automatically removed if they contain any missing values."),
                    p("Variables with zero variance have been automatically removed because they're not useful in a PCA."),
                    uiOutput("choose_columns_pca"),
                    tags$hr(),
                    p("Select options for the PCA computation (we are using the prcomp function here)"),
                    radioButtons(inputId = 'center',  
                                 label = 'Center',
                                 choices = c('Shift variables to be zero centered'='Yes',
                                             'Do not shift variables'='No'), 
                                 selected = 'Yes'),
                    
                    radioButtons('scale.', 'Scale',
                                 choices = c('Scale variables to have unit variance'='Yes',
                                             'Do not scale variables'='No'), 
                                 selected = 'Yes')
            ),
            tabItem(tabName = "PCAplot",
                    h2("Scree plot"),
                    p("The scree plot shows the variances of each PC, and the cumulative variance explained by each PC (in %) "),
                    plotOutput("plot2", height = "300px"),
                    tags$hr(),
                    h2("PC plot: zoom and select points"),
                    p("Select the grouping variable."),
                    p("Only variables where the number of unique values is less than 10% of the total number of observations are shown here (because seeing groups with 1-2 observations is usually not very useful)."),
                    uiOutput("the_grouping_variable"),
                    tags$hr(),
                    p("Select the PCs to plot"),
                    uiOutput("the_pcs_to_plot_x"),
                    uiOutput("the_pcs_to_plot_y"),
                    tags$hr(),
                    p("Click and drag on the first plot below to zoom into a region on the plot. Or you can go directly to the second plot below to select points to get more information about them."),
                    p("Then select points on zoomed plot below to get more information about the points."),
                    p("You can click on the 'Compute PCA' tab at any time to change the variables included in the PCA, and then come back to this tab and the plots will automatically update."),
                    plotOutput ("z_plot1", height = 400,
                                brush = brushOpts(
                                    id = "z_plot1Brush",
                                    resetOnNew = TRUE)),
                    tags$hr(),
                    p("Click and drag on the plot below to select points, and inspect the table of selected points below"),
                    plotOutput("z_plot2", height = 400,
                               brush = brushOpts(
                                   id = "plot_brush_after_zoom",
                                   resetOnNew = TRUE)),
                    tags$hr(),
                    p("Details of the brushed points"),
                    tableOutput("brush_info_after_zoom")
            ),  
            tabItem(tabName = "PCsumm",
                    verbatimTextOutput("pca_details")
            ),
            tabItem(tabName = "PLScompute",
                    p("Choose the columns of your data to include in the PLS."),
                    p("Only columns containing numeric data are shown here because PLS doesn't work with non-numeric data."),
                    p("The PLS is automatically re-computed each time you change your selection."),
                    p("Observations (ie. rows) are automatically removed if they contain any missing values."),
                    p("Variables with zero variance have been automatically removed because they're not useful in a PLS."),
                    uiOutput("choose_columns_pls"),
                    tags$hr(),
                    p("Select options for the PLS computation (we are using the plsr function here)"),
                    radioButtons(inputId = 'center1',  
                                 label = 'Center',
                                 choices = c('Shift variables to be zero centered'='Yes',
                                             'Do not shift variables'='No'), 
                                 selected = 'Yes'),
                    radioButtons('scale.1', 'Scale',
                                 choices = c('Scale variables to have unit variance'='Yes',
                                             'Do not scale variables'='No'), 
                                 selected = 'Yes'),
                    selectizeInput('pls',label = "Select Target Variable", "",multiple= FALSE),
                    radioButtons(inputId = 'val',  
                                 label = 'Validation',
                                 choices = c('CV',
                                             'LOO'
                                 ), 
                                 selected = 'CV'),
                    p("Please choose component wrt the number of variables available"),
                    sliderInput(inputId = "ncomp1", label = "Choose Dimension", min = 1, max = 10, step = 1, value = 3)
            ),
            tabItem(tabName = "PLSsumm",
                    p("We can get an overview of the fit and validation results with the summary method:"),
                    verbatimTextOutput("pls_details"),
                    p("The validation results here are Root Mean Squared Error of Prediction (RMSEP). There are
two cross-validation estimates: CV is the ordinary CV estimate, and adjCV is a bias-corrected
CV estimate. If LOO is choosen in PLS compute tab there is virtually no difference. It also shows the amount of variance explained by the model")
            ),
            tabItem(tabName = "PLSplot",
                    tabsetPanel(
                        tabPanel("RMSEP plot",
                                 fluidRow(
                                     column(width = 8,
                                            withSpinner(plotOutput("plot3")),
                                            p("This plots the estimated RMSEPs as functions of the number of components"))
                                 )
                        ),
                        tabPanel("Scores plot",
                                 fluidRow(
                                     column(width = 12,
                                            p("This gives a pairwise plot of the score values"),
                                            withSpinner(plotOutput("scr")),
                                            p("Score plots are often used to look for patterns, groups or outliers in the data")
                                     )
                                 )
                        ),
                        tabPanel("Loadings plot",
                                 fluidRow(
                                     column(width = 12,
                                            withSpinner(plotOutput("load")),
                                            p("The loading plot is much used for interpretation purposes, for instance to look
for known spectral peaks or profiles")
                                     )
                                 )
                        ),
                        tabPanel("Prediction plot",
                                 fluidRow(
                                     column(width = 12,
                                            withSpinner(plotOutput("pred")),
                                            p("This shows the cross-validated predictions with two components versus measured values")
                                     )
                                 )
                        ),
                        tabPanel("Regression Coefficients",
                                 fluidRow(
                                     column(width = 12,
                                            withSpinner(plotOutput("regcf")),
                                            p(" This allows simultaneous plotting of the regression
vectors for several different numbers of components at once")
                                     )
                                 )
                        )
                    )
            )
        )
    )
    
    dashboardPage(
        dashboardHeader(title = "Dimension Reduction", tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("Refresh1"))))),tags$li(class = "dropdown",tags$a("Deepak Nangamuthu Chanthiramathi"))),
        sidebar,
        body
    )
}


