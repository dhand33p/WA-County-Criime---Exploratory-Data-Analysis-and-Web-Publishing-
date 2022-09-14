library("shiny")
library("ggplot2")
library("dplyr")
library("leaflet")
library("shinythemes")

introduction <- tabPanel("Home", 
                         tags$h1("Introduction"), 
                         tags$b("By Group H2: Alex Li, Xinjie Huang, Evelyn G. Salas Ramos, Dhandeep Suglani"), 
                         tags$br(),
                         tags$em("Checkout our analysis report: "), 
                         tags$a(href="https://info201a-wi20.github.io/project-report-756558719/", "Analysis Report"), 
                         tags$h3("Problem Domain"), 
                         textOutput("prob_domain"), 
                         tags$h3("Datasets"), 
                         tags$a(href="https://www.kaggle.com/mikejohnsonjr/united-states-crime-rates-by-county", "Crime Rates Dataset"),
                         tags$br(), 
                         tags$a(href = "https://data.ers.usda.gov/reports.aspx?ID=17828", "USDA Unemployment Dataset"), 
                         tags$br(),
                         tags$a(href = "https://simplemaps.com/data/us-cities", "County Map Dataset (used for interactive map)"))

# Q1 tab - Alex

q1_map_tab <- tabPanel("Map",  sidebarPanel(radioButtons(inputId = "stat_type_a", label = "Statistics type:",
                                                         choices = c("Both", "Crime Rate (per 1000k)", "Unemployment Rate (%)"), 
                                                         selected = "Both")), 
                       mainPanel(leafletOutput("q1_interact_map")))

q1_plot_tab <- tabPanel("Plot", plotOutput("q1_plot"))

q1_text_tab <- tabPanel("Analysis",  sidebarPanel(radioButtons(inputId = "stat_type_b", label = "Statistics type:",
                                                               choices = c("Both", "Crime Rate (per 1000k)", "Unemployment Rate (%)"), 
                                                               selected = "Both")), 
                        mainPanel(tableOutput("q1_analysis")))

q1_tab <- tabPanel("Relation between Crime Rate & Unemployment", 
                   tabsetPanel(q1_map_tab, q1_plot_tab, q1_text_tab))

# Q2 tab - Dhandeep
q2_plot_crime <- tabPanel("Plot", 
                   sidebarPanel(
                    sliderInput(inputId = "num_county", label = "Top Number Counties arranged by population(highest to lowest population)",
                                         value = 10, min = 5 , max = 25 ),
                     selectInput(inputId = "select", label = "Select box", 
                                 choices = list("Robbery and Burglary vs Unemployment Rate" = 1, "Burglary vs Unemployment Rate" = 2,
                                                "Robbery vs Unemployment Rate" = 3 ))),
                   mainPanel(h4("Interactive Plot"), plotOutput("rob_plot")))

q2_table_tab <- tabPanel("Table", sidebarPanel(
              sliderInput(inputId = "num_count", label = "Top Number Counties arranged by population(highest to lowest population)",
              value = 10, min = 5 , max = 25 ),
              selectInput(inputId = "select_a", label = "Select box", 
              choices = list("Robbery and Burglary vs Unemployment Rate" = 1, "Burglary vs Unemployment Rate" = 2,
                             "Robbery vs Unemployment Rate" = 3 ))), 
              mainPanel(h4("Data Table"),tableOutput("crime_table")))     

q2_analysis <- tabPanel("Analysis", mainPanel(h4("Introduction"), textOutput("text_one"), h4("Method"), textOutput("text_two"), h4("Summary"), textOutput("text_three")))
              
q2_tab <- tabPanel("Robbery & Burglary vs. Unemployment Rate", tabsetPanel(q2_plot_crime, q2_table_tab, q2_analysis))           
                    
                   

# Q3 tab - Xinjie
q3_tab <- tabPanel("Change in Crime per Capita vs. Crime Rate", sidebarPanel(
                   checkboxGroupInput(inputId = "stat_type2", label = "Choose Crime Types to Display:",
                                choices = c("Murder", "Rape", "Robbery", "Agg Assault", "Burglary", "Larceny", "MV Theft", "Arson"),
                                selected = c("Murder", "Rape", "Robbery", "Agg Assault", "Burglary", "Larceny", "MV Theft", "Arson")),
                   sliderInput(inputId = "slider_key", label = "Show Data Only in Crime Rate Range:",
                               min = 7, max = 43,
                               value = c(7, 43)),
                   h4("You have selected the following crime types:"),
                   textOutput("selected_var"),
                   h4("The current plot reflects data in the crime rate range:"),
                   textOutput("selected_range")), 
                   
                   h2("Change in Crime Type Per Capita as Crime Rate Increases"),
                   
                   mainPanel(
                     plotOutput("change_plot"),
                     h4("Greatest change based on selection:"),
                     htmlOutput("greatest_change"),
                     h4("Below is a table of crime per capita changes based on selection:"),
                     tableOutput("change_table")
                   )
                  )


#Summary tab - Evelyn
summary_tab <- tabPanel("Summary")


# Combining all tabs into tabset
tabs <- tabsetPanel(type = "tabs",
                    introduction, 
                    q1_tab, 
                    q2_tab, 
                    q3_tab, 
                    summary_tab)

# Final UI
my_ui <- fluidPage(
 titlePanel("Crime rate vs. Unemployment rate in WA counties"), 
 tabs, fluidPage(theme = shinytheme("slate"))
)

