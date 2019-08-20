## ui.R ##


shinyUI(dashboardPage( 
  dashboardHeader(title = 'Crimes in Boston'),
  dashboardSidebar( 
    sidebarUserPanel("Mario Calderon"),
    sidebarMenu(
      menuItem("Background", tabName = "Background", icon = icon("cog")),
      menuItem("Day of Week", tabName = "DayWeek", icon = icon("database")),
      menuItem("Month", tabName = "Month", icon = icon("database")),
      menuItem("Hour", tabName = "Hour", icon = icon("fas fa-stopwatch")),  
      menuItem("District", tabName = "District ", icon = icon("database")),
      menuItem("Next Steps", tabName = "Next Steps", icon = icon("database"))
    )), 
  dashboardBody(
    tabItems(
       tabItem(tabName = "Background",
               fluidRow(tags$div(
                 tags$h1(tags$b("Crimes in Boston 2015-2018")), br(),
                 tags$h2(tags$b(tags$em("Contents"))),
                 tags$h3("Crime by Day of the Week"),
                 tags$h3("Crrime by Month"),
                 tags$h3("Crime by Hour"),
                 tags$h3("Most Common Crimes by District"),
                 tags$h3("Next Steps"),
                 tags$h4("Source: Data sourced from Kaggle ",
                         tags$a(href='https://www.kaggle.com/AnalyzeBoston/crimes-in-boston'))
               )))
     ,
      tabItem(tabName = "DayWeek", 
               fluidRow(column(6,
                               selectizeInput(inputId = "offense_code_group1",
                                              label = "offense code group",
                                              choices = unique(data$offense_code_group),
                                              selected = "Simple Assault"),
                               selectizeInput(inputId = "Year1",
                                              label = "Year",
                                              choices = unique(data$year),
                                              selected = 2016)),
                        
                        column(6,
                               selectizeInput(inputId = "district1",
                                              label = "District",
                                              choices = unique(data$district),
                                              selected = 'D4')) 
               ),
              plotOutput("countdayofweek"))
     ,
    tabItem(tabName = "Month",  
               fluidRow(column(6,
                              selectizeInput(inputId = "offense_code_group2",
                                             label = "offense code group",
                                             choices = unique(data$offense_code_group),
                                             selected = "Simple Assault"),
                               selectizeInput(inputId = "Year2",
                                             label = "Year",
                                             choices = unique(data$year),
                                             selected = 2016)),
                       
                       column(6,
                              selectizeInput(inputId = "district2",
                                             label = "District",
                                             choices = unique(data$district),
                                             selected = 'D4')) 
              ), 
              plotOutput("countmonth"))
    ,
   tabItem(tabName = "Hour",
               fluidRow(column(6,
                               selectizeInput(inputId = "offense_code_group3",
                                              label = "offense code group",
                                              choices = unique(data$offense_code_group),
                                              selected = "Simple Assault"),
                               selectizeInput(inputId = "Year3",
                                              label = "Year",
                                              choices = unique(data$year),
                                              selected = 2016)),
                        
                        column(6,
                               selectizeInput(inputId = "district3",
                                              label = "District",
                                              choices = unique(data$district),
                                              selected = 'D4'))
               ),  
               plotOutput("counthour"))
      #,
      # tabItem(tabName = "District",
      #         fluidRow(column(6,
      #                         selectizeInput(inputId = "offense_code_group4",
      #                                        label = "offense code group",
      #                                        choices = unique(data$offense_code_group),
      #                                        selected = "Simple Assault"),
      #                         selectizeInput(inputId = "Year4",
      #                                        label = "Year",
      #                                        choices = unique(data$year),
      #                                        selected = 2016))
      #         ),
      #         plotOutput("countdistrict")),
      # tabItem(tabName = "Next Steps",
      #         "to be replaced with map and histogram"),
      
    )
  ))
)

