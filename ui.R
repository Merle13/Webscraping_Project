

shinyUI(fluidPage(
  titlePanel("Which Health Insurance fits your needs?"),
  sidebarLayout(
    sidebarPanel(("Enter your personal information"),
                 #filters in which county you are living
                 selectInput("county", "Pick a county", c("", "Baden_Württemberg", "Bayern", "Berlin",
                                                          "Brandenburg", "Bremen", "Hamburg", "Hessen",
                                                          "Mecklenburg_Vorpommern", "Niedersachsen",
                                                          "Nordrhein_Westfalen", "Rheinland_Pfalz",
                                                          "Saarland", "Sachsen", "Sachsen_Anhalt",
                                                          "Schleswig_Holstein", "Thüringen")),
                 # gets the income input as textInput
                 textInput("income", "Enter your monthly Income", ""),
                 # checkbox for different basic features
                 checkboxGroupInput(inputId = "basic", label = "Basics", 
                                    choices = c("Abroad insurance" = "EmerencyService_Abroad", 
                                                "24/7 Service" = "service_hotline",
                                                "Advice", "Mediation")),
                 #verbatimTextOutput()
                 #checkbox for different Bonuses
                 checkboxGroupInput(inputId = "bonus", label = "Bonus",
                                    choices = c("Check_Up", "Cancer-Screening" = "Cancer_screening",
                                                "Skincancer" = "skinCancer", "Yearly Dentist" = "yearly_dentist",
                                                "No Smoking" = "non_smoker", "BMI", "Gym" = "gym")),
                 # checkbox for different naturpathy
                 checkboxGroupInput(inputId = "natur", label = "Naturpathy",
                                    choices = c("Ayurveda", "Homeopathy" = "HomeopathyTherapy", "Osteopathy",
                                                "Reflexmassage", "TCM"))),
    mainPanel(("Result"),
              textOutput("myincome"),
              DT::dataTableOutput('GKVtable')
  )
))
)
