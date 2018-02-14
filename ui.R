

shinyUI(fluidPage(
  titlePanel("Which Health Insurance fits your needs?"),
  sidebarLayout(
    sidebarPanel(("Enter your personal information"),
                 #filters in which county you are living
                 selectInput("county", "Pick a county", c("", "Baden-Württemberg", "Bayern", "Berlin",
                                                          "Brandenburg", "Bremen", "Hamburg", "Hessen",
                                                          "Mecklenburg-Vorpommern", "Niedersachsen",
                                                          "Nordrhein-Westfalen", "Rheinland-Pfalz",
                                                          "Saarland", "Sachsen", "Sachsen-Anhalt",
                                                          "Schleswig-Holstein", "Thüringen")),
                 
                 
                 # gets the income input as textInput
                 numericInput("income", "Enter your monthly Income in Euro", ""),
                 
                 
                 
                 #checkbox for different Bonuses
                 checkboxGroupInput(inputId = "bonus", label = "Bonus",
                                    choices = c("Check_Up", "Cancer-Screening" = "Cancer_screening",
                                                "Skin Cancer" = "skinCancer", "Yearly Dentist" = "yearly_dentist",
                                                "No Smoking" = "non_smoker", "BMI", "Gym" = "gym")),
                 
                 
                 # checkbox for different naturpathy
                 checkboxGroupInput(inputId = "natur", label = "Naturpathy",
                                    choices = c("Ayurveda", "Homeopathy" = "HomeopathyTherapy", "Osteopathy",
                                                "Reflexmassage", "TCM"))),
    
    
    mainPanel(("Results"),
              #textOutput("myincome"),
              DT::dataTableOutput('GKVtableUnder', width = "100%")
              # (br),
              # (br),
              # plotOutput("barRate")
              
  )
))
)
