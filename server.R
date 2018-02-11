library(shiny)


shinyServer(function(input, output, session){
  
  #displayed data frame
  # display_df = reactive({
  #   workGKV %>%
  #     select(., Name, Premium_rate, Rate)
  # })
  
#filtering for the county Input
  county_df = reactive({
    if(input$county == "Baden-Württemberg") {
      workGKV %>%
        filter(., Baden_Württemberg == "True")
    } else if(input$county == "Bayern") {
      workGKV %>%
        filter(., Bayern == "True")
    } else if(input$county == "Berlin") {
      workGKV %>%
        filter(., Berlin == "True")
    } else if(input$county == "Brandenburg") {
      workGKV %>% 
        filter(., Brandenburg == "True")
    } else if(input$county == "Bremen") {
      workGKV %>%
        filter(., Bremen == "True")
    } else if(input$county == "Hamburg") {
      workGKV %>%
        filter(., Hamburg == "True")
    } else if(input$county == "Hessen") {
      workGKV %>%
        filter(., Hessen == "True")
    } else if(input$county == "Mecklenburg-Vorpommern") {
      workGKV %>%
        filter(., Mecklenburg_Vorpommern == "True")
    } else if(input$county == "Niedersachsen") {
      workGKV %>%
        filter(., Niedersachsen == "True")
    } else if(input$county == "Nordrhein-Westfalen") {
      workGKV %>%
        filter(., Nordrhein_Westfalen == "True")
    } else if(input$county == "Rheinland-Pfalz") {
      workGKV %>%
        filter(., Rheinland_Pfalz == "True")
    } else if(input$county == "Saarland") {
      workGKV %>%
        filter(., Saarland == "True")
    } else if(input$county == "Sachsen") {
      workGKV %>%
        filter(., Sachsen == "True")
    } else if(input$county == "Sachsen-Anhalt") {
      workGKV %>%
        filter(., Sachsen_Anhalt == "True")
    } else if(input$county == "Schleswig-Holstein") {
      workGKV %>% 
        filter(., Schleswig_Holtstein == "True") 
    } else if(input$county == "Thüringen") {
      workGKV %>%
        filter(., Thüringen == "True")
    } else {
      workGKV
    }
  
    
  })

  
  output$myincome <- renderText(input$income)
  
  # output$GKVtable <- renderTable({
  #   county_df()
  #   
  # output$data <- renderTable({
  #   mtcars[, c("mpg", input$variable), drop = FALSE]
  # }, rownames = TRUE)
  # 
  # })
  
  output$GKVtable <- DT::renderDataTable({
    datatable(county_df() %>% 
                #filter(., input$county == "True") %>%
                select(., Name, Premium_rate, Rate))
    })
 
})