library(shiny)


shinyServer(function(input, output, session){
  
  #displayed data frame
  # display_df = reactive({
  #   workGKV %>%
  #     select(., Name, Premium_rate, Rate)
  # })
  
#filtering for the county Input
  county_df = reactive({
    workGKV %>% 
      filter(., input$county == "True") %>%
      select(., Name, Premium_rate, Rate)
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
    datatable(workGKV)
    })
 
})