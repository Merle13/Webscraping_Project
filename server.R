

shinyServer(function(input, output, session){
  
  
  #filtering for the county Input and calcutlating the input 
  county_df = reactive({
    return_df = NULL
    if(input$county == "Baden-Württemberg") {
      return_df = workGKV %>%
        filter(., Baden_Württemberg == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Bayern") {
      return_df = workGKV %>%
        filter(., Bayern == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Berlin") {
      return_df = workGKV %>%
        filter(., Berlin == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Brandenburg") {
      return_df = workGKV %>%
        filter(., Brandenburg == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Bremen") {
      return_df = workGKV %>%
        filter(., Bremen == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Hamburg") {
      return_df = workGKV %>%
        filter(., Hamburg == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Hessen") {
      return_df = workGKV %>%
        filter(., Hessen == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Mecklenburg-Vorpommern") {
      return_df = workGKV %>%
        filter(., Mecklenburg_Vorpommern == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Niedersachsen") {
      return_df = workGKV %>%
        filter(., Niedersachsen == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Nordrhein-Westfalen") {
      return_df = workGKV %>%
        filter(., Nordrhein_Westfalen == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Rheinland-Pfalz") {
      return_df = workGKV %>%
        filter(., Rheinland_Pfalz == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Saarland") {
      return_df = workGKV %>%
        filter(., Saarland == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Sachsen") {
      return_df = workGKV %>%
        filter(., Sachsen == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Sachsen-Anhalt") {
      return_df = workGKV %>%
        filter(., Sachsen_Anhalt == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Schleswig-Holstein") {
      return_df = workGKV %>%
        filter(., Schleswig_Holtstein == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else if(input$county == "Thüringen") {
      return_df = workGKV %>%
        filter(., Thüringen == "True") %>%
        mutate(., Rate = (Rate - 0.073) * input$income)


    } else {
      return_df = workGKV %>%
        mutate(., Rate = (Rate - 0.073) * input$income)
    }
    
    
    # filterin for the bonus inputs
    if(!is.null(input$bonus)) {
      
      if("Check_Up" %in% input$bonus) {
        return_df = return_df  %>%
          filter(., Check_up == TRUE)
      }
      
      if("Cancer-Screeing" %in% input$bonus) {
        return_df = return_df %>%
          filter(., Cancer_screening == TRUE)
      }
      
      if("Skin Cancer" %in% input$bonus) {
        return_df = return_df %>%
          filter(., skinCancer == TRUE)
      }

      if("Yearly Dentist" %in% input$bonus) {
        return_df = return_df %>%
          filter(., yearly_dentist == TRUE)
      }

      if("No Smoking" %in% input$bonus) {
        return_df = return_df %>%
          filter(., non_smoker == TRUE)
      }

      if("BMI" %in% input$bonus) {
        return_df = return_df %>%
          filter(., BMI == TRUE)
      }

      if("Gym" %in% input$bonus) {
        return_df = return_df %>%
          filter(., gym == TRUE)
      }
    }


    # #filtering for naturpathy inputs
    
    if(!is.null(input$natur)) {

      if("Ayurveda" %in% input$natur) {
        return_df = return_df %>%
          filter(., Ayurveda == TRUE)
      }

      if("Homeopathy" %in% input$natur) {
        return_df = return_df %>%
          filter(., HomoeopathyTherapy == TRUE)
      }

      if("Osteopathy" %in% input$natur) {
        return_df = return_df %>%
          filter(., Osteopathy == TRUE)
      }

      if("Reflexmassage" %in% input$natur) {
        return_df = return_df %>%
          filter(., Reflexmassage == TRUE)
      }

      if("TCM" %in% input$natur) {
        return_df = return_df %>%
          filter(., TCM == TRUE)
      }

    }


    
    return_df

  })


  
  output$GKVtableUnder <- DT::renderDataTable({
    datatable(county_df() %>% 
                select(., Name, Premium_rate, Rate) %>%
                # filter(., Rate < mean(Rate)) %>%
                arrange(., Rate))
  })

  # output$barRate = renderPlot({
  #   ggplot(data = county_df(), aes(x = Rate)) +
  #     geom_bar(aes(fill = Rate)) + 
  #     ggtitle("Rates distributed in each County") +
  #     xlab("rates")
  # })
  
  
})