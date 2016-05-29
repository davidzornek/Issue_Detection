#This application mirrors the Issue_Detection_Simulation.R and Univariate_Issue_Detection.R scripts,
#allowing the user to adjust some parameters. The methodology has already been documented at length
#in those scripts, so the reader is referred there for additional information.

shinyServer(function(input, output) {
  
  Frequency_Multiplier <- reactive({
    1 + (input$factor*mean(Std_Dev_Frequency$StdDev, na.rm = TRUE)/Overall_Real_Frequency)
  })
  
  Fake_Data_Before <- reactive({
    Real_Data_Before %>% 
      group_by(Benefit_State, Policy_Year) %>% 
      summarise(Reported_Count = sum(Reported_Count),
                FTE = sum(FTE)) %>% 
      as.data.frame() %>% 
      mutate(Frequency = Frequency_Multiplier()*100*Reported_Count/FTE,
             FTE = FTE/9,
             Reported_Count = round(Frequency*FTE/100,0),
             Frequency = 100*Reported_Count/FTE,
             LOB = "ISSUE") %>% 
      select(LOB, Benefit_State, Policy_Year, Reported_Count, FTE, Frequency)
  })
  
  Total_Data_Before <- reactive({
    rbind(Real_Data_Before, Fake_Data_Before())
  })
  
  Real_Data_After_ExposureAdj <- reactive({
    Real_Data_Before %>% 
      mutate(FTE = ifelse(Policy_Year == Max_Policy_Year, (1 - input$shift)*FTE, FTE),
             Reported_Count = round(FTE*Frequency/100,0))
  })
  
  Fake_Data_After_ExposureAdj <- reactive({
    Fake_Data_Before() %>% 
      mutate(FTE = ifelse(Policy_Year == Max_Policy_Year, (1 + 9*input$shift)*FTE, FTE),
             Reported_Count = round(FTE*Frequency/100, 0))
  })
  
  Total_Data_After_ExposureAdj <- reactive({
    rbind(Real_Data_After_ExposureAdj(), Fake_Data_After_ExposureAdj()) %>% 
      ungroup()
  })
  
  New_Year <- reactive({
    filter(Total_Data_After_ExposureAdj(), Policy_Year == Max_Policy_Year)
  })
  
  New_Lambda <- reactive({
    sum(New_Year()$Reported_Count) / sum(New_Year()$FTE)
  })
  
  Prior_Data <- reactive({
    filter(Total_Data_After_ExposureAdj(), Policy_Year != Max_Policy_Year)
  })
  
  Year_Freq <- reactive({
    Prior_Data() %>% 
      group_by(Policy_Year) %>% 
      summarise(Reported_Count = sum(Reported_Count),
                FTE = sum(FTE)) %>% 
      mutate(Lambda = Reported_Count / FTE)
  })
  
  Lambda <- reactive({
    wtd.mean(Year_Freq()$Lambda, Year_Freq()$FTE)
  })
  
  SampVar_Lambda <- reactive({
    var(Year_Freq()$Lambda)*nrow(Year_Freq())/(nrow(Year_Freq()) - 1)
  })
  
  CI <- reactive({
    qgamma(c((1 - input$confidence)/2, (1 + input$confidence)/2), shape = (Lambda()^2)/SampVar_Lambda(), rate = Lambda()/SampVar_Lambda())
  })

  output$Flag <- renderText({
    if(New_Lambda() < CI()[2]){
      "No Issue Flagged"
    } else {"Issue Flagged"}
  })
  
  Comparison_Table <- reactive({
    Labels <- c("Before", "After")
    
    Overall_Freq_Before <- sum(Total_Data_Before()$Reported_Count)/sum(Total_Data_Before()$FTE)
    Overall_Freq_After <- sum(Total_Data_After_ExposureAdj()$Reported_Count)/sum(Total_Data_After_ExposureAdj()$FTE)
    
    FTE_Before <- sum(Total_Data_Before()$FTE)
    FTE_After <- sum(Total_Data_After_ExposureAdj()$FTE)
    
    data.frame(Stage = Labels, FTE = c(FTE_Before, FTE_After), Overall_Frequency = c(Overall_Freq_Before, Overall_Freq_After))
  })
  
  
  Power_Data <- reactive({
    
    temp <- data.frame(matrix(ncol = 1, nrow = 19))
    names(temp) <- c("True_Lambda")
  
    for(i in 1:19){
      temp[i,1] <- qgamma(i*0.05, shape = (New_Lambda()^2)/SampVar_Lambda(), rate = New_Lambda()/SampVar_Lambda())
    }
    
    temp %>%
      mutate(Miss = pgamma(CI()[2], shape = (True_Lambda^2)/SampVar_Lambda(), rate = True_Lambda/SampVar_Lambda()))
    
  })
  
  Significance_Data <- reactive({
    
    temp <- data.frame(matrix(ncol = 1, nrow = 21))
    names(temp) <- c("Significance")
    temp$Significance <- seq(from = 0, to = 1, by = 0.05)

    temp %>%
      mutate(Threshold = qgamma(1 - Significance, shape = (Lambda()^2)/SampVar_Lambda(), rate = Lambda()/SampVar_Lambda()),
             Miss = pgamma(Threshold, shape = (New_Lambda()^2)/SampVar_Lambda(), rate = New_Lambda()/SampVar_Lambda()))
    
  })
  
  output$Cost_Table <- renderDataTable({
    datatable(
      data.frame("Issue_Flagged" = c("X - S + e", "e + h"), "Issue_Not_Flagged" = c("X",0), row.names = c("Issue Present", "Issue Not Present")),
      colnames = c("Issue Flagged", "Issue Not Flagged"),
      options = list(searching = FALSE, bInfo = FALSE, paging = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:2), list(orderable = FALSE, targets = 0:2)))
    )
  })
  
  output$Power_vs_True_Lambda <- renderPlot({
    ggplot(Power_Data(), aes(x = Power_Data()$True_Lambda, y = Power_Data()$Miss)) +
      geom_line(stat = "identity") + 
      labs(x = "True Lambda", y = "Probability of False Negative")
  })
  
  output$Power_vs_Significance <- renderPlot({
    ggplot(Significance_Data(), aes(x = Significance_Data()$Significance, y = Significance_Data()$Miss)) +
      geom_line(stat = "identity") +
      labs(x = "Probability of False Positive", y = "Probability of False Negative")
  })

  output$Comparison_Table <- DT::renderDataTable({
    datatable(Comparison_Table(),
              rownames = FALSE,
              options = list(searching = FALSE, bInfo = FALSE, paging = FALSE,
                             columnDefs = list(list(className = 'dt-center', targets = 0:2), list(orderable = FALSE, targets = 0:2)))
    )
  })
})