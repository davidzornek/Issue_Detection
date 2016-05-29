
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
  
  True_Lambda <- reactive({
    (Overall_Real_Frequency + input$factor2*mean(Std_Dev_Frequency$StdDev, na.rm = TRUE))/100
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
  
  SDev_Lambda <- reactive({
    sqrt(SampVar_Lambda())
  })
  
  CI <- reactive({
    qgamma(c((1 - input$confidence)/2, (1 + input$confidence)/2), shape = (Lambda()^2)/SampVar_Lambda(), rate = Lambda()/SampVar_Lambda())
  })
  
#Power Calculation
  
  All_Freq <- reactive({
    Total_Data_After_ExposureAdj() %>% 
      group_by(Policy_Year) %>% 
      summarise(Reported_Count = sum(Reported_Count),
                FTE = sum(FTE)) %>% 
      mutate(Lambda = Reported_Count / FTE)
  })
  
  #Assumes variance of lambda is constant over time.
  Miss <- reactive({
    pgamma(CI()[2], shape = (True_Lambda()^2)/SampVar_Lambda(), rate = True_Lambda()/SampVar_Lambda())
  })
  
  
  output$Flag <- renderText({
    if(New_Lambda() < CI()[2]){
      "No issue"
    } else {"Issue Flagged"}
  })
  
  output$test <- renderText({
    paste0("True Lambda is ", True_Lambda())
  })
  
  output$Type2 <- renderText({
    paste0("Probability of a miss is ", Miss())
  })
  
  output$Type1 <- renderText({
    paste0("Probability of a false positive is ", (1 - input$confidence)/2)
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
  
  
  ####### Adopt a null of H_0 = Issue = (True_Lambda = Lambda_2015)
  ####### Minimize type 1 errors = minimize P(Lambda_2015 <= c | True_Lambda = Lambda_2015)
  
  Hit_Miss_Table <- reactive({
    temp <- as.data.frame(matrix(nrow = 2, ncol = 2))
    rownames(temp) <- c("Issue", "No Issue")
    names(temp) <- c("Positive", "Negative")
    
    temp[1,2] <- Miss()
    temp[]
    
  })
  
  output$Comparison_Table <- DT::renderDataTable({
    datatable(Comparison_Table())
  })
  
  output$Total_Data_After <- DT::renderDataTable({
    datatable(Total_Data_After_ExposureAdj())
  })
  
  output$davetest <- DT::renderDataTable({
    datatable()
  })
  
  
})