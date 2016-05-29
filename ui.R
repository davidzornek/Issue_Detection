dashboardPage(dashboardHeader(title = "Issue Detection Sensitivity"),
              dashboardSidebar(
                numericInput("factor", "Standard deviation factor for bad segment:",
                             value = 1.5, min = 0, max = Inf, step = 0.1),
                numericInput("shift", "Distributional shift:",
                             value = 0.1, min = 0, max = 1, step = 0.05),
                sliderInput("confidence", "Select size of confidence interval for lambda:",
                            value = 0.7, min = 0, max = 1, step = 0.05),
                numericInput("factor2", "Standard deviation factor for true lambda after shift:",
                             value = 1.5, min = 0, max = Inf, step = 0.1)
              ),
              dashboardBody(
                h2(textOutput("Flag")),
                textOutput("test"),
                textOutput("Type2"),
                textOutput("Type1"),
                plotOutput("Power_vs_True_Lambda"),
                plotOutput("Power_vs_Significance"),
                dataTableOutput("Comparison_Table"),
                dataTableOutput("Total_Data_After")
              )
)