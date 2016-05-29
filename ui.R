dashboardPage(dashboardHeader(title = "Issue Detection Sensitivity"),
              dashboardSidebar(
                numericInput("factor", "Standard deviation factor for bad segment:",
                             value = 1.5, min = 0, max = Inf, step = 0.1),
                numericInput("shift", "Distributional shift:",
                             value = 0.1, min = 0, max = 1, step = 0.05),
                sliderInput("confidence", "Select size of confidence interval for lambda:",
                            value = 0.7, min = 0, max = 1, step = 0.05),
                div(align = "center", h2(textOutput("Flag")))
              ),
              dashboardBody(
                tabsetPanel(
                  tabPanel(title = "Read Me",
                           br(),
                           div("Before using this app, the reader is encouraged to view the source script for the accompanying analysis, 
                               which can be found on the right side of the screen (if the app is being viewed on shinyapps.io) or at:",
                               a("https://github.com/davidzornek/Issue_Detection", href = "https://github.com/davidzornek/Issue_Detection", target = "_blank")),
                           br(),
                           div(paste0("This app facilitates decisions about where to set the confidence level in ",
                                      "the accompanying Bayesian issue detection methodology. Our goal is to ",
                                      "minimize the client's expected cost due to an underlying issue affecting ",
                                      "claim experience. Consider the table below, which demonstrates the relevant ",
                                      "costs:")
                           ),
                           dataTableOutput("Cost_Table"),
                           br(),
                           div(paste0("X is the gross cost of the issue, S is the savings expected by addressing the ",
                                      "issue, e is the cost of investigating whether a flagged issue is present, ",
                                      "what needs to be done about it, etc., and h is the \"annoyance cost\" of investigating ",
                                      "an issue that does not exist. Taking the probability of landing into each quadrant into ",
                                      "consideration, and under the assumption that X > S >> e > h, minimizing the expected ",
                                      "total cost amounts to minimizing P(Issue Present, Issue Flagged) + P(Issue Present, ",
                                      "Issue Not Flagged), which is just the probability of an issue.")
                           ),
                           br(),
                           div(paste0("Of course, that isn't a very useful metric for us to minimize, since the client is ",
                                      "already presumably doing what they can to minimize the probability of an issue arising. ",
                                      "Our task, then, is to minimize the proability of landing in the upper right hand quadrant ",
                                      "since this quadrant carries the highest cost.")
                           ),
                           br(),
                           div(paste0("This is essentially maximizing statistical power. However, our historical data have no ",
                                      "known issues from which we can estimate the unconditional probability of an issue, ",
                                      "which is required to compute statistical power. We also lack insight into the true claim ",
                                      "frequency following a distributional shift, since our best estimate is the most recent ",
                                      "year's frequency, which is assumed to be gamma distributed. The true frequency could be ",
                                      "anywhere within the domain of our gamma distribution!")
                           ),
                           br(),
                           div(paste0("We do know that minimizing significance will maximize power, but setting significance very ",
                                      "low will greatly increase h (and in some cases e), since it is non-linearly related to ",
                                      "the number of false positives being investigated. Moreover, since h is a psychological cost ",
                                      "it is exceedingly difficult for us to assign a concrete value to at any given significance.")
                           ),
                           br(),
                           div(paste0("The proposed solution is to judgmentally select our significance level based on information ",
                                      "we do have about how power varies against significance and the true lambda under a variety ",
                                      "of modeling assumptions. This module allows the user to investigate these relationships in ",
                                      "order to determine when decreasing significance no longer returns significant enough power ",
                                      "gains to make additional false positives a worthwhile tradeoff.")
                           )
                  ),
                  tabPanel("Module 1",
                           br(),
                           box(title = "Before and After", width = 12,
                               div("This table shows the overall claim frequency and total number of employees (FTE) before and after the distributional shift."),
                               dataTableOutput("Comparison_Table")
                           ),
                           box(title = "Power vs. Significance", width = 12,
                               div("This exhibit shows the payoff we can expect to receive in power by decreasing significance. Notice that as the standard devation factor and distributional shift are increased, the curve approaches a right angle."),
                               br(),
                               plotOutput("Power_vs_Significance")
                           )
                  ),
                  tabPanel("Module 2",
                           box(title = "Power vs. True Lambda", width = 12,
                               div("This module is less important than Module 1, but is still useful to some degree. In light of knowledge that our estimate of frequency for 2015 is not the true frequency for 2015, we can see how power changes depending on what the true frequency is. Tick marks on the x-axis are the quantiles of the gamma distribution for the frequency parameter."),
                               br(),
                               plotOutput("Power_vs_True_Lambda")
                           )
                  )
                )
              )
)