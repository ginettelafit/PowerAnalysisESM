ui <- shinyUI(pageWithSidebar(

    # Application title
  headerPanel("Power analysis to compute the number of participants in ESM studies"),

  # Sidebar with controls to select the outputs to compute power
  sidebarPanel(
      shinyjs::useShinyjs(),
      id = "side-panel",
      withMathJax(),

  # Input: Selector for choosing model ----
      selectInput(inputId = "Model",
                  label = "Choose a model (more information in panel About the Method):",
                  choices = c("Model 1: Mean differences between two groups"=1,
                              "Model 2: Effect of a level-2 continuous predictor on the outcome variable"=2, 
                              "Model 3: Effect of a time-varying predictor (random slope)"=3, 
                              "Model 4: Effect of a time-varying predictor (fixed slope)"=4,
                              "Model 5: Difference in the effect of a time-varying predictor between two groups (random slope)"=5, 
                              "Model 6: Difference in the effect of a time-varying predictor between two groups (fixed slope)"=6,  
                              "Model 7: Cross-level interaction effect between a level-2 continuous predictor and a time-varying predictor (random slope)"=7, 
                              "Model 8: Cross-level interaction effect between a level-2 continuous predictor and a time-varying predictor (fixed slope)"=8, 
                              "Model 9: Multilevel AR(1) model"=9, 
                              "Model 10: Multilevel AR(1) model with differences in the autoregressive effect between two groups"=10,
                              "Model 11: Multilevel AR(1) model with cross-level interaction effect with a level-2 continuous predictor"=11)),


conditionalPanel(
condition = "input.Model == '1'",
         helpText("Model 1: Mean differences between two groups"),
         helpText("Level 1: \\(Y_{it} = \\gamma_{0i} + \\epsilon_{it} \\)"), 
         helpText("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}Z_i + \\nu_{0i} \\)"),
         helpText("\\( Z_i\\) is a dummy variable equal to one if participant is in Group 1 and 0 otherwise"),
         helpText("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) ")
),

conditionalPanel(
condition = "input.Model == '2'",
         helpText("Model 2: Effect of a level-2 continuous predictor on the outcome variable"),
         helpText("Level 1: \\(Y_{it} = \\gamma_{0i} + \\epsilon_{it} \\)"), 
         helpText("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}W_i + \\nu_{0i} \\)"),
         helpText("\\( W_i\\) is Gausssian distributed \\(N(\\mu_{W}^2,\\sigma_{W}^2)\\)"),
         helpText("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) ")
),

conditionalPanel(
condition = "input.Model == '3'",
         helpText("Model 3: Effect of a time-varying predictor (random slope)"),
         helpText("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
         helpText("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\nu_{0i} \\)"),
         helpText("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\nu_{1i} \\)"),
         helpText("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) ")
),

conditionalPanel(
condition = "input.Model == '4'",
         helpText("Model 4: Effect of a time-varying predictor (fixed slope)"),
         helpText("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
         helpText("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\nu_{0i} \\)"),
         helpText("Level 2: \\(\\gamma_{1i} = \\beta_{10} \\)"),
         helpText("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) ")
),

conditionalPanel(
condition = "input.Model == '5'",
         helpText("Model 5: Difference in the effect of a time-varying predictor between two groups (random slope)"),
         helpText("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
         helpText("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}Z_i + \\nu_{0i} \\)"),
         helpText("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}Z_i + \\nu_{1i} \\)"),
         helpText("\\( Z_i\\) is a dummy variable equal to one if participant is in Group 1 and 0 otherwise"),
         helpText("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) ")
),

conditionalPanel(
condition = "input.Model == '6'",
         helpText("Model 6: Difference in the effect of a time-varying predictor between two groups (fixed slope)"),
         helpText("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
         helpText("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}Z_i + \\nu_{0i} \\)"),
         helpText("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}Z_i  \\)"),
         helpText("\\( Z_i\\) is a dummy variable equal to one if participant is in Group 1 and 0 otherwise"),
         helpText("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) ")
),

conditionalPanel(
condition = "input.Model == '7'",
         helpText("Model 7: Cross-level interaction effect between a level-2 continuous predictor and a time-varying predictor (random slope)"),
         helpText("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
         helpText("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}W_i + \\nu_{0i} \\)"),
         helpText("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}W_i + \\nu_{1i} \\)"),
         helpText("\\( W_i\\) is Gausssian distributed \\(N(\\mu_{W}^2,\\sigma_{W}^2)\\)"),
         helpText("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) ")
),

conditionalPanel(
condition = "input.Model == '8'",
         helpText("Model 8: Cross-level interaction effect between a level-2 continuous predictor and a time-varying predictor (fixed slope)"),
         helpText("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
         helpText("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}W_i + \\nu_{0i} \\)"),
         helpText("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}W_i  \\)"),
         helpText("\\( W_i\\) is Gausssian distributed \\(N(\\mu_{W}^2,\\sigma_{W}^2)\\)"),
         helpText("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) ")
),

conditionalPanel(
condition = "input.Model == '9'",
         helpText("Model 9: Multilevel AR(1) Model"),
         helpText("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}Y_{it-1} + \\epsilon_{it} \\)"), 
         helpText("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\nu_{0i} \\)"),
         helpText("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\nu_{1i} \\)"),
         helpText("Independent errors \\( \\epsilon_{it}\\) are Gausssian distributed \\(N(0,\\sigma_{\\epsilon}^2)\\) ")
),

conditionalPanel(
condition = "input.Model == '10'",
         helpText("Model 10: Multilevel AR(1) model with differences in the autoregressive effect between two groups"),
         helpText("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}Y_{it-1} + \\epsilon_{it} \\)"), 
         helpText("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}Z_i + \\nu_{0i} \\)"),
         helpText("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}Z_i + \\nu_{1i} \\)"),
         helpText("\\( Z_i\\) is a dummy variable equal to one if participant is in Group 1 and 0 otherwise"),
         helpText("Independent errors \\( \\epsilon_{it}\\) are Gausssian distributed \\(N(0,\\sigma_{\\epsilon}^2)\\) ")
),

conditionalPanel(
condition = "input.Model == '11'",
         helpText("Model 11: Multilevel AR(1) Model with cross-level interactions"),
         helpText("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}Y_{it-1} + \\epsilon_{it} \\)"), 
         helpText("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}W_i + \\nu_{0i} \\)"),
         helpText("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}W_i + \\nu_{1i} \\)"),
         helpText("\\( W_i\\) is Gausssian distributed \\(N(\\mu_{W}^2,\\sigma_{W}^2)\\)"),
         helpText("Independent errors \\( \\epsilon_{it}\\) are Gausssian distributed \\(N(0,\\sigma_{\\epsilon}^2)\\) ")
),


conditionalPanel(
condition = "input.Model == '2' || input.Model == '3' || input.Model == '4' || input.Model == '7' || 
    input.Model == '8' || input.Model == '9' || input.Model == '11'", 
    helpText("Number of participants: introduce an increasing sequence of positive integers (comma-separated)."),
    textInput("N","Number of participants", NULL)
),

conditionalPanel(
condition = "input.Model == '1' || input.Model == '5' || input.Model == '6' || input.Model == '10'", 
    helpText("Number of participants: introduce an increasing sequence of positive integers (comma-separated). The length of the sequence must be the same in the two groups."),
    textInput("N.0","Number of participants in Group 0 (reference group)", NULL),
    textInput("N.1","Number of participants in Group 1", NULL)
),

    numericInput("n.days", "Number of days:", NULL), 
    numericInput("n.beeps", "Number of beeps per day:", NULL),

    numericInput("b00", "Fixed intercept: \\( \\beta_{00} \\)", NULL), 

conditionalPanel(
condition = "input.Model == '1' || input.Model == '5' || input.Model == '6' || input.Model == '10'", 
    numericInput("b01.Z", "Effect of the level-2 dummy variable on the level-1 intercept: \\( \\beta_{01} \\)", NULL)
),

conditionalPanel(
condition = "input.Model == '2' || input.Model == '7' || input.Model == '8' || input.Model == '11'", 
    numericInput("b01.W", "Effect of the level-2 continuous variable on the level-1 intercept: \\( \\beta_{01} \\)", NULL)
),

conditionalPanel(
condition = "input.Model == '3' || input.Model == '4' || input.Model == '5' || input.Model == '6' || input.Model == '7' || 
    input.Model == '8' || input.Model == '9' || input.Model == '10' || input.Model == '11'", 
    numericInput("b10", "Fixed slope: \\( \\beta_{10} \\)", NULL)
),

conditionalPanel(
condition = "input.Model == '5' || input.Model == '6' || input.Model == '10'", 
    numericInput("b11.Z", "Effect of the level-2 dummy variable on the level-1 slope: \\( \\beta_{11} \\)", NULL)
),

conditionalPanel(
condition = "input.Model == '7' || input.Model == '8' || input.Model == '11'", 
    numericInput("b11.W", "Effect of the level-2 continuous variable on the level-1 slope: \\( \\beta_{11} \\)", NULL)
),

   numericInput("sigma", "Standard deviation of level-1 error: \\( \\sigma_\\epsilon \\)", NULL), 

conditionalPanel(
condition = "input.Model == '1' || input.Model == '2' || input.Model == '3' || input.Model == '4' || input.Model == '5' || input.Model == '6' ||
    input.Model == '7' || input.Model == '8'", 
    numericInput("rho", "Autocorrelation of level-1 error: \\( \\rho_\\epsilon \\)", NULL)
),
 
   numericInput("sigma.v0", "Standard deviation of random intercept: \\( Var(\\nu_{0i})=\\sigma_{\\nu_0}^2 \\)", NULL),

conditionalPanel(
condition = "input.Model == '3' || input.Model == '5' || input.Model == '7' || input.Model == '9' || input.Model == '10' || input.Model == '11'",  
    numericInput("sigma.v1", "Standard deviation of random slope: \\(  Var(\\nu_{1i})=\\sigma_{\\nu_1}^2 \\)", NULL), 
    numericInput("rho.v", "Correlation between the random intercept and random slope: \\( Cor(\\nu_{0i},\\nu_{1i}) = \\rho_{\\nu_{01}} \\)", NULL)
),

conditionalPanel(
condition = "input.Model == '3' || input.Model == '4' || input.Model == '7' || input.Model == '8'", 
    numericInput("mu.X", "Mean of time-varying variable X:", NULL), 
    numericInput("sigma.X", "Standard deviation of time-varying variable X:", NULL) 
),

conditionalPanel(
condition = "input.Model == '5' || input.Model == '6'", 
    numericInput("mu.X0", "Mean of X in Group 0:", NULL), 
    numericInput("sigma.X0", "Standard deviation of X in Group 0:", NULL),
    numericInput("mu.X1", "Mean of X in Group 1:", NULL), 
    numericInput("sigma.X1", "Standard deviation of X in Group 1:", NULL) 
),

conditionalPanel(
condition = "input.Model == '2' || input.Model == '7' || input.Model == '8' || input.Model == '11'", 
    numericInput("mu.W", "Mean of time-invarying variable W:", NULL), 
    numericInput("sigma.W", "Standard deviation of time-invarying variable W:", NULL) 
),

conditionalPanel(
condition = "input.Model == '1'", 
    checkboxInput(inputId = "is.rho.zero", label = strong("Estimate AR(1) correlated errors \\(\\epsilon_{it} \\)"), value = TRUE),  
    numericInput("alpha", "Type I error: \\( \\alpha \\)", 0.05), 
    numericInput("R", "Monte Carlo Replicates", 1000)
),

conditionalPanel(
condition = "input.Model == '2'", 
    checkboxInput(inputId = "isW.center", label = strong("Centering time-invariant variable \\(W_{i} \\)"), value = TRUE),  
    checkboxInput(inputId = "is.rho.zero", label = strong("Estimate AR(1) correlated errors \\(\\epsilon_{it} \\)"), value = TRUE),  
    numericInput("alpha", "Type I error: \\( \\alpha \\)", 0.05), 
    numericInput("R", "Monte Carlo Replicates", 1000)
),

conditionalPanel(
condition = "input.Model == '3'", 
    checkboxInput(inputId = "isX.center", label = strong("Person mean centering \\(X_{it} \\) using the individual mean"), value = TRUE),  
    checkboxInput(inputId = "is.rho.zero", label = strong("Estimate AR(1) correlated errors \\(\\epsilon_{it} \\)"), value = TRUE),  
    numericInput("alpha", "Type I error: \\( \\alpha \\)", 0.05), 
    numericInput("R", "Monte Carlo Replicates", 1000)
),

conditionalPanel(
condition = "input.Model == '4'", 
    checkboxInput(inputId = "isX.center", label = strong("Person mean centering \\(X_{it} \\) using the individual mean"), value = TRUE),  
    checkboxInput(inputId = "is.rho.zero", label = strong("Estimate AR(1) correlated errors \\(\\epsilon_{it} \\)"), value = TRUE),  
    numericInput("alpha", "Type I error: \\( \\alpha \\)", 0.05), 
    numericInput("R", "Monte Carlo Replicates", 1000)
),

conditionalPanel(
condition = "input.Model == '5'", 
    checkboxInput(inputId = "isX.center", label = strong("Person mean centering \\(X_{it} \\) using the individual mean"), value = TRUE),  
    checkboxInput(inputId = "is.rho.zero", label = strong("Estimate AR(1) correlated errors \\(\\epsilon_{it} \\)"), value = TRUE),  
    numericInput("alpha", "Type I error: \\( \\alpha \\)", 0.05), 
    numericInput("R", "Monte Carlo Replicates", 1000)
),

conditionalPanel(
condition = "input.Model == '6'", 
    checkboxInput(inputId = "isX.center", label = strong("Person mean centering \\(X_{it} \\) using the individual mean"), value = TRUE),  
    checkboxInput(inputId = "is.rho.zero", label = strong("Estimate AR(1) correlated errors \\(\\epsilon_{it} \\)"), value = TRUE),  
    numericInput("alpha", "Type I error: \\( \\alpha \\)", 0.05), 
    numericInput("R", "Monte Carlo Replicates", 1000)
),

conditionalPanel(
condition = "input.Model == '7'", 
    checkboxInput(inputId = "isX.center", label = strong("Person mean centering \\(X_{it} \\) using the individual mean"), value = TRUE),  
    checkboxInput(inputId = "isW.center", label = strong("Centering time-invariant variable \\(W_{i} \\)"), value = TRUE),  
    checkboxInput(inputId = "is.rho.zero", label = strong("Estimate AR(1) correlated errors \\(\\epsilon_{it} \\)"), value = TRUE),  
    numericInput("alpha", "Type I error: \\( \\alpha \\)", 0.05), 
    numericInput("R", "Monte Carlo Replicates", 1000)
),

conditionalPanel(
condition = "input.Model == '8'", 
    checkboxInput(inputId = "isX.center", label = strong("Person mean centering \\(X_{it} \\) using the individual mean"), value = TRUE),  
    checkboxInput(inputId = "isW.center", label = strong("Centering time-invariant variable \\(W_{i} \\)"), value = TRUE),  
    checkboxInput(inputId = "is.rho.zero", label = strong("Estimate AR(1) correlated errors \\(\\epsilon_{it} \\)"), value = TRUE),  
    numericInput("alpha", "Type I error: \\( \\alpha \\)", 0.05), 
    numericInput("R", "Monte Carlo Replicates", 1000)
),

conditionalPanel(
condition = "input.Model == '9'", 
    checkboxInput(inputId = "Ylag.center", label = strong("Person mean centering \\(Y_{it-1} \\)"), value = FALSE),  
    numericInput("alpha", "Type I error: \\( \\alpha \\)", 0.05), 
    numericInput("R", "Monte Carlo Replicates", 1000)
),

conditionalPanel(
condition = "input.Model == '10'", 
    checkboxInput(inputId = "Ylag.center", label = strong("Person mean centering \\(Y_{it-1} \\)"), value = TRUE),  
    numericInput("alpha", "Type I error: \\( \\alpha \\)", 0.05), 
    numericInput("R", "Monte Carlo Replicates", 1000)
),

conditionalPanel(
condition = "input.Model == '11'", 
    checkboxInput(inputId = "isW.center", label = strong("Centering time-invariant variable \\(W_{i} \\)"), value = TRUE),  
    checkboxInput(inputId = "Ylag.center", label = strong("Person mean centering \\(Y_{it-1} \\)"), value = TRUE),  
    numericInput("alpha", "Type I error: \\( \\alpha \\)", 0.05), 
    numericInput("R", "Monte Carlo Replicates", 1000)
),

    selectInput(inputId = "Opt.Method",
                  label = "Choose the method to fit linear mixed-effects model",
                  choices = c("Maximizing the log-likelihood"=1, 
                              "Maximizing the restricted log-likelihood"=2)),

    actionButton(inputId = "input_action", label = "Compute Power"),

    actionButton("reset_button", "Reset Page"),


    helpText("Note:",
             "To switch models and set new parameters click the Reset Page button."),

    helpText("Contact: ginette.lafit@kuleuven.be")

),


  mainPanel(
    tabsetPanel(

      tabPanel("Power Analysis", 
      tags$h4("Power Analysis"),
      plotOutput("powerplot",height = "1000px")), 

      tabPanel("Summary Fixed Effects", 
      tags$h4("Summary Fixed Effects"),
      tags$h6("Note:"), 
      tags$h6("Mean is the average of the estimated parameter over the Monte Carlo replicates"),
      tags$h6("Std.error is the standard error of the estimated parameter over the Monte Carlo replicates"),
      tags$h6("Bias is the average of the difference between the estimated parameter and true parameter over the Monte Carlo replicates"),
      tags$h6("(1-alpha)% Coverage is the average of the (1-alpha)% confidence intervals that include the true parameter over the Monte Carlo replicates"),
      tags$h6("Power is the number of times the null hypothesis is rejected over the Monte Carlo replicates"),

      formattableOutput("power")
      ),

      tabPanel("Summary Random Effects", 
      tags$h4("Summary Random Effects"),
      tags$h6("Note:"), 
      tags$h6("Mean is the average of the estimated parameter over the Monte Carlo replicates"),
      tags$h6("Std.error is the standard error of the estimated parameter over the Monte Carlo replicates"),
      tags$h6("Bias is the average of the difference between the estimated parameter and true parameter over the Monte Carlo replicates"),
      formattableOutput("covariance")
      ),

     
      tabPanel("Monte Carlo Simulation", 
      tags$h4("Summary Monte Carlo Simulation"),
      plotOutput("gmplot",height = "1000px"),
      tags$h6("Note:"), 
      tags$h6("The distributions of the estimated parameters correspond to the case which the largest sample size"),
      tags$h6("Dashed lines are the true model parameters")), 

      tabPanel("Y Trajectories",
      tags$h4("Simulated trajectory of the outcome variable"),
      plotOutput("yplot05"),plotOutput("yplot5"),plotOutput("yplot95"),
      tags$h6("Note:"), 
      tags$h6("The trajectories of the outcome variable correspond to the case which the largest sample size"),
      tags$h6("The plots show the simulated trajectory of the outcome variable for three different 
      participants: the participant in the 5% percentile, the participant in the 50% percentile and
      the participant in the 75% percentile.")),

      tabPanel("About the Method",

      tags$h4("Simulation Approach to Estimate Power in Multilevel Linear Models"),
      tags$p("Algorithm:"),
      tags$p("1. Given a model based on the hypothesized theory, set up the population parameters. The parameter values can be decided from previous studies or a pilot study."),
      tags$p("2. Set the sample size and generate a data set based on the model and its population parameters."),
      tags$p("3. Test the signifficance of the null hypothesis (i.e. the hypothesized effect is zero) using the generated data with a Wald test."),
      tags$p("4. Repeat steps 2 and 3 for R times, where R is the number of Monte Carlo replications."),
      tags$p("5. Compute the power which is the number of times the hypothesized effect is signifficant over the Monte Carlo replications."),

      tags$h4("-------------------------------------"),

      tags$h4("Model 1: Mean differences between two groups"),
      tags$p("Level 1: \\(Y_{it} = \\gamma_{0i} + \\epsilon_{it} \\)"), 
      tags$p("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}Z_i + \\nu_{0i} \\)"),

      tags$p("Variables:"),
      tags$p("\\( Y_{it}\\) is the dependent variable measured at each beep"), 
      tags$p("\\( Z_i\\) is a dummy variable equal to one if participant is in Group 1 and 0 otherwise"),
      tags$p("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) "),
      tags$p("Random intercept \\( \\nu_{0i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_0}^2)\\)"),
      tags$p("\\(N_0\\) is the number of individuals in the reference group (i.e. Group 0)"),
      tags$p("\\(N_1\\) is the number of individuals in Group 1"),
      tags$p("\\(n_\\text{days}\\) is the total number of days in which the ESM study is conducted"),
      tags$p("\\(n_\\text{beeps}\\) is the total number of beeps per day"),
      tags$p("\\(T \\) is the total number of repeated measurements per indidividual: \\(T = n_\\text{days} \\times n_\\text{days}\\)"),

      tags$h4("-------------------------------------"),

      tags$h4("Model 2: Effect of a level-2 continuous predictor on the outcome variable"),
      tags$p("Level 1: \\(Y_{it} = \\gamma_{0i} + \\epsilon_{it} \\)"), 
      tags$p("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}W_i + \\nu_{0i} \\)"),

      tags$p("Variables:"),
      tags$p("\\( Y_{it}\\) is the dependent variable measured at each beep"), 
      tags$p("\\( W_i\\) is Gausssian distributed \\(N(\\mu_{W}^2,\\sigma_{W}^2)\\)"),
      tags$p("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) "),
      tags$p("Random intercept \\( \\nu_{0i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_0}^2)\\)"),
      tags$p("\\(N \\) is the total number of individuals"),
      tags$p("\\(n_\\text{days}\\) is the total number of days in which the ESM study is conducted"),
      tags$p("\\(n_\\text{beeps}\\) is the total number of beeps per day"),
      tags$p("\\(T \\) is the total number of repeated measurements per indidividual: \\(T = n_\\text{days} \\times n_\\text{days}\\)"),

      tags$h4("-------------------------------------"),

      tags$h4("Model 3: Effect of a time-varying predictor (random slope)"),
      tags$p("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
      tags$p("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\nu_{0i} \\)"),
      tags$p("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\nu_{1i} \\)"),

      tags$p("Variables:"),
      tags$p("\\( Y_{it}\\) is the dependent variable measured at each beep"), 
      tags$p("\\( X_{it}\\) is the explanatory variable measured at each beep which is assumed that is \\(N(\\mu_X,\\sigma_X^2)\\)"),
      tags$p("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) "),
      tags$p("Random intercept \\( \\nu_{0i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_0}^2)\\)"),
      tags$p("Random slope \\( \\nu_{1i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_1}^2)\\)"),
      tags$p("Correlation between the random intercept and random slope: \\( Cor(\\nu_{0i},\\nu_{1i}) = \\rho_{\\nu_{01}} \\)"),
      tags$p("\\(N \\) is the total number of individuals"),
      tags$p("\\(n_\\text{days}\\) is the total number of days in which the ESM study is conducted"),
      tags$p("\\(n_\\text{beeps}\\) is the total number of beeps per day"),
      tags$p("\\(T \\) is the total number of repeated measurements per indidividual: \\(T = n_\\text{days} \\times n_\\text{days}\\)"),

      tags$h4("-------------------------------------"),

      tags$h4("Model 4: Effect of a time-varying predictor (fixed slope)"),
      tags$p("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
      tags$p("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\nu_{0i} \\)"),
      tags$p("Level 2: \\(\\gamma_{1i} = \\beta_{10}  \\)"),

      tags$p("Variables:"),
      tags$p("\\( Y_{it}\\) is the dependent variable measured at each beep"), 
      tags$p("\\( X_{it}\\) is the explanatory variable measured at each beep which is assumed that is \\(N(\\mu_X,\\sigma_X^2)\\)"),
      tags$p("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) "),
      tags$p("Random intercept \\( \\nu_{0i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_0}^2)\\)"),
      tags$p("\\(N \\) is the total number of individuals"),
      tags$p("\\(n_\\text{days}\\) is the total number of days in which the ESM study is conducted"),
      tags$p("\\(n_\\text{beeps}\\) is the total number of beeps per day"),
      tags$p("\\(T \\) is the total number of repeated measurements per indidividual: \\(T = n_\\text{days} \\times n_\\text{days}\\)"),

      tags$h4("-------------------------------------"),

      tags$h4("Model 5: Difference in the effect of a time-varying predictor between two groups (random slope)"),
      tags$p("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
      tags$p("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}Z_i + \\nu_{0i} \\)"),
      tags$p("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}Z_i + \\nu_{1i} \\)"),

      tags$p("Variables:"),
      tags$p("\\( Y_{it}\\) is the dependent variable measured at each beep"), 
      tags$p("\\( X_{0it}\\) is the explanatory variable of individuals in the reference group (i.e. Group 0) measured at each beep which is assumed that is \\(N(\\mu_{X_0},\\sigma_{X_0}^2)\\)"),
      tags$p("\\( X_{1it}\\) is the explanatory variable of individuals in Group 1 measured at each beep which is assumed that is \\(N(\\mu_{X_1},\\sigma_{X_1}^2)\\)"), 
      tags$p("\\( Z_i\\) is a dummy variable equal to one if participant is in Group 1 and 0 otherwise"),
      tags$p("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) "),
      tags$p("Random intercept \\( \\nu_{0i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_0}^2)\\)"),
      tags$p("Random slope \\( \\nu_{1i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_1}^2)\\)"),
      tags$p("Correlation between the random intercept and random slope: \\( Cor(\\nu_{0i},\\nu_{1i}) = \\rho_{\\nu_{01}} \\)"),
      tags$p("\\(N_0\\) is the number of individuals in the reference group (i.e. Group 0)"),
      tags$p("\\(N_1\\) is the number of individuals in Group 1"),
      tags$p("\\(n_\\text{days}\\) is the total number of days in which the ESM study is conducted"),
      tags$p("\\(n_\\text{beeps}\\) is the total number of beeps per day"),
      tags$p("\\(T \\) is the total number of repeated measurements per indidividual: \\(T = n_\\text{days} \\times n_\\text{days}\\)"), 

      tags$h4("-------------------------------------"),

      tags$h4("Model 6: Difference in the effect of a time-varying predictor between two groups (fixed slope)"),
      tags$p("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
      tags$p("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}Z_i + \\nu_{0i} \\)"),
      tags$p("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}Z_i  \\)"),
 
      tags$p("Variables:"),
      tags$p("\\( Y_{it}\\) is the dependent variable measured at each beep"), 
      tags$p("\\( X_{0it}\\) is the explanatory variable of individuals in the reference group (i.e. Group 0) measured at each beep which is assumed that is \\(N(\\mu_{X_0},\\sigma_{X_0}^2)\\)"),
      tags$p("\\( X_{1it}\\) is the explanatory variable of individuals in Group 1 measured at each beep which is assumed that is \\(N(\\mu_{X_1},\\sigma_{X_1}^2)\\)"), 
      tags$p("\\( Z_i\\) is a dummy variable equal to one if participant is in Group 1 and 0 otherwise"),
      tags$p("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) "),
      tags$p("Random intercept \\( \\nu_{0i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_0}^2)\\)"),
      tags$p("\\(N_0\\) is the number of individuals in the reference group (i.e. Group 0)"),
      tags$p("\\(N_1\\) is the number of individuals in Group 1"),
      tags$p("\\(n_\\text{days}\\) is the total number of days in which the ESM study is conducted"),
      tags$p("\\(n_\\text{beeps}\\) is the total number of beeps per day"),
      tags$p("\\(T \\) is the total number of repeated measurements per indidividual: \\(T = n_\\text{days} \\times n_\\text{days}\\)"),

      tags$h4("-------------------------------------"),

      tags$h4("Model 7: Cross-level interaction effect between a level-2 continuous predictor and a time-varying predictor (random slope)"),
      tags$p("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
      tags$p("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}W_i + \\nu_{0i} \\)"),
      tags$p("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}W_i + \\nu_{1i} \\)"),

      tags$p("Variables:"),
      tags$p("\\( Y_{it}\\) is the dependent variable measured at each beep"), 
      tags$p("\\( X_{it}\\) is the explanatory variable measured at each beep which is assumed that is \\(N(\\mu_X,\\sigma_X^2)\\)"),
      tags$p("\\( W_i\\) is Gausssian distributed \\(N(\\mu_{W}^2,\\sigma_{W}^2)\\)"),
      tags$p("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) "),
      tags$p("Random intercept \\( \\nu_{0i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_0}^2)\\)"),
      tags$p("Random slope \\( \\nu_{1i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_1}^2)\\)"),
      tags$p("Correlation between the random intercept and random slope: \\( Cor(\\nu_{0i},\\nu_{1i}) = \\rho_{\\nu_{01}} \\)"),
      tags$p("\\(N \\) is the total number of individuals"),
      tags$p("\\(n_\\text{days}\\) is the total number of days in which the ESM study is conducted"),
      tags$p("\\(n_\\text{beeps}\\) is the total number of beeps per day"),
      tags$p("\\(T \\) is the total number of repeated measurements per indidividual: \\(T = n_\\text{days} \\times n_\\text{days}\\)"), 

      tags$h4("-------------------------------------"),

      tags$h4("Model 8: Cross-level interaction effect between a level-2 continuous predictor and a time-varying predictor (fixed slope)"),
      tags$p("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}X_{it} + \\epsilon_{it} \\)"), 
      tags$p("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}W_i + \\nu_{0i} \\)"),
      tags$p("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}W_i  \\)"),
 
      tags$p("Variables:"),
      tags$p("\\( Y_{it}\\) is the dependent variable measured at each beep"), 
      tags$p("\\( X_{it}\\) is the explanatory variable measured at each beep which is assumed that is \\(N(\\mu_X,\\sigma_X^2)\\)"),
      tags$p("\\( W_i\\) is Gausssian distributed \\(N(\\mu_{W}^2,\\sigma_{W}^2)\\)"),
      tags$p("AR(1) errors \\( \\epsilon_{it}\\) with autocorrelation \\( \\rho_{\\epsilon}\\) and variance \\( \\sigma_{\\epsilon}^2\\) "),
      tags$p("Random intercept \\( \\nu_{0i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_0}^2)\\)"),
      tags$p("\\(N \\) is the total number of individuals"),
      tags$p("\\(n_\\text{days}\\) is the total number of days in which the ESM study is conducted"),
      tags$p("\\(n_\\text{beeps}\\) is the total number of beeps per day"),
      tags$p("\\(T \\) is the total number of repeated measurements per indidividual: \\(T = n_\\text{days} \\times n_\\text{days}\\)"), 

      tags$h4("-------------------------------------"),

      tags$h4("Model 9: Multilevel AR(1) Model"),
      tags$p("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}Y_{it-1} + \\epsilon_{it} \\)"), 
      tags$p("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\nu_{0i} \\)"),
      tags$p("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\nu_{1i} \\)"),

      tags$p("Variables:"),
      tags$p("\\( Y_{it}\\) is the dependent variable measured at each beep"), 
      tags$p("\\( Y_{it-1}\\) is the dependent variable lag within person and within days"), 
      tags$p("Independent errors \\( \\epsilon_{it}\\) are Gausssian distributed \\(N(0,\\sigma_{\\epsilon}^2)\\) "),
      tags$p("Random intercept \\( \\nu_{0i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_0}^2)\\)"),
      tags$p("Random slope \\( \\nu_{1i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_1}^2)\\)"),
      tags$p("Correlation between the random intercept and random slope: \\( Cor(\\nu_{0i},\\nu_{1i}) = \\rho_{\\nu_{01}} \\)"),
      tags$p("\\(N \\) is the total number of individuals"),
      tags$p("\\(n_\\text{days}\\) is the total number of days in which the ESM study is conducted"),
      tags$p("\\(n_\\text{beeps}\\) is the total number of beeps per day"),
      tags$p("\\(T \\) is the total number of repeated measurements per indidividual: \\(T = n_\\text{days} \\times n_\\text{days}\\)"),

      tags$h4("-------------------------------------"),

      tags$h4("Model 10: Multilevel AR(1) model with differences in the autoregressive effect between two groups"),
      tags$p("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}Y_{it-1} + \\epsilon_{it} \\)"), 
      tags$p("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}Z_i + \\nu_{0i} \\)"),
      tags$p("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}Z_i + \\nu_{1i} \\)"),

      tags$p("\\( Y_{it}\\) is the dependent variable measured at each beep"), 
      tags$p("\\( Y_{it-1}\\) is the dependent variable lag within person and within days"), 
      tags$p("\\( Z_i\\) is a dummy variable equal to one if participant is in Group 1 and 0 otherwise"),
      tags$p("Independent errors \\( \\epsilon_{it}\\) are Gausssian distributed \\(N(0,\\sigma_{\\epsilon}^2)\\) "),
      tags$p("Random intercept \\( \\nu_{0i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_0}^2)\\)"),
      tags$p("Random slope \\( \\nu_{1i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_1}^2)\\)"),
      tags$p("Correlation between the random intercept and random slope: \\( Cor(\\nu_{0i},\\nu_{1i}) = \\rho_{\\nu_{01}} \\)"),
      tags$p("\\(N_0\\) is the number of individuals in the reference group (i.e. Group 0)"),
      tags$p("\\(N_1\\) is the number of individuals in Group 1"),
      tags$p("\\(n_\\text{days}\\) is the total number of days in which the ESM study is conducted"),
      tags$p("\\(n_\\text{beeps}\\) is the total number of beeps per day"),
      tags$p("\\(T \\) is the total number of repeated measurements per indidividual: \\(T = n_\\text{days} \\times n_\\text{days}\\)"),

      tags$h4("-------------------------------------"),

      tags$h4("Model 11: Multilevel AR(1) Model with cross-level interactions"),
      tags$p("Level 1: \\(Y_{it} = \\gamma_{0i} + \\gamma_{1i}Y_{it-1} + \\epsilon_{it} \\)"), 
      tags$p("Level 2: \\(\\gamma_{0i} = \\beta_{00} + \\beta_{01}W_i + \\nu_{0i} \\)"),
      tags$p("Level 2: \\(\\gamma_{1i} = \\beta_{10} + \\beta_{11}W_i + \\nu_{1i} \\)"),

      tags$p("\\( Y_{it}\\) is the dependent variable measured at each beep"), 
      tags$p("\\( Y_{it-1}\\) is the dependent variable lag within person and within days"), 
      tags$p("\\( W_i\\) is \\(N(\\mu_{W}^2,\\sigma_{W}^2)\\)"),
      tags$p("Independent errors \\( \\epsilon_{it}\\) are Gausssian distributed \\(N(0,\\sigma_{\\epsilon}^2)\\) "),
      tags$p("Random intercept \\( \\nu_{0i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_0}^2)\\)"),
      tags$p("Random slope \\( \\nu_{1i}\\) is Gausssian distributed \\(N(0,\\sigma_{\\nu_1}^2)\\)"),
      tags$p("Correlation between the random intercept and random slope: \\( Cor(\\nu_{0i},\\nu_{1i}) = \\rho_{\\nu_{01}} \\)"),
      tags$p("\\(N \\) is the total number of individuals"),
      tags$p("\\(n_\\text{days}\\) is the total number of days in which the ESM study is conducted"),
      tags$p("\\(n_\\text{beeps}\\) is the total number of beeps per day"),
      tags$p("\\(T \\) is the total number of repeated measurements per indidividual: \\(T = n_\\text{days} \\times n_\\text{days}\\)"),

      tags$h4("-------------------------------------"),

      tags$h4("R Syntax"),
      tags$p("Model 1: lme(Y ~ Z ,random = ~1 | subjno,data,correlation = corAR1())"),
      tags$p("Model 2: lme(Y ~ W ,random = ~1 | subjno,data,correlation = corAR1())"),
      tags$p("Model 3: lme(Y ~ X ,random = ~1 + X | subjno,data,correlation = corAR1())"),
      tags$p("Model 4: lme(Y ~ X ,random = ~1 | subjno,data,correlation = corAR1())"),
      tags$p("Model 5: lme(Y ~ Z + X + Z*X ,random = ~1 + X | subjno,data,correlation = corAR1())"),
      tags$p("Model 6: lme(Y ~ Z + X + Z*X ,random = ~1 | subjno,data,correlation = corAR1())"),
      tags$p("Model 7: lme(Y ~ W + X + W*X ,random = ~1 + X | subjno,data,correlation = corAR1())"),
      tags$p("Model 8: lme(Y ~ W + X + W*X ,random = ~1 | subjno,data,correlation = corAR1())"),
      tags$p("Model 9: lme(Y ~ lag(Y) ,random = ~1 + lag(Y) | subjno,data)"),
      tags$p("Model 10: lme(Y ~ Z + lag(Y) + Z*lag(Y) ,random = ~1 + lag(Y) | subjno,data)"),
      tags$p("Model 11: lme(Y ~ W + lag(Y) + W*lag(Y) ,random = ~1 + lag(Y) | subjno,data)"))

    ))


))