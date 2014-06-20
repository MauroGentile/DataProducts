


shinyUI( 
  pageWithSidebar(
    headerPanel("Percentile calculator"),
    sidebarPanel(
      
      h5('distribution'),
      selectInput("distr", label = h6(""), 
                  choices = list("Gaussian" = 1, "t-Student" = 2,
                                 "F-distribution" = 3, 
                                 "Chi-squared" = 4), selected = 1),
      
      conditionalPanel(
        condition = "input.distr==1",
        h5('upper limit of visualization range'), 
        sliderInput("lim", label = h6(""),
                    min = 0, max = 20, value = 5, step=1),
        h5('mean'),
        numericInput("mean", 
                     label = h6(""), 
                     value = 0, step=0.1),
        h5('standard deviation'),
        numericInput("sd", 
                     label = h6(""), 
                     value = 1, step=0.01),
        h5('area of interest'),
        radioButtons("Tail", "",
                     choices=list("Upper tail" = 1,
                       "Lower tail" = 2,
                       "Outer range" = 3,
                       "Inner range"=4), selected=1),
        
        conditionalPanel(
          condition = "input.Tail<3",
          h5('cut-off value'),
          numericInput("Z", 
                       label = h6(""), 
                       value = 0.8, step=0.01)
         ),
      
        conditionalPanel(
          condition = "input.Tail>2",
          h5('lower bound'),
          numericInput("Z_low", 
                       label = h6(""), 
                       value = 0.8, step=0.01),
          
          h5('upper bound'),
          numericInput("Z_hig", 
                       label = h6(""), 
                       value = 2, step=0.01)
          
          )
        
        
        
        
        
        ),
    
      conditionalPanel(
        condition = "input.distr==2",
        h5('upper limit of visualization range'),
        sliderInput("lim_t", label = h6(""),
                    min = 0, max = 20, value = 3, step=1),
        h5('degree of freedom'),
        
        sliderInput("df_t", label = h6(""),
                    min = 1, max = 50, value = 1, step=1),
        
        h5('Select the range'),
        radioButtons("Tail_t", "",
                     c("Upper tail" = 1,
                       "Lower tail" = 2,
                       "Outer range" = 3,
                       "Inner range"=4)),
        
        conditionalPanel(
          condition = "input.Tail_t<3",
          h5('cut-off value'),
          numericInput("t", 
                       label = h6(""), 
                       value = 0.8, step=0.01)
        ),
        
        conditionalPanel(
          condition = "input.Tail_t>2",
          h5('lower bound'),
          numericInput("t_low", 
                       label = h6(""), 
                       value = 0.8, step=0.01),
          
          h5('upper bound'),
          numericInput("t_hig", 
                       label = h6(""), 
                       value = 2, step=0.01)
          
        )
        
        
        
        
        ),
        
        conditionalPanel(
          condition = "input.distr==3",
          h5('upper limit of visualization range'),
          sliderInput("lim_f", label = h6(""),
                      min = 0, max = 100, value = 20, step=1),
  
          h5('degree of freedom 1'),
          sliderInput("df_f_1", label = h6(""),
                      min = 0, max = 50, value = 5, step=1),
          h5('degree of freedom 2'),
          sliderInput("df_f_2", label = h6(""),
                      min = 0, max = 50, value = 5, step=1),
          
          h5('Select the range'),
          radioButtons("Tail_f", "",
                       c("Upper tail" = 1,
                         "Lower tail" = 2,
                         "Outer range" = 3,
                         "Inner range"=4)),
          
          conditionalPanel(
            condition = "input.Tail_f<3",
            h5('cut-off value'),
            numericInput("f", 
                         label = h6(""), 
                         value = 0.8, step=0.01)
          ),
          
          conditionalPanel(
            condition = "input.Tail_f>2",
            h5('lower bound'),
            numericInput("f_low", 
                         label = h6(""), 
                         value = 0.8, step=0.01, min=0),
            
            h5('upper bound'),
            numericInput("f_hig", 
                         label = h6(""), 
                         value = 2, step=0.01)
            
          )
          
          
          
          ),
        
        conditionalPanel(
        condition = "input.distr==4",
        h5('upper limit of visualization range'),
        sliderInput("lim_chi", label = h6(""),
                    min = 0, max = 100, value = 20, step=1),
        h5('degree of freedom'),
        sliderInput("df_chi", label = h6(""),
                    min = 0, max = 50, value = 5, step=1),
        h5('Select the range'),
        radioButtons("Tail_chi", "",
                     c("Upper tail" = 1,
                       "Lower tail" = 2,
                       "Outer range" = 3,
                       "Inner range"=4)),
        
        conditionalPanel(
          condition = "input.Tail_chi<3",
          h5('cut-off value'),
          numericInput("chi", 
                       label = h6(""), 
                       value = 0.8, step=0.01)
        ),
        
        conditionalPanel(
          condition = "input.Tail_chi>2",
          h5('lower bound'),
          numericInput("chi_low", 
                       label = h6(""), 
                       value = 0.8, step=0.01),
          
          h5('upper bound'),
          numericInput("chi_hig", 
                       label = h6(""), 
                       value = 2, step=0.01)
          
        )
        
      )
      

     
    
  ), 
  
  mainPanel(
  
    h3("Instructions"),
    helpText("·Choose the distribution you want to work with"), 
    helpText("·Select the upper limit of the visualization window (the lower limit will be either 0 or the inverse depending on the distribution)"),
    helpText("·Input the parmameters which define the distribution (mean, sd or the degree of freedom etc.)"), 
    helpText("·Define the area of the dsitribution your are interestd in (one of the tails, range or inner range" ),
    helpText("·Input the cut off value (or the upper and lower interval limits for Outer and Inner range)"), 
    helpText("·Check the graph and read the percentile value"),
    helpText("The source files can be found at https://github.com/MauroGentile/DataProducts"),
    h3('Distribution plot'), 
    plotOutput('newp', width = "100%"),
    
    h3('Percentile value  '), 
    verbatimTextOutput("p_value")
    
    
    
    )
    
))

