
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
shinyUI(navbarPage("Permutation Test Tool",
                   tabPanel("About",
                            mainPanel(
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"),
                              #hack
                              h1("Permutation Test Tool"),
                              p("This tool allows you to test whether there is a significant difference in means between two groups of data (i.e. comparing the number of conversions per month between two clients)."),
                              p("The permutation test works by comparing the observed result to results expected by random chance.
                                This is done by randomly swapping the samples between the groups, and calculating how many times we would see a more extreme difference in means than the observed difference."),
                              p("If we often see that we could obtain a larger difference in means in random permutations then the observed difference is likely to be insignificant.
                                On the other hand, if we see that all other random permutations of the groups result in a less extreme difference in means, 
                                then the observed difference would definitely be significant (assuming we test enough permutations)."),
                              h3("Example"),
                            p("A small example is shown below using randomly generated data:"),
                            fluidRow(column(width=6,
                            sliderInput("meanx1",
                                        "Mean of x1:",
                                        min = -5,
                                        max = 5,
                                        value = 0,
                                        step=0.1)),
                            column(width=6,
                            sliderInput("meanx2",
                                        "Mean of x2:",
                                        min = -5,
                                        max = 5,
                                        value = 0,
                                        step=0.1))),hr(),
                            fluidRow(column(width=6,
                            sliderInput("stdx1",
                                        "Standard Deviation of x1:",
                                        min = 0.5,
                                        max = 5,
                                        value = 1,
                                        step=0.1)),
                            column(width=6,
                            sliderInput("stdx2",
                                        "Standard Deviation of x2:",
                                        min = 0.5,
                                        max = 5,
                                        value = 1,
                                        step=0.1))),
                              dataTableOutput('example'),
                            #code("x1 = [2,5,6]"),br(),
                            #code("x2=[3,4,1]"),
                            htmlOutput("examplemeans"),
                            #p("The observed difference in means between the groups is 1.67."),
                            p("We then simulate random permutations of the groups and calculate the difference in the means of these permutations:"),
                            dataTableOutput('example2'),
                            p("We can vizualise this as below, and calculate a p-value to determine if the difference is significant:"),
                            plotOutput('exampleplot'),
                            htmlOutput("exouttext")
                            
                            
                              )),
                            
  tabPanel("Data Upload",
           sidebarLayout(
             sidebarPanel(
               h4('Choose CSV File'),
               p("Your file should have two columns of data: one column titled 'group' with the name of the group that the data point belongs to, and one labelled 'value' with the values of the data points."),
               fileInput('datafile', label = NULL),
               sliderInput("bins",
                           "Bin width:",
                           min = 0.1,
                           max = 1,
                           value = 0.2,
                           step=0.01)
             ),

             
             # Main panel
             mainPanel(
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"),
               #hack
               plotOutput('distPlot'),
               plotOutput('rawDataPlot'),
               dataTableOutput('contents')
             )
           )),
  #?br
  tabPanel("Permutation Test",
  # Application title
  #titlePanel("Old Faithful Geyser Data"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h4("How does it work?"),
      p("The Permutation Test estimates whether the difference between the means of two sample distributions is statistically significant or not (i.e. are the distributions significantly different)."),
      p("It does this by calculating the differences between the means for all possible permutations of the observations between the groups, and then calculating the proportion of times the observed difference is more extreme."),
      p("If the observered difference is often more extreme (i.e. more extreme than most possible differences) then it will be significant."),
      sliderInput("nperms",
                  "Number of permutations:",
                  min = 100,
                  max = 10000,
                  value = 2000,
                  step=100),
      actionButton("goButton", strong("Run Permutation Test")),
      br(),
      br(),
      tags$button("Toggle advanced options",id="advtoggle",onClick="buttonClick()"),
      div(id="advanced", style="display: none",
      radioButtons("hypothesis", "Alternative hypothesis:", c("twosided" = "twosided", "greater"="greater", "less"="less"), selected = "twotailed", inline = FALSE),
      

      sliderInput("binwidth2",
                  "Bin width:",
                  min = 0.001,
                  max = 0.5,
                  value = 0.05,
                  step=0.001)
      
      ),
      includeHTML("script.js")
    ),

    # Show a plot of the generated distribution
    
    
    
    #conditionalPanel(condition="$('html').hasClass('shiny-busy')", img(src="loading.gif"),class="center"),
    
    mainPanel(
      #conditionalPanel(condition="$('html').hasClass('shiny-busy')",includeHTML("www/loading.html"),class="center"),
      
      htmlOutput("outtext"),
      plotOutput('finalPlot')
      #h3("t-test output")
      #verbatimTextOutput("tout")
      
      
    )
  )
  )
))
