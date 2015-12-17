
                                        # This is the server logic for a Shiny web
                                        # You can find out more about building applications with Shiny
                                        # http://shiny.rstudio.
                                        #TODO - rewrite code



# detect if columns named group and value - then already in long format, no need to fix
# if in wide format, need to convert to long
#adjust parts that get data to take it in long not wide


#TODO: Fix changing to long format from wide

#add labels and label if significant
#can we calculate confidence interval?

library(shiny)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(plyr)
library(MASS)
options(shiny.maxRequestSize=250*1024^2)

shinyServer(function(input, output, clientData, session) {
  
  dnames <- reactive({
    data <- rawdata()
    
    if (all(names(data)==c("group", "value")) | all(names(data)==c("value", "group"))){
      #get datanames from factors  
      #print(levels(data[,"group"]))
      #print(head(data))
      return(levels(data[,"group"]))
      
    }
    
    else { #data is wide
      #get datanames from columns - we need to change data to long format 
      return(names(data))
    }
    
  })
  
  rawdata <- reactive({
    infile <- input$datafile
    
    # check for presence of file.  NULL if no file detected
    if (is.null(infile)) {
      return(NULL)
    }
    
    data <- read.csv(infile$datapath, stringsAsFactors = TRUE,header=TRUE)
    if (!is.null(data$group)){
      data$group=factor(data$group)
    }
    #names(data)[1:2] <- c("x1", "x2")
    

        data
    
  })
  
  hfromk<-function(x,k){
    abs(max(x)-min(x))/k
  }
  
  filedata <- reactive({
    
    #TODO add long/wide testing here
                                        #data<-gather_(rawdata(), "group", "value",names(rawdata()))
      data <- rawdata()
      #print(dnames())
      #print(all(dnames() == c("group","value")) )
      #print(all(dnames() == c("value","group")))  
      if (all(names(data)==c("group", "value")) | all(names(data)==c("value", "group"))) {
          #pass
      }
      else {
          x1=data[!is.na(data[,dnames()[1]]),dnames()[1]]
          x2=data[!is.na(data[,dnames()[2]]),dnames()[2]]
                                        #TODO: put in long format - how to do this efficiently?

          tempdf= data.frame(matrix(c(rep(dnames()[1], length(x1)), rep(dnames()[2], length(x2)), x1, x2), ncol = 2))
          
          names(tempdf) = c("group", "value")
          tempdf$value = as.numeric(as.vector(tempdf$value))  
          tempdf$group = factor(tempdf$group)
          
          
          data=tempdf
      }
    #data<-data[complete.cases(data),]
    #print(head(data))
    #Need two separate vectors to handle different data size
    
    choices=c("twosided","greater","less")
    names(choices)=c(paste0(dnames()[1]," and ", dnames()[2], " do not have the same mean"), paste0(dnames()[2], " has a greater mean than ", dnames()[1]), paste0(dnames()[2], " has a lower mean than ", dnames()[1]))
    updateRadioButtons(session, "hypothesis",
                       label = "Alternative hypothesis:",
                       choices=choices,
                       selected = "twosided")
    
    
    
    updateSliderInput(session, "bins", label = "Bin width:", value = signif(hfromk(data$value,round(length(data$value)^(1/3))), digits=3), min = signif(hfromk(data$value,round(length(data$value)^(1/2))), digits=3), max = signif(hfromk(data$value,round(length(data$value)^(1/4))), digits=3), step = signif(hfromk(data$value,length(data$value)),digits=3))
    #return(list(x1,x2))
    data
  })
  
  # is file loaded?
  file_loaded <- reactive(length(input$filedata) != 0)
  plotmeans <- reactive ({
    cdat <- ddply(filedata(), "group", summarise, value.mean=mean(value))
    cdat
  })
  # Create table of data
  output$contents <- renderDataTable({
    filedata()
  })

  # Create plot of data
  output$rawDataPlot <- renderPlot({
    
    df <- filedata()
    bins = input$bins 
    
    ggplot(df, aes(x =value, fill=group)) +
      geom_histogram(binwidth=bins,alpha=.5, position="identity", showLegend=FALSE) +
      theme_bw(base_size = 20) +
      labs(x = "Value", y = "Frequency\n", title = "Histogram plot with means\n") +
      geom_vline(data=plotmeans(), aes(xintercept=value.mean,  colour=group),
                 linetype="dashed", size=1) + guides(fill=FALSE)
    
  })
  
  output$distPlot <- renderPlot({
    ggplot(filedata(), aes(x=group, y=value, fill=group)) + geom_boxplot() +
      theme_bw(base_size = 20) + labs(x="Group\n", y="Value", title="Box plot of groups\n") +
      guides(fill=FALSE) + coord_flip()
  })
  
  #f <- function(){
  #  input$goButton
  #  output$outtext = renderText("Loading...")
    
  #}
  
  
  gendata <- reactive({
    number_of_permutations = input$nperms
    
    isolate({
    data=filedata()
    data$group=factor(data$group)
    combined=data$value
    ?sample
    diff.random = NULL
    for (i in 1 : number_of_permutations) {
      
      # Sample from the combined dataset without replacement
      shuffled = sample(combined, length(combined), replace=FALSE)
      #index = sample.int()
      a.random = shuffled[1 : sum(data$group==levels(data$group)[1])]
      b.random = shuffled[(sum(data$group==levels(data$group)[1]) + 1) : length(combined)]
      
      # Null (permuated) difference
      diff.random[i] = mean(b.random) - mean(a.random)
      ##print(i)
    }
    
    updateSliderInput(session, "binwidth2", label = "Bin width:", value = signif(hfromk(diff.random,round(length(diff.random)^(1/3))), digits=3), min = signif(hfromk(diff.random,round(length(diff.random)^(1/2))), digits=3), max = signif(hfromk(diff.random,round(length(diff.random)^(1/4))), digits=3), step = signif(hfromk(diff.random,length(diff.random)),digits=3))
    })
    diff.random
    
  })
  
  
#   runtests <- reactive({
#   
#     number_of_permutations = input$nperms
#     ah=input$hypothesis
#     obsmeans=by(filedata()$value, filedata()$group, mean)
#     diff.observed=(obsmeans[2]-obsmeans[1])
#     
#     diff.random=gendata()
#     # P-value is the fraction of how many times the permuted difference is equal or more extreme than the observed difference
#     #2sided p value
#     pvalue = sum(abs(diff.random) >= abs(diff.observed)) / number_of_permutations
#     
#     pvaluegreater = sum((diff.random) >= (diff.observed)) / number_of_permutations
#     pvalueless = sum((diff.random) <= (diff.observed)) / number_of_permutations
#     
#     if (pvaluegreater<=pvalueless){
#       return(c("greater", pvalue))
#     }
#     else {
#       return(c("less", pvalue))
#     }
# 
#   })


observe({
  if (!is.null(rawdata()) & input$goButton!=0){
  ##print("Running")
  ##print(input$goButton)
  input$goButton
  ah=input$hypothesis
  
  
  
  isolate({
    diff.random=gendata()
    obsmeans=by(filedata()$value, filedata()$group, mean)
    diff.observed=obsmeans[2]-obsmeans[1]
    number_of_permutations = isolate(input$nperms)
  output$outtext <- renderText({
    #print(length(diff.random))
    
    # P-value is the fraction of how many times the permuted difference is equal or more extreme than the observed difference
    if (ah=="twosided"){
      pvalue = (sum(abs(diff.random) >= abs(diff.observed))+1) / (number_of_permutations+1)
    } else if (ah=="greater") {
      pvalue = (sum((diff.random) >= (diff.observed))+1) / (number_of_permutations+1)
    }
    else { #less
      pvalue = (sum((diff.random) <= (diff.observed))+1) / (number_of_permutations+1)
    }
    
    #     v=runtests()
    #     pvalue=v[2]
    #     ah=v[1]
    
    #TODO other pvalues
    if (pvalue<0.01){
      graphtext="We can see this graphically below, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the shaded areas show the significant regions (if the observed difference of the means is in this region then it will be significant).
      <br><br>The blue line is in the highly significant (red-shaded) region, so the observed difference in means is <b>statistically significant</b> at the <font color='red'><b>99%</b></font> significance level."
      if (ah!="twosided"){
        sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.01 (a 1% chance) and so the result is <b>statistically significant</b> at the <font color='red'><b>99%</b></font> significance level.")
        result=paste0( "The mean of the sample distribution of ",dnames()[2]," appears to be <b>significantly ", ah, "</b> than the mean of the sample distribution of ", dnames()[1],".")
      }
      else{
        sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.01 (a 1% chance) and so the result is <b>statistically significant</b> at the <font color='red'><b>99%</b></font> significance level.")
        result=paste0("The mean of the sample distribution of ",dnames()[2]," appears to be <b>significantly different</b> to the mean of the sample distribution of ", dnames()[1]," at the <font color='red'><b>99%</b></font> significance level.")
        
      }
    }
    
    else if (pvalue<0.05){
      graphtext="We can see this graphically below, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the shaded areas show the significant regions (if the observed difference of the means is in this region then it will be significant).
      <br><br>The blue line is in the significant (orange-shaded) region, so the observed difference in means is <b>statistically significant</b> at the <font color='orange'><b>95%</b></font> significance level."
      if (ah!="twosided"){
        sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.05 (a 5% chance) and so the result is <b>statistically significant</b> at the <font color='orange'><b>95%</b></font> significance level.")
        result=paste0( "The mean of the sample distribution of ",dnames()[2]," appears to be <b>significantly ", ah, "</b> than the mean of the sample distribution of ", dnames()[1],".")
      }
      else{
        sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.05 (a 5% chance) and so the result is <b>statistically significant</b> at the <font color='orange'><b>95%</b></font> significance level.")
        result=paste0("The mean of the sample distribution of ",dnames()[2]," appears to be <b>significantly different</b> to the mean of the sample distribution of ", dnames()[1]," at the <font color='orange'><b>95%</b></font> significance level.")
        
      }
    }
    
    else if (pvalue<0.1){
      graphtext="We can see this graphically below, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the shaded areas show the significant regions (if the observed difference of the means is in this region then it will be significant).
      <br><br>The blue line is in the somewhat significant (yellow-shaded) region, so the observed difference in means is <b>statistically significant</b> at the <font color='yellow'><b>90%</b></font> significance level."
      if (ah!="twosided"){
        sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.1 (a 10% chance) and so the result is <b>statistically significant</b> at the <font color='yellow'><b>90%</b></font> significance level.")
        result=paste0( "The mean of the sample distribution of ",dnames()[2]," appears to be <b>significantly ", ah, "</b> than the mean of the sample distribution of ", dnames()[1]," at the <font color='yellow'><b>90%</b></font> significance level.")
      }
      else{
        sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.1 (a 10% chance) and so the result is <b>statistically significant</b> at the <font color='yellow'><b>90%</b></font> significance level.")
        result=paste0("The mean of the sample distribution of ",dnames()[2]," appears to be <b>significantly different</b> to the mean of the sample distribution of ", dnames()[1]," at the <font color='yellow'><b>90%</b></font> significance level.")
        
      }
    }
    
    else if (pvalue<0.2){
      graphtext="We can see this graphically below, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the shaded areas show the significant regions (if the observed difference of the means is in this region then it will be significant).
      <br><br>The blue line is in the slightly significant (green-shaded) region, so the observed difference in means is <b>statistically significant</b> at the <font color='green'><b>80%</b></font> significance level."
      if (ah!="twosided"){
        sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.2 (a 20% chance) and so the result is <b>statistically significant</b> at the <font color='green'><b>80%</b></font> significance level.")
        result=paste0( "The mean of the sample distribution of ",dnames()[2]," appears to be <b>significantly ", ah, "</b> than the mean of the sample distribution of ", dnames()[1]," at the <font color='yellow'><b>90%</b></font> significance level.")
      }
      else{
        sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.2 (a 20% chance) and so the result is <b>statistically significant</b> at the <font color='green'><b>80%</b></font> significance level.")
        result=paste0("The mean of the sample distribution of ",dnames()[2]," appears to be <b>significantly different</b> to the mean of the sample distribution of ", dnames()[1]," at the <font color='green'><b>80%</b></font> significance level.")
        
      }
    }
    
    
    
    else {
      graphtext="We can see this graphically below, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the red shaded area shows the significant region (if the observed difference of the means is in this region then it will be significant).
      <br><br>The blue line is <b>not</b> in the significant (red-shaded) region, so the observed difference in means is <b>not</b> statistically significant."
      
      if (ah!="twosided"){
        sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is greater than 0.05 (a 5% chance) and so the result is <b>not</b> statistically significant at the 95% significance level.")
        result=paste0( "The mean of the sample distribution of ",dnames()[2]," does <b>not</b> appear to be ", ah, " than the mean of the sample distribution of ", dnames()[1],".")
        
      }
      else {
        sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is greater than 0.05 (a 5% chance) and so the result is <b>not</b> statistically significant at the 95% significance level.")
        result=paste0( "The means of the underlying sample distributions of ",dnames()[1]," and ",dnames()[2]," appear to be <b>the same</b>.")
      }
      
    }
    

    
    paste0("<b>Result:</b><h3>",result, "</h3><br><b>Details:</b><br><br>The observed difference in means was ", format(as.numeric(diff.observed), nsmall=2, digits=3),".<br>", "The p-value for the permutation test is ", format(as.numeric(pvalue), nsmall=2, digits=3)," with ", number_of_permutations," permutations. This means that there is a <b>", format(as.numeric(pvalue)*100, nsmall=1, digits=3),"% probability</b> ",
"of seeing this difference in means just by random chance. This means that the maximum significance level at which this result would be significant is the ", format(100-100*as.numeric(pvalue), nsmall=2, digits=3), "% significance level.",
           " The minimum possible p-value for ", number_of_permutations, " permutations would be ", format(as.numeric(1/(1+number_of_permutations)), nsmall=2, digits=3), ".<br><br>",sigtest,"<br><br>", 
           graphtext)
    
})

#plot hack
output$finalPlot <- renderPlot({
  rectalpha=0.5
  obslinecolour="blue"
  histogramalpha=0.8
  binwidth=input$binwidth2
  #print(length(diff.random))
  
  sortdiff = sort(diff.random)
  siglower95=sortdiff[round(0.05*(1/(1+as.numeric(ah=="twosided")))*length(sortdiff))]
  sigupper95=sortdiff[round((1-(0.05*(1/(1+as.numeric(ah=="twosided")))))*length(sortdiff))]
  siglower99=sortdiff[round(0.01*(1/(1+as.numeric(ah=="twosided")))*length(sortdiff))]
  sigupper99=sortdiff[round((1-(0.01*(1/(1+as.numeric(ah=="twosided")))))*length(sortdiff))]
  siglower90=sortdiff[round(0.10*(1/(1+as.numeric(ah=="twosided")))*length(sortdiff))]
  sigupper90=sortdiff[round((1-(0.1*(1/(1+as.numeric(ah=="twosided")))))*length(sortdiff))]
  siglower80=sortdiff[round(0.20*(1/(1+as.numeric(ah=="twosided")))*length(sortdiff))]
  sigupper80=sortdiff[round((1-(0.2*(1/(1+as.numeric(ah=="twosided")))))*length(sortdiff))]
  #aes(y=..count../sum(..count..)), 
  df = as.data.frame(diff.random)
  .e <- environment() #aes hack
  if (ah=="twosided"){
    diff.observed=abs(diff.observed)
    ggplot(df, aes(x=diff.random), environment=.e) +
      geom_histogram(aes(y=..count../sum(..count..)),binwidth=binwidth,alpha=histogramalpha, position="identity") +
      #geom_histogram(aes(y=..count../sum(..count..)),data=subset(df,abs(diff.random)>=diff.observed),binwidth=binwidth,alpha=histogramalpha, fill="black") +
      theme_bw(base_size = 20) +
      labs(x = "Difference in means", y = "Frequency\n", title = "Histogram of differences\n") +
      geom_vline(xintercept=diff.observed, colour=obslinecolour) +
      geom_vline(xintercept=-1*diff.observed, colour=obslinecolour) +
      #geom_vline(xintercept=siglower, linetype="longdash") +
      #geom_vline(xintercept=sigupper, linetype="longdash") +
      annotate("rect",xmin=sigupper99, xmax=Inf, ymin=-Inf, ymax=Inf, fill="red", alpha=rectalpha) +
      annotate("rect",xmin=-Inf, xmax=siglower99, ymin=-Inf, ymax=Inf, fill="red", alpha=rectalpha) +
      annotate("rect",xmin=sigupper95, xmax=sigupper99, ymin=-Inf, ymax=Inf, fill="orangered", alpha=rectalpha) +
      annotate("rect",xmin=siglower99, xmax=siglower95, ymin=-Inf, ymax=Inf, fill="orangered", alpha=rectalpha) +
      annotate("rect",xmin=sigupper90, xmax=sigupper95, ymin=-Inf, ymax=Inf, fill="yellow", alpha=rectalpha) +
      annotate("rect",xmin=siglower95, xmax=siglower90, ymin=-Inf, ymax=Inf, fill="yellow", alpha=rectalpha) +
      annotate("rect",xmin=sigupper80, xmax=sigupper90, ymin=-Inf, ymax=Inf, fill="green", alpha=rectalpha) +
      annotate("rect",xmin=siglower90, xmax=siglower80, ymin=-Inf, ymax=Inf, fill="green", alpha=rectalpha) +
      #coord_cartesian(ylim=c(0,NA))
      #ylim(0,NA)
      #expand_limits(y=0) + 
      scale_y_continuous( limits = c(0,1), expand = c(0,0) )
    
    
  }
  else if (ah=="greater"){
    ggplot(df, aes(x=diff.random), environment=.e) +
      geom_histogram(aes(y=..count../sum(..count..)),binwidth=binwidth,alpha=histogramalpha, position="identity") +
      #geom_histogram(aes(y=..count../sum(..count..)),data=subset(df,diff.random>=diff.observed),binwidth=binwidth,alpha=histogramalpha, fill="black") +
      theme_bw(base_size = 20) +
      labs(x = "Difference in means", y = "Frequency\n", title = "Histogram of differences\n") +
      geom_vline(xintercept=diff.observed, colour=obslinecolour) +
      #geom_vline(xintercept=sigupper, linetype="longdash") +
      #annotate("rect",xmin=sigupper, xmax=Inf, ymin=-Inf, ymax=Inf, fill="red", alpha=rectalpha) +
      annotate("rect",xmin=sigupper99, xmax=Inf, ymin=-Inf, ymax=Inf, fill="red", alpha=rectalpha) +
      annotate("rect",xmin=sigupper95, xmax=sigupper99, ymin=-Inf, ymax=Inf, fill="orangered", alpha=rectalpha) +
      annotate("rect",xmin=sigupper90, xmax=sigupper95, ymin=-Inf, ymax=Inf, fill="yellow", alpha=rectalpha) +
      annotate("rect",xmin=sigupper80, xmax=sigupper90, ymin=-Inf, ymax=Inf, fill="green", alpha=rectalpha) +
      scale_y_continuous( limits = c(0,1), expand = c(0,0) )
  }
  else {
    
    ggplot(df, aes(x=diff.random), environment=.e) +
      geom_histogram(aes(y=..count../sum(..count..)),binwidth=binwidth,alpha=histogramalpha, position="identity") +
      #geom_histogram(aes(y=..count../sum(..count..)),data=subset(df,diff.random<=diff.observed),binwidth=binwidth,alpha=histogramalpha, fill="black") +
      theme_bw(base_size = 20) +
      labs(x = "Difference in means", y = "Frequency\n", title = "Histogram of differences\n") +
      geom_vline(xintercept=diff.observed, colour=obslinecolour) +
      #geom_vline(xintercept=siglower, linetype="longdash") +
      #annotate("rect",xmin=-Inf, xmax=siglower, ymin=-Inf, ymax=Inf, fill="red", alpha=rectalpha) +
      annotate("rect",xmin=-Inf, xmax=siglower99, ymin=-Inf, ymax=Inf, fill="red", alpha=rectalpha) +
      annotate("rect",xmin=siglower99, xmax=siglower95, ymin=-Inf, ymax=Inf, fill="orangered", alpha=rectalpha) +
      annotate("rect",xmin=siglower95, xmax=siglower90, ymin=-Inf, ymax=Inf, fill="yellow", alpha=rectalpha) +
      annotate("rect",xmin=siglower90, xmax=siglower80, ymin=-Inf, ymax=Inf, fill="green", alpha=rectalpha) +
      scale_y_continuous( limits = c(0,1), expand = c(0,0) )
    
  }
  
})



output$tout <- renderPrint({
  ah=input$hypothesis
  if (ah=="twosided"){
    ah="two.sided"
  }
  else if (ah=="less"){
    ah="greater"
  }
  else {
    ah="less"
  }
  
  t.test(value~group,data=filedata(), alternative=ah)
  
  
})
  })
 ##print("Finished") 
#return(input$goButton)
}
})


  exdnames <- reactive({
      return(names(getexampledata()))
  })

  ####EXAMPLE CODE
output$example <- renderDataTable({
  getexampledata()
},options=list(paging=TRUE, searching=FALSE, pagingType="simple",lengthChange=FALSE, pageLength=5))

output$examplemeans <- renderText({
  edf=getexampledata()
  x1=edf$x1
  x2=edf$x2
  paste0("The observed difference in means is <b>", round(abs(mean(x1)-mean(x2)), digits=3) ,"</b>." )
  
})

getexampledata <- reactive({
  x1<-mvrnorm(n=50, mu=input$meanx1, Sigma=input$stdx1, empirical=TRUE)
  x2<-mvrnorm(n=50,mu=input$meanx2, Sigma=input$stdx2, empirical=TRUE)
  data.frame(x1=x1,x2=x2)
})

examplegendata <- reactive({
  diff.random = NULL
  edf=getexampledata()
  x1=edf$x1
  x2=edf$x2
  aperm <- NULL
  bperm<- NULL
  combined=c(x1,x2)
  #print(length(combined))
  #print(length(x1))
  
  for (i in 1 : 1000) {
    # Sample from the combined dataset without replacement
    shuffled = sample (combined, length(combined))
    
    a.random = shuffled[1 : length(x1)]
    b.random = shuffled[(length(x1)+1): length(combined)]
    # Null (permuated) difference
    diff.random[i] = round(mean(b.random) - mean(a.random),digits=3)
    aperm[i]<-round(mean(a.random),digits=3)
    bperm[i]<-round(mean(b.random),digits=3)
    
  }
  data.frame(meangroup1=aperm,meangroup2=bperm, diffmean=diff.random)
  
  
})

output$example2 <- renderDataTable({
  #loop through permutations

  examplegendata()
}, options=list(paging=TRUE, searching=FALSE, pageLength=5,lengthChange=FALSE,pagingType="simple"))







output$exampleplot <- renderPlot({
  rectalpha=0.25
  obslinecolour="blue"
  histogramalpha=0.8
  df=examplegendata()
  binwidth=0.05
  diff.random=df$diffmean
  edf=getexampledata()
  x1=edf$x1
  x2=edf$x2
  diff.observed=abs(mean(x2)-mean(x1))
  
  .e=environment()
  sortdiff = sort(diff.random)
  siglower95=sortdiff[round(0.05*0.5*length(sortdiff))]
  sigupper95=sortdiff[round((1-(0.05/2))*length(sortdiff))]
  siglower99=sortdiff[round(0.01*0.5*length(sortdiff))]
  sigupper99=sortdiff[round((1-(0.01/2))*length(sortdiff))]
  siglower90=sortdiff[round(0.10*0.5*length(sortdiff))]
  sigupper90=sortdiff[round((1-(0.1/2))*length(sortdiff))]
  siglower80=sortdiff[round(0.20*0.5*length(sortdiff))]
  sigupper80=sortdiff[round((1-(0.2/2))*length(sortdiff))]
  ggplot(df, aes(x=diff.random), environment=.e) +
    geom_histogram(aes(y=..count../sum(..count..)),binwidth=binwidth,alpha=histogramalpha, position="identity") +
    #geom_histogram(aes(y=..count../sum(..count..)),data=subset(df,abs(diff.random)>=diff.observed),binwidth=binwidth,alpha=histogramalpha, fill="black") +
    theme_bw(base_size = 20) +
    labs(x = "Difference in means", y = "Frequency\n", title = "Histogram of differences\n") +
    geom_vline(xintercept=diff.observed, colour=obslinecolour) +
    geom_vline(xintercept=-1*diff.observed, colour=obslinecolour) +
    annotate("rect",xmin=sigupper99, xmax=Inf, ymin=-Inf, ymax=Inf, fill="red", alpha=rectalpha) +
    annotate("rect",xmin=-Inf, xmax=siglower99, ymin=-Inf, ymax=Inf, fill="red", alpha=rectalpha) +
    annotate("rect",xmin=sigupper95, xmax=sigupper99, ymin=-Inf, ymax=Inf, fill="orangered", alpha=rectalpha) +
    annotate("rect",xmin=siglower99, xmax=siglower95, ymin=-Inf, ymax=Inf, fill="orangered", alpha=rectalpha) +
    annotate("rect",xmin=sigupper90, xmax=sigupper95, ymin=-Inf, ymax=Inf, fill="yellow", alpha=rectalpha) +
    annotate("rect",xmin=siglower95, xmax=siglower90, ymin=-Inf, ymax=Inf, fill="yellow", alpha=rectalpha) +
    annotate("rect",xmin=sigupper80, xmax=sigupper90, ymin=-Inf, ymax=Inf, fill="green", alpha=rectalpha) +
    annotate("rect",xmin=siglower90, xmax=siglower80, ymin=-Inf, ymax=Inf, fill="green", alpha=rectalpha) +
    #coord_cartesian(ylim=c(0,NA))
    #ylim(0,NA)
    #expand_limits(y=0) + 
    scale_y_continuous( limits = c(0,0.2), expand = c(0,0) )
  
  
})


output$exouttext <- renderText({
  df=examplegendata()
  diff.random=df$diffmean
  edf=getexampledata()
  x1=edf$x1
  x2=edf$x2
  number_of_permutations=1000
  # P-value is the fraction of how many times the permuted difference is equal or more extreme than the observed difference
  diff.observed=abs(mean(x2)-mean(x1))
  
  pvalue = (sum(abs(diff.random) >= abs(diff.observed))+1) / (number_of_permutations+1)
  
  
  #     v=runtests()
  #     pvalue=v[2]
  #     ah=v[1]
  if (pvalue<0.01){
    graphtext="We can see this graphically below, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the shaded areas show the significant regions (if the observed difference of the means is in this region then it will be significant).
      <br><br>The blue line is in the highly significant (red-shaded) region, so the observed difference in means is <b>statistically significant</b> at the <font color='red'><b>99%</b></font> significance level."

      sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.01 (a 1% chance) and so the result is <b>statistically significant</b> at the <font color='red'><b>99%</b></font> significance level.")
      result=paste0("The mean of the sample distribution of ",exdnames()[2]," appears to be <b>significantly different</b> to the mean of the sample distribution of ", exdnames()[1]," at the <font color='red'><b>99%</b></font> significance level.")
      
    
  }
  
  else if (pvalue<0.05){
    graphtext="We can see this graphically below, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the shaded areas show the significant regions (if the observed difference of the means is in this region then it will be significant).
    <br><br>The blue line is in the significant (orange-shaded) region, so the observed difference in means is <b>statistically significant</b> at the <font color='orange'><b>95%</b></font> significance level."

      sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.05 (a 5% chance) and so the result is <b>statistically significant</b> at the <font color='orange'><b>95%</b></font> significance level.")
      result=paste0("The mean of the sample distribution of ",exdnames()[2]," appears to be <b>significantly different</b> to the mean of the sample distribution of ", exdnames()[1]," at the <font color='orange'><b>95%</b></font> significance level.")
      
    
  }
  
  else if (pvalue<0.1){
    graphtext="We can see this graphically below, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the shaded areas show the significant regions (if the observed difference of the means is in this region then it will be significant).
    <br><br>The blue line is in the somewhat significant (yellow-shaded) region, so the observed difference in means is <b>statistically significant</b> at the <font color='yellow'><b>90%</b></font> significance level."

      sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.1 (a 10% chance) and so the result is <b>statistically significant</b> at the <font color='yellow'><b>90%</b></font> significance level.")
      result=paste0("The mean of the sample distribution of ",exdnames()[2]," appears to be <b>significantly different</b> to the mean of the sample distribution of ", exdnames()[1]," at the <font color='yellow'><b>90%</b></font> significance level.")
      
    
  }
  
  else if (pvalue<0.2){
    graphtext="We can see this graphically below, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the shaded areas show the significant regions (if the observed difference of the means is in this region then it will be significant).
      <br><br>The blue line is in the slightly significant (green-shaded) region, so the observed difference in means is <b>statistically significant</b> at the <font color='green'><b>80%</b></font> significance level."

      sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.2 (a 20% chance) and so the result is <b>statistically significant</b> at the <font color='green'><b>80%</b></font> significance level.")
      result=paste0("The mean of the sample distribution of ",exdnames()[2]," appears to be <b>significantly different</b> to the mean of the sample distribution of ", exdnames()[1]," at the <font color='green'><b>80%</b></font> significance level.")
      
    
  }
  
  
  
  else {
    graphtext="We can see this graphically below, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the red shaded area shows the significant region (if the observed difference of the means is in this region then it will be significant).
      <br><br>The blue line is <b>not</b> in the significant (red-shaded) region, so the observed difference in means is <b>not</b> statistically significant."
    
      sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is greater than 0.05 (a 5% chance) and so the result is <b>not</b> statistically significant at the 95% significance level.")
      result=paste0( "The means of the underlying sample distributions of ",exdnames()[1]," and ",exdnames()[2]," appear to be <b>the same</b>.")
    
  }
  
  
  
  paste0("<b>Result:</b><h3>",result, "</h3><br><b>Details:</b><br>The observed difference in means was ", format(as.numeric(diff.observed), nsmall=2, digits=3),".",
         "<br>", "The p-value for the permutation test is ", format(as.numeric(pvalue), nsmall=2, digits=3)," with ", number_of_permutations," permutations. This means that there is a <b>", format(as.numeric(pvalue)*100, nsmall=1, digits=3),"% probability</b> ",
         "of seeing this difference in means just by random chance. This means that the maximum significance level at which this result would be significant is the ", format(100-100*as.numeric(pvalue), nsmall=2, digits=3), "% significance level.",
         " The minimum possible p-value for ", number_of_permutations, " permutations would be ", format(as.numeric(1/(1+number_of_permutations)), nsmall=2, digits=3), ".<br><br>",sigtest,"<br><br>", 
         graphtext)
  
})



})

# ###OLD CODE
# output$exampleplot <- renderPlot({
#   rectalpha=0.25
#   obslinecolour="blue"
#   histogramalpha=0.8
#   df=examplegendata()
#   binwidth=0.05
#   diff.random=df$diffmean
#   edf=getexampledata()
#   x1=edf$x1
#   x2=edf$x2
#   .e=environment()
#   sortdiff = sort(diff.random)
#   siglower=sortdiff[ceiling(0.05*length(sortdiff))]
#   sigupper=sortdiff[round(0.95*length(sortdiff))]
#   diff.observed=abs(mean(x2)-mean(x1))
#   ggplot(df, aes(x=diff.random), environment=.e) +
#     geom_histogram(aes(y=..count../sum(..count..)),binwidth=binwidth,alpha=histogramalpha, position="identity") +
#     #geom_histogram(aes(y=..count../sum(..count..)),data=subset(df,abs(diff.random)>=diff.observed),binwidth=binwidth,alpha=histogramalpha, fill="black") +
#     theme_bw(base_size = 20) +
#     labs(x = "Difference in means", y = "Frequency\n", title = "Histogram of differences\n") +
#     geom_vline(xintercept=diff.observed, colour=obslinecolour) +
#     geom_vline(xintercept=-1*diff.observed, colour=obslinecolour) +
#     geom_vline(xintercept=siglower, linetype="longdash") +
#     geom_vline(xintercept=sigupper, linetype="longdash") +
#     annotate("rect",xmin=sigupper, xmax=Inf, ymin=-Inf, ymax=Inf, fill="red", alpha=rectalpha) +
#     annotate("rect",xmin=-Inf, xmax=siglower, ymin=-Inf, ymax=Inf, fill="red", alpha=rectalpha) +
#     #coord_cartesian(ylim=c(0,NA))
#     #ylim(0,NA)
#     #expand_limits(y=0) + 
#     scale_y_continuous( limits = c(0,0.2), expand = c(0,0) )
#   
#   
# })
# 
# 
# output$exouttext <- renderText({
#   df=examplegendata()
#   diff.random=df$diffmean
#   edf=getexampledata()
#   x1=edf$x1
#   x2=edf$x2
#   number_of_permutations=1000
#   # P-value is the fraction of how many times the permuted difference is equal or more extreme than the observed difference
#   diff.observed=abs(mean(x2)-mean(x1))
#   
#     pvalue = (sum(abs(diff.random) >= abs(diff.observed))+1) / (number_of_permutations+1)
# 
#   
#   #     v=runtests()
#   #     pvalue=v[2]
#   #     ah=v[1]
#   if (pvalue<0.05){
#     graphtext="We can see this graphically below, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the red shaded area shows the significant region (if the observed difference of the means is in this region then it will be significant).
#     <br><br>The blue line is in the significant (red-shaded) region, so the observed difference in means is <b>statistically significant</b>."
#       sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is less than 0.05 (a 5% chance) and so the result is <b>statistically significant</b> at the 95% significance level.")
#       result=paste0("The mean of the sample distribution of ",names(getexampledata())[2]," appears to be <b>significantly different</b> to the mean of the sample distribution of ", names(getexampledata())[1],".")
#   }
#   else {
#     graphtext="We can see this graphically above, the histogram shows the possible differences in the means from random chance, the blue line shows the observed difference in means and the red shaded area shows the significant region (if the observed difference of the means is in this region then it will be significant).
#     <br><br>The blue line is <b>not</b> in the significant (red-shaded) region, so the observed difference in means is <b>not</b> statistically significant."
# 
#       sigtest=paste0("The p-value, ", format(as.numeric(pvalue), nsmall=2, digits=3),",  is greater than 0.05 (a 5% chance) and so the result is <b>not</b> statistically significant at the 95% significance level.")
#       result=paste0( "The means of the underlying sample distributions of ",names(getexampledata())[1]," and ",names(getexampledata())[2]," appear to be <b>the same</b>.")
#     
#   }
#   
#   
#   
#   paste0("<b>Result:</b><h3>",result, "</h3><br><b>Details:</b><br>", "The p-value for the permutation test is ", format(as.numeric(pvalue), nsmall=2, digits=3)," with ", number_of_permutations," permutations. This means that there is a ", format(as.numeric(pvalue)*100, nsmall=1, digits=3),"% probability of seeing this difference in means just by random chance.
#          The minimum possible p-value for ", number_of_permutations, " permutations would be ", format(as.numeric(1/(1+number_of_permutations)), nsmall=2, digits=3), ", which is 1/(1+number_of_permutations).<br><br>",sigtest,"<br><br>", 
#          graphtext)
#   
# })
# 
# 
# 
# })

