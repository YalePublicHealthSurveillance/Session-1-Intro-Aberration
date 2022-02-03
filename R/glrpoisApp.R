glrpois_App <-function(ds=ds1, datevar='date', casevar='cases'){
  surv.ds1 <- surv.ds.convert(ds, datevar=datevar, casevar=casevar)
  
  
  shinyApp(
    ui=fluidPage(
      
      
      sliderInput("week.test", "Current Week:",
                  min=63, max=nrow(ds), value=60, step=1),
      sliderInput("set.thresh", "Threshold for alarm (default=5):",
                  min=3, max=10, value=5),
      checkboxInput('adjust.season', 'Adjust seasonality?', value=T ),
      checkboxInput('adjust.trend', 'Adjust trend?', value=F ),
      
      plotOutput("periodPlot")
    ),
    server=function(input, output){
      output$periodPlot = renderPlot({
        
        mod1<- algo.glrpois(surv.ds1,control=list(
          range=c(53:input$week.test),
          c.ARL=input$set.thresh,
          M=-1, #How many time points back should we look? Negative 1: use all cases
          ret=c('value'),
          mu0=list( trend=input$adjust.trend, #Trend adjustment?
                    S=input$adjust.season) #Seasonality? 0=no, 1 or 2 = # harmonics to include
        ))
        
        glr.vec<- c(rep(NA, times=(mod1$control$range[1]-1)),mod1$upperbound[,1] )
        m.vec<- c(rep(NA, times=(mod1$control$range[1]-1)),mod1$control$mu0 )
        
        alarm.vec<- c(rep(1, times=(mod1$control$range[1]-1)),mod1$alarm+2 )
        col.alarm.vec=c('gray', 'black','red')
        
        par(mfrow=c(2,1), mar=c(2,2,2,1))
        plot.obs <- mod1$disProgObj$observed[,1]
        plot.obs[(input$week.test+1):nrow(ds)] <- NA
        plot(plot.obs, bty='l',type='p', ylab='Observed cases',col=col.alarm.vec[alarm.vec], pch=16)
        points(m.vec, type='l', col='black')
        title('Observed cases and mean')
        
        plot(glr.vec, bty='l',type='p', ylab='GLR statistic',col=col.alarm.vec[alarm.vec], pch=16, xlim=c(0,length(plot.obs)))
        abline(h=mod1$control$c.ARL, lty=2)
        title('GLR statistic')
        
      },width = "auto", height = "auto")
    }
  )
}