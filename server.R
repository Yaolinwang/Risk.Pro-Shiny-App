library(quantmod)
library(OpenMx)
## getting data ##
#end_date=Sys.Date()
single_data_f <- function(ticker,start_date,end_date){
  options(download.file.method="libcurl")
  single_data=as.data.frame(getSymbols(ticker,src='google',from=start_date,to=end_date,env = NULL))
  data_adjusted=single_data[,ncol(single_data)-1]
  return(diff(log(data_adjusted), lag=1))
}

## Calculation ##
## (a) Historical VaR  ## VaR_his_f('BABA',1000,0.95,100)  ## testing
VaR_his_f <- function(ticker,position,CI,St_buy,start_date,end_date){
  data=single_data_f(ticker,start_date,end_date)
  data_sorted=sort(data)
  data_sorted[1]
  rt=data_sorted[floor(length(data_sorted)*(1-CI))]
  St=St_buy*exp(rt)
  return(abs(round(position*(St-St_buy),digits = 2)))
}

## (b) Parametric VaR ## VaR_Parametric_f('GS',1000,0.95,100) ## testing
VaR_Parametric_f <-function(ticker,position,CI,St_buy,start_date,end_date){
  data=single_data_f(ticker,start_date,end_date)
  Z=qnorm(1-CI)
  shock=exp(Z*sd(data))
  return(abs(round(position*St_buy*(shock-1),digits = 2)))
}

## (c) Monte Carlo ## VaR_MC_f(10000,0.95,100)  ## testing
VaR_MC_f<-function(ticker,position,CI,St_buy,start_date,end_date){
  return_log=single_data_f(ticker,start_date,end_date)
  vol=sd(return_log)
  data=rnorm(10000)
  data_sorted=sort(data)
  rt=data_sorted[floor(length(data_sorted)*(1-CI))]
  St=St_buy*exp(rt*vol)
  return(abs(round(position*(St-St_buy),digits = 2)))
}

## Expected shortfall
ES_f<-function(CI,VaR){
  Z=qnorm(1-CI)
  return(round(-dnorm(Z)/((1-CI)*Z)*VaR,2))
}

## Portfolio VaR ###########################
data_f<-function(symbols,start_date,end_date){
  log_return=single_data_f(symbols[1],start_date,end_date)
  for (s in symbols[-1]){
    log_return_single=as.data.frame(single_data_f(s,start_date,end_date))
    log_return=cbind(log_return,log_return_single)
  }
  colnames(log_return) <- symbols
  return(log_return)
}

port_var_f<-function(position_1,position_2,position_3,position_4,purchase_1,purchase_2,purchase_3,purchase_4,symbol_1,symbol_2,symbol_3,symbol_4,start_date,end_date,CI_2){
  p =c(position_1,position_2,position_3,position_4)
  purchase=c(purchase_1,purchase_2,purchase_3,purchase_4)
  purchase=vec2diag(purchase)
  asset_value=purchase%*%p
  symbols=c(symbol_1,symbol_2,symbol_3,symbol_4)
  cov_m <- cov(data_f(symbols,start_date,end_date))
  Z=qnorm(1-CI_2)
  portfolio_var=Z*sqrt(t(asset_value)%*%cov_m%*%asset_value)
  return(abs(round(portfolio_var,digits = 2)))
}


retuns_data_f <- function(ticker,start_date,end_date){
  options(download.file.method="libcurl")
  single_data=as.data.frame(getSymbols(ticker,src='google',from=start_date,to=end_date,env = NULL))
  data_adjusted=single_data[,ncol(single_data)-1]
  return(data_adjusted)
}

data_f_2<-function(symbols,start_date,end_date,purchase_price){
  return_num=retuns_data_f(symbols[1],start_date,end_date)
  for (s in symbols[-1]){
    return_single=as.data.frame(retuns_data_f(s,start_date,end_date))
    return_num=cbind(return_num,return_single)
  }
  colnames(return_num) <- symbols
  return_num$pl_1=(return_num[,1]-purchase_price[1])
  return_num$pl_2=(return_num[,2]-purchase_price[2])
  return_num$pl_3=(return_num[,3]-purchase_price[3])
  return_num$pl_4=(return_num[,4]-purchase_price[4])
  return(return_num)
}




#### Stress testing
VaR=function(St_buy,r,position){
  St=St_buy*exp(r)
  return(abs(round(position*(St-St_buy),digits = 2)))
}

single_data_f <- function(ticker,start_date,end_date){
  options(download.file.method="libcurl")
  single_data=as.data.frame(getSymbols(ticker,src='google',from=start_date,to=end_date,env = NULL))
  data_adjusted=single_data[,ncol(single_data)-1]
  return(diff(log(data_adjusted), lag=1))
}

VaR=function(St_buy,r,position){
  St=St_buy*exp(r)
  return(abs(round(position*(St-St_buy),digits = 2)))
}

indexReturns_data_f <- function(start_date,end_date){
  URL <- "https://stooq.com/q/d/l/?s=^dji&i=d"
  dat <- read.csv(URL)
  dat$Date <- as.Date(dat$Date, "%Y-%m-%d")
  da <- subset(dat, Date > as.Date(start_date-1) )
  da_2 <- subset(da, Date < as.Date(end_date+1) )
  data <- da_2[,ncol(da_2)-1]
  return(diff(log(data), lag=1))
}

stress_test=function(symbol_st,St_buy,position,start_date,scenario){
  end_date=Sys.Date()
  stockReturns_log=single_data_f(symbol_st,start_date,end_date)
  indexReturns_log=indexReturns_data_f(start_date,end_date)
  fit.log <- lm(stockReturns_log ~ indexReturns_log)
  beta <- fit.log$coefficients[2]
  
  ## quarterly return
  dj_server_adverse=-0.119425509
  dj_adverse=-0.057476378
  dj_baseline=0.011846379
  
  ## daily return
  dj_server_adverse_daily=(1+dj_server_adverse)^(1/80)-1
  dj_adverse_daily=(1+dj_adverse)^(1/80)-1
  dj_baseline_daily=(1+dj_baseline)^(1/80)-1
  
  ## risk free rate
  rf_server_adverse=0.001
  rf_adverse=0.001
  rf_baseline=0.003
  
  ## CAPM result
  return_server_adverse=rf_server_adverse+(beta*(dj_server_adverse_daily-rf_server_adverse))
  return_adverse=rf_adverse+(beta*(dj_adverse_daily-rf_adverse))
  return_baseline=rf_baseline+(beta*(dj_baseline_daily-rf_baseline))
  
  
  
  if(scenario == 'sd' ){
    v=VaR(St_buy,return_server_adverse,position)
    return(c(v,dj_server_adverse_daily,rf_server_adverse,beta))
  }
  if(scenario == 'a' ){
    v=VaR(St_buy,return_adverse,position)
    return(c(v,dj_adverse_daily,rf_adverse,beta))
  }
  if(scenario == 'b' ){
    v=VaR(St_buy,return_baseline,position)
    return(c(v,dj_baseline_daily,rf_baseline,beta))
  }
}
## DJI plot
DJI_plot=function(scenario){
  data_b=c(23551.5,
           23830.5,
           24123,
           24421.8,
           24726.8,
           25042.2,
           25354.2,
           25667.6,
           25967.5,
           26268.6,
           26570.7,
           26874.3,
           27172.8)
  
  data_sd=c(15373.6,
            13537.6,
            12294.8,
            11704.3,
            12337.7,
            13325.5,
            14348.1,
            15625,
            17069.7,
            18738.7,
            19908.7,
            21185.7,
            22577.4)
  
  data_a=c(15959.6,
           15042.3,
           14289.9,
           13982.2,
           14367.4,
           15001,
           15692.9,
           16603.2,
           17519.5,
           18513.7,
           19242.6,
           20025.4,
           20867)
  plot(data_a)
  
  date_st=c('2017 Q1',
            '2017 Q2',
            '2017 Q3',
            '2017 Q4',
            '2018 Q1',
            '2018 Q2',
            '2018 Q3',
            '2018 Q4',
            '2019 Q1',
            '2019 Q2',
            '2019 Q3',
            '2019 Q4',
            '2020 Q1')
  if(scenario == 'sd' ){
    plot(data_sd,xaxt="n",type='o',col='deepskyblue',lwd=3,
         main='Dow Jones Index in chosen scenario',xlab = 'Date', ylab = 'Dow Jones Index')
    axis(1,at=1:13,date_st)
  }
  if(scenario == 'a' ){
    plot(data_a,xaxt="n",type='o',col='deepskyblue',lwd=3,
         main='Dow Jones Index in chosen scenario',xlab = 'Date', ylab = 'Dow Jones Index')
    axis(1,at=1:13,date_st)
  }
  if(scenario == 'b' ){
    plot(data_b,xaxt="n",type='o',col='deepskyblue',lwd=3,
         main='Dow Jones Index in chosen scenario',xlab = 'Date', ylab = 'Dow Jones Index')
    axis(1,at=1:13,date_st)
  }
}



##############################################


function(input, output) {
  ## Single VaR ##############
  output$plot1 <- renderPlot({
    returns=single_data_f(input$symbol,input$date,input$date_end)
    hist(returns,
         main=paste('Histogram of ', input$symbol,' Log Return'), xlab = 'Log Return', ylab = 'Probability Densities',freq = FALSE)
    curve(dnorm(x, mean=mean(returns), sd=sd(returns)), add=TRUE,col='deepskyblue',lwd=2)
  })
  
  output$plot2 <- renderPlot({
    options(download.file.method="libcurl")
    History=as.data.frame(getSymbols(input$symbol,src='google',from=input$date,to=input$date_end,env = NULL))
    chartSeries(History,theme=chartTheme('white'))
  })  
  
  output$plot3 <- renderPlot({
    returns=single_data_f(input$symbol,input$date,input$date_end)
    qqnorm(returns)
    qqline(returns,col='deepskyblue',lwd=2)
  })
  
  output$plot4 <- renderPlot({
    purchase_price=c(input$purchase_1,input$purchase_2,input$purchase_3,input$purchase_4)
    symbols=c(input$symbol_1,input$symbol_2,input$symbol_3,input$symbol_4)
    p=c(input$position_1,input$position_2,input$position_3,input$position_4)
    
    returns_s=data_f_2(symbols,input$date_2,input$date_end_2,purchase_price)
    purchase=vec2diag(purchase_price)
    asset_value=purchase%*%p
    asset_return=(as.matrix(returns_s[,5:8])%*%p)/sum(asset_value)
    
    
    spy=retuns_data_f('spy',input$date_2,input$date_end_2)
    spy_return=spy/spy[1]-1
    
    m=min(min(asset_return),min(spy_return))
    mm=max(max(asset_return),max(spy_return))
  
    plot(asset_return,type = 'l',
         ylim = c(m,mm),main=paste('P&L of your portfolio from ',input$date_2," to ",input$date_end_2), xlab = 'Days', ylab = 'P&L',lwd=2,col='deepskyblue')
    lines(spy_return)
    legend(mm, legend=c("Portfolio", "SPY500"),
           col=c("deepskyblue", "black"), lty=1:2, cex=0.8,lwd=2,
           box.lty=0)
  })
  
  output$plot5 <- renderPlot({
    end_date=Sys.Date()
    stockReturns_log=single_data_f(input$symbol_st,input$start_date_st,end_date)
    indexReturns_log=indexReturns_data_f(input$start_date_st,end_date)
    fit.log <- lm(stockReturns_log ~ indexReturns_log)
    plot(indexReturns_log,stockReturns_log,pch = 16, cex = 1, col = "deepskyblue",
         main='Log Returns',xlab = 'Dow Jones Index ', ylab = paste(input$symbol_st))
    abline(fit.log,lwd=2)
  }) 
  
  output$plot6 <- renderPlot({
    DJI_plot(input$scenario_num)
  })  

  ## table_1
  sliderValues_1 <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Symbol", 
               "Historical VaR",
               "Parametric VaR",
               "Monte Carlo VaR",
               "Expected shortfall"
               ),
      Value = as.character(c(input$symbol, 
                             VaR_his_f(input$symbol,input$position,input$CI,input$St_buy,input$date,input$date_end),
                             VaR_Parametric_f(input$symbol,input$position,input$CI,input$St_buy,input$date,input$date_end),
                             VaR_MC_f(input$symbol,input$position,input$CI,input$St_buy,input$date,input$date_end),
                             ES_f(input$CI,VaR_Parametric_f(input$symbol,input$position,input$CI,input$St_buy,input$date,input$date_end)))
                             ), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues_1()
  })
  
  ## table_2
  sliderValues_2 <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c(paste(input$symbol_1, " VaR"), 
               paste(input$symbol_2, " VaR"),
               paste(input$symbol_3, " VaR"),
               paste(input$symbol_4, " VaR"),
               "Portfolio VaR"
      ),
      Value = as.character(c(VaR_Parametric_f(input$symbol_1,input$position_1,input$CI_2,input$purchase_1,input$date_2,input$date_end_2),
                             VaR_Parametric_f(input$symbol_2,input$position_2,input$CI_2,input$purchase_2,input$date_2,input$date_end_2),
                             VaR_Parametric_f(input$symbol_3,input$position_3,input$CI_2,input$purchase_3,input$date_2,input$date_end_2),
                             VaR_Parametric_f(input$symbol_4,input$position_4,input$CI_2,input$purchase_4,input$date_2,input$date_end_2),
                             port_var_f(input$position_1,input$position_2,input$position_3,input$position_4,
                                        input$purchase_1,input$purchase_2,input$purchase_3,input$purchase_4,
                                        input$symbol_1,input$symbol_2,input$symbol_3,input$symbol_4,
                                        input$date_2,
                                        input$date_end_2,
                                        input$CI_2))
      ), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values_2 <- renderTable({
    sliderValues_2()
  })
  

  
  ## Stress Testing ###########
  ## table_3
  sliderValues_3 <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Symbol", 
               "Market Return(%)",
               "Risk Free Rate(%)",
               "Beta",
               "Stressed VaR($)"
      ),
      
      Value = as.character(c(input$symbol_st,
                             round(as.numeric(stress_test(input$symbol_st,input$St_buy_st,input$position_st,input$start_date_st,input$scenario_num)[2])*100,2),
                             round(as.numeric(stress_test(input$symbol_st,input$St_buy_st,input$position_st,input$start_date_st,input$scenario_num)[3])*100,2),
                             round(as.numeric(stress_test(input$symbol_st,input$St_buy_st,input$position_st,input$start_date_st,input$scenario_num)[4]),2),
                             as.numeric(stress_test(input$symbol_st,input$St_buy_st,input$position_st,input$start_date_st,input$scenario_num)[1])
                             )
                             
      ), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values_3 <- renderTable({
    sliderValues_3()
  })
}
