library(quantmod)
library(OpenMx)
## getting data ##
#end_date=Sys.Date()
single_data_f <- function(ticker,start_date,end_date){
  single_data=as.data.frame(getSymbols(ticker,from=start_date,to=end_date,env = NULL))
  data_adjusted=single_data[,ncol(single_data)]
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
  single_data=as.data.frame(getSymbols(ticker,from=start_date,to=end_date,env = NULL))
  data_adjusted=single_data[,ncol(single_data)]
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
    Hisory=as.data.frame(getSymbols(input$symbol,from=input$date,to=input$date_end,env = NULL))
    chartSeries(Hisory,theme=chartTheme('white'))
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
}