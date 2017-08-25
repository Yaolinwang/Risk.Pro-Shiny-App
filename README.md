# Risk.Pro - Risk Management Tool
[https://riskpro.shinyapps.io/riskpro/](https://riskpro.shinyapps.io/riskpro/)

## What is Risk.Pro?
Risk.Pro is a Shiny based web-app. With Risk-Pro, you can:
* Evaluate the market risk of portfolio/single asset.
* Measure historical VaR, Parametric VaR, Monte Carlo Simulated VaR and expected shortfall for each position in the portfolio.
* Conduct Stress Testing on given positions in 3 different CCAR scenarios, calculated corresponding stressed VaR.
* Visualize portfolio performance over time with benchmark (SPY 500); presented descriptive statistics (Confidence Interval, etc.) and graphs (Q-Q plot, histogram, return distribution, etc.)

## How does it work?
There are three sections.
First, based on your input, Risk.Pro retrieves data from Google API.
Then, Risk.Pro feeds data into calculation functions to come out results.
Finally, Risk.Pro presents results in table/graphs.
