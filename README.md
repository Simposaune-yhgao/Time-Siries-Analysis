# Time-Siries-Analysis
- The goal of my final project is to predict the return of my own portfolio using time seireis theory and R.
- I used the data of exchange-traded equity stock, in 250-trading-day intervals to predict 10-day incoming returns and volatilities. I ended up constructed a portfolio using 3-year predicted data and found an optimistic return-volatility ratio, which is the Sharpe ratio, at 8.74.
- I have found that these 4 stock return data can all be fit in ARIMA models. Consequently, I can use 250-day moving windows to predict the next 10 days’ return and volatility.
- In the procedure of selecting models, I have done the ACF to check if they are stationary, and the Box test to test if they have ARCH effect.
- Using those predicted values to form a portfolio on the efficient frontier, as known as the “Modern Portfolio Theory”. This means that I can find portfolio weights that have the highest return on each different volatility.
- I calculated and adjusted the portfolio’s weight every 3 months, using the predicted return data. Next, I set the risk-free rate at 2.57% and concluded that the 3-year backtesting Sharpe ratio is at a reasonable 8.74.
