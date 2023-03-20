# Readme

This repo consists of my solution for the forecasting track of the M6 competition where **I made the third place** in Februrary 2022. **My final score was 0.15649 which is about 0.03% less good than the first place.**

| Name                  | Rank   | Score  | 
| :----                 |-----:  |------------:|
| Dan                   |  1     |   0.15645   |     
| Miguel Pérez Michaus  |  2     |   0.15648   |           
| **SebastianR**            |  **3**     |    **0.15649**  |        

For further details see: [M6 Leaderboard](https://m6competition.com/Leaderboard)

Currently the code consists only of my final version for the probability forecasts, which I used starting with September 2022 for the submissions 8 to 12. But my basic assumption for modeling was the same throughout the competition: Equity prices follow a geometric random walk with drift. Therefore I used the differences of the log adjusted close prices as target variable.

For modeling I used the following steps:

1.) Estimation of the conditional mean given a stack of linear models that was fitted over weekly data (Datasource: alphavantage). On the one hand using all 100 (99) equities of the M6-Universe. On the other hand extending the M6-Universe by a few hundred further stocks and etfs that are traded at NASDAQ. 

2.) The covariance estimation took place using daily adjusted log close data. (Datasource: yahoo)

3.) Monte-Carlo-Sampling from a multivariate t-distribution with 4 degrees of freedom. The covariance matrix from step 2 was used without scaling. 

4.) Counting and averaging ranks for each of the M6-equities.


### Sample Code

To fetch the relevant data for modeling run. 

```
source("fetch_data.r")
```

As datasource only yahoo is used, since it is free. (In contrast to alphavantage, which I used for the competition for estimating the conditional mean.)

Then generate the probabilities by executing.

```
source("generate_rank_probabilities.r")
```

The file 'probabilities.csv' will be saved in your working directory.
