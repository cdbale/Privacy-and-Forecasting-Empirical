# Privacy and Forecasting Empirical Analysis

Paper examining the effects of various privacy protection methods on the forecast accuracy achieved by multivariate forecasting models.

### *Forecasting Models to Implement:*

* Exponential smoothing
   - SES
   - DES
   - TES
* Autoregressive
* Light GBM
* LSTM (pinball loss)
* VAR model (have to see if scalable)

Code for some of these methods (in R) can be found [here](https://github.com/Mcompetitions/M4-methods) from the M4 competition.

### *Data Protection Methods to Implement:*

* Additive noise
* Bottom Coding
* Top coding
* Method from Schneider and Lee (2021) - *k*-mTS
* Other methods applied to disaggregate time series?

### *Model Accuracy Metrics*

* RMSE (root mean-squared error)
* MAE (mean absolute error)

**Goal**: determine whether forecast accuracy is better or worse for each model for each protection method.
