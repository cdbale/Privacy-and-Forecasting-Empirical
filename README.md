# Privacy-and-Forecasting-Empirical

Paper examining the effects of various privacy protection methods on the forecast accuracy achieved by multivariate forecasting models.

*Forecasting Models to Implement:*

* Exponential smoothing
* Autoregressive
* Light GBM
* LSTM (pinball loss)
* VAR model (have to see if scalable)

*Data Protection Methods to Implement:*

* Random noise
* Method from Schneider and Lee (2021)
* Other methods applied to disaggregate time series

*Model Accuracy Metrics*

* RMSE (room mean-squared error)
* MAE (mean absolute error)

**Goal**: determine whether forecast accuracy is better or worse for each model for each protection method.
