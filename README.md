# Privacy and Forecasting Empirical Analysis

Paper examining the effects of various privacy protection methods on the forecast accuracy achieved by multivariate forecasting models.

### *Forecasting Models to Implement:*

* ~~Exponential smoothing~~
   - ~~SES~~
   - ~~DES~~
   - ~~TES~~
* ~~Autoregressive~~
* ~~Light GBM~~
* LSTM (pinball loss) - I think pinball loss is for training uncertainty forecasts. Do we want to examine these and point forecasts?
* ~~Prophet~~
* ~~NeuralProphet~~
* ~~VAR model (have to see if scalable)~~

Code for some of these methods (in R) can be found [here](https://github.com/Mcompetitions/M4-methods) from the M4 competition.

### *Data Protection Methods to Implement:*

(want 'simple' ones)

* Additive noise
* Method from Schneider and Lee (2021) - *k*-mTS
* Differential privacy
* Look up some more innovative ones
* Other methods applied to disaggregate time series?

### *Model Accuracy Metrics*

* RMSE (root mean-squared error)
* MAE (mean absolute error)

**Goal**: determine whether forecast accuracy is better or worse for each model for each protection method.
