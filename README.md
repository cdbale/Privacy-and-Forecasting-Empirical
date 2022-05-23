# Privacy and Forecasting Empirical Analysis

Paper examining the effects of various privacy protection methods on the forecast accuracy achieved by multivariate forecasting models.

### *Forecasting Models to Implement:*

* ~~Exponential smoothing~~
   - ~~SES~~
   - ~~DES~~
   - ~~TES~~
* ~~Autoregressive~~
* ~~LGBM - univariate~~
* Prophet
* NeuralProphet
* ~~VAR model~~
* Lasso-Var?
* GreyKite (LinkedIn)
* RNN
* LSTM (multivariate)

Code for some of these methods (in R) can be found [here](https://github.com/Mcompetitions/M4-methods) from the M4 competition.

### *Data Protection Methods to Implement:*

(want 'simple' ones)

* ~~Additive noise~~
* ~~Top coding (.10, .20, .40)~~
* ~~Bottom coding (.10, .20, .40)~~
* Method from Schneider and Lee (2021) - *k*-mTS
* ~~Differential privacy~~

### *Model Accuracy Metrics*

* MAE (mean absolute error)

**Goal**: determine whether forecast accuracy is better or worse for each model for each protection method.
