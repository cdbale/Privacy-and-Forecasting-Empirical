# Can We Protect Time Series Data While Maintaining Accurate Forecasts?

This repository contains the code and (links to) the data used to produce the results for [this paper](https://www.researchgate.net/publication/372621568_Can_We_Protect_Time_Series_Data_While_Maintaining_Accurate_Forecasts), written by Cameron D. Bale, Matthew J. Schneider, and Jinwook Lee.

Abstract: We evaluate the usefulness of protected time series by exploring how privacy protection affects forecast accuracy. Using both simulated and real-world time series data sets, we test various privacy methods, including a proposed swapping-based method (*k*-nTS+) designed to maintain time series features, a differentially private method, and an approach based on sharing model weights trained on unprotected data. Based on forecasts from both simple and machine learning models, we find that none of the privacy methods can consistently maintain forecast accuracy at an acceptable level of privacy. We also show that sharing model weights enables accurate forecasts, but accurate forecasts can be used to uncover the identities of protected time series. To overcome these problems, we transform continuous time series into bounded rates to increase the similarities of features, values, and forecasts across time series. This enables our proposed method to produce protected data with a reduction in average forecast accuracy of just 6%. Overall, we acknowledge that except under certain conditions, generating time series with acceptable privacy levels is incompatible with the goal of obtaining accurate forecasts.

---

