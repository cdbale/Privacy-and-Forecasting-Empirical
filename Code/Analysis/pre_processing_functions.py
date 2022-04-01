##### This file contains various functions used to pre-process
##### time series data. Functions include:
#####   -
#####   -

##### Author: Cameron Bale

################################################################################

import pandas as pd
from sktime.transformations.series.detrend import ConditionalDeseasonalizer

# perform conditional deseasonalization
def deseasonalize(ts_data, sp, seasonality_type):
    """
    Performs deseasonalization conditional on 90% autocorrelation seasonality test.

    :param ts_data: pandas dataframe containing series in rows, time periods in
        columns.
    :param sp: seasonal periodicity.
    :param seasonality_type: what type of model to use to estimate seasonal
        component, either "additive" or "multiplicative".
    :return transformed_data: pandas dataframe containing the transformed series
        in rows, time periods in columns.
    """

    # instantiate conditional deseasonalizer
    transformer = ConditionalDeseasonalizer(sp=sp, model=seasonality_type)

    # fit and transform each series
    transformed_data = pd.concat([transformer.fit_transform(row) for _, row in ts_data.iterrows()], axis=1).T
    
    return transformed_data
