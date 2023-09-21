from data_processing_functions import *

x = [pd.Series([5, -3, -3, 2, 10])]

diffed = difference_to_stationarity(x)

print(diffed)

undiffed = reverse_difference_to_stationarity(h=1, forecasts=diffed, ts_data=x, is_simulated=True)

print(diffed)
print(undiffed)