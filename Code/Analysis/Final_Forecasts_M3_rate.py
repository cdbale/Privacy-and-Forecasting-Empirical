## Forecast for the original and baseline protected (additive noise and differential privacy)## data sets for the second to last time period (h2), i.e., the last time period that is available## to the data owner. The forecast error MAE is computed and will be predicted using time## series features in the feature selection methodology for k-nTS+.# Author: Cameron Baleimport timeimport osfrom forecasting_functions import *from error_distribution_generator import *data_folder = "M3_rate/"# store the cleaned data file names including the baseline protected data setscleaned_data_path = "../../Data/Cleaned/" + data_foldercleaned_files = os.listdir(cleaned_data_path)h1_train_files = [x for x in cleaned_files if "_h1_train" in x]h1_test_files = [x for x in cleaned_files if "_h1_test" in x]forecasts_path = "../../Outputs/Forecasts/" + data_folderresults_path = "../../Outputs/Results/" + data_folder# loop over forecasting models and training data files# for seasonal models, we have to account for some data not having a seasonal period.# for TES, we only apply it to monthly and quarterly data# we applied the log prior to the rate transformation, and we no longer want to truncate# since rates can be negativeforecasting_models = {"SES": {"sp":None, "truncate":False, "log":False, "mean_normalize":False, "options":None},                      "DES": {"sp":None, "truncate":False, "log":False, "mean_normalize":False, "options":None},                      "TES": {"sp":None, "truncate":False, "log":False, "mean_normalize":False, "options":None},                      "ARIMA": {"sp":None, "truncate":False, "log":False, "mean_normalize":False, "options":None},                      "VAR": {"sp":None, "truncate":False, "log":False, "mean_normalize":False, "options": {'save_params': True, 'simulate_series': True}},                      "LGBM": {"sp":None, "truncate":False, "log":False, "mean_normalize":True, "options": {'max_samples_per_ts': None, 'window_length': None}},                      "RNN": {"sp":None, "truncate":False, "log":False, "mean_normalize":True, "options": {'input_chunk_length': None, 'training_length': None, 'max_samples_per_ts': 10, 'num_ensemble_models': 10, 'use_gpu': False}}}for m in forecasting_models.items():        model = m[0]    model_args = m[1]    for f in h1_train_files:                if "monthly" in f:            model_args["sp"] = 12            if model == "LGBM":                model_args["options"]["window_length"] = 25            elif model == "RNN":                model_args["options"]["input_chunk_length"] = 25                model_args["options"]["training_length"] = 30        elif "quarterly" in f:            model_args["sp"] = 4            if model == "LGBM":                model_args["options"]["window_length"] = 11            elif model == "RNN":                model_args["options"]["input_chunk_length"] = 11                model_args["options"]["training_length"] = 13        else:            if model == "TES":                continue            model_args["sp"] = 1            if model == "LGBM":                model_args["options"]["window_length"] = 11            elif model == "RNN":                model_args["options"]["input_chunk_length"] = 11                model_args["options"]["training_length"] = 13                    test_file = [x for x in h1_test_files if x[:-9] in f]        [test_file] = test_file        generate_and_save_forecasts(data_folder=data_folder,                                    train_file=f,                                    test_file=test_file,                                    forecasts_path=forecasts_path,                                    results_path=results_path,                                    model=model,                                    model_args=model_args,                                    h=1,                                    file_suffix=f)print('Forecasting done.')############################################################################################################### generate error distributions for forecasts made for second to last period - these are## used in k-nTS+# create a list of the original file namesog_files = os.listdir(cleaned_data_path)og_files = [x[:-12] for x in og_files if not any(y in x for y in ['_train', '_h1_', 'k-nts', 'AN_', 'DP_', '.DS_Store'])]# for each of these, we need to extract the error distributions for all forecasting models and # baseline data setsmodels = ["SES", "DES", "TES", "ARIMA", "VAR", "LGBM", "RNN"]protection_methods = {"original": [""],                      "AN_": [0.25, 0.5, 1, 1.5, 2],                      "DP_": [0.1, 1, 4.6, 10, 20],                      "k-nts_": [3, 5, 7, 10, 15],                      "k-nts-plus_": [3, 5, 7, 10, 15],                      "k-nts-plus-bounded_": [3, 5, 7, 10, 15]}# make column to store times for error computation and save since this file# will be called inside the `error_distribution_generator` functionfor f in og_files:    error_distribution_generator(data_folder=data_folder,                                 file_string=f,                                 forecasts_path=forecasts_path,                                  results_path=results_path,                                  model_list=models,                                  protection_method_dict=protection_methods,                                  forecast_horizon="h1",                                 track_comp_time=False,                                 is_rate=True,                                 inverse_rate=True)