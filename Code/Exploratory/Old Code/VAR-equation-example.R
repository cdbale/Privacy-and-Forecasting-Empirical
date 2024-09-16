actuals <- c(3360, 2010, 2760, 5760, 2410)

unprotected_data <- matrix(c(1200, 3480, 2490, 2670, 3600, 6840, 2160, 3000, 2190, 2880), ncol=2, byrow=TRUE)
protected_data <- matrix(c(1600,	1758.205433, 3180,	1933.834022, 2400,	3729.685974, 2813.994085,	2850, 2945.805122,	2820), ncol=2, byrow=TRUE)

log_unprotected_data <- log(unprotected_data)
log_protected_data <- log(protected_data)

dlog_unprotected_data <- log_unprotected_data[,2] - log_unprotected_data[,1]
dlog_protected_data <- log_protected_data[,2] - log_protected_data[,1]

##### create coefficient matrix for unprotected model

# Provided rounded intercept coefficients
intercept_coefficients <- c(-0.008822313, -0.000550054, -0.001097389, -0.004822254, -0.00934684)

# Provided values for the second column
second_column_values <- c(-0.673239447, 0.043568457, -0.06696914, -0.120771019, -0.013059014)

# Provided values for the third column
third_column_values <- c(-0.157493931, -0.515407024, -0.19580173, -0.035989974, 0.088335228)

# Provided values for the fifth column
fourth_column_values <- c(-0.055855153, 0.019757831, -0.497117791, -0.080006832, -0.036863632)

# Provided values for the sixth column
fifth_column_values <- c(0.080285315, 0.011943105, 0.087755359, -0.718771621, -0.004640593)

# Provided values for the seventh column
sixth_column_values <- c(0.317319166, -0.27683028, 0.0113765, 0.557241776, -0.302574508)

# Create a 5 x 7 matrix
var_matrix <- matrix(NA, nrow = 5, ncol = 6)

# Fill the first three columns with rounded intercept coefficients and provided values
var_matrix[, 1:6] <- cbind(intercept_coefficients, second_column_values, third_column_values,
                           fourth_column_values, fifth_column_values, sixth_column_values)

# round to three decimals
var_matrix <- round(var_matrix, 3)

# Print the resulting matrix
print(var_matrix)

# generate unprotected forecasts
unprotected_fcasts <- var_matrix %*% matrix(c(1.0, dlog_unprotected_data), ncol=1)
protected_fcasts <- var_matrix %*% matrix(c(1.0, dlog_protected_data), ncol=1)

unprotected_fcasts <- log_unprotected_data[,2] + unprotected_fcasts
protected_fcasts <- log_protected_data[,2] + protected_fcasts

unprotected_fcasts <- exp(unprotected_fcasts)
protected_fcasts <- exp(protected_fcasts)

mean(abs(unprotected_fcasts-actuals))
mean(abs(protected_fcasts-actuals))

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

##### create coefficient matrix for protected model

# Provided rounded intercept coefficients
intercept_coefficients <- c(-0.023508343, -0.008379707, -0.000597788, -0.002109143, -0.004151323)

# Provided values for the second column
second_column_values <- c(-0.445783983,	0.104429834,	0.004818401,	0.1510992,	0.012095048)

# Provided values for the third column
third_column_values <- c(-0.249989344,	-0.536472256,	0.385088873,	-0.191591416,	-0.215040648)

# Provided values for the fifth column
fourth_column_values <- c(-0.143005988,	-0.048649332,	-0.643452186,	-0.041083396,	-0.021744102)

# Provided values for the sixth column
fifth_column_values <- c(0.006996057,	-0.010054954,	0.129259356,	-0.560590176,	-0.167944022)

# Provided values for the seventh column
sixth_column_values <- c(-0.044223019,	-0.105082367,	-0.124574228,	0.147320125,	-0.426968035)

# Create a 5 x 7 matrix
var_matrix <- matrix(NA, nrow = 5, ncol = 6)

# Fill the first three columns with rounded intercept coefficients and provided values
var_matrix[, 1:6] <- cbind(intercept_coefficients, second_column_values, third_column_values,
                           fourth_column_values, fifth_column_values, sixth_column_values)

# round to three decimals
var_matrix <- round(var_matrix, 3)

# Print the resulting matrix
print(var_matrix)

# generate unprotected forecasts
unprotected_fcasts <- var_matrix %*% matrix(c(1.0, dlog_unprotected_data), ncol=1)
protected_fcasts <- var_matrix %*% matrix(c(1.0, dlog_protected_data), ncol=1)

unprotected_fcasts <- log_unprotected_data[,2] + unprotected_fcasts
protected_fcasts <- log_protected_data[,2] + protected_fcasts

unprotected_fcasts <- exp(unprotected_fcasts)
protected_fcasts <- exp(protected_fcasts)

mean(abs(unprotected_fcasts-actuals))
mean(abs(protected_fcasts-actuals))

