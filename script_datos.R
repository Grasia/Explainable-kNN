library(zoo)
library(foreach)
library(forecast)
library(tseries)
library(knnp)

### Choose which series to use. If it has timestamps use them for plotting, else use the index
# y <- nottem
y <- datasets::sunspot.month
dates <- as.Date(time(y))
full_dates <- as.Date(ts( c(y,1), start = time(y)[1], frequency = frequency(y) ))

# y <- as.matrix(interCompFore, ncols = 1)
# dates <- 1:length(y)
# full_dates <- 1:(length(y)+1)

# y <- as.matrix(btcnWeekRend, ncols = 1)
# dates <- as.Date(read.csv("D:/Daniel/Investigación/BTC-EUR-weekly.csv")[,1])
# full_dates <- as.Date(ts( c(y,1), start = time(y)[1], frequency = frequency(y) ))

# y <- ts(get("rain"), start = "1914-01-01", frequency = 365)
# dates <- as.Date(time(y))
# full_dates <- as.Date(ts( c(y,1), start = time(y)[1], frequency = frequency(y) ))

# y <- forecast::taylor
# # # y <- tail(y, length(y)*0.9)
# dates <- 1:length(y)
# full_dates <- c(dates, length(y) + 1)

# Then we set all the optimization parameters
n <- NROW(y)
train_init <- floor(n * 0.75)
test_init <- floor(n * 0.9)
y_train <- head(y, test_init)
distance <- "manhattan"
error_measure <- "RMSE"
weight <- "proportional"
n_threads <- 8
ks <-  1:50
ds <- 1:50
min_y <- min(y)
max_y <- max(y)

# Get errors matrix, and best k and d combination
res <- knn_param_search(y = y_train, k = ks, d = ds, initial = train_init, distance = distance, 
                        error_measure = error_measure, weight = weight, threads = n_threads)

# Generate the forecast from the beginning of the train to the end of the series
optimal <- knn_past(y = y, k = res$opt_k, d = res$opt_d, initial = train_init, 
                    distance = distance, weight = weight, threads = n_threads)

# And split the forecast between train and test intervals
optimal_train <- head(optimal$fitted, test_init - train_init )
optimal_test <- tail(optimal$fitted, n - test_init )

# Split series to measure train and test accuracy
y_err <- tail(y, (n - train_init )) # ts(y[(train_init + 1):n]) #  
y_train_err <-  y[(train_init + 1):test_init] # tail( head(y, test_init), (test_init - train_init)) #
y_test_err <- y[(test_init + 1):n] #  tail(y, (n - test_init - 1) ) #

# Get train+test dates for plots
sub_dates <- tail(dates, length(y) - train_init)

# Generate naive forecast
naive <- y[train_init:(n - 1)] 

# sesPred <- c(y_train_err, y_test_err) - tail(tsCV(y = y, ses, h = 1, initial = train_init, window = 1), n - train_init)
# etsPred <- y[(train_init + 1):n] - na.remove(tsCV(y = y, stlf, h = 1, initial = train_init - 1))
# attr(sesPred, "na.removed") <- NULL

# stlfPred <- y[(train_init + 1):n] - na.remove(tsCV(y = y, stlf, h = 1, initial = train_init - 1))
# attr(stlfPred, "na.removed") <- NULL

# stlfPred <- rep(1, length(length(naive)))
# for (i in 1:(n-train_init)) {
#     stlfPred[i] <- forecast::stlf(y = head(y, (train_init+i-1)), h = 1)$mean
# }

# etsPred <- rep(1, length(naive))
# for (i in 1:(n - train_init)) {
#     etsPred[i] <- forecast(forecast::ets(y = head(y, (train_init + i - 1))), h = 1)$mean
# }

# seasnaiPred <- c(y_train_err, y_test_err) - head(tail(tsCV(y = y, snaive, h = 1, initial = train_init - 1), length(optimal$mean)), n - train_init - 1)
# seasnaiPred <- y[(train_init + 1):(n)] - na.remove(tsCV(y = y, snaive, h = 1, initial = train_init - 1))
# attr(seasnaiPred, "na.removed") <- NULL

# minimums <- head(sort.int(res$errors, index.return = TRUE)$ix , 5)
minimums <- sort.int(res$errors, index.return = TRUE)$ix

# Indexes of K and D minimums
x_minims <- ((minimums - 1) %% length(ks)) + 1
y_minims <- ceiling(minimums/length(ks))


# Data for residuals
residuals_matrix <- matrix(nrow = 5, ncol = length(y_err))
residuals_matrix[1, ] <- y_err - optimal$fitted
residuals_matrix[2, ] <- y_err - naive

# Data for errors table
names_col <- c("Optimal", "Naive", "Seasonal Naive", "", "")

optimal_train_error <- accuracy(optimal_train, y)
optimal_test_error  <- accuracy(optimal_test, y)

naive_train_error <- accuracy(naive[1:length(y_train_err)], y_train_err)
naive_test_error  <- accuracy(naive[(length(y_train_err) + 1):length(naive)], y_test_err)

# ets_train_error <- accuracy(etsPred[1:length(y_train_err)], y_train_err)
# ets_test_error  <- accuracy(etsPred[(length(y_train_err) + 1):length(naive)], y_test_err)
# 
# snai_train_error <- accuracy(seasnaiPred[1:length(y_train_err)], y_train_err)
# snai_test_error  <- accuracy(seasnaiPred[(length(y_train_err) + 1):length(naive)], y_test_err)

# stlf_train_error <- accuracy(stlfPred[1:length(y_train_err)], y_train_err)
# stlf_test_error  <- accuracy(stlfPred[(length(y_train_err) + 1):length(naive)], y_test_err)

# Errors heatmap plot information
cont_min <- min(res$errors)
cont_max <- max(res$errors)
cont_max_fix <- (cont_max - (cont_max - cont_min) * 0.4)
num_contours <- 18

# Generate errors matrix for error table in Optimization tab
errors_matrix <- matrix(nrow = 2, ncol = 10)
colnames(errors_matrix) <- c("Train-ME", "RMSE", "MAE", "MPE", "MAPE", "Test-ME", "RMSE", "MAE", "MPE", "MAPE")
rownames(errors_matrix) <- c("kNN", "Naive") #, "ets", "SeasNai")
errors_matrix[1, ] <- c(optimal_train_error[1:5], optimal_test_error[1:5])
errors_matrix[2, ] <- c(naive_train_error[1:5], naive_test_error[1:5])
# errors_matrix[3, ] <- c(ets_train_error[1:5], ets_test_error[1:5])
# errors_matrix[4, ] <- c(snai_train_error[1:5], snai_test_error[1:5])
# errors_matrix[5, ] <- c(stlf_train_error[1:5], stlf_test_error[1:5])

errors_matrix_tab1 <- errors_matrix
errors_matrix_tab2 <- errors_matrix

# Data for selected methods
selected_methods <- rep(FALSE, 5)

# Data for selected points in contour
selected_points <- matrix(rep(FALSE, NROW(res$errors) * NCOL(res$errors)), nrow = NROW(res$errors), ncol = NCOL(res$errors))
# selected_points_aux <<- selected_points
previous_countour <<- "default"

# Standard deviation of distances to k-nearest neighbors of all predictions 
future_values <- matrix(y[optimal$neighbors], nrow = nrow(optimal$neighbors))
neighs_stdev <- sqrt(rowSums((t(future_values) - colMeans(future_values))**2) / (nrow(future_values) - 1))
rm(future_values)

# lowess_DistStddev <- lowess(x = optimal$knn_dists, y = neighs_stdev)
# lowess_DistErr <- lowess(x = optimal$knn_dists, y = abs(residuals_matrix[1, ]))
# lowess_StddevErr <- lowess(x = neighs_stdev, y = abs(residuals_matrix[1, ]))

# Set confidence interval for loess prediction
confid_int <- 0.95

# Loess model for Deviation depending on Distance
loess_mod_DisDev <- loess(neighs_stdev ~ optimal$knn_dists)
loess_ind_DisDev <- sort.int(loess_mod_DisDev$x[,1], index.return = TRUE)$ix
loess_mean_DisDev <- predict(loess_mod_DisDev, se = TRUE)
loess_ci_DisDev <- qt(confid_int,loess_mean_DisDev$df) * loess_mean_DisDev$se

# Loess model for Error depending on Distance
loess_mod_DisErr <- loess(abs(residuals_matrix[1, ]) ~ optimal$knn_dists, span = 0.75)
loess_ind_DisErr <- sort.int(loess_mod_DisErr$x[,1], index.return = TRUE)$ix
loess_mean_DisErr <- predict(loess_mod_DisErr, se = TRUE)
loess_ci_DisDev <- qt(confid_int, loess_mean_DisErr$df) * loess_mean_DisErr$se

# Loess model for Error depending on Deviation
loess_mod_DevErr <- loess(abs(residuals_matrix[1, ]) ~ neighs_stdev, span = 0.75)
loess_ind_DevErr <- sort.int(loess_mod_DevErr$x[,1], index.return = TRUE)$ix
loess_mean_DevErr <- predict(loess_mod_DevErr, se = TRUE)
loess_ci_DisDev <- qt(confid_int, loess_mean_DevErr$df) * loess_mean_DevErr$se

# Generate loess model for all combinations of distances and deviations
loess_points <- 100
loess_distances <- seq.int(from = min(optimal$knn_dists), to = max(optimal$knn_dists), length.out = loess_points)
loess_deviations <- seq.int(from = min(neighs_stdev), to = max(neighs_stdev), length.out = loess_points)
loess_DisDev_Err <- loess(abs(residuals_matrix[1, ]) ~ optimal$knn_dists + neighs_stdev)
loess_DisDev_heatmap <- matrix(predict(object = loess_DisDev_Err, newdata = matrix( 
          c(rep(loess_distances, each = loess_points), rep(loess_deviations, times = loess_points)), ncol = 2)), nrow = loess_points)
loess_DisDev_heatmap[loess_DisDev_heatmap < 0] <- NA





# chosK_nice
# [1]  9 28  1 28  1  2  3  3 15  1
# chosD_nice
# [1] 10 18  3  4  3  7  5  9  2  2
# plot_ly(x = 1:(n - floor(NROW(y) * 0.9)), y = tail(y, n - floor(NROW(y) * 0.9)), type = "scatter", mode = "lines", name = "Observed") %>% 
#   add_trace(y = knn_past(y, res$opt_k, res$opt_d, initial = floor(NROW(y) * 0.9))$fitted, name = "param_search") %>% 
#   add_trace( y = cf_prediction(y, chosK_nice, chosD_nice)$predictions, name = "Comb.Forec.")

