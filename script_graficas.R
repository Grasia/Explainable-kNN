library(plotly)
library(shiny)
library(shinyWidgets)
library(DT)
library(rdist)

# Set some useful colors
colPalette <- c("royalblue", "seagreen", "orange", "aquamarine", "blueviolet", "darkred", "peru","darkcyan" , "salmon")

#                                     Plots for main tab
pMain <- plot_ly(x = dates, y = y, type = "scatter",  name = "Observed Time Series", mode = "lines", source = "main", 
                 legendgroup = "real", hoverinfo = "x+y" , line = list(color = colPalette[1]))

# Draw superior line over train set
pMain <- add_segments(pMain, x = dates[train_init+1], xend = dates[test_init], y = 1.05 * max(y[(train_init+1):test_init]),
                      yend = 1.05 * max(y[(train_init+1):test_init]), name = "Train", showlegend = FALSE, text = paste0("Train (", dates[train_init+1]," / ", dates[test_init], ")"),
                      hoverinfo = "text", legendgroup = "lines", line = list(color = colPalette[1], width = 1.5, dash = "dash"))
# Draw inferior line under train set
pMain <- add_segments(pMain, x = dates[train_init+1], xend = dates[test_init], y = 0.95 * min(y[(train_init+1):test_init]),
                      yend = 0.95 * min(y[(train_init+1):test_init]), name = "Train", showlegend = FALSE, paste0("Train (", dates[train_init+1]," / ", dates[test_init], ")"),
                      hoverinfo = "text", legendgroup = "lines", line = list(color = colPalette[1], width = 1.5, dash = "dash"))
# Draw superior line over test set
pMain <- add_segments(pMain, x = dates[test_init+1], xend = dates[length(dates)], y = 1.05 * max(y[(test_init+1):n]),
                      yend = 1.05 * max(y[(test_init+1):n]), name = "Test", showlegend = FALSE, text = paste0("Test (", dates[test_init+1]," / ", dates[length(dates)], ")"),
                      hoverinfo = "text", legendgroup = "lines", line = list(color = colPalette[1], width = 1.5, dash = "dash"))
# Draw inferior line under test set
pMain <- add_segments(pMain, x = dates[test_init+1], xend = dates[length(dates)], y = 0.95 * min(y[(test_init+1):n]),
                      yend = 0.95 * min(y[(test_init+1):n]), name = "Test", showlegend = FALSE, text = paste0("Test (", dates[test_init+1]," / ", dates[length(dates)], ")"),
                      hoverinfo = "text", legendgroup = "lines", line = list(color = colPalette[1], width = 1.5, dash = "dash"))

# Plot shaded rectangles under train and test sets
pMainShapes <- list(
  list(type = "rect",
       fillcolor = colPalette[1], line = list(color = colPalette[1]), opacity = 0.1,
       x0 = dates[train_init + 1], x1 = dates[test_init], xref = "x", yref = "y", 
       # y0 = min_y - 0.05 * (max_y - min_y), y1 = max_y + 0.05 * (max_y - min_y)),
       y0 = 0.95 * min(y[(train_init + 1):test_init]), y1 = 1.05 * max(y[(train_init + 1):test_init]) ),
  list(type = "rect",
       fillcolor = colPalette[1], line = list(color = colPalette[1]), opacity = 0.15,
       x0 = dates[test_init], x1 = dates[length(dates)], xref = "x", yref = "y",
       # y0 = min_y - 0.05 * (max_y - min_y), y1 = max_y + 0.05 * (max_y - min_y))))
       y0 = 0.95 * min(y[(test_init + 1):n]), y1 = 1.05 * max(y[(test_init + 1):n]) ) ) 

pMainLayout <- pMain

# Add layout information for x-axis range and slider
pMain <- layout(pMain, xaxis = list(range = list( dates[1], dates[length(dates)]), 
                                    rangeslider = list(range = list( dates[1], dates[length(dates)]) )), shapes = pMainShapes)

pMainBase <- pMain

# ############################## COMPROBAR SI SOBRA
# Add forecast of best k-d combination to plot
# pMain <- add_trace(pMain, x = sub_dates, y = optimal$fitted, legendgroup = "optim", 
#                    name = paste0("Optimal (k = ", res$opt_k, ", d = ", res$opt_d, ")"), line = list(color = colPalette[2]))


# Distances to all neighbors
pkNNDistOptim <- plot_ly(name = "Neighbors distances", showlegend = TRUE, hoverinfo = "x+y",
                         type = "bar", marker = list(color = colPalette[3]),
                         # x = sub_dates, y = optimal$knn_dists/optimal$model$k)
                         x = head(tail(dates, length(y) + 1 - train_init), length(sub_dates)),
                         y = optimal$knn_dists/optimal$model$k)


# Neighbors' future values variance plot
pNeighStdevOptim <- plot_ly(name = "Future values variance", showlegend = TRUE, hoverinfo = "x+y",
                            type = "bar", marker = list(color = colPalette[1]),
                            # x = sub_dates, y = neighs_stdev)
                            x = head(tail(dates, length(y) + 1 - train_init), length(sub_dates)), y = neighs_stdev)


# Scatter plots to see relation between Distances, Std. Deviations and Errors, 2 by 2
pScatDistStddevOptim <- plot_ly(name = "Distances vs Std.Dev.", type = 'scatter', mode = "lines", hoverinfo = "x+y",
                                x = loess_mod_DisDev$x[,1][loess_ind_DisDev], y = loess_mean_DisDev$fit[loess_ind_DisDev], showlegend = FALSE) %>%
  add_trace(y = (loess_mean_DisDev$fit - qt(0.95, loess_mean_DisDev$df) * loess_mean_DisDev$se.fit)[loess_ind_DisDev]) %>%
  add_trace(y = (loess_mean_DisDev$fit + qt(0.95, loess_mean_DisDev$df) * loess_mean_DisDev$se.fit)[loess_ind_DisDev], 
            fill = "tonexty", fillcolor = "rgba(0,0,100,0.35)") %>%
  add_trace(x = optimal$knn_dists, y = neighs_stdev, mode = "markers", marker = list(opacity = 0.4),
            hoverinfo = "x+y+text", text = head(tail(dates, length(y) + 1 - train_init), length(sub_dates))) %>%
  layout(xaxis = list(title = "Distances"), yaxis = list(title = "Std. Dev."))

pScatDistErrOptim <- plot_ly(name = "Distances vs Errors", type = 'scatter', mode = "lines", hoverinfo = "x+y",
                             x = loess_mod_DisErr$x[,1][loess_ind_DisErr], y = loess_mean_DisErr$fit[loess_ind_DisErr], showlegend = FALSE) %>%
  add_trace(y = (loess_mean_DisErr$fit - qt(0.95,loess_mean_DisErr$df) * loess_mean_DisErr$se.fit)[loess_ind_DisErr]) %>%
  add_trace(y = (loess_mean_DisErr$fit + qt(0.95,loess_mean_DisErr$df) * loess_mean_DisErr$se.fit)[loess_ind_DisErr], 
            fill = "tonexty", fillcolor = "rgba(0,0,100,0.35)") %>%
  add_trace(x = optimal$knn_dists, y = abs(residuals_matrix[1, ]), mode = "markers", marker = list(opacity = 0.4),
            hoverinfo = "x+y+text", text = head(tail(dates, length(y) + 1 - train_init), length(sub_dates))) %>%
  layout(xaxis = list(title = "Distances"), yaxis = list(title = "Errors"))

pScatStddevErrOptim <- plot_ly(name = "Std. Dev. vs Errors", type = 'scatter', mode = "lines", hoverinfo = "x+y",
                               x = loess_mod_DevErr$x[,1][loess_ind_DevErr], y = loess_mean_DevErr$fit[loess_ind_DevErr], showlegend = FALSE) %>%
  add_trace(y = (loess_mean_DevErr$fit - qt(0.95,loess_mean_DevErr$df) * loess_mean_DevErr$se.fit)[loess_ind_DevErr]) %>%
  add_trace(y = (loess_mean_DevErr$fit + qt(0.95,loess_mean_DevErr$df) * loess_mean_DevErr$se.fit)[loess_ind_DevErr], 
            fill = "tonexty", fillcolor = "rgba(0,0,100,0.35)") %>%
  add_trace(x = neighs_stdev, y = abs(residuals_matrix[1, ]), mode = "markers", marker = list(opacity = 0.4),
            hoverinfo = "x+y+text", text = head(tail(dates, length(y) + 1 - train_init), length(sub_dates))) %>%
  layout(xaxis = list(title = "Std. Dev."), yaxis = list(title = "Errors"))




#                               Plots for optimization tab
pOpt <- plot_ly(x = dates, y = y, type = "scatter",  name = "Observed Time Series", 
                mode = "lines", legendgroup = "real", hoverinfo = "x+y", line = list(color = colPalette[1])) %>%
  add_trace(pOpt, x = sub_dates, y = optimal$fitted, legendgroup = "optim", 
            name = paste0("Optimal (k = ", res$opt_k, ", d = ", res$opt_d, ")"), line = list(color = colPalette[2])) %>%
  add_segments(pOpt, x = dates[train_init], xend = dates[train_init], y = min_y - 0.05 * (max_y - min_y), 
               yend = max_y + 0.05 * (max_y - min_y), name = "Train", showlegend = FALSE, text = "Train", 
               hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash")) %>%
  add_segments(pOpt, x = dates[test_init], xend = dates[test_init], y = min_y - 0.05 * (max_y - min_y), 
               yend = max_y + 0.05 * (max_y - min_y), name = "Test", showlegend = FALSE, text = "Test", 
               hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash")) %>%
  layout(pOpt, xaxis = list(rangeslider = list(visible = TRUE)))

pOptBase <- pOpt

# Error bars basic type of plot
pErrorsOpt <- plot_ly(type = "bar", hoverinfo = "x+y") %>% layout(title = "Prediction errors")
pBarsOptBase <- pErrorsOpt

combPlotOpt <- subplot(pOpt, pBarsOptBase, nrows = 2, shareX = TRUE)

pLinesBaseOpt <- plot_ly(type = "scatter", mode = "lines" , hoverinfo = "x+y" ) %>% layout(title = "Prediction errors")








#                                     Contour
#Default
pContourBase <- plot_ly(x = ks , y = ds, z = res$errors, transpose = TRUE, type = "contour", source = "contour", 
                        colorscale = "Jet", contours = list(showlabels = TRUE, coloring = "heatmap", 
                                                            start = cont_min, end = cont_max,
                                                            size = (cont_max - cont_min)/(4*num_contours)),
                        hoverinfo = "x+y+z") %>%
  layout(pContourBase, xaxis = list(title = "k"), yaxis = list(title = "d") ) %>%
  add_trace(pContourBase, type = "scatter", mode = "markers", x = x_minims[1], y = y_minims[1],
            text = paste0("Best combination\n", as.character( round(res$errors[x_minims[1], y_minims[1] ], digits = 8) ) ),
            marker = list(color = "rgb(0, 250, 0)", size = 10), hoverinfo = "x+y+text", showlegend = FALSE) %>% colorbar(title = "Error")

#Worse values than Naive trimmmed
pContourNaive <- plot_ly(x = ks , y = ds, z = res$errors, transpose = TRUE, type = "contour", source = "contour", 
                         colorscale = "Jet", contours = list(showlabels = TRUE, coloring = "heatmap", 
                                                             start = cont_min, end = naive_total_error),
                         zmin = cont_min, zmax = cont_max, hoverinfo = "x+y+z") %>%
  layout(pContourNaive, xaxis = list(title = "k"), yaxis = list(title = "d") ) %>%
  add_trace(pContourNaive, type = "scatter", mode = "markers", x = x_minims[1], y = y_minims[1],
            text = paste0("Best combination\n", as.character( round(res$errors[x_minims[1], y_minims[1] ], digits = 8) ) ),
            marker = list(color = "rgb(0, 250, 0)", size = 10), hoverinfo = "x+y+text", showlegend = FALSE) %>% colorbar(title = "Error")

#Top-values trimmed
pContourTrim <- plot_ly(x = ks , y = ds, z = res$errors, transpose = TRUE, type = "contour", source = "contour", 
                        colorscale = "Jet", contours = list(showlabels = TRUE, coloring = "heatmap", 
                                                            start = cont_min, end = (cont_max_fix + cont_min)/2, 
                                                            size = (((cont_max_fix + cont_min)/2) - cont_min)/num_contours),
                        zmin = cont_min, zmax = cont_max_fix, hoverinfo = "x+y+z") %>%
  layout(pContourTrim, xaxis = list(title = "k"), yaxis = list(title = "d") ) %>%
  add_trace(pContourTrim, type = "scatter", mode = "markers", x = x_minims[1], y = y_minims[1], 
            text = paste0("Best combination\n", as.character( round(res$errors[x_minims[1], y_minims[1] ], digits = 8) ) ),
            marker = list(color = "rgb(0, 250, 0)", size = 10), hoverinfo = "x+y+text", showlegend = FALSE) %>% colorbar(title = "Error")

# By default use base contour heatmap
pContour <- pContourBase






# 3D plot of all existent combinations of Distances, Deviations and Errors  + respective loess prediction model
# plot_ly(x = loess_DisDev_Err$x[,1], y = loess_DisDev_Err$x[,2], z = loess_DisDev_Err$fitted, name = "Loess", type = "mesh3d", color = "royalblue") %>%
#   add_trace(x = loess_DisDev_Err$x[,1], y = loess_DisDev_Err$x[,2], z = loess_DisDev_Err$y, type = "scatter3d", mode = "markers", name = "Observed",
#             showlegend = TRUE, marker = list(color = "rgba(0, 250, 0, 1)", size = 3)) %>%
#   layout( xaxis = list(title = "Distance"), yaxis = list(title = "Std.Dev."))




# Heatmap of all possible combinations that could exist between Distances, Deviations and Errors
pDisDevHeatOptim <- plot_ly(x = loess_distances, y = loess_deviations, z = loess_DisDev_heatmap, type = "contour", 
        colorscale = "Jet", hoverinfo = "x+y+z", showlegend = FALSE) %>% 
  add_trace(x = optimal$knn_dists, y = neighs_stdev, type = "scatter", mode = "markers", 
            marker = list(color = "rgba(138,43,226,0.7)", size = 3), hoverinfo = "x+y") %>%
  layout(xaxis = list(title = "Distance"), yaxis = list(title = "Std. Dev.") ) %>% colorbar(title = "Error")

