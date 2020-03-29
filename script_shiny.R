library(plotly)
library(shiny)
library(shinyWidgets)
library(DT)
library(rdist)

selected_points <- matrix(rep(FALSE, NROW(res$errors) * NCOL(res$errors)), nrow = NROW(res$errors), ncol = NCOL(res$errors))
selected_points_aux <<- selected_points

server <- function(input, output, session) {
    # Plot of series + errors + Std. Deviations. Default optimal k-d but can be changed dynamically. Also clickable for interactivity
    output$elemsPlot <- renderPlotly({
        pMain <<- pMainBase
        # If selected K-D is the optimal or leaved blank, use pre-made plots as base
        if ( (input$selKtabDist == res$opt_k || input$selKtabDist == "" ) & 
             (input$selDtabDist == res$opt_d || input$selDtabDist == "") ) {
            
            # Plot optimal k-d combination forecast
            pMain <<- add_trace(pMain, x = sub_dates, y = optimal$fitted, line = list(color = colPalette[2]), 
                                name = paste0("Optimal (k = ", res$opt_k, ", d = ", res$opt_d, ")"), legendgroup = "optim")
            
            errors <- residuals_matrix[1, ]
            
            actK <- res$opt_k
            actD <- res$opt_d
            
            # Save 'y' range for plotted lines when clicked
            y_low  <- min_y - 0.05 * (max_y - min_y)
            y_high <- max_y + 0.05 * (max_y - min_y)
            
            # Forecast errors in absolute value or not, depending on check. Also save Errors range for click event
            if (input$chbabsDist == TRUE) {
                pError <<- plot_ly(x = sub_dates, y = abs(errors), legendgroup = "optim", hoverinfo = "x+y", 
                                   name = "Optimal error", type = "bar", marker = list(color = colPalette[2]) )
                err_low  <- min(abs(errors)) - 0.05 * (max(abs(errors)) - min(abs(errors)))
                err_high <- max(abs(errors)) + 0.05 * (max(abs(errors)) - min(abs(errors)))
            }
            else {
                pError <<- plot_ly(x = sub_dates, y = errors, legendgroup = "optim", hoverinfo = "x+y", 
                                   name = "Optimal error", type = "bar", marker = list(color = colPalette[2]) )
                err_low  <- min(errors) - 0.05 * (max(errors) - min(errors))
                err_high <- max(errors) + 0.05 * (max(errors) - min(errors))
            }
            pkNNDist <- pkNNDistOptim
            pNeighStdev <- pNeighStdevOptim
        }
        
        # Non-optimal case. Make all dynamically
        else {
            actK <- ifelse(input$selKtabDist == "", res$opt_k, as.numeric(input$selKtabDist))
            actD <- ifelse(input$selDtabDist == "", res$opt_d, as.numeric(input$selDtabDist))
            
            # Generate new forecast  and repeat all the steps metioned in previous case
            newPred <- knn_past(y = y, k = actK, d = actD, initial = train_init, distance = distance, 
                                weight = weight, threads = n_threads)
            preds <- newPred$fitted
            
            pMain <<- add_trace(pMain, x = sub_dates, y = preds, name = paste0("k = " , actK, ", d = " , actD, " prediction"), 
                                legendgroup = paste("k", actK, "d", actD), line = list(color = colPalette[2]))
            
            errors <- y_err - preds
            
            y_low  <- min(preds) - 0.05 * (max(preds) - min(preds))
            y_high <- max(preds) + 0.05 * (max(preds) - min(preds))
            
            if (input$chbabsDist == TRUE) {
                pError <<- plot_ly(x = sub_dates, y = abs(errors), legendgroup = paste("k", actK, "d", actD), hoverinfo = "x+y",
                                   name = paste0("k = " , actK, ", d = " , actD, " error"), type = "bar", marker = list(color = colPalette[2]) ) 
                err_low  <- min(abs(errors)) - 0.05 * (max(abs(errors)) - min(abs(errors)))
                err_high <- max(abs(errors)) + 0.05 * (max(abs(errors)) - min(abs(errors)))
            }
            else {
                pError <<- plot_ly(x = sub_dates, y = errors, legendgroup = paste("k", actK, "d", actD), hoverinfo = "x+y",
                                   name = paste0("k = " , actK, ", d = " , actD, " error"), type = "bar", marker = list(color = colPalette[2]) ) 
                err_low  <- min(errors) - 0.05 * (max(errors) - min(errors))
                err_high <- max(errors) + 0.05 * (max(errors) - min(errors))
            }
            
            # Generate plot of mean distance to all neighbors
            pkNNDist <- plot_ly(name = "Neighbors distances", showlegend = TRUE, hoverinfo = "x+y",
                                type = "bar", marker = list(color = colPalette[3]),
                                # x = sub_dates, y = newPred$knn_dists/actK)
                                x = head(tail(dates, length(y) + 1 - train_init), length(sub_dates)), y = newPred$knn_dists/actK)
            
            # Calculate standard deviation for next value of k-neighbors
            future_values <- matrix(y[newPred$neighbors], nrow = nrow(newPred$neighbors))
            pNeighStdev <- plot_ly(name = "Future values variance", showlegend = TRUE, hoverinfo = "x+y",
                                   type = "bar", marker = list(color = colPalette[1]),
                                   # x = sub_dates, 
                                   x = head(tail(dates, length(y) + 1 - train_init), length(sub_dates)),
                                   y = rowSums((t(future_values) - colMeans(future_values))**2) / (nrow(future_values) - 1))
        }
        
        # Clicked on main plot
        click <- event_data("plotly_click", source = "main")
        if (!is.null(click)) {
            x_click <- match(as.Date(click[[3]]), dates)
            
            # Add line where clicked
            pMain <<- add_segments(pMain, x = click[[3]], xend = click[[3]], y = y_low, yend = y_high, 
                                   name = "Knn", showlegend = FALSE, hoverinfo = "x", # text = "Knn",  
                                   legendgroup = "knn", line = list(color = "blue", width = 1.5, dash = "dash"))
            
            # Also if clicked element is not the last one, plot it's corresponding error
            if ( x_click < n ) {
                pError <<- add_segments(pError, x = full_dates[x_click+1], xend = full_dates[x_click+1], y = err_low, yend = err_high, 
                                        name = "Knn", showlegend = FALSE, hoverinfo = "text", type = "line", mode = "line",  
                                        legendgroup = "knn", line = list(color = "blue", width = 1.5, dash = "dash"), text = "Prediction error \n for selected")
            }
            
            # Save the number of shapes already present in the main plot, in order to add new shaded areas on each neighbor
            shapesInd <- length(pMain[["x"]][["layoutAttrs"]][[ pMain[["x"]][["cur_data"]] ]][["shapes"]]) + 1
            
            # Get indexes of the neighbors
            neighbors <- knn_forecast(y = head(y, x_click), k = actK, d = actD, distance = distance, 
                                      weight = weight, threads = n_threads)$neighbors
            
            ind <- 1
            for (i in neighbors) {
                # Add a vertical line on the neighbor
                pMain <<- add_segments(pMain, x = dates[i], xend = dates[i], y = y_low, yend = y_high, name = "Knn",
                                       showlegend = FALSE, text = paste0(ind, "-nearest"), hoverinfo = "x+text",
                                       legendgroup = "knn", line = list(color = "red", width = 1.5, dash = "dash"))
                
                # And add a rectangle with lenth D close to the line
                pMain[["x"]][["layoutAttrs"]][[pMain[["x"]][["cur_data"]]]][["shapes"]][[shapesInd]] <<- 
                    list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.2,
                         x0 = dates[(i + 1 - actD)], x1 = dates[i], xref = "x", yref = "y",
                         # y0 = min_y - 0.05 * (max_y - min_y), y1 = max_y + 0.05 * (max_y - min_y)),
                         y0 = 0.95 * min(y[(i + 1 - actD):i]), y1 = 1.05 * max(y[(i + 1 - actD):i])) 
                
                ind <- ind + 1
                shapesInd <- shapesInd + 1
            }
        } 
        s <- subplot(pMain, pkNNDist, pNeighStdev, pError, nrows = 4, shareX = TRUE)
        s$x$source <- "main"
        s
    })
    
    
    # Plot of relation between neighbors' Distances and it's Std. Deviations
    # output$scattDistStddev <- renderPlotly({
    #     # If optimal K-D combination use pre-made plot
    #     if ( (input$selKtabDist == res$opt_k || input$selKtabDist == "" ) && 
    #          (input$selDtabDist == res$opt_d || input$selDtabDist == "") ) {
    #         pScatDistStddevOptim
    #     }
    #     # In case another combination is chosen
    #     else {
    #         actK <- ifelse(input$selKtabDist == "", res$opt_k, as.numeric(input$selKtabDist))
    #         actD <- ifelse(input$selDtabDist == "", res$opt_d, as.numeric(input$selDtabDist))
    #         
    #         # Generate forecast for selected combination 
    #         newPred <- knn_past(y = y, k = actK, d = actD, initial = train_init, distance = distance, 
    #                             weight = weight, threads = n_threads)
    #         
    #         # Calculate standard deviation of neighbors
    #         future_values <- matrix(y[newPred$neighbors], nrow = nrow(newPred$neighbors))
    #         std_devs <- sqrt(rowSums((t(future_values) - colMeans(future_values))**2) / (nrow(future_values) - 1))
    #         
    #         #Generate loess model
    #         loess_mod <- loess(std_devs ~ newPred$knn_dists)
    #         loess_ind <- sort.int(loess_mod$x[,1], index.return = TRUE)$ix
    #         loess_mean <- predict(loess_mod, se = TRUE)
    #         
    #         # Plot of relation between Distances and Std. Deviation, including Loess stimation with confidence interval
    #         plot_ly(name = "Distances vs Std.Dev.", type = 'scatter', mode = "lines", hoverinfo = "x+y",
    #                 x = loess_mod$x[,1][loess_ind], y = loess_mean$fit[loess_ind], showlegend = FALSE) %>%
    #             add_trace(y = (loess_mean$fit - qt(0.95, loess_mean$df) * loess_mean$se.fit)[loess_ind]) %>%
    #             add_trace(y = (loess_mean$fit + qt(0.95, loess_mean$df) * loess_mean$se.fit)[loess_ind], 
    #                       fill = "tonexty", fillcolor = "rgba(0,0,100,0.35)") %>%
    #             add_trace(x = newPred$knn_dists, y = std_devs, mode = "markers", marker = list(opacity = 0.4),
    #                       hoverinfo = "x+y+text", text = head(tail(dates, length(y) + 1 - train_init), length(sub_dates))) %>%
    #             layout(xaxis = list(title = "Distances"), yaxis = list(title = "Std. Dev."))
    #     }
    # })
    
    # Plot of relation between neighbors' Distances and prediction errors
    output$scattDistErr <- renderPlotly({
        # If optimal K-D combination use pre-made plot
        if ( (input$selKtabDist == res$opt_k || input$selKtabDist == "" ) && 
             (input$selDtabDist == res$opt_d || input$selDtabDist == "") ) {
            # pScatDistErrOptim
            subplot(pScatDistErrOptim,pScatStddevErrOptim,pDisDevHeatOptim, nrows = 1, widths = c(0.25,0.25,0.5),
                    titleX = TRUE, titleY = TRUE)
        }
        # In case another combination is chosen
        else {
            actK <- ifelse(input$selKtabDist == "", res$opt_k, as.numeric(input$selKtabDist))
            actD <- ifelse(input$selDtabDist == "", res$opt_d, as.numeric(input$selDtabDist))
            
            # Generate forecast for selected combination 
            newPred <- knn_past(y = y, k = actK, d = actD, initial = train_init, distance = distance, 
                                weight = weight, threads = n_threads)
            
            #Generate loess model
            loess_mod <- loess(abs(y_err - newPred$fitted) ~ newPred$knn_dists, span = 0.75)
            loess_ind <- sort.int(loess_mod$x[,1], index.return = TRUE)$ix
            loess_mean <- predict(loess_mod, se = TRUE)
            
            # Plot of relation between Distances and Errors, including Loess stimation with confidence interval
            p1 <- plot_ly(name = "Distances vs Errors", type = 'scatter', mode = "lines", hoverinfo = "x+y",
                    x = loess_mod$x[,1][loess_ind], y = loess_mean$fit[loess_ind], showlegend = FALSE) %>%
                add_trace(y = (loess_mean$fit - qt(0.95, loess_mean$df) * loess_mean$se.fit)[loess_ind]) %>%
                add_trace(y = (loess_mean$fit + qt(0.95, loess_mean$df) * loess_mean$se.fit)[loess_ind], 
                          fill = "tonexty", fillcolor = "rgba(0,0,100,0.35)") %>%
                add_trace(x = newPred$knn_dists, y = abs(y_err - newPred$fitted), mode = "markers", marker = list(opacity = 0.4),
                          hoverinfo = "x+y+text", text = head(tail(dates, length(y) + 1 - train_init), length(sub_dates))) %>%
                layout(xaxis = list(title = "Distances"), yaxis = list(title = "Errors"))
            
            # Calculate standard deviation of neighbors
            future_values <- matrix(y[newPred$neighbors], nrow = nrow(newPred$neighbors))
            std_devs <- sqrt(rowSums((t(future_values) - colMeans(future_values))**2) / (nrow(future_values) - 1))
            
            #Generate loess model
            loess_mod <- loess(abs(y_err - newPred$fitted) ~ std_devs)
            loess_ind <- sort.int(loess_mod$x[,1], index.return = TRUE)$ix
            loess_mean <- predict(loess_mod, se = TRUE)
            
            # Plot of relation between Std. Deviations and Errors, including Loess stimation with confidence interval
            p2 <- plot_ly(name = "Std. Dev. vs Errors", type = 'scatter', mode = "lines", hoverinfo = "x+y",
                    x = loess_mod$x[,1][loess_ind], y = loess_mean$fit[loess_ind], showlegend = FALSE) %>%
                add_trace(y = (loess_mean$fit - qt(0.95, loess_mean$df) * loess_mean$se.fit)[loess_ind]) %>%
                add_trace(y = (loess_mean$fit + qt(0.95, loess_mean$df) * loess_mean$se.fit)[loess_ind], 
                          fill = "tonexty", fillcolor = "rgba(0,0,100,0.35)") %>%
                add_trace(x = std_devs, y = abs(y_err - newPred$fitted), mode = "markers", marker = list(opacity = 0.4),
                          hoverinfo = "x+y+text", text = head(tail(dates, length(y) + 1 - train_init), length(sub_dates))) %>%
                layout(xaxis = list(title = "Std. Dev."), yaxis = list(title = "Errors"))
            
            
            # Generate loess model for all combinations of distances and deviations
            points <- 100
            dist <- seq.int(from = min(newPred$knn_dists), to = max(newPred$knn_dists), length.out = points)
            dev <- seq.int(from = min(neighs_stdev), to = max(neighs_stdev), length.out = points)
            DisDev_Err <- loess(abs(y_err - newPred$fitted) ~ newPred$knn_dists + std_devs)
            DisDev_heatmap <- matrix(predict(object = DisDev_Err, 
                                             newdata = matrix( 
                                                 c(rep(dist, each = points), rep(loess_deviations, times = points)), 
                                                 ncol = 2)), nrow = points)
            DisDev_heatmap[DisDev_heatmap < 0] <- NA
            
            
            # Heatmap of all possible combinations that could exist between Distances, Deviations and Errors
            p3 <- plot_ly(x = dist, y = dev, z = DisDev_heatmap, type = "contour", 
                    colorscale = "Jet", hoverinfo = "x+y+z", showlegend = FALSE) %>% 
                add_trace(x = newPred$knn_dists, y = std_devs, type = "scatter", mode = "markers", 
                          marker = list(color = "rgba(138,43,226,0.7)", size = 3), hoverinfo = "x+y") %>%
                layout(xaxis = list(title = "Distance"), yaxis = list(title = "Std. Dev.") )
            
            subplot(p1,p2,p3, nrows = 1, widths = c(0.25,0.25,0.5), titleX = TRUE, titleY = TRUE)
        }
    })
    
    # # Plot of relation between neighbors' Std. Deviations and prediction errors
    # output$scattStddevErr <- renderPlotly({
    #     # If optimal K-D combination use pre-made plot
    #     if ( (input$selKtabDist == res$opt_k || input$selKtabDist == "" ) && 
    #          (input$selDtabDist == res$opt_d || input$selDtabDist == "") ) {
    #         pScatStddevErrOptim
    #     }
    #     # In case another combination is chosen
    #     else {
    #         actK <- ifelse(input$selKtabDist == "", res$opt_k, as.numeric(input$selKtabDist))
    #         actD <- ifelse(input$selDtabDist == "", res$opt_d, as.numeric(input$selDtabDist))
    #         
    #         # Generate forecast for selected combination 
    #         newPred <- knn_past(y = y, k = actK, d = actD, initial = train_init, distance = distance, 
    #                             weight = weight, threads = n_threads)
    #         
    #         # Calculate standard deviation of neighbors
    #         future_values <- matrix(y[newPred$neighbors], nrow = nrow(newPred$neighbors))
    #         std_devs <- sqrt(rowSums((t(future_values) - colMeans(future_values))**2) / (nrow(future_values) - 1))
    #         
    #         #Generate loess model
    #         loess_mod <- loess(abs(y_err - newPred$fitted) ~ std_devs)
    #         loess_ind <- sort.int(loess_mod$x[,1], index.return = TRUE)$ix
    #         loess_mean <- predict(loess_mod, se = TRUE)
    #         
    #         # Plot of relation between Std. Deviations and Errors, including Loess stimation with confidence interval
    #         plot_ly(name = "Std. Dev. vs Errors", type = 'scatter', mode = "lines", hoverinfo = "x+y",
    #                 x = loess_mod$x[,1][loess_ind], y = loess_mean$fit[loess_ind], showlegend = FALSE) %>%
    #             add_trace(y = (loess_mean$fit - qt(0.95, loess_mean$df) * loess_mean$se.fit)[loess_ind]) %>%
    #             add_trace(y = (loess_mean$fit + qt(0.95, loess_mean$df) * loess_mean$se.fit)[loess_ind], 
    #                       fill = "tonexty", fillcolor = "rgba(0,0,100,0.35)") %>%
    #             add_trace(x = std_devs, y = abs(y_err - newPred$fitted), mode = "markers", marker = list(opacity = 0.4),
    #                       hoverinfo = "x+y+text", text = head(tail(dates, length(y) + 1 - train_init), length(sub_dates))) %>%
    #             layout(xaxis = list(title = "Std. Dev."), yaxis = list(title = "Errors"))
    #     }
    # })
    
    
    # output$heatScatErr <- renderPlotly({
    #     # If optimal K-D combination use pre-made plot
    #     if ( (input$selKtabDist == res$opt_k || input$selKtabDist == "" ) && 
    #          (input$selDtabDist == res$opt_d || input$selDtabDist == "") ) {
    #         pDisDevHeatOptim
    #     }
    #     # In case another combination is chosen
    #     else {
    #         actK <- ifelse(input$selKtabDist == "", res$opt_k, as.numeric(input$selKtabDist))
    #         actD <- ifelse(input$selDtabDist == "", res$opt_d, as.numeric(input$selDtabDist))
    #         
    #         # Generate forecast for selected combination 
    #         newPred <- knn_past(y = y, k = actK, d = actD, initial = train_init, distance = distance, 
    #                             weight = weight, threads = n_threads)
    #         
    #         # Calculate standard deviation of neighbors
    #         future_values <- matrix(y[newPred$neighbors], nrow = nrow(newPred$neighbors))
    #         std_devs <- sqrt(rowSums((t(future_values) - colMeans(future_values))**2) / (nrow(future_values) - 1))
    #         
    #         
    #         
    #         # Generate loess model for all combinations of distances and deviations
    #         points <- 100
    #         dist <- seq.int(from = min(newPred$knn_dists), to = max(newPred$knn_dists), length.out = points)
    #         dev <- seq.int(from = min(neighs_stdev), to = max(neighs_stdev), length.out = points)
    #         DisDev_Err <- loess(abs(y_err - newPred$fitted) ~ newPred$knn_dists + std_devs)
    #         DisDev_heatmap <- matrix(predict(object = DisDev_Err, 
    #                  newdata = matrix( 
    #                      c(rep(dist, each = points), rep(loess_deviations, times = points)), 
    #                      ncol = 2)), nrow = points)
    #         DisDev_heatmap[DisDev_heatmap < 0] <- NA
    #         
    #         
    #         # Heatmap of all possible combinations that could exist between Distances, Deviations and Errors
    #         plot_ly(x = dist, y = dev, z = DisDev_heatmap, type = "contour", 
    #                 colorscale = "Jet", hoverinfo = "x+y+z") %>% 
    #             add_trace(x = newPred$knn_dists, y = std_devs, type = "scatter", mode = "markers", 
    #                       marker = list(color = "rgba(138,43,226,0.7)", size = 3), hoverinfo = "x+y") %>%
    #             layout(xaxis = list(title = "Dist"), yaxis = list(title = "StdDev") )
    #     }
    # })
    
    # Plot of k-nearest neighbors of selected instant and produced forecast
    output$neighborsPlot <- renderPlotly({
        click <- event_data("plotly_click", source = "main")
        # This plot only shows if main Plot is clicked
        if (!is.null(click)) {
            # Get index of click
            x_click <- match(as.Date(click[[3]]), dates)
            
            # Get actual K-D combination
            actK <- ifelse(input$selKtabDist == "", res$opt_k, as.numeric(input$selKtabDist))
            actD <- ifelse(input$selDtabDist == "", res$opt_d, as.numeric(input$selDtabDist))
            
            # Plot selected element, with d observations
            pKNN <- plot_ly(type = "scatter",  mode = "lines+markers", showlegend = TRUE) %>% 
                add_trace(x = (-(actD - 1)):0, y = y[(x_click + 1 - actD):x_click], line = list(color = colPalette[1], width = 5), 
                          name = paste0("Observed (", dates[x_click], ")"), marker = list(color = colPalette[1], size = 7), hoverinfo = "text+y", 
                          legendgroup = "Observed", text = paste0("Observed, \n", format(dates[(x_click + 1 - actD):x_click], format = "%B %Y")) ) %>%
                layout(title = list(text = paste0(actK, "-nearest neighboors"), font = list(size = 15) ), 
                       xaxis = list(zerolinecolor = "blue", zerolinewidth = 2) )
            
            # If clicked element is not the last one, also print it's next value
            if ( x_click < n ) {
                pKNN <- add_trace(pKNN, x = 0:1, y = y[x_click:(x_click + 1)], line = list(color = colPalette[1], width = 5, dash = "dash"), 
                                  name = "Observed", marker = list(color = colPalette[1], size = 7), hoverinfo = "text+y", legendgroup = "Observed",
                                  text = paste0("Observed,\n", format(dates[x_click:(x_click + 1)], format = "%B %Y")), showlegend = FALSE)
            }
            
            # Plot generated forecast witch actual K-D combination
            pKNN <- add_trace(pKNN, x = 1, y = knn_forecast(y = head(y, x_click), k = actK, d = actD, 
                                                            distance = distance, weight = weight, threads = n_threads)$mean, 
                              name = paste0("Prediction (", full_dates[(x_click + 1)], ")"), legendgroup = "Prediction", mode = "marker", hoverinfo = "text+y",
                              marker = list(color = colPalette[2], size = 8), text = paste0("Prediction for ", full_dates[(x_click + 1)]) )
            
            # To count how many neighbors have been plot
            ind <- 1
            
            # Set color intensity and transparency degradation as all neighbors are plotted
            redDecr   <- (250 - 150) / (actK - 1)
            transDecr <- (0.9 - 0.3) / (actK - 1)
            
            # If optimal K-D combination and in range of train+test, re-use data            
            if (actK == res$opt_k && actD == res$opt_d && x_click + 1 >= train_init && x_click < n) {
                # For each of k-neighbors, plot it's d-observations and next value
                for (i in optimal$neighbors[,(x_click + 1 - train_init)]) {
                    pKNN <- add_trace(pKNN, x = (-(actD - 1)):0, y = y[(i + 1 - actD):i], name = paste0(ind,"-NN (", dates[(i)], ")" ), legendgroup = paste0(ind,"-NN"),
                                      line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4)),
                                      marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                      text = paste0(ind,"-nearest, \n", format(dates[(i + 1 - actD):i], format = "%B %Y")), hoverinfo = "text+y") %>%
                        add_trace(pKNN, x = 0:1, y = y[i:(i+1)], showlegend = FALSE, name = paste0(ind,"-NN (", dates[(i)], ")" ),legendgroup = paste0(ind,"-NN"),
                                  line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4), dash = "dash"),
                                  marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                  text = paste0(ind,"-nearest, \n", format(dates[(i):(i + 1)], format = "%B %Y")), hoverinfo = "text+y")
                    
                    ind <- ind + 1
                }
            }
            # Required data is not already calculated
            else {
                # Call forecast function to obtain k-neighbors of selected instant
                neighbors <- knn_forecast(y = head(y, x_click), k = actK, d = actD, distance = distance, 
                                          weight = weight, threads = n_threads)$neighbors
                
                # For each of k-neighbors, plot it's d-observations and next value
                for (i in neighbors ) {
                    pKNN <- add_trace(pKNN, x = (-(actD - 1)):0, y = y[(i + 1 - actD):i], name = paste0(ind,"-NN (", dates[(i)], ")" ), legendgroup = paste0(ind,"-NN"),
                                      line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4)),
                                      marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                      text = paste0(ind,"-nearest, \n", format(dates[(i + 1 - actD):i], format = "%B %Y")), hoverinfo = "text+y") %>%
                        add_trace(pKNN, x = 0:1, y = y[(i):(i + 1)], showlegend = FALSE, name = paste0(ind,"-NN (", dates[(i)], ")" ),legendgroup = paste0(ind,"-NN"),
                                  line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4), dash = "dash"),
                                  marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                  text = paste0(ind,"-nearest, \n", format(dates[(i):(i+1)], format = "%B %Y")), hoverinfo = "text+y")
                    
                    ind <- ind + 1
                }
            }
            pKNN
        }
        # There wasn't any click on main plot
        else {NULL}
    })
    
    
    # Plot of distances to all previous elements
    output$distsPlot <- renderPlotly({
        click <- event_data("plotly_click", source = "main")
        # This plot only shows if main Plot is clicked
        if (!is.null(click)) {
            # Get index of click
            x_click <- match(as.Date(click[[3]]), dates)
            
            # Get actual K-D combination
            actK <- ifelse(input$selKtabDist == "", res$opt_k, as.numeric(input$selKtabDist))
            actD <- ifelse(input$selDtabDist == "", res$opt_d, as.numeric(input$selDtabDist))
            
            # Get all distances from selected element to all previous by calling Forecast function
            distances <- knn_forecast(y = head(y, x_click), k = actK, d = actD, distance = distance, 
                                      weight = weight, threads = n_threads)$distances
            
            # Parse quaZ threshold of distances to plot
            percentile <- ifelse(input$selPerctabDist == "", 0.25, as.numeric(input$selPerctabDist))
            index <- which(distances <= quantile(distances, probs = percentile))
            
            # Plot from dark color to light color 
            distColors <- c("darkcyan", "lightskyblue", "lightcyan")
            distColors <- colorRamp(distColors)
            # distColors <- rev(distColors) # REVERSE COLORS
            
            # RELATIVE TO MAX DISTANCE
            # distances <- (max(distances)*1.1) - distances
            
            # RELATIVE TO DISTANCE BETWEEN MIN AND MAX DISTANCES
            # distances <- 1 - ((distances-(min(distances))) / (max(distances) - min(distances)))
            
            # INVERSELY PROPORTIONAL TO MIN DISTANCE
            # distances <- min(distances) / distances
            
            # Calculate distances between selected element's next value and each other element
            sumDists <- as.vector(cdist(y[x_click+1], y[(actD+1):x_click], metric = distance))
            
            # Plot distances to previous elements
            # pDists <<- plot_ly(x = dates[actD:x_click], y = y[actD:x_click], name  = "Time series", showlegend = FALSE,
            #                    hoverinfo = "x+y", type = "scatter", mode = "lines", line = list(color = colPalette[1])) %>%
            #     add_trace(x = tail(head(dates, length(distances) + actD - 1), length(distances))[index], y = distances[index]/actD, name  = "Knn distances",
            #               showlegend = TRUE, hoverinfo = "x+y", type = "bar", color = distances[index], colors = distColors(3)) %>%
            #     layout(xaxis = list( range = list(dates[(actD)], dates[(actD + length(distances) - 1)]),
            #                          rangeslider = list( range = list(dates[ (actD)], dates[(actD + length(distances) - 1)]) ) ) )
            pDists <<- plot_ly(x = dates[actD:x_click]) %>%
                
                
                add_trace(x = tail(head(dates, length(distances) + actD - 1), length(distances))[index], y = distances[index], 
                          name  = "Knn distances", showlegend = FALSE, hoverinfo = "x+y", type = "bar", color = distances[index], 
                          colors = distColors, yaxis = "y2") %>%
                add_trace(x = dates[actD:x_click], y = y[actD:x_click], name  = "Time series", showlegend = FALSE, yaxis = "y",
                          hoverinfo = "x+y", type = "scatter", mode = "lines", line = list(color = colPalette[1])) %>%
                layout(xaxis = list( range = list(dates[(actD)], dates[(actD + length(distances) - 1)]),
                          rangeslider = list( range = list(dates[ (actD)], dates[(actD + length(distances) - 1)]))),
                       yaxis = list(side = "left", range = c(min(y[actD:x_click]), max(y[actD:x_click])), overlaying = "y2"), yaxis2 = list( side = "right"))
            
            # Plot distances to previous element's next values
            pNeighs <- plot_ly(x = tail(head(dates, length(distances) + actD - 1), length(distances))[index], y = sumDists[index], 
                               name  = "Next-value distances", showlegend = FALSE, hoverinfo = "x+y", type = "bar")
            
            # pMixed <- 
                subplot(pDists, pNeighs, nrows = 2, shareX = TRUE)
            
            # pRelation <- plot_ly(type = "scatter", mode = "markers", x = distances[index], y = sumDists[index]) %>%
            #     layout(xaxis = list(title = "Distances"), yaxis = list(title = "Next-value Error"))
            # 
            # subplot(pMixed, pRelation, nrows = 1, widths = c(0.6,0.4))
        } 
        else {NULL}
    })
    
    # Plot of relationship between all distances to neighbors and distances to their next values 
    output$distScatPlot <- renderPlotly({
        click <- event_data("plotly_click", source = "main")
        # This plot only shows if main Plot is clicked
        if (!is.null(click)) {
            # Get index of click
            x_click <- match(as.Date(click[[3]]), dates)
            
            # Get actual K-D combination
            actK <- ifelse(input$selKtabDist == "", res$opt_k, as.numeric(input$selKtabDist))
            actD <- ifelse(input$selDtabDist == "", res$opt_d, as.numeric(input$selDtabDist))
            
            # Get all distances from selected element to all previous by calling Forecast function
            distances <- knn_forecast(y = head(y, x_click), k = actK, d = actD, distance = distance, 
                                      weight = weight, threads = n_threads)$distances
            
            # Parse quaZ threshold of distances to plot
            percentile <- ifelse(input$selPerctabDist == "", 0.25, as.numeric(input$selPerctabDist))
            index <- which(distances <= quantile(distances, probs = percentile))
            
            # Calculate distances between selected element's next value and each other element
            sumDists <- as.vector(cdist(y[x_click+1], y[(actD+1):x_click], metric = distance))
            
            plot_ly(type = "scatter", mode = "markers", x = distances[index], y = sumDists[index]) %>%
                layout(xaxis = list(title = "Neighbors distances"), yaxis = list(title = "Next-value distances"))

        } 
        else {NULL}
    })
    
    
    # Plot of all tested k-d combiations and their errors
    output$contourPlot <- renderPlotly({
        click <- event_data("plotly_click", source = "contour")
        if (!is.null(click)) {
            # Process indexes of click
            k <- click[[3]]
            d <- click[[4]]
            
            # Check if previous click was the same as actual
            if ( exists("last_click") ) {
                same_click <- k == last_click[1]
                same_click <- ifelse(d == last_click[2], same_click, FALSE)
                same_click <- ifelse( is.null(click[["z"]]) == last_click[3], same_click, FALSE)
                same_click <- ifelse( click[["curveNumber"]] == last_click[4], same_click, FALSE)
            }
            else {
                last_click <<- NULL
                same_click <- FALSE 
            }
            
            # If actual click is a new one and is not the one corresponding to optimal K-D combination
            if ( !same_click && (x_minims[1] != k || y_minims[1] != d) ) {
                # Negate selection
                selected_points[k, d] <<- !selected_points[k, d]
            }
            
            # Save click information
            last_click[1] <<- k
            last_click[2] <<- d
            last_click[3] <<- is.null(click[["z"]])
            last_click[4] <<- click[["curveNumber"]]
        }
        
        # Check and save actual contour selected and previous one
        if (input$contourType == "trim") {
            pContour <<- pContourTrim
            previous_countour <<- "trim"
        }
        else if (input$contourType == "naive") {
            pContour <<- pContourNaive
            previous_countour <<- "naive"
        }
        else {
            pContour <<- pContourBase
            previous_countour <<- "default"
        }
        
        # Parse number of minimums selected to plot
        nDots <- ifelse(input$contourMinims == "", 5, as.numeric(input$contourMinims))
        
        # If more than 1 point is selected, add all of them to the plot
        if (nDots > 1 ) {
            # Generate as many texts as selected dots minus one
            texts <- "2nd best \n"
            if (nDots > 2 ) {
                texts <- c(texts, "3rd best \n")
                if (nDots > 3 ) {
                    texts <- c(texts, paste0( 4:nDots, rep("th best \n", nDots - 3) ))
                }
            }
            
            # Generate as many colors as selected dots minus one
            colors <- paste0( "rgba(40," , 230 - (0:(nDots-1)) * ((230 - 128) / (nDots - 1)), " ,40, 0.95)" )
            
            # Plot all dots corresponding to selected minimums
            pContour <<- add_trace(pContour, type = "scatter", mode = "markers", x = x_minims[2:nDots], y = y_minims[2:nDots],
                                   text = paste0(texts, as.character( round(res$errors[x_minims[2:nDots], y_minims[2:nDots] ][1,], digits = 8) ) ),
                                   # marker = list(color = 0:(nDots-2), colorscale = c("seagreen", "chartreuse")), size = 8,
                                   marker = list(color = colors, size = 8),
                                   hoverinfo = "x+y+text", showlegend = FALSE, opacity = 1)
        }
        
        if (input$chbLocalMins == 1) {
            radius <- ifelse(input$localMinRad == "", 1, as.numeric(input$localMinRad))
            res$errors
            
            delta <- seq(-radius, radius)
            for (c in 1:NCOL(res$errors)) for (r in 1:NROW(res$errors)) {
                # From actual position, get all nearby at given radius
                rows <- r + delta
                # If any position falls out of errors matrix, remove them
                rows <- rows[(rows > 0 & rows <= NROW(res$errors))]
                # From actual position, get all nearby at given radius
                cols <- c + delta
                # If any position falls out of errors matrix, remove them
                cols <- cols[(cols > 0 & cols <= NCOL(res$errors))]
                # print(paste0("Filas: ", rows))
                # print(paste0("Columnas: ", cols))
                # If evaluated error is lower than surrounding, excluding NAs previously excluded by threshold, we take it
                if (all(res$errors[r,c] <= res$errors[rows, cols])) {
                    print(paste0("Pintando punto ", r, ", ", c))
                    pContour <<- add_trace(pContour, type = "scatter", mode = "markers", x = ks[r], y = ds[c], 
                                   text = as.character(round(res$errors[r, c], digits = 4)), marker = list(color = "yellow"), 
                                   hoverinfo = "x+y+text", showlegend = FALSE)
                }
            }
        }
        # If any point is selected manually, plot all of them
        if (any( selected_points == TRUE ) ) {
            for (i in 1:NROW(selected_points)) {
                for (j in 1:NCOL(selected_points)) {
                    if (selected_points[i, j])
                        pContour <<- add_trace(pContour, type = "scatter", mode = "markers", x = ks[i], y = ds[j], 
                                               text = as.character(round(res$errors[i, j], digits = 4)), marker = list(color = "red"), 
                                               hoverinfo = "x+y+text", showlegend = FALSE)
                }
            }
        }
        pContour
    })
    
    
    # Plot of comparison between forecast generated by best k-d combination and other methods
    output$optPlot <- renderPlotly({
        click <- event_data("plotly_click", source = "contour")
        
        if ( !is.null(click) ) {
            # Process indexes of click
            k <- click[[3]]
            d <- click[[4]]
            
            # Check if previous click was the same as actual
            if ( exists("last_click") ) {
                same_click <- k == last_click[1]
                same_click <- ifelse(d == last_click[2], same_click, FALSE)
                same_click <- ifelse( is.null(click[["z"]]) == last_click[3], same_click, FALSE)
                same_click <- ifelse( click[["curveNumber"]] == last_click[4], same_click, FALSE)
            }
            else {
                last_click <<- NULL
                same_click <- FALSE 
            }
            
            # If actual click is a new one and is not the one corresponding to optimal K-D combination
            if ( !same_click && (x_minims[1] != k || y_minims[1] != d) ) {
                # Negate selection
                selected_points[k, d] <<- !selected_points[k, d]
            }
            
            # Save click information
            last_click[1] <<- k
            last_click[2] <<- d
            last_click[3] <<- is.null(click[["z"]])
            last_click[4] <<- click[["curveNumber"]]
        }
        
        # If only best K-D combination is plotted, re-use information
        if ( all( selected_points == FALSE ) && input$chbNaiveOpt == 0 && input$chbSeasNaiveOpt == 0) {
            pErrorsOpt <<- pBarsOptBase
            # Whether absolute error is chosen or not
            if (input$chbabs_tab2 == 1) { 
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[1, ]), showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", marker = list(color = colPalette[2]))
                mini <- min( abs(residuals_matrix[1, ]) )
                maxi <- max( abs(residuals_matrix[1, ]) )
            }
            else {
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[1, ], showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", marker = list(color = colPalette[2]))
                mini <- min( residuals_matrix[1, ])
                maxi <- max( residuals_matrix[1, ])
            }
            
            # Line where train set begins
            pErrorsOpt <- add_segments(pErrorsOpt, x = dates[train_init], xend = dates[train_init], y = mini - 0.05 * (maxi - mini), 
                                       yend = maxi + 0.05 * (maxi - mini), name = "Train", showlegend = FALSE, text = "Train", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            # Line where test set begins
            pErrorsOpt <- add_segments(pErrorsOpt, x = dates[test_init], xend = dates[test_init], y = mini - 0.05 * (maxi - mini), 
                                       yend = maxi + 0.05 * (maxi - mini), name = "Test", showlegend = FALSE, text = "Test", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            
            combPlotOpt <<- subplot(pOptBase, pErrorsOpt, nrows = 2, shareX = TRUE)
        }
        else {
            # Initialize plots
            pOpt <<- pOptBase
            pErrorsOpt <<- pLinesBaseOpt
            
            # Whether absolute error is chosen or not
            if (input$chbabs_tab2 == 1) { 
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[1, ]), showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", line = list(color = colPalette[2]))
                mini <- min( abs(residuals_matrix[1, ]) )
                maxi <- max( abs(residuals_matrix[1, ]) )
            }
            else {
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[1, ], showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", line = list(color = colPalette[2]))
                mini <- min( residuals_matrix[1, ])
                maxi <- max( residuals_matrix[1, ])
            }
            
            # Count how many series will be plotted
            totalSeries <- sum(selected_points)
            if (input$chbNaiveOpt == 1) {
                totalSeries <- totalSeries + 1
            }
            if (input$chbSeasNaiveOpt == 1) {
                totalSeries <- totalSeries + 1
            }
            
            # Depending on the number of series the type of plot would be Bars or Lines
            if (totalSeries > 1) {
                pComparOptim <<- plot_ly(type = "scatter", mode = "lines" , hoverinfo = "x+y" ) # %>% layout(title = "Errors comparison")
            }
            else {
                pComparOptim <<- plot_ly(type = "bar", hoverinfo = "x+y" ) # %>% layout(title = "Errors comparison")
            }
            
            # Initialize minimums and maximums
            min_compar <- Inf
            max_compar <- -Inf
            colorIndex <- 5
            
            # Naive activated with checkbox
            if (input$chbNaiveOpt == 1) {
                # Add Naive forecast to plot and it's errors
                pOpt <<- add_trace(pOpt, x = sub_dates, y = naive, name = "Naive", legendgroup = "naive", line = list(color = colPalette[3]))
                if (input$chbabs_tab2 == 1) {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[2, ]), line = list(color = colPalette[3]),
                                             name = "Naive error", legendgroup = "naive", showlegend = FALSE)
                }
                else {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[2, ], line = list(color = colPalette[3]),
                                             name = "Naive error", legendgroup = "naive", showlegend = FALSE)
                }
                
                dif <- abs(residuals_matrix[1, ]) - abs(residuals_matrix[2, ])
                if (totalSeries > 1) {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, line = list(color = colPalette[3]),
                                               name = "Naive comparison", legendgroup = "naive", showlegend = FALSE)
                }
                else {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, marker = list(color = colPalette[3]), 
                                               name = "Naive comparison", legendgroup = "naive", showlegend = FALSE)
                }
                
                # Update min and max
                min_compar <- min( min_compar, dif)
                max_compar <- max( max_compar, dif)
            }
            
            # Seasonal naive activated with checkbox
            if (input$chbSeasNaiveOpt == 1) {
                # Generate seasonal forecast and calculate error
                seasLag <- as.numeric(input$seasNaivOptLag)
                isolate({
                    snaive <- ts(y[(train_init - seasLag + 1):(n - seasLag)])
                })
                residuals_matrix[3, ] <- y_err - snaive
                
                # Add Seasonal Naive forecast to plot and it's errors
                pOpt <<- add_trace(pOpt, x = sub_dates, y = snaive, name = paste0("S. Naive (", seasLag, ")"), legendgroup = "snaive", line = list(color = colPalette[4]))
                if (input$chbabs_tab2 == 1) {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[3, ]), line = list(color = colPalette[4]),
                                             name = "S. Naive error", legendgroup = "snaive", showlegend = FALSE)
                }
                else {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[3, ], line = list(color = colPalette[4]),
                                             name = "S. Naive error", legendgroup = "snaive", showlegend = FALSE)
                }
                
                dif <- abs(residuals_matrix[1, ]) - abs(residuals_matrix[3, ])
                if (totalSeries > 1) {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, line = list(color = colPalette[4]),
                                               name = "S. Naive comparison", legendgroup = "snaive", showlegend = FALSE)
                }
                else {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, marker = list(color = colPalette[4]), 
                                               name = "S. Naive comparison", legendgroup = "snaive", showlegend = FALSE)
                }
                
                # Update min and max
                min_compar <- min( min_compar, dif)
                max_compar <- max( max_compar, dif)
            }
            
            # Plot all k-d combinations selected
            if (any( selected_points ) ) {
                for (i_ind in 1:NROW(selected_points)) 
                    for (j_ind in 1:NCOL(selected_points)) 
                        if (selected_points[i_ind, j_ind]) {
                            i <- ks[i_ind]
                            j <- ds[j_ind]
                            
                            # Generate corresponding forecast
                            newPred <- knn_past(y = y, k = i, d = j, initial = train_init, distance = distance, 
                                                weight = weight, threads = n_threads)$fitted
                            newPred <- as.vector(newPred)
                            
                            # Add forecast to series Plot
                            pOpt <<- add_trace(pOpt, x = sub_dates, y = newPred, name = paste0("k = " , i, ", d = " , j, " prediction"), 
                                               legendgroup = paste("k", i, "d", j), line = list(color = colPalette[colorIndex]))
                            
                            # It's error to the plot
                            error <- y_err - newPred
                            if (input$chbabs_tab2 == 1) {
                                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(error), line = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                         name = paste0("k = " , i, ", d = " , j, "", " error"), legendgroup = paste("k", i, "d", j))
                                mini <- min( mini, abs(error))
                                maxi <- max( maxi, abs(error))
                            }
                            else {
                                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = error, line = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                         name = paste0("k = " , i, ", d = " , j, "", " error"), legendgroup = paste("k", i, "d", j))
                                mini <- min( mini, error)
                                maxi <- max( maxi, error)
                            }
                            
                            # And to the third plot of errors comparison
                            dif <- abs(residuals_matrix[1, ]) - abs(error)
                            if (totalSeries > 1) {
                                pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, line = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                           name = paste0("k = " , i, ", d = " , j, "", " comparison"), legendgroup = paste("k", i, "d", j))
                            }
                            else {
                                pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, marker = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                           name = paste0("k = " , i, ", d = " , j, "", " comparison"), legendgroup = paste("k", i, "d", j))
                            }
                            
                            # Update min and max
                            min_compar <- min( min_compar, dif)
                            max_compar <- max( max_compar, dif)
                            colorIndex <- colorIndex + 1
                        }
            }
            
            # Add lines of Train and Test sets to bot plots
            pErrorsOpt <<- add_segments(pErrorsOpt, x = dates[train_init], xend = dates[train_init], y = mini - 0.05 * (maxi - mini), 
                                        yend = maxi + 0.05 * (maxi - mini), name = "Train", showlegend = FALSE, text = "Train", 
                                        hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            pErrorsOpt <<- add_segments(pErrorsOpt, x = dates[test_init], xend = dates[test_init], y = mini - 0.05 * (maxi - mini), 
                                        yend = maxi + 0.05 * (maxi - mini), name = "Test", showlegend = FALSE, text = "Test", 
                                        hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            
            pComparOptim <<- add_segments(pComparOptim, x = dates[train_init], xend = dates[train_init], y = min_compar - 0.05 * (max_compar - min_compar), 
                                          yend = max_compar + 0.05 * (max_compar - min_compar), name = "Train", showlegend = FALSE, text = "Train", 
                                          hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            pComparOptim <<- add_segments(pComparOptim, x = dates[test_init], xend = dates[test_init], y = min_compar - 0.05 * (max_compar - min_compar), 
                                          yend = max_compar + 0.05 * (max_compar - min_compar), name = "Test", showlegend = FALSE, text = "Test", 
                                          hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            
            
            combPlotOpt <<- subplot(pOpt, pErrorsOpt, pComparOptim, nrows = 3, shareX = TRUE )
        }
        
        combPlotOpt
        
    })
    
    # Table of errors
    output$table_OptimTab <- renderDataTable({
        click <- event_data("plotly_click", source = "contour")
        if (!is.null(click)) {
            k = click[[3]]
            d = click[[4]]
        }
        
        # Obtain names and errors matrix
        names_col_local <- c(names_col[1])
        errors_matrix_local <- matrix(errors_matrix[1, ], nrow = 1)
        
        # Naive activated with checkbox
        if (input$chbNaiveOpt == 1) {
            names_col_local <- c(names_col_local, names_col[2])
            errors_matrix_local <- rbind(errors_matrix_local, errors_matrix[2, ])
        }
        
        # Seasonal naive activated with checkbox
        if (input$chbSeasNaiveOpt == 1) {
            seasLag <- as.numeric(input$seasNaivOptLag)
            names_col_local <- c(names_col_local, paste0(names_col[3], " (", seasLag, ")"))
            isolate({
                snaive <- ts(y[(train_init - seasLag + 1):(n - seasLag)])
            })
            train_error <- forecast::accuracy(snaive[1:length(y_train_err)], y_train_err)
            test_error <- forecast::accuracy(snaive[(length(y_train_err) + 1):length(snaive)], y_test_err)
            errors_matrix_local <- rbind(errors_matrix_local, c(train_error, test_error))
        }
        # Plot all k-d combinations selected
        if (any( selected_points ) ) {
            for (i_ind in 1:NROW(selected_points))
                for (j_ind in 1:NCOL(selected_points))
                    if (selected_points[i_ind, j_ind]) {
                        i <- ks[i_ind]
                        j <- ds[j_ind]
                        # Add it's name
                        names_col_local <- c(names_col_local, paste0("k = " , i, ", d = " , j))
                        # Generate forecast
                        newPred <- knn_past(y = y, k = i, d = j, initial = train_init, distance = distance, 
                                            weight = weight, threads = n_threads)$fitted
                        newPred <- as.vector(newPred)
                        
                        # Measure forecast errors
                        train_error <- forecast::accuracy(ts(newPred[1:length(y_train_err)]), y_train_err)
                        test_error <- forecast::accuracy(ts(newPred[(length(y_train_err) + 1):length(newPred)]), y_test_err)
                        
                        errors_matrix_local <- rbind(errors_matrix_local, c(train_error, test_error))
                    }
        }
        # Generate associated datatable
        DT::datatable(data.frame(
            Name = names_col_local, 
            trainME = round(errors_matrix_local[, 1], digits = 8), 
            trainRMSE = round(errors_matrix_local[, 2], 8),
            trainMAE = round(errors_matrix_local[, 3], digits = 8), 
            testME = round(errors_matrix_local[, 6], digits = 8),
            testRMSE = round(errors_matrix_local[, 7], 8), 
            testMAE = round(errors_matrix_local[, 8], digits = 8)
        ), colnames = c("Name", "ME (train)", "RMSE (train)", "MAE (train)", 
                        "ME (test)", "RMSE (test)", "MAE (test)"))        
    })
    
} 

ui <- navbarPage("",
                 tabPanel("Distances",
                          fluidPage(
                              headerPanel("Distances between elements"),
                              mainPanel(width = 11,
                                        plotlyOutput("elemsPlot", width = "100%", height = "600px")
                              )
                              ,sidebarPanel(width = 1,
                                            tags$head(
                                                tags$style(HTML("hr {border-top: 1px solid #cbcbcb;}"))
                                            ),
                                            textInput(inputId = "selKtabDist", label = "K", value = res$opt_k, 
                                                      width = NULL, placeholder = "Value for K (blank for Optimal)"),
                                            textInput(inputId = "selDtabDist", label = "D", value = res$opt_d, 
                                                      width = NULL, placeholder = "Value for D (blank for Optimal)"),
                                            hr(),
                                            materialSwitch(inputId = "chbabsDist", label = "Absolute Error", value = TRUE, status = "primary")
                              )
                              
                              # ,fluidRow(
                              #     # column(4, plotlyOutput("scattDistStddev")),
                              #     column(5, plotlyOutput("scattDistErr", height = "500px"))
                              #     # ,column(3, plotlyOutput("scattStddevErr", height = "500px"))
                              #     ,column(7, plotlyOutput("heatScatErr", height = "500px"))
                              # )
                              ,mainPanel(width = 12,
                                         plotlyOutput("scattDistErr", height = "500px") )
                              ,hr()
                              ,mainPanel(width = 12,
                                         plotlyOutput("neighborsPlot", height = "400px") )
                              ,hr()
                              # ,fluidRow(
                              #     column(6, plotlyOutput("distsPlot", height = "500px")),
                              #     column(5, plotlyOutput("distScatPlot", height = "500px")),
                              #     column(1, sidebarPanel(
                              #                   tags$head(tags$style(HTML("hr {border-top: 1px solid #cbcbcb;}"))),
                              #                 textInput(inputId = "selPerctabDist", label = "Percentile threshold", 
                              #                       value = 0.8, width = NULL, placeholder = "0.0-1.0")
                              #               )
                              #           )
                              # )
                              ,mainPanel(width = 6,
                                         plotlyOutput("distsPlot", height = "500px")
                              )
                              ,mainPanel(width = 5,
                                         plotlyOutput("distScatPlot", height = "500px")
                              )
                              ,sidebarPanel(width = 1,
                                            tags$head(
                                                tags$style(HTML("hr {border-top: 1px solid #cbcbcb;}"))
                                            ),
                                            textInput(inputId = "selPerctabDist", label = "Percentile threshold", value = 0.8,
                                                      width = NULL, placeholder = "0.0-1.0")
                              )
                          )
                 ),
                 
                 
                 tabPanel("Optimization",
                          headerPanel(HTML(paste0("Errors for each <em>k</em> and <em>d</em> (", error_measure, " error)"))),
                          mainPanel( width = 10,
                                     plotlyOutput("contourPlot")
                          ),
                          sidebarPanel(width = 2,
                                       radioButtons(inputId = "contourType", label = "Type of contour", selected = "default",
                                                    choices = list("Default" = "default", "Contour lines under Naive" = "naive", 
                                                                   "Top-values color trimmed" = "trim")),
                                       hr(),
                                       textInput(inputId = "contourMinims", label = "Number of minimums to plot:", 
                                                 value = 5, placeholder = "Leave blank to default (5)"),
                                       hr(),
                                       checkboxInput("chbLocalMins", label = "Local Minimums", value = FALSE), 
                                       textInput(inputId = "localMinRad", label = "Local radius:", value = 1),
                                       
                          ),
                          headerPanel("Time Series and Predictions"),
                          mainPanel( width = 10,
                                     plotlyOutput("optPlot", height = "500px") 
                          ),
                          sidebarPanel( width = 2, 
                                        tags$head( tags$style(HTML("hr {border-top: 1px solid #cbcbcb;}")) ),
                                        checkboxInput("chbNaiveOpt", label = "Naive", value = FALSE),
                                        hr(),
                                        checkboxInput("chbSeasNaiveOpt", label = "Seasonal Naive", value = FALSE), 
                                        textInput(inputId = "seasNaivOptLag", label = "Lag:", value = 12),
                                        hr(),
                                        materialSwitch(inputId = "chbabs_tab2", label = "Absolute Error", value = TRUE, status = "primary"),
                                        hr(),
                                        checkboxInput("chbload", label = "Custom", value = FALSE),
                                        textInput("path", "File:", placeholder = "Absolute path to the file"),
                                        actionButton("browse", "Browse")
                          ),
                          headerPanel("Errors Table"),
                          sidebarPanel(
                              DT::dataTableOutput("table_OptimTab"),
                              width = 11
                          )
                 )
)

# Now run the following: 
runApp(shinyApp(server = server, ui = ui, options = list("quiet")))
