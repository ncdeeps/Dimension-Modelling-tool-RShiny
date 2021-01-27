

library(shiny)
library(shinydashboard)
library(shinyLP)
library(summarytools)
library(dplyr)
library(visdat)
library(corrgram)
library(ggplot2)
library(GGally)
library(DataExplorer)
library(devtools)
library(scales)
library(RColorBrewer)
library(stats)
library(vegan)
library(shinycssloaders)
library(xray)
library(pls)

shinyServer(function(input, output,session) {
     
    #This function is responsible for loading in the selected file
    filedata <- reactive({
        infile <- input$datafile 
        if (is.null(infile)) {
            # User has not uploaded a file yet
            return(NULL)
        }
        df<- read.csv(infile$datapath)
        
        #Used for scatterplot
        updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'var', label = 'Variable to display',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'hom', label = 'Variable to display',
                          choices = names(df[,sapply(df,is.numeric)]), selected = names(df[,sapply(df,is.numeric)]))
        updateSelectInput(session, inputId = 'corr', label = 'Variable to display',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'distr', label = 'Variable to display',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'rise', label = 'Variable to display',
                          choices = names(df[,sapply(df,is.numeric)]), selected = names(df[,sapply(df,is.numeric)]))
        updateSelectInput(session, inputId = 'box', label = 'Variable to display',
                          choices = names(df[,sapply(df,is.numeric)]), selected = names(df[,sapply(df,is.numeric)]))
        updateSelectInput(session, inputId = 'pls', label = 'Select Target Variable',
                          choices = names(df[,sapply(df,is.numeric)]), selected = names(df[,sapply(df,is.numeric)]))
        
        return(df) 
    })
    observeEvent(filedata(), {
        updateSelectInput(session, "col", choices = names(filedata()))
    })
    
    
    
    #This previews the CSV data file
    
    output$contents <- DT::renderDataTable({
        DT::datatable(filedata()[, input$var],options = list(orderClasses = TRUE)) # sorted columns are colored
    })
    
    output$filetable <- renderTable({
        req(filedata())
        
    })
    
    #Summary
    output$dfsumm <- renderPrint({
        dfSummary(filedata() %>% as.data.frame(),
                  method = 'render',
                  headings = TRUE,
                  bootstrap.css = FALSE)
    })
    
    #Anomalies
    output$anomaly <- renderPrint({
        xray::anomalies(filedata())
    })
    
    
    # If condition for plot
    
    output$plot <- renderPlot({
        req(input$datafile)
        df <- filedata()
        if(input$plottype == "MissingData"){
            vis_miss(filedata(), cluster = input$clstr)
        }
        else if(input$plottype == "CorrelationPlot"){
            DataExplorer::plot_correlation(filedata()[, input$corr])
        }
        else if(input$plottype == "HomogenietyPlot"){
            
            numData <- scale(na.omit(df[,sapply(df,is.numeric)]), center = TRUE, scale = TRUE) # Normalise so they can share a common y axis
            mypalette <- rainbow(ncol(numData))
            matplot(numData[, input$hom], type = "l", col = mypalette ) #use transparency so you can see into the data
            if(length(input$hom)>1){
                mypalette <- rainbow(ncol(numData))
                legend(legend = colnames(df[, input$hom]), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(df)^0.3))
            }
        }
        else if(input$plottype == "RisingOrder"){
            {for (col in 1:ncol(df)) {
                df[,col] <- df[order(df[,col]),col] #sort each column in ascending order
            }
                numData1 <- scale(x = df[,sapply(df,is.numeric)], center = TRUE, scale = TRUE)  # scale so they can be graphed with a shared Y axis
                mypalette <- rainbow(ncol(numData1))
                matplot(y = df[, input$rise], type = "l", xlab = "Observations", ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising Order chart")
                if(length(input$rise)>1){
                    legend(legend = colnames(df[, input$rise]), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(df)^0.3))
                }
            }
            
        }
        else if(input$plottype == "ScatterPlot"){
            x <- filedata()[, c(input$xcol, input$ycol)]
            plot(x)
        }
        else if(input$plottype == "BoxPlot"){
            boxplot(df[, input$box],outline = input$outliers, range = input$range)
        }
        
        else if(input$plottype == "Distributions"){
            xray::distributions(filedata()[, input$distr])
        }  
        
    }    
    )
    
    ## PCA
    # Check boxes to choose columns
    output$choose_columns_pca <- renderUI({
        
        df <- filedata()
        
        # Get the data set with the appropriate name
        
        # we only want to show numeric cols
        the_data_num <- na.omit(df[,sapply(df,is.numeric)])
        # exclude cols with zero variance
        the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
        
        
        colnames <- names(the_data_num)
        
        # Create the checkboxes and select them all by default
        checkboxGroupInput("columns", "Choose columns", 
                           choices  = colnames,
                           selected = colnames)
    })
    
    # choose a grouping variable
    output$the_grouping_variable <- renderUI({
        df <- filedata()
        
        
        # for grouping we want to see only cols where the number of unique values are less than 
        # 10% the number of observations
        grouping_cols <- sapply(seq(1, ncol(df)), function(i) length(unique(df[,i])) < nrow(df)/10 )
        
        the_data_group_cols <- df[, grouping_cols, drop = FALSE]
        # drop down df
        selectInput(inputId = "the_grouping_variable", 
                    label = "Grouping variable:",
                    choices=c("None", names(the_data_group_cols)))
        
    })
    
    pca_objects <- reactive({
        # Keep the selected columns
        columns <-    input$columns
        df <- na.omit(filedata())
        the_data_subset <- na.omit(df[, columns, drop = FALSE])
        
        
        pca_output <- prcomp(na.omit(the_data_subset), 
                             center = (input$center == 'Yes'), 
                             scale. = (input$scale. == 'Yes'))
        
        
        # data.frame of PCs
        pcs_df <- cbind(df, pca_output$x)
        
        return(list(df = df, 
                    the_data_subset = the_data_subset,
                    pca_output = pca_output, 
                    pcs_df = pcs_df))
        
    })
    
    output$the_pcs_to_plot_x <- renderUI({
        pca_output <- pca_objects()$pca_output$x
        
        # drop down selection
        selectInput(inputId = "the_pcs_to_plot_x", 
                    label = "X axis:",
                    choices= colnames(pca_output), 
                    selected = 'PC1')
    })
    
    output$the_pcs_to_plot_y <- renderUI({
        pca_output <- pca_objects()$pca_output$x
        
        # drop down selection
        selectInput(inputId = "the_pcs_to_plot_y", 
                    label = "Y axis:",
                    choices= colnames(pca_output), 
                    selected = 'PC2')
    })
    
    output$plot2 <- renderPlot({
        pca_output <- pca_objects()$pca_output
        eig = (pca_output$sdev)^2
        variance <- eig*100/sum(eig)
        cumvar <- paste(round(cumsum(variance),1), "%")
        eig_df <- data.frame(eig = eig,
                             PCs = colnames(pca_output$x),
                             cumvar =  cumvar)
        ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
            geom_bar(stat = "identity", fill = "white", colour = "black") +
            geom_text(label = cumvar, size = 4,
                      vjust=-0.4) +
            theme_bw(base_size = 14) +
            xlab("PC") +
            ylab("Variances") +
            ylim(0,(max(eig_df$eig) * 1.1))
    })
    
    
    
    # PC plot
    pca_biplot <- reactive({
        pcs_df <- pca_objects()$pcs_df
        pca_output <-  pca_objects()$pca_output
        
        var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
        var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
        labels <- rownames(pca_output$x)
        grouping <- input$the_grouping_variable
        
        if(grouping == 'None'){
            # plot without grouping variable
            pc_plot_no_groups  <- ggplot(pcs_df, 
                                         aes_string(input$the_pcs_to_plot_x, 
                                                    input$the_pcs_to_plot_y
                                         )) +
                
                
                geom_text(aes(label = labels),  size = 5) +
                theme_bw(base_size = 14) +
                coord_equal() +
                xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
                ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
            # the plot
            pc_plot_no_groups
            
        } else {
            # plot with grouping variable
            
            pcs_df$fill_ <-  as.character(pcs_df[, grouping, drop = TRUE])
            pc_plot_groups  <- ggplot(pcs_df, aes_string(input$the_pcs_to_plot_x, 
                                                         input$the_pcs_to_plot_y, 
                                                         fill = 'fill_', 
                                                         colour = 'fill_'
            )) +
                stat_ellipse(geom = "polygon", alpha = 0.1) +
                
                geom_text(aes(label = labels),  size = 5) +
                theme_bw(base_size = 14) +
                scale_colour_discrete(guide = FALSE) +
                guides(fill = guide_legend(title = "groups")) +
                theme(legend.position="top") +
                coord_equal() +
                xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
                ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
            # the plot
            pc_plot_groups
        }
    })
    
    output$brush_info <- renderTable({
        # the brushing function
        brushedPoints(pca_objects()$pcs_df, input$plot_brush)
    })
    
    
    # for zooming
    output$z_plot1 <- renderPlot({
        
        pca_biplot() 
        
    })
    
    # zoom ranges
    zooming <- reactiveValues(x = NULL, y = NULL)
    
    observe({
        brush <- input$z_plot1Brush
        if (!is.null(brush)) {
            zooming$x <- c(brush$xmin, brush$xmax)
            zooming$y <- c(brush$ymin, brush$ymax)
        }
        else {
            zooming$x <- NULL
            zooming$y <- NULL
        }
    })
    
    
    # for zooming
    output$z_plot2 <- renderPlot({
        
        pca_biplot() + coord_cartesian(xlim = zooming$x, ylim = zooming$y) 
        
        
    })
    
    output$brush_info_after_zoom <- renderTable({
        # the brushing function
        brushedPoints(pca_objects()$pcs_df, input$plot_brush_after_zoom)
    })
    
    output$pca_details <- renderPrint({
        # 
        print(pca_objects()$pca_output$rotation)
        summary(pca_objects()$pca_output)
        
    })
    
    
    # MDS 
    
    output$MDS.dist <- renderPrint({
        MDS.dist1 <- stats::dist(filedata(), method = input$distmethod)  # compute distance matrix
        MDS.dist1
    })
    output$MDS.data2d <- renderPlot({
        MDS.dist1 <- stats::dist(filedata(), method = input$distmethod)  # compute distance matrix
        MDS.dist1
        data2d <- stats::cmdscale(MDS.dist1, k = input$k) # k is the number of dim
        plot(data2d)
    })
    
    # NMDS
    
    output$NMDS.dist <- renderPrint({
        NMDS.dist1 <- stats::dist(filedata(), method = input$distmethod1)  # compute distance matrix
        NMDS.dist1
    })
    output$NMDS.data2d <- renderPlot({
        NMDS.dist1 <- stats::dist(filedata(), method = input$distmethod1)  # compute distance matrix
        NMDS.mds <- vegan::metaMDS(NMDS.dist1,k = input$k1 )
        plot(NMDS.mds$points,  type = "n")
        text(NMDS.mds$points, labels = rownames(NMDS.mds$points))
    })
    
    
    
    #Add date-day to header
    output$Refresh1 <- renderText({
        toString(format(Sys.Date(), format = "%A  %d %b %Y"))
    })
    
    #PLS
    
    # Check boxes to choose columns
    output$choose_columns_pls <- renderUI({
        
        df <- filedata()
        
        # Get the data set with the appropriate name
        
        # we only want to show numeric cols
        the_data_num <- na.omit(df[,sapply(df,is.numeric)])
        # exclude cols with zero variance
        the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
        
        
        colnames <- names(the_data_num)
        
        # Create the checkboxes and select them all by default
        checkboxGroupInput("columns1", "Choose columns", 
                           choices  = colnames,
                           selected = colnames)
    })
    
    pls_objects <- reactive({
        
        
        # Keep the selected columns
        columns1 <-    input$columns1
        df <- na.omit(filedata())
        the_data_subset1 <- na.omit(df[, columns1, drop = FALSE])
        
        
        # Model fit
        pls_output <- pls::plsr(the_data_subset1[, input$pls] ~ ., 
                                data = the_data_subset1,
                                center1 = (input$center == 'Yes'), 
                                scale.1 = (input$scale == 'Yes'),
                                ncomp =input$ncomp1,
                                validation = input$val
        )
        
        return(list(df = df,
                    the_data_subset1 = the_data_subset1,
                    pls_output = pls_output
        ))
        
        
        
    })
    
    output$pls_details <- renderPrint({
        summary(pls_objects()$pls_output)
    })
    
    #PLSPlot
    output$plot3 <- renderPlot({
        plot(RMSEP(pls_objects()$pls_output),legendpos = "topright")
    })
    
    output$scr <- renderPlot({
        plot((pls_objects()$pls_output),plottype = "scores",comps =1:input$ncomp1)
    })
    output$load <- renderPlot({
        plot((pls_objects()$pls_output),plottype = "loadings",comps =1:input$ncomp1,
             legendpos = "topleft")
    })
    output$pred <- renderPlot({
        plot((pls_objects()$pls_output),ncomp = input$ncomp1,asp =1, line = TRUE)
    })
    output$regcf <- renderPlot({
        plot((pls_objects()$pls_output),ncomp = 1:input$ncomp1,plottype = "coef", legendpos = "bottomleft")
    })
    
}   
)
