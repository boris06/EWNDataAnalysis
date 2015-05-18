library(ggplot2)
library(grid)

ewndata <- read.csv("./data/eurowaternet_slovenian.csv")
ewndata$yearMonth15 <- as.Date(ewndata$yearMonth15)
ewndata$station_id <- as.character(ewndata$station_id)

shinyServer(
        function(input, output) { 
                numberStations <- reactive({
                        if (input$station_id == "all") {
                                numberStations <- 4
                        }
                        else {
                                numberStations <- 1
                        }
                        
                })
                ewnsel <- reactive({
                        numberStations <- 1
                        ewnsel <- ewndata[, c("yearMonth15","station_id",
                                              "depth",input$variable, input$variable2)]
                        ewnsel <- ewndata
                        if (input$station_id == "all") {
                                ewnsel
                                numberStations <- 4
                        }
                        else {
                                ewnsel <- ewnsel[ewnsel$station_id == input$station_id, ]                                
                        }
                        if (input$depth == "all") {
                                ewnsel
                        }
                        else {
                                ewnsel <- ewnsel[ewnsel$depth == as.numeric(input$depth), ]                                                                     
                        }
                })
                output$timeSeriesPlot <- renderPlot({ 
                        ylabel <- names(which(listVar==input$variable))
                        jet.colors <- rev(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
                        g <- ggplot(ewnsel(), aes_string(x = "yearMonth15", y = input$variable, colour = "factor(depth)"), environment = environment())
                        g <- g + geom_point(size = 1) 
                        g <- g + stat_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1, linetype = 1) 
                        g <- g + facet_wrap(~ station_id, nrow = numberStations(), ncol =1) 
                        g <- g + scale_colour_manual(values=jet.colors) 
                        g <- g + theme_bw(base_size = 24) 
                        g <- g + labs(x = "Year") + labs(y = ylabel) 
                        g <- g + labs(color = "Depth (m)") 
                        g <- g + scale_fill_continuous(guide = guide_legend()) +
                                theme(legend.position="bottom", legend.key.size=unit(1, "cm"),
                                      legend.key.height=unit(0.7, "cm"))
                        g
                }, height=750)
                output$regressionPlot <- renderPlot({
                        xlabel <- names(which(listVar==input$varX)) 
                        ylabel <- names(which(listVar==input$varY)) 
                        ggplot(ewnsel(), aes_string(x=input$varX, y=input$varY)) +
                                labs(x = xlabel) + labs(y = ylabel) +
                                geom_point(shape=1) +    # Use hollow circles
                                geom_smooth(method=lm)   # Add linear regression line 
                        #  (by default includes 95% confidence region)
                })
                observeEvent(input$simpModButton, {
                        model.formula <- as.formula(paste(input$varY,
                                                          input$varX,
                                                          sep=" ~ "))
                        model.simp <- lm(model.formula, ewnsel())
                        output$text1 <- renderPrint({summary(model.simp)})
                        
                })
                observeEvent(input$bestModButton, {
                        output$text1 <- renderText("Please, wait ...")
                        notna <- apply(ewnsel()[,c(-1,-2,-3)],2,
                                       FUN=function(x) mean(!is.na(x)))
                        names.notna <- c(names(ewnsel()[,1:3]),names(notna[notna>0.75]))
                        no.na.data <- na.omit(ewnsel()[,names.notna])
                        if (input$varY %in% names(no.na.data)) {
                                model.best <- step(lm(as.formula(paste(input$varY, 
                                                                       ".", sep=" ~ ")), 
                                                      data = no.na.data[,names.notna[c(-1,-2,-3)]]), 
                                                   trace=0)
                                output$text1 <- renderPrint({summary(model.best)})                                
                        }
                        else {
                                output$text1 <- renderPrint({paste("Best model is not available for variable",
                                                                   input$varY,
                                                                   "(>25% missing values)")}) 
                        }
                }) 
        }
)

