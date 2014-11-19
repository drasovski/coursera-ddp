suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(shiny))

shinyServer(function(input, output) {
    
    options(shiny.reactlog=TRUE)
    
    # data set selection UI
    output$datasetui <- renderUI({
        radioButtons("dataset",
                     label = "Choose a data set:",
                     choices = list("Motor Trend Car Road Tests" = "mtcars",
                                    "Swiss Fertility and Socioeconomic Indicators (1888)" = "swiss",
                                    "Lawyers' Ratings of State Judges in the US Superior Court" = "USJudgeRatings"))
    })
    
    # data set input
    inputs <- reactive({
        mydataset <- input$dataset
        
        # value checking and default starting values
        if (is.null(mydataset) | length(mydataset) == 0) {
            mydataset <- "mtcars"
            mydf <- get(mydataset)
        } else {
            mydf <- get(mydataset)
        }
        
        l <- list("mydataset" = mydataset,
                  "mydf" = mydf)
        return(l)
    })
    
    # variable selection UI for "One Variable Explorer"
    output$onevarui <- renderUI({
        inp <- inputs()
        selectInput("onevar",
                    label = "Choose a variable to explore by itself:",
                    choices = colnames(inp$mydf))
    })
    
    # variable 1 selection UI for "Two Variable Explorer"
    output$twovar1ui <- renderUI({
        inp <- inputs()
        selectInput("twovar1",
                    label = "Choose two variables to explore together:",
                    choices = colnames(inp$mydf))
    })
    
    # variable 2 selection UI for "Two Variable Explorer"
    output$twovar2ui <- renderUI({
        inp <- inputs()
        selectInput("twovar2",
                    label = NULL,
                    choices = colnames(inp$mydf),
                    selected = colnames(inp$mydf)[2])
    })
    
    # variable inputs
    varinputs <- reactive({
        inp <- inputs()
        
        myonevar <- input$onevar
        mytwovar1 <- input$twovar1
        mytwovar2 <- input$twovar2
            
        # value checking and default starting values
        if (!is.null(myonevar) & is.element(myonevar,colnames(inp$mydf)) & length(myonevar) > 0) {
            myonevar <- input$onevar
            myonecol <- paste0(inp$mydataset,"$",myonevar)
        } else {
            myonevar <- colnames(inp$mydf)[1]
            myonecol <- paste0(inp$mydataset,"$",myonevar)
        }
        if (!is.null(mytwovar1) && is.element(mytwovar1,colnames(inp$mydf)) && length(mytwovar1) > 0) {
            mytwovar1 <- input$twovar1
            mytwocol1 <- paste0(inp$mydataset,"$",mytwovar1)
        } else {
            mytwovar1 <- colnames(inp$mydf)[1]
            mytwocol1 <- paste0(inp$mydataset,"$",mytwovar1)
        }
        if (!is.null(mytwovar2) && is.element(mytwovar2,colnames(inp$mydf)) && length(mytwovar2) > 0) {
            mytwovar2 <- input$twovar2
            mytwocol2 <- paste0(inp$mydataset,"$",mytwovar2)
        } else {
            mytwovar2 <- colnames(inp$mydf)[2]
            mytwocol2 <- paste0(inp$mydataset,"$",mytwovar2)
        }
        
        l <- list("myonevar" = myonevar,
                  "mytwovar1" = mytwovar1,
                  "mytwovar2" = mytwovar2,
                  "myonecol" = myonecol,
                  "mytwocol1" = mytwocol1,
                  "mytwocol2" = mytwocol2)
        return(l)
    })
    
    # "About the Data" page
    output$about <- reactive({
        inp <- inputs()
        prefix <- "http://stat.ethz.ch/R-manual/R-patched/library/datasets/html/"
        suffix <- ".html"
        link <- paste0(prefix,inp$mydataset,suffix)
        ab <- includeHTML(link)
        return(ab)
    })
    
    # data set variable summary table
    output$summary <- renderDataTable({
        inp <- inputs()
        sum <- data.frame(describe(inp$mydf))[,c(1:5,8:9)]
        sum$vars <- rownames(sum)
        colnames(sum)[1] <- "variable"
        return(sum)
    }, options = list(bPaginate=FALSE, bFilter=FALSE))
    
    # histogram
    output$histogram <- renderPlot({
        inp <- inputs()
        vinp <- varinputs()

        # makes sure that variables correspond to correct data set upon data set change
        if (is.element(vinp$myonevar,colnames(inp$mydf))) {
            d <- eval(parse(text=vinp$myonecol))
            h <- hist(d, freq = FALSE, xlab = vinp$myonevar, main = paste("Histogram of",vinp$myonevar), col = "lightsteelblue1")
        } else {
            h <- plot.new()
        }
        return(h)
    })
    
    # boxplot
    output$boxplot <- renderPlot({
        inp <- inputs()
        vinp <- varinputs()
        
        # makes sure that variables correspond to correct data set upon data set change
        if (is.element(vinp$myonevar,colnames(inp$mydf))) {
            d <- eval(parse(text=vinp$myonecol))
            b <- boxplot(d, xlab = vinp$myonevar, main = paste("Boxplot of",vinp$myonevar), col = "coral1")
        } else {
            b <- plot.new()
        }
        return(b)
    })
    
    # scatterplot with regression line
    output$scatterplot <- renderPlot({
        inp <- inputs()
        vinp <- varinputs()
        
        # makes sure that variables correspond to correct data set upon data set change
        if (is.element(vinp$mytwovar1,colnames(inp$mydf)) & is.element(vinp$mytwovar1,colnames(inp$mydf))) {
            d1 <- eval(parse(text=vinp$mytwocol1))
            d2 <- eval(parse(text=vinp$mytwocol2))
            s <- plot(d1,d2,xlab = vinp$mytwovar1, ylab = vinp$mytwovar2, col = "palegreen4", cex = 2,
                      main = paste("Scatter plot of",vinp$mytwovar2,"against",vinp$mytwovar1,"with a regression line"))
            s <- abline(lm(d2~d1), col = "red3")
        } else {
            s <- plot.new()
        }
        return(s)
    })
    
})