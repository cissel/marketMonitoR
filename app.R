# MBS Home Dashboard
# James H Cissel V // jhcv.csv 

##### Set Up #####

##### Required packages #####

require(shiny)
require(shinydashboard)
require(dashboardthemes)
require(tidyverse)
require(readxl)
require(scales)
require(plotly)
require(httr)
require(jsonlite)
require(lubridate)
require(tesseract)
require(magrittr)
require(magick)
require(fredr)
require(forecast)
require(sweep)
require(gganimate)
require(transformr)
require(pdftools)

#####

##### Federal Reserve API Authentication #####

fredr_set_key("d47e2b30bf4826314df23a57408a56a6")

#####

##### Plot Appearance Theme #####

myTheme <- theme(legend.position = "none",
                 plot.background = element_rect(fill = "#02233F"),
                 panel.background = element_rect(fill = "#02233F"),
                 panel.grid = element_line(color = "#274066"),
                 axis.ticks = element_line(color = "#274066"),
                 axis.text = element_text(color = "white"),
                 axis.title = element_text(color = "white"),
                 plot.title = element_text(color = "white",
                                           hjust = .5),
                 plot.subtitle = element_text(color = "white",
                                              hjust = .5))

#####

##### Legend appearance theme #####

myLegend <- theme(legend.position = "right",
                  legend.background = element_rect(fill = "#02233F"),
                  legend.text = element_text(color = "white"),
                  legend.title = element_text(color = "white"))

#####

##### UI Theme #####

custom_theme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(255,255,255)"
  ,primaryFontColor = "rgb(255,255,255)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(2, 35, 63)"
  
  ### header
  ,logoBackColor = "rgb(2, 35, 63)"
  
  ,headerButtonBackColor = "rgb(2, 35, 63)"
  ,headerButtonIconColor = "rgb(210,210,210)"
  ,headerButtonBackColorHover = "rgb(49,81,130)"
  ,headerButtonIconColorHover = "rgb(255,255,255)"
  
  ,headerBackColor = "rgb(2, 35, 63)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(2, 35, 63)"
    ,colorMiddle = "rgb(2, 35, 63)"
    ,colorEnd = "rgb(2, 35, 63)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 1
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "0px 0px 0px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(49,81,130)"
  ,sidebarSearchIconColor = "rgb(255,255,255)"
  ,sidebarSearchBorderColor = "rgb(49,81,130)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(49,81,130)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(2, 35, 63)"
    ,colorMiddle = "rgb(2, 35, 63)"
    ,colorEnd = "rgb(2, 35, 63)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(210,210,210)"
  ,sidebarTabRadiusSelected = "0px 0px 0px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(49,81,130)"
    ,colorMiddle = "rgb(49,81,130)"
    ,colorEnd = "rgb(49,81,130)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 0px 0px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(49,81,130)"
  ,boxBorderRadius = 0
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = "rgb(75,126,151)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(49,81,130)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(210,210,210)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

#####

##### Custom CSS for TabBoxes #####
js <- '.nav-tabs-custom .nav-tabs, .nav-tabs-custom .nav-tabs li.active a, .nav-tabs-custom .nav-tabs li.active, .nav-tabs-custom .tab-content {
    background-color: #315182;
    border-color: #315182;
    border-radius: 0px;
 }

 .nav-tabs-custom .nav-tabs li.active a {
   color: rgb(255, 255, 255);
 }

.nav-tabs-custom .nav-tabs li.active:hover a {
  background-color: #4674b9;
  border-color: #FFFFFF;
  color: #315182;
}'

#####

##### UI #####

header <- dashboardHeader(title = "supplychainr")

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    
    menuItem("Home",
             tabName = "home",
             icon = icon("home")),
    
    menuItem("Maps",
             tabName = "map",
             icon = icon("map"))
    
  )
  
)

body <- dashboardBody(
  
  custom_theme,
  tags$style(js),
  
  tabItems(
    
    ##### Home page #####
    
    tabItem(
      
      tabName = "home",
      
      h1("U.S. Macro Conditions"),
      
      h2("Treasury Securities"),
      
      fluidRow(
        
        box(width = 4,
            plotlyOutput("yieldCurve",
                         height = "300px")),
        box(width = 4,
            plotlyOutput("irhp",
                         height = "300px")),
        box(width = 4,
            plotlyOutput("yieldSpreads",
                         height = "300px"))
        
      ),
      
      h2("Consumer Confidence"),
      
      fluidRow(
        
        box(width = 3,
            plotlyOutput("dfedtar",
                         height = "300px")),
        box(width = 3,
            plotlyOutput("cpip",
                         height = "300px")),
        box(width = 3,
            plotlyOutput("unrate",
                         height = "300px")),
        box(width = 3,
            plotlyOutput("m30us",
                         height = "300px"))
        
      ),
      
      h2("Housing"),
      
      fluidRow(
        
        box(width = 3,
            plotlyOutput("uc",
                         height = "300px")),
        box(width = 3, 
            plotlyOutput("uuc",
                         height = "300px")),
        box(width = 3,
            plotlyOutput("houst",
                         height = "300px")),
        box(width = 3,
            plotlyOutput("bp",
                         height = "300px"))
        
      )
      
    ),
    
    #####
    
    ##### Supply Chain page #####
    
    tabItem(
      
      tabName = "map",
      
      h1("Supply Chain Maps"),
      
      h2("Sea"),
      
      fluidRow(
        
        box(width = 12,
            tags$script(
              '
    var width="100%";         // width in pixels or percentage
    var height="950";         // height in pixels
    var latitude="30.4005";     // center latitude (decimal degrees)
    var longitude="-81.3841";    // center longitude (decimal degrees)
    var zoom="10";             // initial zoom (between 3 and 18)
    var names=true;           // show vessel names
    var speed=true; // show speed?
    '
            ),
            tags$script(src = "https://www.vesselfinder.com/aismap.js"))
        
      ),
      
      h2("Sky"),
      
      fluidRow(
        
        box(width = 12)
        
      )
      
    )
    
    #####
    
  ),
  
  align = "center"
)

ui <- dashboardPage(header, sidebar, body)

#####

server <- function(input, output) {
  
  ##### server side functions #####
  
  getFredData <- function(series = "UNRATE") {
    
    data <- fredr_series_observations(series) |>
      
      select(date,
             value)
    
    return(data)
    
  }
  
  plotFredData <- function(data) {
    
    plot <- ggplot(data,
                   aes(x = date,
                       y = value)) +
      
      geom_line(color = "cyan") +
      
      geom_hline(aes(yintercept = tail(value, 1)),
                 color = "cyan") +
      
      myTheme
    
    return(plot)
    
  }
  
  #####
  
  ##### Treasuries #####
  
  output$yieldCurve <- renderPlotly({
    
    # short term treasuries (dur<1y)
    
    stir <- data.frame("mte" = c(1, 3, 6),
                       "name" = c("DGS1MO",
                                  "DGS3MO",
                                  "DGS6MO"))
    
    stdf <- data.frame()
    
    for (i in 1:nrow(stir)) {
      
      data <- getFredData(stir$name[i]) |>
        
        mutate(value = value/100)
      
      data$mte <- stir$mte[i]
      
      stdf <- rbind(stdf, data)
      
    }
    
    # long term treasuries (dur>1y)
    
    ltir <- data.frame("yte" = c(1, 2, 3, 5, 7, 10, 20, 30))
    
    ltir$name <- ""
    
    for (i in 1:nrow(ltir)) {
      
      ltir$name[i] <- paste("DGS",
                            ltir$yte[i],
                            sep = "")
      
    }
    
    ltdf <- data.frame()
    
    for (i in 1:nrow(ltir)) {
      
      hd <- getFredData(ltir$name[i]) |>
        
        mutate("value" = value/100)
      
      hd$yte <- ltir$yte[i]
      
      ltdf <- rbind(ltdf, hd)
      
    }
    
    stl <- stdf |>
      
      group_by(mte) |>
      
      summarize("last" = value[which.max(date)],
                "asOf" = max(date)) |>
      
      select(mte, last, asOf)
    
    ltl <- ltdf |> 
      
      group_by(yte) |>
      
      summarize("last" = value[which.max(date)],
                "asOf" = max(date)) |>
      
      mutate("mte" = yte*12) |>
      
      select(mte, last, asOf)
    
    ycdf <- rbind(stl, ltl)
    
    ycdf$yte <- ycdf$mte/12
    
    ycp <- ggplot(ycdf,
                  aes(x = yte,
                      y = last)) +
      
      geom_line(color = "white") +
      
      labs(x = "Time to Maturity",
           y = "Interest Rate",
           title = paste("Market Yield on U.S. Treasury Securities at Constant Maturity:",
                         max(ycdf$asOf),
                         sep = " ")) +
      
      scale_x_log10(breaks = ycdf$yte,
                    labels = c("1 Month",
                               "3 Months",
                               "6 Months",
                               "1 Year",
                               "2 Years",
                               "3 Years",
                               "5 Years",
                               "7 Years",
                               "10 Years",
                               "20 Years",
                               "30 Years")) +
      
      scale_y_continuous(labels = percent) +
      
      myTheme +
      theme(axis.text.x = element_text(angle = -45,
                                       size = 8),
            plot.title = element_text(size = 7))
    
    ycp
    
  })
  
  output$irhp <- renderPlotly({
    
    # short term treasuries (dur<1y)
    
    stir <- data.frame("mte" = c(1, 3, 6),
                       "name" = c("DGS1MO",
                                  "DGS3MO",
                                  "DGS6MO"))
    
    stdf <- data.frame()
    
    for (i in 1:nrow(stir)) {
      
      data <- getFredData(stir$name[i]) |>
        
        mutate(value = value/100)
      
      data$mte <- stir$mte[i]
      
      stdf <- rbind(stdf, data)
      
    }
    
    stdf$yte <- 0 
    
    for (i in 1:nrow(stdf)) {
      
      stdf$yte[i] <- round(stdf$mte[i]/12, 2)
      
    }
    
    stdf <- stdf |> select(date, value, yte)
    
    stdf$stlt <- "short"
    
    # long term treasuries (dur>1y)
    
    ltir <- data.frame("yte" = c(1, 2, 3, 5, 7, 10, 20, 30))
    
    ltir$name <- ""
    
    for (i in 1:nrow(ltir)) {
      
      ltir$name[i] <- paste("DGS",
                            ltir$yte[i],
                            sep = "")
      
    }
    
    ltdf <- data.frame()
    
    for (i in 1:nrow(ltir)) {
      
      hd <- getFredData(ltir$name[i]) |>
        
        mutate("value" = value/100)
      
      hd$yte <- ltir$yte[i]
      
      ltdf <- rbind(ltdf, hd)
      
    }
    
    ltdf$stlt <- "long"
    
    irdf <- rbind(stdf, ltdf)
    
    irp <- ggplot(irdf,
                  aes(x = date,
                      y = value,
                      color = factor(yte))) +
      
      geom_line() +
      
      labs(x = "Time",
           y = "Interest Rate",
           color = "Years to Maturity",
           title = "Market Yield On U.S. Treasury Securities at Constant Maturity") +
      
      scale_y_continuous(labels = percent) +
      
      myTheme +
      myLegend +
      theme(plot.title = element_text(size = 8),
            legend.title = element_text(size = 9)) 
    
    irp
    
  })
  
  output$yieldSpreads <- renderPlotly({
    
    t10y3m <- getFredData("T10Y3M") |>
      
      mutate(value = value/100,
             series = "T10Y3M")
    
    t10y2y <- getFredData("T10Y2Y") |>
      
      mutate(value = value/100,
             series = "T10Y2Y")
    
    ysdf <- rbind(t10y3m, t10y2y)
    
    ysds <- ysdf |> group_by(series) |>
      
      summarize("last" = tail(value, 1))
    
    ysp <- ggplot(ysdf,
                  aes(x = date,
                      y = value,
                      color = factor(series))) +
      
      geom_hline(aes(yintercept = 0),
                 color = "white") +
      
      geom_line() +
      
      geom_hline(data = ysds,
                 aes(yintercept = last,
                     color = factor(series))) +
      
      labs(x = "Time",
           y = "Spread Value",
           color = "Spread",
           title = "U.S. Treasury Security Spreads") +
      
      scale_y_continuous(labels = percent) +
      
      myTheme +
      myLegend
    
    ysp
    
  })
  
  #####
  
  ##### Consumers #####
  
  output$dfedtar <- renderPlotly({
    
    dft <- getFredData("DFEDTARU") |>
      
      mutate(value = value/100)
    
    dftp <- plotFredData(dft)
    
    dftp +
      
      labs(x = "Time",
           y = "Rate",
           title = "Federal Funds Target Rate") +
      
      scale_y_continuous(labels = percent) +
      
      myTheme
    
  })
  
  output$cpip <- renderPlotly({
    
    cpi <- getFredData("CPIAUCNS")
    
    cpi$pct <- 0
    
    for (i in 13:nrow(cpi)) {
      
      cpi$pct[i] <- (cpi$value[i]-cpi$value[i-12])/cpi$value[i-12]
      
    }
    
    cpi <- cpi |> drop_na()
    
    cpip <- ggplot(cpi,
                   aes(x = date,
                       y = pct)) +
      
      geom_hline(aes(yintercept = 0),
                 color = "white") +
      
      geom_line(color = "cyan") +
      geom_hline(aes(yintercept = tail(pct, 1)),
                 color = "cyan") +
      
      scale_y_continuous(labels = percent) +
      
      labs(x = "Time",
           y = "Inflation Rate",
           title = "Consumer Price Index for All Urban Consumers: All Items in U.S. City Average Year-Over-Year") +
      
      myTheme +
      theme(plot.title = element_text(size = 7))
    
    cpip
    
  })
  
  output$unrate <- renderPlotly({
    
    urd <- getFredData("UNRATE") |>
      
      mutate(value = value/100)
    
    urp <- plotFredData(urd)
    
    urp +
      
      labs(x = "Time",
           y = "Value",
           title = "Unemployment Rate") +
      
      scale_y_continuous(labels = percent)
    
  })
  
  output$m30us <- renderPlotly({
    
    m30us <- getFredData("MORTGAGE30US") |>
      
      mutate(value = value/100)
    
    m30p <- plotFredData(m30us) +
      
      labs(x = "Time",
           y = "Interest Rate",
           title = "30-Year Fixed Rate Mortgage Average") +
      
      scale_y_continuous(labels = percent)
    
  })
  
  #####
  
  ##### Housing #####
  
  output$uc <- renderPlotly({
    
    uc <- getFredData("COMPUTNSA")
    
    plotFredData(uc) +
      
      labs(x = "Time",
           y = "Thousands of Units",
           title = "New Privately-Owned Housing Units Completed: Total Units") +
      
      theme(plot.title = element_text(size = 9))
    
  })
  
  output$uuc <- renderPlotly({
    
    uuc <- getFredData("UNDCONTNSA")
    
    plotFredData(uuc) +
      
      labs(x = "Time",
           y = "Thousands of Units",
           title = "New Privately-Owned Housing Units Under Construction: Total Units") +
      
      theme(plot.title = element_text(size = 8))
    
  })
  
  output$houst <- renderPlotly({
    
    houst <- getFredData("HOUSTNSA")
    
    plotFredData(houst) +
      
      labs(x = "Time",
           y = "Thousands of Units",
           title = "New Privately-Owned Housing Units Started: Total Units") +
      
      theme(plot.title = element_text(size = 9))
    
  })
  
  output$bp <- renderPlotly({
    
    hbp <- getFredData("PERMITNSA")
    
    plotFredData(hbp) +
      
      labs(x = "Time",
           y = "Thousands of Units",
           title = "New Privately-Owned Housing Units Authorized: Total Units") +
      
      theme(plot.title = element_text(size = 9))
    
  })
  
  #####
  
}

# Run the application 
shinyApp(ui = ui, server = server)
