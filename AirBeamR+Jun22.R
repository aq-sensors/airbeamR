
## Libraries

library(RColorBrewer)
library(shiny)
library(bslib)
library(DT)
library(httr)
library(jsonlite)
library(rjson)
library(data.table)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(mapview)
library(sf)
library(tidyr)
library(shinyTime)
library(shinybusy)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(calendR)
library(openair)
library(viridis)

options(shiny.maxRequestSize=1000*1024^2)

#####
#####

## UI Layout

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty", "primary"="#00b2ef", "secondary"="#00b2ef"),
  useShinyjs(), 
  
  titlePanel("AirBeamR"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput('analysis'),
      fileInput("file1", "Upload Single/Multiple CSV Files:",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),
                multiple = TRUE),
      uiOutput('choice.data'),
      uiOutput('aggregate'),
      uiOutput('daterange')
    ),
    mainPanel(
      uiOutput('tabs')
    )
  )
)

#####
#####

## Tabsets

server <- function(input, output, session) {

  thematic::thematic_shiny()
  
  output$tabs <- renderUI({
  
    if (input$mode=="Mobile" && length(mycsvs())==1) {
      
    tabsetPanel(
      tabPanel("Data Table",  value='tab1',
          DTOutput("table1") %>% withSpinner(type=7, color="#00b2ef"),
               downloadButton("downloadData", "Download")
      ),
      tabPanel("AirBeam Summary", value='tab1',
          DTOutput("summary.ab") %>% withSpinner(type=7, color="#00b2ef"),
               hr(), br(),
          plotOutput("plot.den") %>% withSpinner(type=7, color="#00b2ef"),
               div(style="background-color: #e4eff5",
                   textOutput("text.plots.den")
               ),
               hr(), br(),
          plotOutput("plot.box")    %>% withSpinner(type=7, color="#00b2ef"),
               div(style="background-color: #e4eff5",
                   textOutput("text.plots.box")
               )
      ), 
      tabPanel("Govt Monitor Summary", value='tab1',
          DTOutput("summary.aq") %>% withSpinner(type=7, color="#00b2ef"),
               hr(), br(),
          plotOutput("plot.den.aq") %>% withSpinner(type=7, color="#00b2ef"),
               div(style="background-color: #e4eff5",
                textOutput("text.plots.den.govt")
               ), 
               hr(), br(),
          plotOutput("plot.box.aq") %>% withSpinner(type=7, color="#00b2ef"),
               div(style="background-color: #e4eff5",
                textOutput("text.plots.box.govt"),
               ),
      ), 
       tabPanel("Compare",  value='tab1',
               plotOutput("plot.ab")     %>% withSpinner(type=7, color="#00b2ef"),
               plotOutput("plot.aq")     %>% withSpinner(type=7, color="#00b2ef"),
               plotOutput("plot.ab.adj") %>% withSpinner(type=7, color="#00b2ef"),
               div(style="background-color: #e4eff5",
                   textOutput("text.plots.adj"), 
               ),
               downloadButton("downloadDataTime", "Adjusted Data")
               ),
      tabPanel("AirBeam Map",  value='tab2',
               leafletOutput('leaf0')   %>% withSpinner(type=7, color="#00b2ef"),
      ),
      tabPanel("Govt Monitor Map", value='tab2',
               leafletOutput('nearest.buffer')   %>% withSpinner(type=7, color="#00b2ef")
      ),
      id ="tabselected"
      )
    }
    
    else if (input$mode=='Mobile' && length(mycsvs())>1) {
      
      tabsetPanel(
        tabPanel("Data Table",  value='tab1',
                 DTOutput("table1") %>% withSpinner(type=7, color="#00b2ef"),
                 downloadButton("downloadData", "Download")
        ),
        tabPanel("AirBeam Summary", value='tab1',
                 DTOutput("summary.ab") %>% withSpinner(type=7, color="#00b2ef"),
                 hr(), br(),
                 plotOutput("plot.den.multi")    %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                   textOutput("text.plots.den")
                 ),
                 hr(), br(),
                 plotOutput("plot.box.multi")    %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                  textOutput("text.plots.box")
                 )
        ), 
        tabPanel("Govt Monitors Summary", value='tab1',
                 DTOutput("summary.aq") %>% withSpinner(type=7, color="#00b2ef"),
                 hr(), br(),
                 plotOutput("plot.den.aq.multi") %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                  textOutput("text.plots.den.govt"), 
                 ),
                 hr(), br(),
                 plotOutput("plot.box.aq.multi") %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                 textOutput("text.plots.box.govt"),
                 )
        ), 
        tabPanel("Compare",  value='tab1',
                 plotOutput("plot.ab.multi")     %>% withSpinner(type=7, color="#00b2ef"),
                 plotOutput("plot.aq.multi")     %>% withSpinner(type=7, color="#00b2ef"),
                 plotOutput("plot.ab.adj.multi") %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                     textOutput("text.plots.adj"), 
                 ),
                 downloadButton("downloadDataTime", "Adjusted Data")
        ),
        
        tabPanel("AirBeam Map",  value='tab2',
                 leafletOutput('leaf0')   %>% withSpinner(type=7, color="#00b2ef")
        ),
        
        tabPanel("Govt Monitor Map", value='tab2',
                 leafletOutput('nearest.buffer')   %>% withSpinner(type=7, color="#00b2ef")
        ),
        id ="tabselected"
      )
    }
    
    else if (input$mode=='Fixed' && length(mycsvs())==1) {
      
      tabsetPanel(
        tabPanel("Data Table",  value='tab3',
                 DTOutput("table1") %>% withSpinner(type=7, color="#00b2ef"),
                 downloadButton("downloadData", "Download")
        ),
        tabPanel("AirBeam Summary", value='tab3',
                 DTOutput("summary.ab") %>% withSpinner(type=7, color="#00b2ef"),
                 hr(), br(),
                 plotOutput("plot.den")    %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                  textOutput("text.plots.den")
                 ),
                 hr(), br(),
                 plotOutput("plot.box")    %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                  textOutput("text.plots.box")
                 ),
        ), 
        tabPanel("Govt Monitors Summary", value='tab3',
                 DTOutput("summary.aq") %>% withSpinner(type=7, color="#00b2ef"),
                 hr(), br(),
                 plotOutput("plot.den.aq") %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                  textOutput("text.plots.den.govt")
                 ), 
                 hr(), br(),
                 plotOutput("plot.box.aq") %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                 textOutput("text.plots.box.govt"),
                 )
        ), 
        tabPanel("Compare",  value='tab3',
                 plotOutput("plot.ab")     %>% withSpinner(type=7, color="#00b2ef"),
                 plotOutput("plot.aq")     %>% withSpinner(type=7, color="#00b2ef"),
        ),
        tabPanel("Calendar", value='tab3',
                 uiOutput('years'),
                 uiOutput('choice.mon'),
                 uiOutput('choice.cal'),
               
                 plotOutput("calendar1") %>% withSpinner(type=7, color="#00b2ef")),
        tabPanel("AirBeam Map",  value='tab3',
                 leafletOutput('leaf0')   %>% withSpinner(type=7, color="#00b2ef")
        ),
        tabPanel("Govt Monitor Map", value='tab3',
                 leafletOutput('nearest.buffer')   %>% withSpinner(type=7, color="#00b2ef")
        )
        ,id ="tabselected"
      )
    }
    
    else if (input$mode=='Fixed' && length(mycsvs())>1) {
      
      tabsetPanel(
        tabPanel("Data Table",  value='tab3',
                 DTOutput("table1") %>% withSpinner(type=7, color="#00b2ef"),
                 downloadButton("downloadData", "Download")
        ),
        tabPanel("AirBeam Summary", value='tab3',
                 DTOutput("summary.ab") %>% withSpinner(type=7, color="#00b2ef"),
                 hr(), br(),
                 plotOutput("plot.den")    %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                     textOutput("ab.su")
                 ),
                 hr(), br(),
                 plotOutput("plot.box")    %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                     textOutput("text.plots.box")
                 ),
        ), 
        tabPanel("Govt Monitors Summary", value='tab3',
                 DTOutput("summary.aq") %>% withSpinner(type=7, color="#00b2ef"),
                 hr(), br(),
                 plotOutput("plot.den.aq.multi") %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                     textOutput("text.plots.den.govt")
                 ), 
                 hr(), br(),
                 plotOutput("plot.box.aq.multi") %>% withSpinner(type=7, color="#00b2ef"),
                 div(style="background-color: #e4eff5",
                     textOutput("text.plots.box.govt")
                 )
        ), 
        tabPanel("Compare", value='tab3',
                 plotOutput("plot.ab.multi")     %>% withSpinner(type=7, color="#00b2ef"),
                 plotOutput("plot.aq.multi")     %>% withSpinner(type=7, color="#00b2ef"),

        ),
        tabPanel("Calendar", value='tab3',
                 uiOutput('years'),
                 uiOutput('choice.mon'),
                 uiOutput('choice.cal'),
                 plotOutput("calendar1") %>% withSpinner(type=7, color="#00b2ef")),
        tabPanel("AirBeam Map",  value='tab3',
                 leafletOutput('leaf0')   %>% withSpinner(type=7, color="#00b2ef"),
                 #textOutput('near.dist.text.multi')  %>% withSpinner(type=7, color="#00b2ef")
        ),
        tabPanel("Govt Monitor Map", value='tab3',
                 leafletOutput('nearest.buffer')   %>% withSpinner(type=7, color="#00b2ef")
        ),
        id ="tabselected"
      )
    }
    
    else if (length(mycsvs())==0) {
      
      tabsetPanel(
        fluidRow(
                 column(
                   br(),
                   tags$div(
                     "Welcome to AirBeamR! This is an interactive data tool to visualize and work with", tags$a(href="https://www.habitatmap.org", "AirBeam"), "data. For test data and examples,",
                     tags$a(href="https://mega.nz/file/fCABTQyC#qDft-FaEnA5foHWuvDh4yjFIJ7aLY1dIOxwAxGedRx8", 
                            "please download from here."),
                     ),
                 width=8,
                  br(),
          )
        )
      )    
    }
  }) 
  
#####
#####

## Text Outputs

    output$text.cal      <- renderText({"This plot shows average mean value of AirBeam values as a calendar."})
    output$text.plots.vs <- renderText({"Plots show AirBeam values and corresponding monitor values over selected time period."})
 
    output$text.plots.den <- renderText({
      x <- paste0("Density plots show the distribution of a numeric variable. It is a smoothed version of the histogram.")
    })
   
    output$text.plots.box <- renderText({
      x <- paste0("Box plot displays the minimum, 1st quartile, median, 3rd quartile, and maximum. A vertical line goes thorugh the box at the median. The whiskers go from each quartile to the minimum or maximum. Red stars indicate outliers.")
    })
    
    output$text.plots.den.govt <- renderText({
     dist <- near.dist()/1000
     dist <- round(dist, 3)
     near.moni <- near.moni()
     x <- paste0("Density plot for nearest govt monitor: \"", near.moni, "\", located ", dist, "km away. Density plots show the distribution of a numeric variable. It is a smoothed version of the histogram.")
    })

    output$text.plots.box.govt <- renderText({
      dist <- near.dist()/1000
      dist <- round(dist, 3)
      near.moni <- near.moni()
      x <- paste0("Box plot for nearest govt monitor: \"", near.moni, "\", located ", dist, "km away. Box plot displays the minimum, 1st quartile, median, 3rd quartile, and maximum. A vertical line goes thorugh the box at the median. The whiskers go from each quartile to the minimum or maximum. Red stars indicate outliers.")
    })

    output$text.plots.adj <- renderText({"Temporal adjustment is done by multiplying AirBeam measurements to ratio of matching regulatory monitor hourly value and dividing by the average of monitor value of the entire sampling period."})
  
#####
#####

## Data
    
  output$choice.data <- renderUI({
     selectInput("selectdata", "Select data:",
     choices = c("All", unique(file0()$File_Num))
    )
  })
 
  choices <- reactiveValues(
     tab1 = c("1 min", "1 hr", "1 day", "Raw"),
     tab2 = c("1 min", "1 hr", "1 day"),
     tab3 = c("1 hr", "1 day", "1 min")
   )
      
  output$aggregate <- renderUI({
      dt <- file0()
      selectInput(inputId="agg",  "Aggregate data:", c(''))
  })
  
  output$daterange <- renderUI({
    df <- file.agg()
      dateRangeInput(inputId="dateRange", "Change date range:", 
        start = as.character(format(as.Date(min(df$date))),"yyyy-mm-dd"), 
        end   = as.character(format(as.Date(max(df$date))),"yyyy-mm-dd"))
   })
  
   output$analysis <- renderUI({
   radioButtons("mode", "",
                  c("Mobile" = "Mobile",
                    "Fixed" = "Fixed"))
   })
   
   file0 <- reactive({
     
     req(input$file1)
     dataf <- list()
     
     for(i in 1:length(input$file1[,1])){
       df <- fread(input$file1[[i,'datapath']])
       names <- as.character(df[4,])
       names <- gsub("(.*)-(.*)", "\\2", names)
       colnames(df) <- names
       df <- df[-(1:9),] 
       colnames(df)[1:5] <- c("ObjectID", "Session_Name", "Timestamp", "Latitude", "Longitude")
       df$File_Num <- paste0("#",i," ", df$Session_Name)
       df$FileN <- i
       df.pm <- dplyr::select(df, ObjectID, Session_Name, Timestamp, Latitude, Longitude, PM2.5, File_Num, FileN) 
       df.pm$PM25 <- as.numeric(df.pm$PM2.5)
       df.pm <- df.pm %>% dplyr::select(-PM2.5) %>% drop_na()
       
       dt1 <- data.table(df.pm)
       dt1$time <- as_datetime(as.character(dt1$Timestamp))
       dt1$year <- as.numeric(substring(dt1$time, 1, 4))
       dt1$date <- lubridate::date(dt1$Timestamp)
       dt1$month <- month(dt1$Timestamp)
       dt1$day <- day(dt1$Timestamp)
       dt1$hour <- as.numeric(substring(dt1$time, 12, 13))
       dt1$minu <- as.numeric(substring(dt1$time, 15, 16))
       dt <- data.table(dt1)
       dataf[[i]] <- dt
     }
     out <- rbindlist(dataf)
   })
  
   file.select  <- reactive({
   out <- file0()
   select <- input$selectdata
   
     if (select=="All") {
       out1 <- out
     } else if (select!="All") {
       out1 <- filter(out, File_Num==select)
     }
   
   return(out1)
   })
   
   mycsvs <-reactive({
     tmp <- lapply(input$file1$datapath, fread)
     names(tmp) <- input$file1$name
     tmp
   })
  
   output$count <- renderText(length(mycsvs()))
   
   length0 <- renderText({
     req(input$file1)
     leng <- length(input$file1[,1])
     return(leng)
  })

   length1 <- renderText({
     tt <- length0()
     return(tt)
   })
   
#####
#####
   
## Aggregation
   
  file.agg <- reactive({
    dt1 <- file.select()
    
    if (input$agg=="Raw") {
      dt.agg <- dt1 %>% mutate(time0= make_datetime(year, month, day, hour, minu))
    } else if (input$agg=="1 min") {
      dt.agg <- dt1 %>% group_by(Session_Name, File_Num, year, month, day, hour, minu) %>% dplyr::summarize(time=mean(time), PM25=mean(PM25), Latitude=mean(as.numeric(Latitude)), Longitude=mean(as.numeric(Longitude)), n=n()) %>% mutate(time0= make_datetime(year, month, day, hour, minu))
    } else if (input$agg=="1 hr") {
      dt.agg <- dt1 %>% group_by(Session_Name, File_Num, year, month, day, hour) %>% dplyr::summarize(time=mean(time), PM25=mean(PM25), Latitude=mean(as.numeric(Latitude)), Longitude=mean(as.numeric(Longitude)), n=n()) %>% mutate(time0=make_datetime(year, month, day, hour))
    } else if (input$agg=="1 day") {
      dt.agg <- dt1 %>% group_by(Session_Name, File_Num, date) %>% dplyr::summarize(time=mean(time), PM25=mean(PM25), Latitude=mean(as.numeric(Latitude)), Longitude=mean(as.numeric(Longitude)), n=n()) %>% mutate(time0=date)
    }
    
    dt.agg$date <- date(dt.agg$time0)
    dt2 <- data.table(dt.agg)
    dt2$session <- dt1$Session_Name[1]
    return(dt2)
  })  

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("output", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(file.out(), file, row.names = FALSE)
    }
  )
  
  output$downloadDataTime <- downloadHandler(
    filename = function() {
      paste("adjusted", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(merged.clean(), file, row.names = FALSE)
    }
  )

## Naming
  
  name <- reactive({
    session.name <- file.cal()
    session.name <- session.name$Session_Name[1]
  })  
  
  name1 <- reactive({
    select <- input$selectdata
    session.name <- file.cal()
      if (select=="All") {
        session.name <- "Multiple Sessions"
      } else if (select!="All") {
        session.name <- session.name$Session_Name[1]
      }
  })  
  
## Date Range
  
  file.cal <- reactive({
    new <- file.agg()
    dt3 <- filter(new, between(date, as.Date(input$dateRange[1]),
                 as.Date(input$dateRange[2])))
    return(dt3)
  })
  
#####
#####
  
## JSON
  
  aq.dt <- reactive({
    
    dt <- file.cal()
    File_Num <- unique(dt$File_Num)
    dt0 <- split(dt, dt$File_Num)
    
    empty <- list()
    for (i in 1:length(dt0)) {
    File_Num0 <- File_Num[i]
    dtt <- dt0[[i]]
    date1 <- lubridate::date(min(dtt$time))-1
    date2 <- lubridate::date(max(dtt$time))+1
    hour1 <- " 00:00:00"
    hour2 <- " 23:59:59"
    
    time1 <- as.numeric(as.POSIXct(paste0(date1,hour1), tz="UTC"))
    time2 <- as.numeric(as.POSIXct(paste0(date2,hour2), tz="UTC"))

    as.POSIXct(as.numeric(time1), origin="1970-01-01")
    as.POSIXct(as.numeric(time2), origin="1970-01-01")
    
    json.aq <- paste0("http://aircasting.org/api/fixed/active/sessions.json?q=%7B%22time_from%22%3A", time1, "%2C%22time_to%22%3A", time2, "%2C%22tags%22%3A%22%22%2C%22usernames%22%3A%22", "OpenAQ", "%22%2C%22west%22%3A-73.96594349649638%2C%22east%22%3A-76.9480263402769%2C%22south%22%3A30.70849431072799%2C%22north%22%3A50.713585287529995%2C%22limit%22%3A1000%2C%22offset%22%3A0%2C%22sensor_name%22%3A%22OpenAQ-pm2.5%22%2C%22measurement_type%22%3A%22Particulate%20Matter%22%2C%22unit_symbol%22%3A%22%C2%B5g%2Fm%C2%B3%22%7D")
    
    getdata.a <- function(x) {
      aq <- jsonlite::fromJSON(json.aq)
      return(aq)
    }
    
    start_time <- Sys.time()
    aq <- getdata.a(json.aq)
    end_time <- Sys.time()
    
    end_time - start_time
    title <- dplyr::select(aq$sessions, title, id)
    aq.ab <- aq$sessions$streams$'OpenAQ-PM2.5'
    
    aq.ab <- data.table(aq.ab) %>% dplyr::select(-average_value)
    aq.ab <- merge(aq.ab, title, by.x=c("session_id"), by.y=c("id"))
    aq.ab$File_Num <- File_Num0
    empty[[i]] <- aq.ab
    
    }
      aq.dt <- rbindlist(empty)
    })
  
#####
#####

## Calculate Nearest

  near.dist <- reactive({  
  
    ab.dt <- file.cal()
    aq.dt <- aq.dt()
    
    aq.dt <- data.table(aq.dt)
    ab.dt <- data.table(ab.dt)
    
    lat.mid <- mean(as.numeric(ab.dt$Latitude), na.rm=T)
    lon.mid <- mean(as.numeric(ab.dt$Longitude), na.rm=T)
    middle <- data.table(lat.mid, lon.mid)
    middle <- middle %>% st_as_sf(coords=c("lon.mid", "lat.mid"))
    
    aq.loc <- st_as_sf(aq.dt, coords=c("start_longitude", "start_latitude")) %>% drop_na()

    st_crs(aq.loc) <- 4326
    st_crs(middle) <- 4326
    
    near <- st_nearest_feature(middle, aq.loc)
    ls = st_length(st_nearest_points(middle, aq.loc[near,], pairwise = TRUE))
    ls <- as.numeric(ls)
  })
  
  near.buffer <- reactive({  
    
    ab.dt <- file.cal()
    aq.dt <- aq.dt()
    
    aq.dt <- data.table(aq.dt)
    ab.dt <- data.table(ab.dt)
    
    aq.loc <- st_as_sf(aq.dt, coords=c("start_longitude", "start_latitude")) %>% drop_na()
    
    lat.mid <- mean(as.numeric(ab.dt$Latitude), na.rm=T)
    lon.mid <- mean(as.numeric(ab.dt$Longitude), na.rm=T)
    middle <- data.table(lat.mid, lon.mid)
    middle <- middle %>% st_as_sf(coords=c("lon.mid", "lat.mid"))
    
    st_crs(aq.loc) <- 4326
    st_crs(middle) <- 4326

    buffer <- st_buffer(middle, 20000)
    aq.buffer <- st_intersection(buffer, aq.loc)
    
    near <- st_nearest_feature(middle, aq.loc)

    buffer.id <- aq.buffer$id
    title.id <- aq.buffer$title
    
    dt <- list()
    for (i in 1:length(buffer.id)) {
      sess1 <- paste0("http://aircasting.org/api/measurements.json/%3Fstream_ids%3D",buffer.id[i])
      ss1 <- jsonlite::fromJSON(sess1)
      s1 <- data.table(ss1)
      s1$title <- title.id[i]
      dt[[i]] <- data.table(s1[1])
    }
    
    df <- ldply(dt, data.frame)
    return(df)
  })
  
  near.multi <- reactive({  
    
    ab.dt <- file.cal()
    aq.dt <- aq.dt()
    
    aq.dt <- data.table(aq.dt)
    ab.dt <- data.table(ab.dt)
    
    dt0 <- split(ab.dt, ab.dt$File_Num)
    aq0 <- split(aq.dt, aq.dt$File_Num)
    
    empty <- list()
    
    File_Num <- unique(ab.dt$File_Num)
    
    filelength <- length(File_Num)
    
    for (i in 1:filelength) {

      File_Num0 <- File_Num[i]
      
      dt.loc <- dt0[[i]]
      aq.loc <- aq0[[i]]

      aq.loc <- st_as_sf(aq.dt, coords=c("start_longitude", "start_latitude")) %>% drop_na()
      lat.mid <- mean(as.numeric(dt.loc$Latitude), na.rm=T)
      lon.mid <- mean(as.numeric(dt.loc$Longitude), na.rm=T)
      middle <- data.table(lat.mid, lon.mid)
      middle <- middle %>% st_as_sf(coords=c("lon.mid", "lat.mid"))
      
      st_crs(aq.loc) <- 4326
      st_crs(middle) <- 4326
      
    near <- st_nearest_feature(middle, aq.loc)

    near.id <- aq.loc[near,]
    stream.id <- as.numeric(near.id$id)
    title <- near.id$title
    
    dx <- list()
    for (j in 1:1) {
      sess1 <- paste0("http://aircasting.org/api/measurements.json/%3Fstream_ids%3D",stream.id)
      ss1 <- jsonlite::fromJSON(sess1)
      s1 <- data.table(ss1)
      dx[[j]] <- data.table(s1)
    }
    
    df <- ldply(dx, data.frame)
    df$time <- as_datetime(df$time/1000)
    moni.dt <- df
    moni.dt$time <- as_datetime(moni.dt$time)
    moni.dt$year <- as.numeric(substring(moni.dt$time, 1, 4))
    moni.dt$minu <- as.numeric(substring(moni.dt$time, 15, 16))
    moni.dt$date <- substring(moni.dt$time, 6, 10)
    moni.dt$hour <- as.numeric(substring(moni.dt$time, 12, 13))
    moni.dt$date1 <- lubridate::date(moni.dt$time)
    moni.dt$File_Num <- File_Num0
    moni.dt$title <- title
    empty[[i]] <- moni.dt
    }
    
    moni.dt2 <- rbindlist(empty)
    return(moni.dt2)
  })
  
  near.moni <- reactive({
    near.moni <- near.multi()
    near.moni$title[1]
  })
  
  subset.multi <- reactive({
    dt <- file.cal()
    moni.dt <- near.multi()
    date4 <- lubridate::date(dt$time)
    dates <- c(date4)
    subset <- moni.dt %>% filter(date1 %in% dates)
    return(subset)
  })
  
  merged.multi <- reactive({
    dt <- file.cal()
    subset <- near.multi()
    subset <- filter(subset, value>=0)
    dt$year <- year(dt$time)
    dt$date <- date(dt$time)
    
    if (input$agg=="1 day") {
      subset.day <- subset %>% group_by(title, File_Num, date1) %>% dplyr::summarize(time.moni=mean(time), value=mean(value), Latitude.moni=mean(as.numeric(latitude)), Longitude.moni=mean(as.numeric(longitude))) 
      mean.pm <- mean(subset.day$value, na.rm=T)
      merged.dt <- merge(dt, subset.day, by.x=c("File_Num", "date"), by.y=c("File_Num", "date1"), all.x=T,)
      merged.dt$avg.pm <- mean.pm
      merged.dt$ratio <- merged.dt$value/merged.dt$avg.pm
      merged.dt$adj.pm <- merged.dt$PM25/merged.dt$ratio
      
    } else if (input$agg!="1 day") {
      subset$hour1 <- subset$hour-1
      subset.day <- subset %>% group_by(title, File_Num, date1, hour1, year) %>% dplyr::summarize(time.moni=mean(time), value=mean(value), Latitude.moni=mean(as.numeric(latitude)), Longitude.moni=mean(as.numeric(longitude))) 
      mean.pm <- mean(subset.day$value, na.rm=T)
      setDT(subset.day)
      merged.dt <- merge(dt, subset.day, by.x=c("File_Num", "date", "hour", "year"), by.y=c("File_Num", "date1", "hour1", "year"), all.x=T,)
      merged.dt$avg.pm <- mean.pm
      merged.dt$ratio <- merged.dt$value/merged.dt$avg.pm
      merged.dt$adj.pm <- merged.dt$PM25/merged.dt$ratio
    }
    
    return(merged.dt)
  }) 
  
  merged.clean <- reactive({
    merged <- merged.multi()
    merged <- dplyr::rename(merged, Time=time, Govt_Monitor=title, Time_Monitor=time.moni, Latitude_Monitor=Latitude.moni, Longitude_Monitor=Longitude.moni, PM25_Monitor=value, PM25_Adjusted=adj.pm, Ratio=ratio)
    merged <- merged %>% select(Session_Name, PM25, Latitude, Longitude, Time, Govt_Monitor, Time_Monitor, PM25_Monitor, Latitude_Monitor, Longitude_Monitor, PM25_Adjusted, Ratio)
  })
  
  points0 <- reactive({
    ab.dt <- file.cal()
    return(ab.dt)
  })
  
  points0.fix <- reactive({
    ab.dt <- file.cal()
    req(input$slide)
    number <- as.character(input$slide[2])
    number <- ymd_hms(number)
    filter <- ab.dt %>% dplyr::filter(time==number)
    return(filter)
  })
  
  points0.mob <- reactive({
    ab.dt <- file.cal()
    number <- input$slide
    filter <- ab.dt %>% filter(time<=number)
    return(filter)
  })
  
  years <- reactive({
    dt.cal <- file.cal()
    years <- unique(year(dt.cal$date))
  })
  
#####
#####

## Calendar
  
  output$calendar <- renderPlot({
    
    dt.cal <- file.cal()
    dt.cal <- dt.cal %>% dplyr::group_by(date) %>% dplyr::summarize(pm25=mean(PM25, na.rm=T))
    my_data <- dt.cal$pm25
    
    from.date <- dt.cal$date[1]
    to.date <- dt.cal$date[nrow(dt.cal)]
    day(from.date) <- 1
    to.date <- ceiling_date(to.date, unit="month")-days(1)
    
    date.length <- as.numeric(to.date-from.date)
    dates <- data.table(seq(as.Date(from.date), as.Date(to.date), 'day'))
  
    dates2 <- merge(dates, dt.cal, by.x=c("V1"), by.y=c("date"),  all.x=T)
    dates2[is.na(dates2$pm25)] <- 0
    
    days <- rep(min(my_data)-.02, date.length+1)
    days[] <- dates2$pm25
    
    calendR(#year = 2021,
            start_date = from.date,  # Start date
            end_date = to.date,
            special.days = days,
            gradient = TRUE,   
            special.col = rgb(1, 0, 0, alpha = 0.6), 
            low.col = "white") 
    
  })
  
  output$choice.cal <- renderUI({
    selectInput("cal", "Select Visualization Type:",
                choices = c("Relative Intensity", "Air Quality Index"))
  })
  
  output$years <- renderUI({
    selectInput("selectyear", "Select Year:",
                choices = years())
  })

  output$choice.mon <- renderUI({
    selectInput("selectmonth", "Select Month:",
                choices = c("All", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
  })
  
  output$calendar1 <- renderPlot({
    
    dt.cal <- file.cal()
    yr <- input$selectyear
    month <- input$selectmonth
    
    if (month=="All") {
      month1 <- 1:12
    } else if (month!="All") {
      month1 <- month
    }
    
    month1 <- as.numeric(month1)
    
    if (input$cal=="Heatmap") { 
      calendarPlot(selectByDate(dt.cal, month=month1, year=yr), pollutant="PM25", year=yr) 
    } else if (input$cal=="Relative Intensity") {
      calendarPlot(selectByDate(dt.cal, month=month1, year=yr), pollutant="PM25", year=yr, annotate = "value", 
                   lim = 50, 
                   cols = brewer.pal(8, "Blues") ,
                   col.lim = c("black", "orange"),
                   cex.lim=c(1,2)
           )   
    } else if (input$cal=="Air Quality Index") {
      calendarPlot(selectByDate(dt.cal, month=month1, year=yr), pollutant="PM25", year=yr, annotate = "value", 
                   breaks = c(0, 12, 35, 55, 150, 250, 100000),
                   labels = c("Good (0-12)", "Moderate (12-35)", "Unhealthy for Sensitive Groups (35-55)", "Unhealthy (55-150)", "Very Unhealthy (150-250)", "Hazardous(250+)"),
                   cols = c("#abd162", "#f7d460", "#fc9956", "#f5676d","#a37db8","#a07684"),
                   cex.lim=c(1,2)
      )   
    }
  })

#####
#####

## Leaflets
  
    output$leaf0 <- renderLeaflet({
    ab.dt <- file.cal()
    ab.loc <- st_as_sf(ab.dt, coords=c("Longitude", "Latitude")) 
    st_crs(ab.loc) <- 4326
    pal <- inferno
    mapview(ab.loc, zcol="PM25", layer.name=c("PM2.5"), col.regions=pal, alpha=0, lwd=0)@map
  })
  
  output$leaf1 <- renderLeaflet({
    ab.dt <- file.cal()
    dt0 <- split(ab.dt, dt$File_Num)
    for (i in 1:length(dt0)) {
      File_Num0 <- File_Num[i]
      File <- File_Num0[1]
    }
    
    ab.loc <- st_as_sf(ab.dt, coords=c("Longitude", "Latitude")) 
    st_crs(ab.loc) <- 4326
    pal <- inferno
    mapview(ab.loc, zcol="PM25", layer.name=c("PM2.5"), col.regions=pal, alpha=0, lwd=0)@map
  })
  
  output$leaf.mob <- renderLeaflet({
    ab.dt <- points0.fix()
    ab.loc <- st_as_sf(ab.dt, coords=c("Longitude", "Latitude")) 
    st_crs(ab.loc) <- 4326
    pal <- inferno
    mapview(ab.loc, zcol="PM25", layer.name=c("PM2.5"), col.regions=pal, alpha=0, lwd=0)@map
  })
  

   output$nearest <- renderLeaflet({
     moni.dt <- near.multi()
     data.near <- data.table(unique(moni.dt$title), unique(moni.dt$longitude), unique(moni.dt$latitude))
     colnames(data.near) <- c("Monitor Name", "longitude", "latitude")
     aq.loc <- st_as_sf(data.near, coords=c("longitude", "latitude"))
     st_crs(aq.loc) <- 4326
     mapview(aq.loc, layer.name=c("Nearest Monitor(s)"), alpha=0)@map
   })
  
   output$nearest.multi <- renderLeaflet({
     moni.dt <- near.multi()
     data.near <- data.table(unique(moni.dt$title), unique(moni.dt$longitude), unique(moni.dt$latitude))
     colnames(data.near) <- c("Monitor Name", "longitude", "latitude")
     aq.loc <- st_as_sf(data.near, coords=c("longitude", "latitude"))
     st_crs(aq.loc) <- 4326
     mapview(aq.loc, layer.name=c("Nearest Monitor(s)"), alpha=0)@map
   })
   
   output$nearest.buffer <- renderLeaflet({
     moni.dt <- near.buffer()
     data.near <- data.table(unique(moni.dt$title), unique(moni.dt$longitude), unique(moni.dt$latitude))
     colnames(data.near) <- c("Monitor Name", "longitude", "latitude")
     aq.loc <- st_as_sf(data.near, coords=c("longitude", "latitude"))
     st_crs(aq.loc) <- 4326
     mapview(aq.loc, layer.name=c("Nearest Monitor(s)"), alpha=0)@map
   })
   
##### 
#####
 
## Summaries
  
  file.out <- reactive({
     x <- file.cal()
     x <- select(x, Session_Name, PM25, Latitude, Longitude, time)
     x$Latitude <- round(as.numeric(x$Latitude), 4)
     x$Longitude <- round(as.numeric(x$Longitude), 4)
     x$PM25 <- round(as.numeric(x$PM25), 4)
     x$time <- as.character(x$time)
     x <- dplyr::rename(x, Time=time)
     x <- dplyr::rename(x, PM2.5=PM25)
     return(x)
   })
   
   output$table1 <- DT::renderDataTable({
     x <- file.out()
     DT::datatable(x, options=list(searching=F, ordering=F, pageLength=10, scrollY = "500px"))
   })
   
   output$summary.ab <- DT::renderDataTable({
     summary <- file.cal()
     x <- data.frame(matrix(summary(summary$PM25)))
     sd <- sd(summary$PM25, na.rm=T)
     xx <- rbind(round(x,3), round(sd,3))
     names <- c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum", "St.Dev")
     x <- data.frame(names, xx)
     x <- DT::datatable(x, colnames = c("AirBeam Summary Statistics", "PM<sub>2.5</sub> Value"), escape=FALSE, options=list(searching=F, ordering=F, lengthChange = FALSE, info = FALSE,dom='t'))
   })
   
   output$summary.aq <- renderDataTable({
     summary1 <- merged.multi()
     x <- data.frame(matrix(summary(summary1$value)))[1:6,]
     na <- sum(!complete.cases(summary1$value))
     sd <- sd(summary1$value, na.rm=T)
     xx <- c(round(x,3), round(sd,3),  round(na,3))
     names <- c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum",  "St.Dev", "N Missing")
     x <- data.frame(names, xx)
     x <- datatable(x, colnames = c("Govt Monitor Summary Statistics", "PM<sub>2.5</sub> Value"), escape=FALSE, options=list(searching=F, ordering=F,lengthChange = FALSE, info=FALSE, dom='t'))
   })
   
  output$near.dist.text <- renderText({
    dist <- near.dist()/1000
    dist <- round(dist, 3)
    session <- name()
    near.moni <- near.moni()
    x <- paste0("Session name \"", session, "\", nearest govt monitor \"", near.moni, "\", is located ", dist, "km away.")
  })
  
  output$near.dist.text.multi <- renderText({
    dist <- near.dist()/1000
    dist <- round(dist, 3)
    session <- name1()
    near.moni <- near.moni()
    x <- paste0("Session name \"", session, "\", nearest govt monitor \"", near.moni, "\", is located ", dist, "km away.")
  })
  
  output$ab.summary <- renderPrint({
    t <- expression(paste("Summary statistics for AirBeam ", PM[2.5]))
    quoted=T
  })
  
  output$moni.summary <- renderPrint({
    tt <- expression(paste("Summary statistics for Monitor ", PM[2.5]))
    quoated=T
  })
  
  ##
  
  output$numberslider <- renderUI({
    
    df <- file.cal()
    sliderTextInput("slide", "Timelapse", 
                    choices = unique(df$time),
                    from_fixed = min(df$time),
                    to_max = max(df$time), 
                    grid=F,
                    animate=T)
  })

  
#####
#####

## Compare Plots
  
  output$plot.aq = renderPlot({
    data.plot <-  merged.multi()
    data.plot$time1 <- as_datetime(data.plot$time)
    ggplot(data=data.plot, aes(x=time1, y=value), color="#00b2ef") + geom_point() + geom_line() + xlab("Time") + ylab(bquote("Avg"~PM[2.5])) + theme_minimal() + theme(legend.title = element_blank()) + ggtitle("Nearest Govt Reference Monitor Values") + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))   
  })
  
  output$plot.aq.multi = renderPlot({
    data.plot <-  merged.multi()
    data.plot$time1 <- as_datetime(data.plot$time)
    ggplot(data=data.plot, aes(x=time1, y=value), color="#00b2ef") + geom_point() + geom_line() + xlab("Time") + ylab(bquote("Avg"~PM[2.5])) + theme_minimal() + theme(legend.title = element_blank()) + ggtitle("Nearest Govt Reference Monitors Values") + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))   
  })
  
  output$plot.ab = renderPlot({
    dt <- file.cal()
    name <- name()
    dt$time1 <- as_datetime(dt$time)
    title <- paste0("AirBeam Values for Session: \"", name, "\"")
    ggplot(data=dt, aes(x=time1, y=PM25), color="#00b2ef") + geom_point() + geom_line() + xlab("Time") + ylab(bquote("Avg"~PM[2.5])) + theme_minimal() + theme(legend.title = element_blank()) + ggtitle(title) + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15)) 
  })
  
  output$plot.ab.multi = renderPlot({
    dt <- file.cal()
    name <- name1()
    dt$time1 <- as_datetime(dt$time)
    title <- paste0("AirBeam Values for Session: \"", name, "\"")
    ggplot(data=dt, aes(x=time1, y=PM25), color="#00b2ef") + geom_point() + geom_line() + xlab("Time") + ylab(bquote("Avg"~PM[2.5])) + theme_minimal() + theme(legend.title = element_blank()) + ggtitle(title) + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))   
  })
  
  output$plot.ab.adj = renderPlot({
    dt.merge <- merged.multi()
    dt.merge$time1 <- as_datetime(dt.merge$time)
    ggplot(data=dt.merge, aes(x=time1, y=adj.pm), color="#00b2ef") + geom_point() + geom_line() + xlab("Time") + ylab(bquote("Avg"~PM[2.5])) + theme_minimal() + theme(legend.title = element_blank()) + ggtitle("Temporally Adjusted AirBeam Values") + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))   
  })
  
  output$plot.ab.adj.multi = renderPlot({
    dt.merge <- merged.multi()
    dt.merge$time1 <- as_datetime(dt.merge$time)
    ggplot(data=dt.merge, aes(x=time1, y=adj.pm),  color="#00b2ef") + geom_point() + geom_line() + xlab("Time") + ylab(bquote("Avg"~PM[2.5])) + theme_minimal() + theme(legend.title = element_blank()) + ggtitle("Temporally Adjusted AirBeam Values") + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))   
  })
  
## AirBeam Plots
  
  output$plot.den = renderPlot({
    dt.den <- file.cal()
    name <- name()
    dt.den$time1 <- as_datetime(dt.den$time)
    title <- paste0("Density Plot for AirBeam Values, Session: \"", name, "\"")    
    ggplot(data=dt.den, aes(x=PM25)) + theme_minimal() + geom_density(color="darkblue", fill="lightblue", adjust=0.9) + xlab(bquote("Avg"~PM[2.5])) + ylab("Density") + ggtitle(title) + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))   
  })
  
  output$plot.box = renderPlot({
    box.plot <-  file.cal()
    name <- name()
    box.plot$time1 <- as_datetime(box.plot$time)
    title <- paste0("Boxplot for AirBeam Values, Session: \"", name, "\"")    
    ggplot(data=box.plot, aes(x=PM25)) + geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                       outlier.size=4) + xlab(bquote("Avg"~PM[2.5])) + ylab("") + theme_minimal() + theme(legend.title = element_blank()) + ggtitle(title) + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))   
  })

  output$plot.den.multi = renderPlot({
    dt.den <- file.cal()
    name <- name1()
    dt.den$time1 <- as_datetime(dt.den$time)
    title <- paste0("Density Plot for AirBeam Values, Session: \"", name, "\"")    
    ggplot(data=dt.den, aes(x=PM25)) + theme_minimal() + geom_density(color="darkblue", fill="lightblue", adjust=0.9) + xlab(bquote("Avg"~PM[2.5])) + ylab("Density") + ggtitle(title) + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))   
  })
  
  output$plot.box.multi = renderPlot({
    box.plot <-  file.cal()
    name <- name1()
    box.plot$time1 <- as_datetime(box.plot$time)
    title <- paste0("Boxplot for AirBeam Values, Session: \"", name, "\"")    
    ggplot(data=box.plot, aes(x=PM25)) + geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                       outlier.size=4) + xlab(bquote("Avg"~PM[2.5])) + ylab("") + theme_minimal() + theme(legend.title = element_blank()) + ggtitle(title) + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))   
  })
  
  ## AQ Plots

  output$plot.den.aq = renderPlot({
    dt.den <- merged.multi()
    name <- name()
    dt.den$time1 <- as_datetime(dt.den$time)
    title <- paste0("Density Plot for Nearest Govt Monitor Values, for AirBeam Session: \"", name, "\"")    
    ggplot(data=dt.den, aes(x=value)) + theme_minimal() + geom_density(color="darkblue", fill="lightblue", adjust=0.9) + xlab(bquote("Avg"~PM[2.5])) + ylab("Density") + ggtitle(title) + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))   
  }) 
  
  output$plot.box.aq = renderPlot({
    box.plot <- merged.multi()
    name <- name()
    title <- paste0("Boxplot for Nearest Govt Monitor Values, for AirBeam Session: \"", name, "\"")    
    ggplot(data=box.plot, aes(x=value)) + geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                       outlier.size=4) + xlab(bquote("Avg"~PM[2.5])) + ylab("") + theme_minimal() + theme(legend.title = element_blank()) + ggtitle(title) + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))      
  })
  
  output$plot.den.aq.multi = renderPlot({
    dt.den <- merged.multi()
    name <- name1()
    dt.den$time1 <- as_datetime(dt.den$time)
    title <- paste0("Density Plot for Nearest Govt Monitor Values, for AirBeam Session: \"", name, "\"")    
    ggplot(data=dt.den, aes(x=value)) + theme_minimal() + geom_density(color="darkblue", fill="lightblue", adjust=0.9) + xlab(bquote("Avg"~PM[2.5])) + ylab("Density") + ggtitle(title) + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))   
  }) 
  
  output$plot.box.aq.multi = renderPlot({
    box.plot <- merged.multi()
    name <- name1()
    title <- paste0("Boxplot for Nearest Govt Reference Values, for AirBeam Session: \"", name, "\"")    
    ggplot(data=box.plot, aes(x=value)) + geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                        outlier.size=4) + xlab(bquote("Avg"~PM[2.5])) + ylab("") + theme_minimal() + theme(legend.title = element_blank()) + ggtitle(title) + theme(axis.text = element_text(size = 13)) + theme(text = element_text(size = 15))      
  })
  
  #####
  #####
  
  observeEvent(input$tabselected, {
    updateSelectInput(session, 'agg', choices = choices[[input$tabselected]], selected=)
  })

  }

# Run the app ----
shinyApp(ui, server)

