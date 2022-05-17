#' "My Breast Friend": nursing and diaper monitoring app
#'
#' @return Launches a `Shiny` app that allows you to log new breastfeeding times and
#' dirty diapers and review your childcare history. Impress your pediatrician with solid data on
#' your baby's in's and out's! The app will look for a `data.csv` file within your
#' working directory, to which it will save new events and from which it will compile
#' user-history visualizations. If this file is not found, it will be created.
#'
#' @export
#'
#' @import shiny
#' @import shinyWidgets
#' @import DT
#' @import dplyr
#' @import ggplot2
#' @import lubridate
#'
breastfeeding <- function(){

if(FALSE){
  library(shiny)
  library(shinyWidgets)
  library(lubridate)
  library(DT)
  library(dplyr)
  library(ggplot2)
}

if(! file.exists('data.csv')){
  write.csv(data.frame(datetime=as.character(Sys.time()),event= 'None'), file='data.csv', quote=FALSE, row.names=FALSE)
}

(mr_df <- read.csv('data.csv', stringsAsFactors=FALSE))


ui <- fluidPage(
  tags$style(type = "text/css",
             "label { font-size: 40px; }"
  ),
  tags$style(HTML(".radio {margin-right: 42px;}")),
  headerPanel('My breast friend: lactation monitoring'),
  sidebarPanel(
    shinyWidgets::prettyRadioButtons("status",
                                     label = NULL,
                selected ='None',
                choices = c("Left side",
                "None",
                "Right side"),
                thick = TRUE,
                bigger = TRUE),
    br(),
    br(),
    actionButton('peepee', h4('Wet diaper'), width='95%'),
    br(), br(),
    actionButton('poopoo', h4('Dirty diaper'), width='95%'),
    br(), br(), br(),
    textOutput('last'),
    br(), br(),
    sliderInput('lookback',h4('Summarize patterns for past ...'), min=1, max=14, step=1, value=14,post=' days'),
    br(),
    textOutput('wet_diapers'),
    br(),
    textOutput('dirty_diapers'),
    br(),
    plotOutput('interval_hist'),
    br(),
    plotOutput('duration_hist'),
    br()
  ),
  mainPanel(
    plotOutput('plot1'),
    br(),
    hr(),
    br(),
    plotOutput('plot2'),
    br(),
    hr(),
    br(),
    DTOutput('tbl')
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues()
  rv$df <- mr_df
  rv$mr <- data.frame()

  observeEvent(input$status,{
    (ev <- paste0(as.character(Sys.time()),',',input$status,'\n'))
    isolate({
      cat(ev, file='data.csv', append=TRUE)
      rv$df <- read.csv('data.csv',stringsAsFactors=FALSE)
      #print(rv$df)
    })
  })

  observeEvent(input$peepee,{
    isolate({
      (ev <- paste0(as.character(Sys.time()),',Pee-pee diaper\n'))
      cat(ev, file='data.csv', append=TRUE)
      (ev <- paste0(as.character(Sys.time()),',None\n'))
      cat(ev, file='data.csv', append=TRUE)
      rv$df <- read.csv('data.csv',stringsAsFactors=FALSE)
    })
  })

  observeEvent(input$poopoo,{
    isolate({
      (ev <- paste0(as.character(Sys.time()),',Poo-poo diaper\n'))
      cat(ev, file='data.csv', append=TRUE)
      (ev <- paste0(as.character(Sys.time()),',None\n'))
      cat(ev, file='data.csv', append=TRUE)
      rv$df <- read.csv('data.csv',stringsAsFactors=FALSE)
    })
  })

  observeEvent(rv$df,{
    df <- rv$df
    #df <- mr_df

    mr <- data.frame()
    status <- 'None'
    (dt_start <- df$datetime[1] %>% lubridate::as_datetime(tz='America/Chicago'))
    i=2
    for(i in 2:nrow(df)){
      (dfi <- df[i,])
      stati <- dfi$event
      if(stati != status){
        (dt_end <- dfi$datetime[1] %>% lubridate::as_datetime(tz='America/Chicago'))
        (dur <- difftime(dt_end, dt_start, units='mins') %>% as.numeric)
        dfi <- data.frame(event = status, dt = dt_start, duration = dur)
        status <- stati
        dt_start <- dt_end
        mr <- rbind(mr, dfi)
      }
    }
    mr

    mr <-
      mr %>%
      filter(event != 'None') %>%
      group_by(event) %>%
      mutate(tot = cumsum(duration))

    diapers <- grep('diaper',mr$event)
    mr$duration[diapers] <- NA
    mr$tot[diapers] <- NA

    rv$mr <- mr
    print(rv$mr)
  })

  output$plot1 <- renderPlot({
    mr <- rv$mr %>% dplyr::filter(event %in% c('Left side','Right side'),
                                  duration > 1)
    mr
    ggplot(mr, aes(x=dt, y=tot, color=event)) +
      geom_point() +
      geom_line(lwd=2, alpha=.6) +
      ylab('Cumulative duration') +
      xlab(NULL) +
      #scale_x_datetime(date_breaks = '24 hours',
      #                 date_labels = '%m-%d %H:%M') +
      labs(title = 'Cumulative nursing time on each side', color = 'Side:') +
      theme(text = element_text(size = 20))
  })

  output$plot2 <- renderPlot({
    (diapers <- rv$mr[grep('diaper',rv$mr$event),] %>% rename(Diaper = event))
    print(diapers)

    mr <- rv$mr %>% dplyr::filter(event %in% c('Left side','Right side'),
                                  duration > 1)
    mr

    ggplot(mr, aes(x=dt, y=duration, color=event)) +
      geom_point() +
      geom_line(lwd=2, alpha=.6) +
      ylab('Minutes per feed') +
      xlab(NULL) +
      scale_y_continuous(limits=c(0, max(mr$duration))) +
      #scale_x_datetime(date_breaks = '24 hours',
      #                 date_labels = '%m-%d %H:%M') +
      labs(title = 'Time nursing on each side for each session', color = 'Side:') +
      theme(text = element_text(size = 20)) +
      geom_vline(data=diapers, mapping=aes(xintercept=dt, lty = Diaper))
  })

  output$last <- renderText({
    paste0('Most recent feed: ',
           rv$mr$event[nrow(rv$mr)],
           ' ',
           as.character(rv$mr$dt[nrow(rv$mr)]))
  })

  output$wet_diapers <- renderText({
    mr <- rv$mr
    mri <- mr %>%
      dplyr::filter(dt >= (lubridate::as_datetime(Sys.time()) - lubridate::days(abs(as.numeric(input$lookback))))) %>%
      dplyr::filter(event == 'Pee-pee diaper')

    print(input$lookback)

    #mri <-
    #  mr %>%
    #  dplyr::filter(dt >= (lubridate::as_datetime(Sys.time()) - lubridate::days(2))) %>%
    #  dplyr::filter(event == 'Pee-pee diaper')

    paste0('# wet diapers: ', nrow(mri))
  })

  output$dirty_diapers <- renderText({
    mr <- rv$mr
    mri <- mr %>%
      dplyr::filter(dt >= (lubridate::as_datetime(Sys.time()) - lubridate::days(abs(as.numeric(input$lookback))))) %>%
      dplyr::filter(event == 'Poo-poo diaper')

    #mri <-
    #  mr %>%
    #  dplyr::filter(dt >= (lubridate::as_datetime(Sys.time()) - lubridate::days(2))) %>%
    #  dplyr::filter(event == 'Poo-poo diaper')

    paste0('# dirty diapers: ', nrow(mri))
  })

  output$duration_hist <- renderPlot({
    mr <- rv$mr
    mri <- mr %>%
      dplyr::filter(dt >= (lubridate::as_datetime(Sys.time()) - lubridate::days(input$lookback))) %>%
      dplyr::filter(event %in% c('Left side', 'Right side'),
                    duration > 1)

    if(nrow(mri)>0){
      hist(mri$duration, xlab='Minutes', main='Nursing session duraiton (per side)', col='grey60', border='grey85')
    }
  })

  output$interval_hist <- renderPlot({
    mr <- rv$mr
    mri <- mr %>%
      dplyr::filter(dt >= (lubridate::as_datetime(Sys.time()) - lubridate::days(input$lookback))) %>%
      dplyr::filter(event %in% c('Left side', 'Right side'),
                    duration > 1)

    #mri <-
    #  mr %>%
    #  dplyr::filter(dt >= (lubridate::as_datetime(Sys.time()) - lubridate::days(2))) %>%
    #  dplyr::filter(event %in% c('Left side', 'Right side'))

    if(nrow(mri)>0){
      mrt <- data.frame(t1 = mri$dt[1:(nrow(mri)-1)],
                        t2 = mri$dt[2:(nrow(mri))])
      mrt$diff <- difftime(mrt$t2, mrt$t1, units='hours') %>% as.numeric %>% round(1)
      mrt
      mrt <- mrt %>% dplyr::filter(diff > 0.5)

      hist(mrt$diff, xlab='Hours', main='Hours between nursing', col='grey60', border='grey85')
    }
  })

  output$tbl = renderDT(
    {rv$mr %>%
        arrange(desc(dt)) %>%
        rename(Event = event) %>%
        mutate(Date = paste0(lubridate::month(dt),'-', lubridate::day(dt)),
               Time = paste0(lubridate::hour(dt),':', lubridate::minute(dt)),
               Minutes = round(duration, 1),
               Cumulative = round(tot)) %>%
        select(Date, Time, Event, Minutes, Cumulative)
        },
    options = list(lengthChange = FALSE,
                   bFilter=0)
  )

}

shinyApp(ui, server)

}
