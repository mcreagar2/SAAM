#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

###############################################################################
##                               WELCOME TO SAAM                             ##
##     YOUR PERSONAL DATABASE ACCESS TO SONGS, ARTISTS, ALBUMS, AND MORE     ##
##                               ENJOY YOUR STAY!                            ##
###############################################################################

## WARNING: This will check for installed packages and install if they are not.
## If you want to read over the documentation on each package before installation,
## see "stat850-report.qmd".

requiredPackages <- c('shiny','shinythemes','tidyverse','patchwork','DT', 'highcharter')
for(package in requiredPackages){
  if(package %in% rownames(installed.packages()) == FALSE) {install.packages(package)}
}




library(shiny)
library(shinythemes)
library(tidyverse)
library(patchwork)
library(DT)
library(highcharter)

cleaned <- readr::read_csv("data/cleaned.csv")
cleaned_artist <- readr::read_csv("data/cleaned_artist.csv")
cleaned_album <- readr::read_csv("data/cleaned_album.csv")
db_misc <- readr::read_csv("data/db_misc.csv", guess_max = 1800)


### UI ###
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  navbarPage(
    # Application title
    title = "SAAM",
    
    # Main Tabs (S,A,A,M)
    # Songs Main Tab
    tabPanel(
      "Songs",
      # Individual Song Tabs
      tabsetPanel(
        id = 'songs',
        type = "tabs",
        
        # By Month Tab
        tabPanel(
          "By Month",
          
          sidebarPanel(
            selectInput("month_choice_songs",
                        "Choose Month:",
                        choices = sort(unique(
                          as.Date(cleaned$dates, format = "%B %d %Y")
                        ))),
            
            sliderInput(
              "topHrsMonth",
              "Hours",
              min = 0,
              max = 200,
              value = c(0, 200)
            ),
            sliderInput(
              "topPlayedMonth",
              "Plays",
              min = 0,
              max = 2500,
              value = c(0, 2500)
            ),
            selectInput(
              "secondarySortingFilterMonth",
              "Sort By:",
              choices = c(
                "-",
                "Most Time Improved",
                "Most Play Improved",
                "Most Rank Improved",
                "Most Value Improved"
              ),
              selected = "-"
            ),
            checkboxGroupInput(
              "tertiarySortingFilterMonth",
              "Include:",
              choices = c(
                "Artist",
                "Album"
              )
            )
            ,actionButton('jumpToIndivSongM', "Song Information")
          ),
          mainPanel(dataTableOutput("monthSong"))
        ),
        
        # Individiual Song Tab
        tabPanel(
          "Individual Song Data",
          
          sidebarPanel(
            selectizeInput(
              "song_name",
              "Song Name",
              choices = sort(unique(cleaned$SongName)),
              selected = "Instant Crush (feat. Julian Casablancas)"
            )
          ),
          
          mainPanel(
            tableOutput("song_info"),
            tableOutput("song_big4"),
            tableOutput("song_max_info"),
            tableOutput("bourgeoisie_song_info"),
            column(width = 6, highchartOutput("songBig4Plot1"), highchartOutput("songBig4Plot3")),
            column(width = 6, highchartOutput("songBig4Plot2"), highchartOutput("songBig4Plot4")),
            highchartOutput("dateTimeListenNC")
          )
        ),
        
        #Overall Song Tab
        tabPanel(
          "Overall Song Data",
          
          sidebarPanel(
            sliderInput(
              "topHrs",
              "Hours",
              min = 0,
              max = 200,
              value = c(0, 200)
            ),
            sliderInput(
              "topPlayed",
              "Plays",
              min = 0,
              max = 2500,
              value = c(0, 2500)
            ),
            dateRangeInput(
              "song_date_range",
              "Show Songs Added Between:",
              start = "2013-08-01",
              end = "2022-09-01",
              min = "2013-08-01",
              max = NULL,
              format = "yyyy-mm-dd",
              startview = "month",
              weekstart = 0,
              language = "en",
              separator = " to "
            ),
            selectInput(
              "secondarySortingFilter",
              "Sort By:",
              choices = c(
                "-",
                "Most Time Improved",
                "Most Play Improved",
                "Most Rank Improved",
                "Most Value Improved"
              ),
              selected = "-"
            ),
           
            checkboxGroupInput(
              "tertiarySortingFilter",
              "Include:",
              choices = c(
                "Artist",
                "Album"
              )
            ),
            actionButton('jumpToIndivSongO', "Song Information")
          ),
          
          mainPanel(dataTableOutput("overallSong"))
        )
      )
      
      
    ),
    
    
    # Main Artists Tab
    tabPanel("Artists",
             
             tabsetPanel(
               id= 'artists',
               type = "tabs",
               
               # Overall Artist Tab
               tabPanel(
                 "Overall Artist Data",
                 
                 sidebarPanel(
                   sliderInput(
                     "topArtistHrs",
                     "Hours",
                     min = 0,
                     max = 2000,
                     value = c(0, 2000)
                   ),
                   sliderInput(
                     "topArtistPlayed",
                     "Plays",
                     min = 0,
                     max = 25000,
                     value = c(0, 25000)
                   ),
                   dateRangeInput(
                     "artist_date_range",
                     "Show Artists Added Between:",
                     start = "2013-08-01",
                     end = "2022-09-01",
                     min = "2013-08-01",
                     max = NULL,
                     format = "yyyy-mm-dd",
                     startview = "month",
                     weekstart = 0,
                     language = "en",
                     separator = " to "
                   ),
                   actionButton('jumpToIndivArtist', "Artist Information")
                   
                 ),
                 mainPanel(dataTableOutput("overallArtist"))
                 
               ),
               
               # Individual Artist Tab
               tabPanel(
                 "Individual Artist Data",
                 
                 sidebarPanel(
                   selectizeInput(
                     "artist_name",
                     "Artist",
                     choices = sort(unique(cleaned$Artist)),
                     selected = "Daft Punk"
                   )
                 ),
                 
                 mainPanel(
                   tableOutput("overall_artist_info"),
                   tableOutput("artist_song_info"),
                   column(width = 6, highchartOutput("artistBig4Plot1"), highchartOutput("artistBig4Plot3")),
                   column(width = 6, highchartOutput("artistBig4Plot2"), highchartOutput("artistBig4Plot4")),
                   highchartOutput("dateTimeListenNCA")
                 )
               )
             )),
    
    
    # Main Albums Tab
    # Also the only Albumns tab: There are no subtabs.
    tabPanel("Albums",
             tabsetPanel(
               id = 'albums',
               type = "tabs",
               tabPanel(
                 "Individual Album Data",
                 sidebarPanel(
                   selectizeInput(
                     "album_name",
                     "Album",
                     choices = sort(unique(cleaned$Album)),
                     selected = "21st Century Breakdown"
                   )
                 ),
                 
                 mainPanel(
                   tableOutput("overall_album_info"),
                   tableOutput("album_info"),
                   column(width = 6, highchartOutput("albumBig4Plot1"), highchartOutput("albumBig4Plot3")),
                   column(width = 6, highchartOutput("albumBig4Plot2"), highchartOutput("albumBig4Plot4")),
                   highchartOutput("dateTimeListenNCAl")
                 )
               ))),
    
    # Main Miscellaneous Tab
    tabPanel("Miscellaneous",
             
             tabsetPanel(type = "tabs",
                         
                         # Bour. By Month Tab
                         tabPanel(
                           "Bourgeoisie by Month",
                           sidebarPanel(
                             selectizeInput(
                               "month_name_bourgeoisie",
                               "Month",
                               choices = unique(colnames(db_misc)[5:ncol(db_misc)-1]),
                               selected = "January 2016"
                             ),
                           ),
                           mainPanel(
                             tableOutput("month_stats_bourgeoisie"),
                             tableOutput("month_songs_bourgeoisie")
                           )
                         ),
                         # Overall Bour. Tab
                         tabPanel(
                           "Overall Bourgeoisie Info",
                           mainPanel(
                             dataTableOutput("overallBourgeoisie"),
                             verbatimTextOutput('selectionB')
                           )
                         ))
    )    
  )
)

## END OF UI ##


### SERVER ######

server <- function(input, output, session) {
  
  ###### SONGS ###########
  subset_plays_by_month <- reactive({
    cleaned %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(
        !is.na(TimeInHours),
        TimeInHours >= input$topHrsMonth[1],
        TimeInHours <= input$topHrsMonth[2],
        TimeInPlays >= input$topPlayedMonth[1],
        TimeInPlays <= input$topPlayedMonth[2],
        datenum == as.Date(input$month_choice_songs))
  })
  
  subset_plays <- reactive({
    cleaned %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      group_by(SongID) %>%
      filter(!is.na(TimeInHours)) %>%
      mutate(DateAdded = min(datenum)) %>%
      ungroup() %>%
      filter(
        TimeInHours >= input$topHrs[1],
        TimeInHours <= input$topHrs[2],
        TimeInPlays >= input$topPlayed[1],
        TimeInPlays <= input$topPlayed[2],
        DateAdded >= as.Date(input$song_date_range[1]),
        DateAdded <= as.Date(input$song_date_range[2])
      )  %>%
      group_by(SongID) %>%
      mutate(DateOfOccurrence = last(dates)) %>%
      ungroup()
  })
  
  
  cln_subset <- reactive({
    
    cleaned %>%
      filter(SongName == input$song_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"))
  })
  
  song_static_info <- reactive({
    cleaned %>%
      filter(SongName == input$song_name) %>%
      filter(!is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      summarise(ArtistName = unique(Artist),
                AlbumName = unique(Album),
                DateAdded = dplyr::first(dates))
  })
  
  cln_subset_overall_info <- reactive({
    
    subBour <- db_misc %>%
      filter(SongName == input$song_name)
    
    
    cleaned %>%
      filter(SongName == input$song_name) %>%
      filter(!is.na(TimeInHours), !is.na(dHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             rowN = first(which(RankOutOfN == MaxRank)),
             DateOfMaxRank = dates[rowN],
             rowD = first(which(dHours == max(dHours))),
             Max_dHours = dHours[rowD],
             DateOfMaxdHours = dates[rowD]) %>%
      summarize(
        MaximumRank = as.integer(first(MaxRank)),
        DateOfMaxRank = first(DateOfMaxRank),
        DateOfMaxdHours = first(DateOfMaxdHours),
        Max_dHours = first(Max_dHours)
      ) %>%
      mutate(MonthsOnBourgeoisie = as.integer(subBour$NumberOfMonths),
             LongestStreakOnBourgeoisie = as.integer(subBour$LongestStreak),
             CurrentStreakOnBourgeoisie = as.integer(subBour$CurrentStreak))
    
    
  })
  
  
  # Overall Song Server
  output$overallSong <- renderDataTable({
    
    if("Most Time Improved" == input$secondarySortingFilter){
      
      if(("Album" %in% input$tertiarySortingFilter) & ("Artist" %in% input$tertiarySortingFilter)){
        subset_plays() %>%
          group_by(SongName,Artist,Album, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalTimeImprovement = max(TotaldTime)) %>%
          ungroup() %>%
          select(SongName, Artist, Album, TotalTime, TotalPlays, DateOfOccurrence, TotalTimeImprovement) %>%
          arrange(desc(TotalTimeImprovement))
        
      }
      else if(!("Album" %in% input$tertiarySortingFilter) & ("Artist" %in% input$tertiarySortingFilter)){
        
        subset_plays() %>%
          group_by(SongName, Artist, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalTimeImprovement = max(TotaldTime)) %>%
          ungroup() %>%
          select(SongName, Artist, TotalTime, TotalPlays, DateOfOccurrence, TotalTimeImprovement) %>%
          arrange(desc(TotalTimeImprovement))
        
      }
      else if(("Album" %in% input$tertiarySortingFilter) & !("Artist" %in% input$tertiarySortingFilter)){
        
        
        subset_plays() %>%
          group_by(SongName, Album, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalTimeImprovement = max(TotaldTime)) %>%
          ungroup() %>%
          select(SongName, Album, TotalTime, TotalPlays, DateOfOccurrence,TotalTimeImprovement) %>%
          arrange(desc(TotalTimeImprovement))
        
      }
      else{
        
        subset_plays() %>%
          group_by(SongName, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalTimeImprovement = max(TotaldTime)) %>%
          ungroup() %>%
          select(SongName, TotalTime, TotalPlays,DateOfOccurrence,TotalTimeImprovement) %>%
          arrange(desc(TotalTimeImprovement))
        
      }
    }
    else if("Most Play Improved" == input$secondarySortingFilter){
      
      if(("Album" %in% input$tertiarySortingFilter) & ("Artist" %in% input$tertiarySortingFilter)){
        subset_plays() %>%
          group_by(SongName,Artist,Album, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalPlayImprovement = max(TotaldPlays)) %>%
          ungroup() %>%
          select(SongName, Artist, Album, TotalTime, TotalPlays, DateOfOccurrence, TotalPlayImprovement) %>%
          arrange(desc(TotalPlayImprovement))
        
      }
      else if(!("Album" %in% input$tertiarySortingFilter) & ("Artist" %in% input$tertiarySortingFilter)){
        
        subset_plays() %>%
          group_by(SongName, Artist, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalPlayImprovement = max(TotaldPlays)) %>%
          ungroup() %>%
          select(SongName, Artist, TotalTime, TotalPlays, DateOfOccurrence, TotalPlayImprovement) %>%
          arrange(desc(TotalPlayImprovement))
        
      }
      else if(("Album" %in% input$tertiarySortingFilter) & !("Artist" %in% input$tertiarySortingFilter)){
        
        
        subset_plays() %>%
          group_by(SongName, Album, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalPlayImprovement = max(TotaldPlays)) %>%
          ungroup() %>%
          select(SongName, Album, TotalTime, TotalPlays, DateOfOccurrence,TotalPlayImprovement) %>%
          arrange(desc(TotalPlayImprovement))
        
      }
      else{
        
        subset_plays() %>%
          group_by(SongName, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalPlayImprovement = max(TotaldPlays)) %>%
          ungroup() %>%
          select(SongName, TotalTime, TotalPlays,DateOfOccurrence,TotalPlayImprovement) %>%
          arrange(desc(TotalPlayImprovement))
        
      }
    }
    else if("Most Rank Improved" == input$secondarySortingFilter){
      
      if(("Album" %in% input$tertiarySortingFilter) & ("Artist" %in% input$tertiarySortingFilter)){
        subset_plays() %>%
          group_by(SongName,Artist,Album, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalRankImprovement = max(TotaldRank)) %>%
          ungroup() %>%
          select(SongName, Artist, Album, TotalTime, TotalPlays, DateOfOccurrence, TotalRankImprovement) %>%
          arrange(desc(TotalRankImprovement))
        
      }
      else if(!("Album" %in% input$tertiarySortingFilter) & ("Artist" %in% input$tertiarySortingFilter)){
        
        subset_plays() %>%
          group_by(SongName, Artist, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalRankImprovement = max(TotaldRank)) %>%
          ungroup() %>%
          select(SongName, Artist, TotalTime, TotalPlays, DateOfOccurrence, TotalRankImprovement) %>%
          arrange(desc(TotalRankImprovement))
        
      }
      else if(("Album" %in% input$tertiarySortingFilter) & !("Artist" %in% input$tertiarySortingFilter)){
        
        
        subset_plays() %>%
          group_by(SongName, Album, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalRankImprovement = max(TotaldRank)) %>%
          ungroup() %>%
          select(SongName, Album, TotalTime, TotalPlays, DateOfOccurrence,TotalRankImprovement) %>%
          arrange(desc(TotalRankImprovement))
        
      }
      else{
        
        subset_plays() %>%
          group_by(SongName, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalRankImprovement = max(TotaldRank)) %>%
          ungroup() %>%
          select(SongName, TotalTime, TotalPlays,DateOfOccurrence,TotalRankImprovement) %>%
          arrange(desc(TotalRankImprovement))
        
      }
    }
    else if("Most Value Improved" == input$secondarySortingFilter){
      
      if(("Album" %in% input$tertiarySortingFilter) & ("Artist" %in% input$tertiarySortingFilter)){
        subset_plays() %>%
          group_by(SongName,Artist,Album, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalValueImprovement = max(TotaldValue)) %>%
          ungroup() %>%
          select(SongName, Artist, Album, TotalTime, TotalPlays, DateOfOccurrence, TotalValueImprovement) %>%
          arrange(desc(TotalValueImprovement))
        
      }
      else if(!("Album" %in% input$tertiarySortingFilter) & ("Artist" %in% input$tertiarySortingFilter)){
        
        subset_plays() %>%
          group_by(SongName, Artist, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalValueImprovement = max(TotaldValue)) %>%
          ungroup() %>%
          select(SongName, Artist, TotalTime, TotalPlays, DateOfOccurrence, TotalValueImprovement) %>%
          arrange(desc(TotalValueImprovement))
        
      }
      else if(("Album" %in% input$tertiarySortingFilter) & !("Artist" %in% input$tertiarySortingFilter)){
        
        
        subset_plays() %>%
          group_by(SongName, Album, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalValueImprovement = max(TotaldValue)) %>%
          ungroup() %>%
          select(SongName, Album, TotalTime, TotalPlays, DateOfOccurrence,TotalValueImprovement) %>%
          arrange(desc(TotalValueImprovement))
        
      }
      else{
        
        subset_plays() %>%
          group_by(SongName, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays),
                    TotalValueImprovement = max(TotaldValue)) %>%
          ungroup() %>%
          select(SongName, TotalTime, TotalPlays,DateOfOccurrence,TotalValueImprovement) %>%
          arrange(desc(TotalValueImprovement))
        
      }
    }
    else{
      
      if(("Album" %in% input$tertiarySortingFilter) & ("Artist" %in% input$tertiarySortingFilter)){
        subset_plays() %>%
          group_by(SongName,Artist,Album, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays)) %>%
          ungroup() %>%
          select(SongName, Artist, Album, TotalTime, TotalPlays, DateOfOccurrence) %>%
          arrange(desc(TotalTime))
        
      }
      else if(!("Album" %in% input$tertiarySortingFilter) & ("Artist" %in% input$tertiarySortingFilter)){
        
        subset_plays() %>%
          group_by(SongName, Artist, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays)) %>%
          ungroup() %>%
          select(SongName, Artist, TotalTime, TotalPlays, DateOfOccurrence) %>%
          arrange(desc(TotalTime))
        
      }
      else if(("Album" %in% input$tertiarySortingFilter) & !("Artist" %in% input$tertiarySortingFilter)){
        
        
        subset_plays() %>%
          group_by(SongName, Album, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays)) %>%
          ungroup() %>%
          select(SongName, Album, TotalTime, TotalPlays, DateOfOccurrence) %>%
          arrange(desc(TotalTime))
        
      }
      else{
        
        subset_plays() %>%
          group_by(SongName, DateOfOccurrence) %>%
          summarise(TotalTime = max(TimeInHours),
                    TotalPlays = max(TimeInPlays)) %>%
          ungroup() %>%
          select(SongName, TotalTime, TotalPlays,DateOfOccurrence) %>%
          arrange(desc(TotalTime))
        
      }
    }
  },server=F, selection='single')
  
  observeEvent(input$jumpToIndivSongO,{s = input$overallSong_cell_clicked$value
  updateTabsetPanel(session=getDefaultReactiveDomain(),'songs', selected = "Individual Song Data")
  updateSelectizeInput(session=getDefaultReactiveDomain(), 'song_name',selected=s)})
  
  # Individual Song Server
  output$song_info <- renderTable({
    song_static_info() %>%
      select(ArtistName, AlbumName, DateAdded)
  })
  
  output$song_max_info <- renderTable({
    cln_subset_overall_info() %>%
      select(MaximumRank, DateOfMaxRank, Max_dHours, DateOfMaxdHours) 
  })
  
  output$song_big4 <- renderTable({
    cln_subset() %>%
      filter(!is.na(TimeInHours)) %>%
      slice_tail(n = 1) %>%
      select(RankOutOfN, ValueOutOf1, TimeInHours, TimeInPlays)
  })
  
  output$bourgeoisie_song_info <- renderTable({
    cln_subset_overall_info() %>%
      select(MonthsOnBourgeoisie, LongestStreakOnBourgeoisie, CurrentStreakOnBourgeoisie)
  })
  
  output$songBig4Plot1 <- renderHighchart({
    
    
    tb <- cleaned %>%
      filter(SongName == input$song_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    
    p3 <- hchart(
      tb, "line",
      hcaes(x =  datenum, y = RankOutOfN),
      showInLegend = TRUE,
      name = "Rank"
    ) %>% 
      hc_yAxis(reversed = TRUE)%>%
      hc_title(text = "Overall Rank Over Time")
    
    p3
    
    
  })
  
  output$songBig4Plot2 <- renderHighchart({
    
    tb <- cleaned %>%
      filter(SongName == input$song_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    p4 <- hchart(
      tb, "line",
      hcaes(x =  datenum, y = ValueOutOf1),
      showInLegend = TRUE,
      name = "Value"
    ) %>%
      hc_title(text = "Overall Value Over Time")
    
    p4
  })
  
  output$songBig4Plot3 <- renderHighchart({
    
    tb <- cleaned %>%
      filter(SongName == input$song_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    p1 <-  hchart(
      tb, "line",
      hcaes(x =  datenum, y = TimeInHours),
      showInLegend = TRUE,
      name = "Hours"
    ) %>%
      hc_title(text = "Cumulative Hours Listened Over Time")
    
    p1
  })
  
  output$songBig4Plot4 <- renderHighchart({
    
    tb <- cleaned %>%
      filter(SongName == input$song_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    p2 <-  hchart(
      tb, "line",
      hcaes(x =  datenum, y = TimeInPlays),
      showInLegend = TRUE,
      name = "Hours"
    ) %>%
      hc_title(text = "Cumulative Plays Over Time")
    
    p2
  })
  
  
  
  output$dateTimeListenNC <-
    renderHighchart({
      #non-cumulative listening hours
      tb <- cleaned %>%
        filter(SongName == input$song_name, !is.na(TimeInHours)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y"))
      
      hchart(
        tb, "line",
        hcaes(x =  datenum, y = dHours),
        showInLegend = TRUE,
        name = "Hours"
      ) %>%
        hc_title(text = "Hours Listened by Date")
    })
  
  
  observeEvent(input$jumpToIndivArtist,{s = input$overallArtist_cell_clicked$value
  updateTabsetPanel(session=getDefaultReactiveDomain(),'artists', selected = "Individual Artist Data")
  updateSelectizeInput(session=getDefaultReactiveDomain(), 'artist_name',selected=s)})
  
  
  
  # Monthly Song Server
  output$monthSong <- renderDataTable({
    if("Most Time Improved" == input$secondarySortingFilterMonth){
      if(("Album" %in% input$tertiarySortingFilterMonth) & ("Artist" %in% input$tertiarySortingFilterMonth)){
        
        subset_plays_by_month() %>%
          select(SongName, Artist, Album, TimeInHours, TimeInPlays, dHours) %>%
          arrange(desc(dHours))
        
      }
      else if(("Album" %in% input$tertiarySortingFilterMonth) & !("Artist" %in% input$tertiarySortingFilterMonth)){
        
        subset_plays_by_month() %>%
          select(SongName, Album, TimeInHours, TimeInPlays,dHours) %>%
          arrange(desc(dHours))
        
      }
      else if(!("Album" %in% input$tertiarySortingFilterMonth) & ("Artist" %in% input$tertiarySortingFilterMonth)){
        subset_plays_by_month() %>%
          select(SongName, Artist, TimeInHours, TimeInPlays,dHours) %>%
          arrange(desc(dHours))
        
      }
      else{
        subset_plays_by_month() %>%
          select(SongName, TimeInHours, TimeInPlays,dHours) %>%
          arrange(desc(dHours))
        
      }
    }
    else if("Most Play Improved" == input$secondarySortingFilterMonth){
      if(("Album" %in% input$tertiarySortingFilterMonth) & ("Artist" %in% input$tertiarySortingFilterMonth)){
        
        subset_plays_by_month() %>%
          select(SongName, Artist, Album, TimeInHours, TimeInPlays, dPlays) %>%
          arrange(desc(dPlays))
        
      }
      else if(("Album" %in% input$tertiarySortingFilterMonth) & !("Artist" %in% input$tertiarySortingFilterMonth)){
        
        subset_plays_by_month() %>%
          select(SongName, Album, TimeInHours, TimeInPlays,dPlays) %>%
          arrange(desc(dPlays))
        
      }
      else if(!("Album" %in% input$tertiarySortingFilterMonth) & ("Artist" %in% input$tertiarySortingFilterMonth)){
        subset_plays_by_month() %>%
          select(SongName, Artist, TimeInHours, TimeInPlays,dPlays) %>%
          arrange(desc(dPlays))
        
      }
      else{
        subset_plays_by_month() %>%
          select(SongName, TimeInHours, TimeInPlays,dPlays) %>%
          arrange(desc(dPlays))
        
      }
    }
    else if("Most Rank Improved" == input$secondarySortingFilterMonth){
      if(("Album" %in% input$tertiarySortingFilterMonth) & ("Artist" %in% input$tertiarySortingFilterMonth)){
        
        subset_plays_by_month() %>%
          select(SongName, Artist, Album, TimeInHours, TimeInPlays, dRank) %>%
          arrange(desc(dRank))
        
      }
      else if(("Album" %in% input$tertiarySortingFilterMonth) & !("Artist" %in% input$tertiarySortingFilterMonth)){
        
        subset_plays_by_month() %>%
          select(SongName, Album, TimeInHours, TimeInPlays,dRank) %>%
          arrange(desc(dRank))
        
      }
      else if(!("Album" %in% input$tertiarySortingFilterMonth) & ("Artist" %in% input$tertiarySortingFilterMonth)){
        subset_plays_by_month() %>%
          select(SongName, Artist, TimeInHours, TimeInPlays,dRank) %>%
          arrange(desc(dRank))
        
      }
      else{
        subset_plays_by_month() %>%
          select(SongName, TimeInHours, TimeInPlays,dRank) %>%
          arrange(desc(dRank))
        
      }
    }
    else if("Most Value Improved" == input$secondarySortingFilter){
      if(("Album" %in% input$tertiarySortingFilterMonth) & ("Artist" %in% input$tertiarySortingFilterMonth)){
        
        subset_plays_by_month() %>%
          select(SongName, Artist, Album, TimeInHours, TimeInPlays, dValue) %>%
          arrange(desc(dValue))
        
      }
      else if(("Album" %in% input$tertiarySortingFilterMonth) & !("Artist" %in% input$tertiarySortingFilterMonth)){
        
        subset_plays_by_month() %>%
          select(SongName, Album, TimeInHours, TimeInPlays,dValue) %>%
          arrange(desc(dValue))
        
      }
      else if(!("Album" %in% input$tertiarySortingFilterMonth) & ("Artist" %in% input$tertiarySortingFilterMonth)){
        subset_plays_by_month() %>%
          select(SongName, Artist, TimeInHours, TimeInPlays,dValue) %>%
          arrange(desc(dValue))
        
      }
      else{
        subset_plays_by_month() %>%
          select(SongName, TimeInHours, TimeInPlays,dValue) %>%
          arrange(desc(dValue))
        
      }
    }
    else{
      if(("Album" %in% input$tertiarySortingFilterMonth) & ("Artist" %in% input$tertiarySortingFilterMonth)){
        
        subset_plays_by_month() %>%
          select(SongName, Artist, Album, TimeInHours, TimeInPlays) %>%
          arrange(desc(TimeInHours))
        
      }
      else if(("Album" %in% input$tertiarySortingFilterMonth) & !("Artist" %in% input$tertiarySortingFilterMonth)){
        
        subset_plays_by_month() %>%
          select(SongName, Album, TimeInHours, TimeInPlays) %>%
          arrange(desc(TimeInHours))
        
      }
      else if(!("Album" %in% input$tertiarySortingFilterMonth) & ("Artist" %in% input$tertiarySortingFilterMonth)){
        subset_plays_by_month() %>%
          select(SongName, Artist, TimeInHours, TimeInPlays) %>%
          arrange(desc(TimeInHours))
        
      }
      else{
        subset_plays_by_month() %>%
          select(SongName, TimeInHours, TimeInPlays) %>%
          arrange(desc(TimeInHours))
        
      }
    }
  },server=F,selection='single')
  
  observeEvent(input$jumpToIndivSongM,{s = input$monthSong_cell_clicked$value
  updateTabsetPanel(session=getDefaultReactiveDomain(),'songs', selected = "Individual Song Data")
  updateSelectizeInput(session=getDefaultReactiveDomain(), 'song_name',selected=s)})
  
### END OF SONGS SERVER ###
  
  ########## ARTISTS #############
  
  subset_artist_plays <- reactive({
    cleaned_artist %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      group_by(ArtistID) %>%
      filter(!is.na(TimeInHours)) %>%
      mutate(DateAdded = min(datenum)) %>%
      ungroup() %>%
      filter(
        !is.na(TimeInHours),
        TimeInHours >= input$topArtistHrs[1],
        TimeInHours <= input$topArtistHrs[2],
        TimeInPlays >= input$topArtistPlayed[1],
        TimeInPlays <= input$topArtistPlayed[2],
        DateAdded >= input$artist_date_range[1],
        DateAdded <= input$artist_date_range[2]
      )
    
  })
  
  cln_artist_by_song_info <- reactive({
    cleaned %>%
      filter(Artist == input$artist_name, !is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             MaxRank = as.integer(MaxRank))
  })
  
  cln_artist_subset <- reactive({
    cleaned_artist %>%
      filter(Artist == input$artist_name, !is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             MaxRank = as.integer(MaxRank))
  })
  
  cln_artist_overall_info <- reactive({
    cleaned_artist %>%
      filter(Artist == input$artist_name) %>%
      filter(!is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             MaxRank = as.integer(MaxRank),
             rowN = first(which(RankOutOfN == MaxRank)),
             DateOfMaxRank = dates[rowN]) %>%
      summarize(MaximumRank = as.integer(max(MaxRank)),
                SongCount = as.integer(max(SongCount)),
                DateOfMaxRank = first(DateOfMaxRank))
  })
  
  
  # Overall Artists Server
  output$overallArtist <- renderDataTable({
    subset_artist_plays() %>%
      select(Artist, TimeInHours, TimeInPlays) %>%
      group_by(Artist) %>%
      summarise(TotalTime = max(TimeInHours),
                TotalPlays = max(TimeInPlays)) %>%
      arrange(desc(TotalTime))
    
  },server=F,selection='single')
  
  # Individual Artist Server
  output$artist_song_info <- renderTable({
    cln_artist_by_song_info() %>%
      select(SongName, Album, MaxRank) %>%
      group_by(SongName, Album, MaxRank) %>%
      summarise()
    
  })
  
  output$overall_artist_info <- renderTable({
    cln_artist_overall_info() %>%
      select(MaximumRank, DateOfMaxRank, SongCount)
  })
  
  
  
  output$artistBig4Plot1 <- renderHighchart({
    
    tb <- cleaned_artist %>%
      filter(Artist == input$artist_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    p3a <-  hchart(
      tb, "line",
      hcaes(x =  datenum, y = RankOutOfN),
      showInLegend = TRUE,
      name = "Rank"
    ) %>%
      hc_yAxis(reversed = TRUE) %>%
      hc_title(text = "Overall Rank Over Time")
    
    p3a
  })
  
  output$artistBig4Plot2 <- renderHighchart({
    
    tb <- cleaned_artist %>%
      filter(Artist == input$artist_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    p4a <-  hchart(
      tb, "line",
      hcaes(x =  datenum, y = ValueOutOf1),
      showInLegend = TRUE,
      name = "Value"
    ) %>%
      hc_title(text = "Overall Value Over Time")
    
    p4a
  })
  
  output$artistBig4Plot3 <- renderHighchart({
    
    tb <- cleaned_artist %>%
      filter(Artist == input$artist_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    p1a <-  hchart(
      tb, "line",
      hcaes(x =  datenum, y = TimeInHours),
      showInLegend = TRUE,
      name = "Plays"
    ) %>%
      hc_title(text = "Cumulative Hours Listened Over Time")
    
    p1a
  })
  
  output$artistBig4Plot4 <- renderHighchart({
    
    tb <- cleaned_artist %>%
      filter(Artist == input$artist_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    p2a <-  hchart(
      tb, "line",
      hcaes(x =  datenum, y = TimeInPlays),
      showInLegend = TRUE,
      name = "Hours"
    ) %>%
      hc_title(text = "Cumulative Plays Over Time")
    
    p2a
  })
  
  
  output$dateTimeListenNCA <-
    renderHighchart({
      #non-cumulative listening hours
      
      
      tb <- cleaned_artist %>%
        filter(Artist == input$artist_name, !is.na(TimeInHours)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y"))
      
      hchart(
        tb, "line",
        hcaes(x =  datenum, y = dHours),
        showInLegend = TRUE,
        name = "Hours"
      ) %>%
        hc_title(text = "Hours Listened by Date")
      
    })
  
  ### END OF ARTISTS SERVER ###
  
  ########## ALBUMS #############
  
  cln_album_subset <- reactive({
    cleaned_album %>%
      filter(Album == input$album_name, !is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"))
  })
  
  
  cln_album_overall_info <- reactive({
    cleaned_album %>%
      filter(Album == input$album_name) %>%
      filter(!is.na(TimeInHours)) %>%
      summarize(SongCount = as.integer(max(SongCount)))
  })
  
  cln_album_by_song_info <- reactive({
    cleaned %>%
      filter(Album == input$album_name,!is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"))
  })
  
  
  
  output$album_info <- renderTable({
    cln_album_by_song_info() %>%
      select(SongName, Artist, MaxRank) %>%
      group_by(SongName, Artist, MaxRank) %>%
      summarise()
    
  })
  
  output$overall_album_info <- renderTable({
    cln_album_overall_info() %>%
      select(SongCount)
  })
  
  
  
  output$albumBig4Plot1 <- renderHighchart({
    
    tb <- cleaned_album %>%
      filter(Album == input$album_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    p3al <-  hchart(
      tb, "line",
      hcaes(x =  datenum, y = RankOutOfN),
      showInLegend = TRUE,
      name = "Rank"
    ) %>%
      hc_yAxis(reversed = TRUE) %>%
      hc_title(text = "Overall Rank Over Time")
    
    p3al
  })
  
  output$albumBig4Plot2 <- renderHighchart({
    
    tb <- cleaned_album %>%
      filter(Album == input$album_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    p4al <-  hchart(
      tb, "line",
      hcaes(x =  datenum, y = ValueOutOf1),
      showInLegend = TRUE,
      name = "Value"
    ) %>%
      hc_title(text = "Overall Value Over Time")
    
    p4al
  })
  
  output$albumBig4Plot3 <- renderHighchart({
    
    tb <- cleaned_album %>%
      filter(Album == input$album_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    p1al <-  hchart(
      tb, "line",
      hcaes(x =  datenum, y = TimeInHours),
      showInLegend = TRUE,
      name = "Plays"
    ) %>%
      hc_title(text = "Cumulative Hours Listened Over Time")
    
    p1al
  })
  
  output$albumBig4Plot4 <- renderHighchart({
    
    tb <- cleaned_album %>%
      filter(Album == input$album_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours))
    
    p2al <-  hchart(
      tb, "line",
      hcaes(x =  datenum, y = TimeInPlays),
      showInLegend = TRUE,
      name = "Hours"
    ) %>%
      hc_title(text = "Cumulative Plays Over Time")
    
    p2al
  })
  
  
  output$dateTimeListenNCAl <-
    renderHighchart({
      #non-cumulative listening hours
      
      tb <- cleaned_album %>%
        filter(Album == input$album_name, !is.na(TimeInHours)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y"))
      
      
      hchart(
        tb, "line",
        hcaes(x =  datenum, y = dHours),
        showInLegend = TRUE,
        name = "Hours"
      ) %>%
        hc_title(text = "Hours Listened by Date")
    })
  
  ### END ALBUMS SERVER ###
  
  
  ########## MISCELLANEOUS #############
  
  
  month_subset <- reactive({
    db_misc %>%
      select(input$month_name_bourgeoisie, SongName, Artist, Album) %>%
      na.omit()
    
  })
  
  overall_bour_stats <- reactive({
    db_misc %>%
      select(SongName, Artist, Album, NumberOfMonths, LongestStreak, CurrentStreak) %>%
      na.omit()
    
  })
  
  output$month_stats_bourgeoisie <- renderTable({
    month_subset() %>%
      mutate(NumberOfSongs = n()) %>%
      select(NumberOfSongs) %>%
      slice_head(n=1)
  })
  # Monthly Bour. Server
  output$month_songs_bourgeoisie <- renderTable({
    month_subset() %>%
      arrange(SongName) %>%
      mutate(n = row_number()) %>%
      select(n, SongName, Artist, Album)
  })
  
  # Overall Bour. Server
  output$overallBourgeoisie <- renderDataTable({
    overall_bour_stats()
  },server=F,selection='none')
  
  ### END MISC. SERVER ###
  
  ##### END OF SERVER #####
}

# Run the application
shinyApp(ui = ui, server = server)

### ALL DONE :) ###