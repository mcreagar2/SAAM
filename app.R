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

requiredPackages <- c('shiny','shinythemes','tidyverse','patchwork','DT', 'highcharter','lubridate')
for(package in requiredPackages){
  if(package %in% rownames(installed.packages()) == FALSE) {install.packages(package)}
}




library(shiny)
library(shinythemes)
library(tidyverse)
library(patchwork)
library(DT)
library(highcharter)
library(lubridate)

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
                        choices = sort(unique(as.Date(cleaned$dates, format = "%B %d %Y")
                        )),
                        selected = sort(unique(as.Date(cleaned$dates, format = "%B %d %Y")))[-1]),
            
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
                "Most Time Improved - Past Month",
                "Most Play Improved - Past Month",
                "Most Rank Improved - Past Month",
                "Most Value Improved - Past Month",
                "Most Time Improved - Overall",
                "Most Play Improved - Overall",
                "Most Rank Improved - Overall",
                "Most Value Improved - Overall"
              ),
              selected = "-"
            ),
            checkboxGroupInput(
              "tertiarySortingFilterMonth",
              "Include:",
              choices = c(
                "SongName",
                "Artist",
                "Album",
                "TimeInHours",
                "TimeInPlays"
              ),
              selected = "SongName"
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
                   tableOutput("counting_artist_24hr_songs"),
                   dataTableOutput("artist_song_info"),
                   column(width = 6, highchartOutput("artistBig4Plot1"), highchartOutput("artistBig4Plot3")),
                   column(width = 6, highchartOutput("artistBig4Plot2"), highchartOutput("artistBig4Plot4")),
                   highchartOutput("dateTimeListenNCA"),
                   plotOutput("artist_song_graph"),
                   checkboxGroupInput("songnameforgraph", "Select songs to graph", choices = NULL)
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
                     selected = "Random Access Memories"
                   )
                 ),
                 
                 mainPanel(
                   tableOutput("overall_album_info"),
                   dataTableOutput("album_info"),
                   column(width = 6, highchartOutput("albumBig4Plot1"), highchartOutput("albumBig4Plot3")),
                   column(width = 6, highchartOutput("albumBig4Plot2"), highchartOutput("albumBig4Plot4")),
                   highchartOutput("dateTimeListenNCAl"),
                   plotOutput("album_song_graph"),
                   checkboxGroupInput("songnameforgraph2", "Select songs to graph", choices = NULL)
                   
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
                               choices = unique(colnames(db_misc)[5:ncol(db_misc)-2]),
                               selected = unique(colnames(db_misc)[5:ncol(db_misc)-2])[-1]
                             ),
                             checkboxGroupInput(
                               "tertiarySortingFilterBour",
                               "Include:",
                               choices = c("SongName", "Artist", "Album", "CurrentStreak"),
                               selected = "SongName"
                             ),
                           ),
                           mainPanel(
                             tableOutput("month_stats_bourgeoisie"),
                             dataTableOutput("month_songs_bourgeoisie")
                           )
                         ),
                         
                         # Overall Bour. Tab
                         tabPanel(
                           "Overall Bourgeoisie Info",
                           mainPanel(
                             dataTableOutput("overallBourgeoisie"),
                             verbatimTextOutput('selectionB')
                           )
                         ),
                         
                         
                         tabPanel(
                           "SAAM Unwrapped",
                           sidebarPanel(
                             selectizeInput(
                               "ytd_summary",
                               "Please select a summary option:",
                               choices = c("2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014","2013", "Past Year"),
                               selected = "2022"
                             ),
                             selectizeInput(
                               "artist_song",
                               "View by Artist or Song:",
                               choices = c("Artist", "Album", "Song"),
                               selected = "Artist"
                             )
                           ),
                           mainPanel(
                             tableOutput("overall_stats"),
                             dataTableOutput("overall_year_artist_song")
                           )
                         ),
                         tabPanel("Comparisons - Songs",
                                  sidebarPanel(
                                    selectizeInput(
                                      "song_compare1",
                                      "Please select Song 1 to chart:",
                                      choices = sort(unique(cleaned$SongName)),
                                      selected = "Instant Crush (feat. Julian Casablancas)"
                                    ),
                                    selectizeInput(
                                      "song_compare2",
                                      "Please select Song 2 to chart:",
                                      choices = sort(unique(cleaned$SongName)),
                                      selected = "State of Grace"
                                    ),

                         br(),
                         actionButton("button_for_SC", "Graph!")
                                  ),
                                  mainPanel(
                                    plotOutput("songs_compare_graph")
                                  ))

             )    
    )
  )
)
## END OF UI ##


### SERVER ######

server <- function(input, output, session) {
  
  ###### SONGS ###########
  subset_plays_by_month <- reactive({
    spm <- cleaned %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(
        !is.na(TimeInHours),
        TimeInHours >= input$topHrsMonth[1],
        TimeInHours <= input$topHrsMonth[2],
        TimeInPlays >= input$topPlayedMonth[1],
        TimeInPlays <= input$topPlayedMonth[2],
        datenum == as.Date(input$month_choice_songs))
    
    if("Most Time Improved - Past Month" == input$secondarySortingFilterMonth){
      spm <- spm[c(input$tertiarySortingFilterMonth, "dHours")]
      
      spm %>% filter(!is.na(dHours)) %>%
        arrange(desc(dHours))
      
    }
    else if("Most Play Improved - Past Month" == input$secondarySortingFilterMonth){
      
      spm <- spm[c(input$tertiarySortingFilterMonth, "dPlays")]
      
      spm %>% filter(!is.na(dPlays)) %>%
        arrange(desc(dPlays))
      
    }
    else if("Most Rank Improved - Past Month" == input$secondarySortingFilterMonth){
      
      spm <- spm[c(input$tertiarySortingFilterMonth, "dRank")]
      
      spm %>% filter(!is.na(dRank)) %>%
        arrange(desc(dRank))
    }
    else if("Most Value Improved - Past Month" == input$secondarySortingFilterMonth){
      spm <- spm[c(input$tertiarySortingFilterMonth, "dValue")]
      
      spm %>% filter(!is.na(dValue)) %>%
        arrange(desc(dValue))
    }
    else if("Most Time Improved - Overall" == input$secondarySortingFilterMonth){
      spm <- spm[c(input$tertiarySortingFilterMonth, "TotaldTime")]
      
      spm %>% filter(!is.na(TotaldTime)) %>%
        arrange(desc(TotaldTime))
    }
    else if("Most Play Improved - Overall" == input$secondarySortingFilterMonth){
      spm <- spm[c(input$tertiarySortingFilterMonth, "TotaldPlays")]
      
      spm %>% filter(!is.na(TotaldPlays)) %>%
        arrange(desc(TotaldPlays))
    }
    else if("Most Rank Improved - Overall" == input$secondarySortingFilterMonth){
      spm <- spm[c(input$tertiarySortingFilterMonth, "TotaldRank")]
      
      spm %>% filter(!is.na(TotaldRank)) %>%
        arrange(desc(TotaldRank))
    }
    else if("Most Value Improved - Overall" == input$secondarySortingFilterMonth){
      spm <- spm[c(input$tertiarySortingFilterMonth, "TotaldValue")]
      
      spm %>% filter(!is.na(TotaldValue)) %>%
        arrange(desc(TotaldValue))
    }
    else{
        spm <- spm[input$tertiarySortingFilterMonth]
        
        spm

    }
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
  },server=F, selection='single', rownames=FALSE)
  
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
    # %>%
    #   hc_tooltip(pointFormat = '{point.x: %B %Y}
    # 
    #                         {point.y:.3f}')
    
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
    
      subset_plays_by_month()
    
  },server=F,selection='single',rownames = FALSE)
  
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
             rowN = ifelse(!is.na(RankOutOfN), first(which(RankOutOfN == max(RankOutOfN))), NA),
             DateOfMaxRank = dates[rowN],
             MaxRank = RankOutOfN[rowN]) %>%
      group_by(SongName) %>%
      mutate(TotalTime = last(TimeInHours)) %>%
      filter(!is.na(TotaldTime)) %>%
      mutate(TotaldTime = last(TotaldTime)) %>%
      mutate(Yes24 = ifelse(!is.na(`24hrMark`), 1, 0)) %>%
      ungroup()
  })
  
  cln_artist_subset <- reactive({
    cleaned_artist %>%
      filter(Artist == input$artist_name, !is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             rowN = ifelse(!is.na(RankOutOfN), first(which(RankOutOfN == max(RankOutOfN))), NA),
             DateOfMaxRank = dates[rowN],
             MaxRank = RankOutOfN[rowN])
  })
  
  
  cln_artist_overall_info <- reactive({
    cleaned_artist %>%
      filter(Artist == input$artist_name) %>%
      filter(!is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             rowN = ifelse(!is.na(RankOutOfN), first(which(RankOutOfN == max(RankOutOfN))), NA),
             DateOfMaxRank = dates[rowN],
             MaxRank = RankOutOfN[rowN]) %>%
      filter(!is.na(SongCount)) %>%
      summarize(MaximumRank = as.integer(max(MaxRank)),
                SongCount = as.integer(max(SongCount)),
                DateOfMaxRank = first(DateOfMaxRank),
                HoursPer = max(HoursPer))
  })
  

  
  observe({
    artist <- input$artist_name
    
    songsub <- cleaned %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(Artist == artist) %>%
      filter(!is.na(TimeInHours)) %>%
      select(SongName)
    
    
    updateCheckboxGroupInput(session, "songnameforgraph", "Select songs to graph", choices = unique(songsub$SongName), selected = unique(songsub$SongName)[1])
    
    
    
  })
  
  artist_songs_for_graphing <- reactive({
    cleaned %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(Artist == input$artist_name) %>%
      filter(!is.na(TimeInHours)) %>%
      filter(SongName %in% input$songnameforgraph)
    
    
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
  output$artist_song_info <- renderDataTable({
    cln_artist_by_song_info() %>%
      select(SongName, Album, TotalTime, TotaldTime) %>%
      group_by(SongName, Album, TotalTime, TotaldTime) %>%
      summarise() %>%
      arrange(desc(TotalTime))
    
  })
  
  output$overall_artist_info <- renderTable({
    cln_artist_overall_info() %>%
      select(MaximumRank, DateOfMaxRank, SongCount, HoursPer)
  })
  
  output$counting_artist_24hr_songs <- renderTable({
    cln_artist_by_song_info() %>%
      filter(datenum == max(datenum)) %>%
      mutate(NumberOf24HrSongs = as.integer(sum(Yes24))) %>%
      summarise(NumberOf24HrSongs = first(NumberOf24HrSongs)) %>%
      select(NumberOf24HrSongs)
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
  
  
  output$artist_song_graph <- renderPlot({
    artist_songs_for_graphing() %>%
      ggplot(aes(x = datenum, y = TimeInHours)) + 
      # add points
      geom_line(aes(color = SongName)) + 
      ggtitle("Cumulative Hours Listened Over Time") + 
      xlab("Date") + 
      ylab("Hours")
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
      filter(!is.na(TimeInHours), !is.na(SongCount)) %>%
      summarize(SongCount = as.integer(max(SongCount)))
  })
  
  cln_album_by_song_info <- reactive({
    cleaned %>%
      filter(Album == input$album_name,!is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      group_by(SongName) %>%
      mutate(TotalTime = last(TimeInHours)) %>%
      filter(!is.na(TotaldTime)) %>%
      mutate(TotaldTime = last(TotaldTime)) %>%
      ungroup()
  })
  
  
  observe({
    album <- input$album_name
    
    songsub2 <- cleaned %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(Album == album) %>%
      filter(!is.na(TimeInHours)) %>%
      select(SongName)
    
    
    updateCheckboxGroupInput(session, "songnameforgraph2", "Select songs to graph", choices = unique(songsub2$SongName), selected = unique(songsub2$SongName)[1])
    
    
    
  })
  
  
  album_songs_for_graphing <- reactive({
    
    cleaned %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(Album == input$album_name) %>%
      filter(!is.na(TimeInHours)) %>%
      filter(SongName %in% input$songnameforgraph2)
    
  })
  
  
  output$album_info <- renderDataTable({
    cln_album_by_song_info() %>%
      select(SongName, Artist, TotalTime, TotaldTime) %>%
      group_by(SongName, Artist, TotalTime, TotaldTime) %>%
      summarise()%>%
      arrange(desc(TotalTime))
    
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
  
  
  
  output$album_song_graph <- renderPlot({
  
    album_songs_for_graphing() %>%
      ggplot(aes(x = datenum, y = TimeInHours)) + 
      # add points
      geom_line(aes(color = SongName)) + 
      ggtitle("Cumulative Hours Listened Over Time") + 
      xlab("Date") + 
      ylab("Hours")
  })
  
  
  ### END ALBUMS SERVER ###
  
  
  ########## MISCELLANEOUS #############
  
  songs_subset <- reactive({
    tb <-  cleaned %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"))
    
    if(length(input$song_compare > 0)){
    
    for(i in 1:length(input$song_compare)){
      tb2 <-cleaned %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        filter(SongName == input$song_compare[i])
      
      nameslist <- intersect(names(tb), names(tb2))
      
      tb <- left_join(tb, tb2, by = nameslist)
      
    }
    }
    tb
    
  })
  

  
  month_subset <- reactive({
    
    bpm <- db_misc %>%
      mutate(CurrentStreak = as.integer(CurrentStreak))
    
    bpm <- bpm[c(input$tertiarySortingFilterBour, input$month_name_bourgeoisie)]
    
    bpm %>%
      na.omit()
    
  })
  
  overall_bour_stats <- reactive({
    db_misc %>%
      select(SongName, Artist, Album, NumberOfMonths, LongestStreak, CurrentStreak) %>%
      na.omit()
    
  })
  
  
  overall_year_artist <- reactive({

    if(input$ytd_summary == "2022"){
    cleaned %>%
        filter(!is.na(TimeInHours)) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        group_by(SongName, Artist) %>%
        mutate(DateAdded = min(datenum)) %>%
        ungroup() %>%
      filter(datenum >= as.Date("January 01 2022", format = "%B %d %Y"),
             datenum < as.Date("January 01 2023", format = "%B %d %Y")) %>%  #dHours != 0
        group_by(Artist) %>%
        mutate(TotalTime = sum(dHours),
               TotalPlays = sum(dPlays)) %>%
        ungroup()
    }
    
    else if(input$ytd_summary == "2021"){
      cleaned %>%
        filter(!is.na(TimeInHours)) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        group_by(SongName, Artist) %>%
        mutate(DateAdded = min(datenum)) %>%
        ungroup() %>%
        filter(datenum >= as.Date("January 01 2021", format = "%B %d %Y"),
               datenum < as.Date("January 01 2022", format = "%B %d %Y")) %>%
        group_by(Artist) %>%
        mutate(TotalTime = sum(dHours),
               TotalPlays = sum(dPlays)) %>%
        ungroup()
    }
    else if(input$ytd_summary == "2020"){
      cleaned %>%
        filter(!is.na(TimeInHours)) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        group_by(SongName, Artist) %>%
        mutate(DateAdded = min(datenum)) %>%
        ungroup() %>%
        filter(datenum >= as.Date("January 01 2020", format = "%B %d %Y"),
               datenum < as.Date("January 01 2021", format = "%B %d %Y")) %>%
        group_by(Artist) %>%
        mutate(TotalTime = sum(dHours),
               TotalPlays = sum(dPlays)) %>%
        ungroup()
    }
    else if(input$ytd_summary == "2019"){
      cleaned %>%
        filter(!is.na(TimeInHours)) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        group_by(SongName, Artist) %>%
        mutate(DateAdded = min(datenum)) %>%
        ungroup() %>%
        filter(datenum >= as.Date("January 01 2019", format = "%B %d %Y"),
               datenum < as.Date("January 01 2020", format = "%B %d %Y")) %>%
        group_by(Artist) %>%
        mutate(TotalTime = sum(dHours),
               TotalPlays = sum(dPlays)) %>%
        ungroup()
    }
    else if(input$ytd_summary == "2018"){
      cleaned %>%
        filter(!is.na(TimeInHours)) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        group_by(SongName, Artist) %>%
        mutate(DateAdded = min(datenum)) %>%
        ungroup() %>%
        filter(datenum >= as.Date("January 01 2018", format = "%B %d %Y"),
               datenum < as.Date("January 01 2019", format = "%B %d %Y")) %>%
        group_by(Artist) %>%
        mutate(TotalTime = sum(dHours),
               TotalPlays = sum(dPlays)) %>%
        ungroup()
    }
    else if(input$ytd_summary == "2017"){
      cleaned %>%
        filter(!is.na(TimeInHours)) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        group_by(SongName, Artist) %>%
        mutate(DateAdded = min(datenum)) %>%
        ungroup() %>%
        filter(datenum >= as.Date("January 01 2017", format = "%B %d %Y"),
               datenum < as.Date("January 01 2018", format = "%B %d %Y")) %>%
        group_by(Artist) %>%
        mutate(TotalTime = sum(dHours),
               TotalPlays = sum(dPlays)) %>%
        ungroup()
    }
    else if(input$ytd_summary == "2016"){
      cleaned %>%
        filter(!is.na(TimeInHours)) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        group_by(SongName, Artist) %>%
        mutate(DateAdded = min(datenum)) %>%
        ungroup() %>%
        filter(datenum >= as.Date("January 01 2016", format = "%B %d %Y"),
               datenum < as.Date("January 01 2017", format = "%B %d %Y")) %>%
        group_by(Artist) %>%
        mutate(TotalTime = sum(dHours),
               TotalPlays = sum(dPlays)) %>%
        ungroup()
    }
    else if(input$ytd_summary == "2015"){
      cleaned %>%
        filter(!is.na(TimeInHours)) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        group_by(SongName, Artist) %>%
        mutate(DateAdded = min(datenum)) %>%
        ungroup() %>%
        filter(datenum >= as.Date("January 01 2015", format = "%B %d %Y"),
               datenum < as.Date("January 01 2016", format = "%B %d %Y")) %>%
        group_by(Artist) %>%
        mutate(TotalTime = sum(dHours),
               TotalPlays = sum(dPlays)) %>%
        ungroup()
    }
    else if(input$ytd_summary == "2014"){
      cleaned %>%
        filter(!is.na(TimeInHours)) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        group_by(SongName, Artist) %>%
        mutate(DateAdded = min(datenum)) %>%
        ungroup() %>%
        filter(datenum >= as.Date("January 01 2014", format = "%B %d %Y"),
               datenum < as.Date("January 01 2015", format = "%B %d %Y")) %>%
        group_by(Artist) %>%
        mutate(TotalTime = sum(dHours),
               TotalPlays = sum(dPlays)) %>%
        ungroup()
    }
    else if(input$ytd_summary == "2013"){
      cleaned %>%
        filter(!is.na(TimeInHours)) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        group_by(SongName, Artist) %>%
        mutate(DateAdded = min(datenum)) %>%
        ungroup() %>%
        filter(datenum >= as.Date("January 01 2013", format = "%B %d %Y"),
               datenum < as.Date("January 01 2014", format = "%B %d %Y")) %>%
        group_by(Artist) %>%
        mutate(TotalTime = sum(dHours),
               TotalPlays = sum(dPlays)) %>%
        ungroup()
    }
    else{ ## if the input selection is past year (e.g., May to May)
      cleaned %>%
        filter(!is.na(TimeInHours)) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
        group_by(SongName, Artist) %>%
        mutate(DateAdded = min(datenum)) %>%
        ungroup() %>%
        filter(datenum < Sys.Date(),
               datenum >= Sys.Date() %m-% years(1)) %>%
        group_by(Artist) %>%
        mutate(TotalTime = sum(dHours),
               TotalPlays = sum(dPlays)) %>%
        ungroup()

    
    }
  })
    
    overall_year_album <- reactive({

      if(input$ytd_summary == "2022"){
        cleaned_album %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2022", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2023", format = "%B %d %Y")) %>%  #dHours != 0
          group_by(Album) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      
      else if(input$ytd_summary == "2021"){
        cleaned_album %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2021", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2022", format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2020"){
        cleaned_album %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2020", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2021", format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2019"){
        cleaned_album %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2019", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2020", format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2018"){
        cleaned_album %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2018", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2019", format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2017"){
        cleaned_album %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2017", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2018", format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2016"){
        cleaned_album %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2016", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2017", format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2015"){
        cleaned_album %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2015", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2016", format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2014"){
        cleaned_album %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2014", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2015", format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2013"){
        cleaned_album %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2013", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2014", format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
        
      }
      else{ ## if the input selection is past year (e.g., May to May)
        cleaned_album %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(Album) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum < Sys.Date(),
                 datenum >= Sys.Date() %m-% years(1)) %>%
          group_by(Album) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
        
        
        
      }
      })
    
      
    overall_year_song <- reactive({
      if(input$ytd_summary == "2022"){
        cleaned %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(SongName, Artist) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2022", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2023", format = "%B %d %Y")) %>%  #dHours != 0
          group_by(SongName) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      
      else if(input$ytd_summary == "2021"){
        cleaned %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(SongName, Artist) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2021", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2022", format = "%B %d %Y")) %>%
          group_by(SongName) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2020"){
        cleaned %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(SongName, Artist) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2020", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2021", format = "%B %d %Y")) %>%
          group_by(SongName) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2019"){
        cleaned %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(SongName, Artist) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2019", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2020", format = "%B %d %Y")) %>%
          group_by(SongName) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2018"){
        cleaned %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(SongName, Artist) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2018", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2019", format = "%B %d %Y")) %>%
          group_by(SongName) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2017"){
        cleaned %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(SongName, Artist) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2017", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2018", format = "%B %d %Y")) %>%
          group_by(SongName) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2016"){
        cleaned %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(SongName, Artist) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2016", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2017", format = "%B %d %Y")) %>%
          group_by(SongName) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2015"){
        cleaned %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(SongName, Artist) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2015", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2016", format = "%B %d %Y")) %>%
          group_by(SongName) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2014"){
        cleaned %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(SongName, Artist) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2014", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2015", format = "%B %d %Y")) %>%
          group_by(SongName) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
      }
      else if(input$ytd_summary == "2013"){
        cleaned %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(SongName, Artist) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum >= as.Date("January 01 2013", format = "%B %d %Y"),
                 datenum < as.Date("January 01 2014", format = "%B %d %Y")) %>%
          group_by(SongName) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
        
      }
      else{ ## if the input selection is past year (e.g., May to May)
        cleaned %>%
          filter(!is.na(TimeInHours)) %>%
          mutate_all(~replace(., is.na(.), 0)) %>%
          mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
          group_by(SongName, Artist) %>%
          mutate(DateAdded = min(datenum)) %>%
          ungroup() %>%
          filter(datenum < Sys.Date(),
                 datenum >= Sys.Date() %m-% years(1)) %>%
          group_by(SongName) %>%
          mutate(TotalTime = sum(dHours),
                 TotalPlays = sum(dPlays)) %>%
          ungroup()
        
        
        
      }
  })
  
  output$month_stats_bourgeoisie <- renderTable({
    month_subset() %>%
      mutate(NumberOfSongs = n()) %>%
      select(NumberOfSongs) %>%
      slice_head(n=1)
  })
  
  # Monthly Bour. Server
  output$month_songs_bourgeoisie <- renderDataTable({
    month_subset() %>%
      select(input$tertiarySortingFilterBour)
  })
  
  # Overall Bour. Server
  output$overallBourgeoisie <- renderDataTable({
    overall_bour_stats()
  },server=F,selection='none')
  
  
  # Overall Year Server
  
  output$overall_stats <- renderTable({
    tb <- overall_year_song() %>%
      mutate(rowTS = first(which(TotalTime == max(TotalTime))),
             TopSong = SongName[rowTS]
             ) %>%
      group_by(SongName) %>%
      filter(row_number()==1) %>%
      ungroup() %>%
      mutate(NumberOfArtists = length(unique(Artist))) %>%
      summarise(HoursListened = sum(TotalTime),
                TopSong = first(TopSong),
                NumberOfSongs = n(),
                NumberOfArtists = first(NumberOfArtists)
                )
    
    tb %>%
      select(HoursListened, TopSong, NumberOfSongs, NumberOfArtists)
  })

  #        summarise(TotalTime = sum(TotalTime), TotalPlays = sum(TotalPlays)) %>%
  
  output$overall_year_artist_song <- renderDataTable({
    if(input$artist_song == "Artist"){
      overall_year_artist() %>%
        group_by(Artist) %>%
        filter(row_number()==1) %>%
        select(Artist, TotalTime, TotalPlays) %>%
        arrange(desc(TotalTime))
    }
    else if(input$artist_song == "Album"){
      overall_year_album() %>%
        group_by(Album) %>%
        filter(row_number()==1) %>%
        select(Album, TotalTime, TotalPlays) %>%
        arrange(desc(TotalTime))
    }
    else{
      overall_year_song() %>%
        group_by(SongName) %>%
        filter(row_number()==1) %>%
        select(SongName, Artist, TotalTime, TotalPlays) %>%
        arrange(desc(TotalTime))
    }
  },server=F,selection='none')
  

  
  observeEvent(input$button_for_SC, {
    #cat("Showing", input$song_compare1, "and", input$song_compare2)
  })
  
  tb <- eventReactive(input$button_for_SC, {
    cleaned %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(SongName == input$song_compare1 | SongName == input$song_compare2) %>%
      filter(!is.na(TimeInHours))
  })
  
  
  output$songs_compare_graph <- renderPlot({
    
    
    tb() %>%
    ggplot(aes(x = datenum, y = TimeInHours)) + 
      # add points
      geom_line(aes(color = SongName)) + 
      ggtitle("Cumulative Hours Listened Over Time") + 
      xlab("Date") + 
      ylab("Hours")
    
    # SC <-  hchart(
    #   tb, "line",
    #   hcaes(x =  datenum, y = TimeInHours, group = SongName),
    #   showInLegend = TRUE
    # ) %>%
    #   hc_title(text = "Cumulative Hours Listened Over Time")
    # 
    # SC

  })
  

  observeEvent(input$song_compare, {
    updateSelectizeInput(session, 'song_compare', choices = sort(unique(cleaned$SongName)), selected = input$song_compare, server = TRUE)
  })

  ### END MISC. SERVER ###
  
  ##### END OF SERVER #####
}

# Run the application
shinyApp(ui = ui, server = server)

### ALL DONE :) ###