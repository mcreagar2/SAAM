SAAM Description
================

## Shiny app

The app itself can be found here:

https://mollyc.shinyapps.io/saam/

## Data

There are four separate Google Sheets with possible data sources. Most of the data is stored in the "Songs" Google Sheet, though we also utilize data from the "Artists", "Albums", and "Miscellaneous" sheets

There are many variables in each of the files. Common variables across each file are: Song Name, Artist, Album, Hours (or some form of this), Play Count (or some form of this). Other variables are Date Added, Max Rank, Hour Differential, Play Differential, Total (all-time) Hours, Total (all-time) Plays, and others. There are more than 2000 songs in the database, and more are added each month.


## R Packages Used:

- shiny
- shinythemes 
- tidyverse
- rsconnect
- DT
- highcharter

## Short Description

- An R Shiny app makes the experience much more user friendly and much more exploratory. 
 - The user could, for example, search for statistics for a single song they like without having to know much of anything on how R or data filtering works.
- An R Shiny app also helps with managing all of the data sets at once. Since all of the hard work is done behind the visage of the app, users will not have to worry about which data set they need to look at to get the information they want: we take care of this in the server based on their user interface (UI) input.


## Longer Description with Examples

The Shiny app contains four main pages. The main goal of the application is to present the data to a viewer in a clean, easy-to-access manner. The user should ideally be able to think of a song, artist, or album and see firstly if the song is in the data set and secondly see the statistics on that song.

For Songs, there are three sub-tabs that the user can use to investigate the data set. These will be for by month data, individual statistics, and overall statistics. Individual aims to give a deeper look at any specific song and how it has performed during its time being listened to. 
Overall aims to encapsulate data about all songs. The user will be able to sort and filter their data. Each song is hyperlinked to the individual song data page. 

Artists and Albums tabs follow closely to Songs regarding the content. For the Artist tab, two sub-tabs are shown: Overall Artist Data and Individual Artist Data. These are nearly the same as the corresponding Song tabs, with the Overall Artist Data sub-tab having the ability to filter by total hours or plays along with date ranges. Currently, the album tab only lists Individual Album Data. There is minimal information shown currently.


The Miscellaneous tab tracks the songs on a playlist named Bourgeoisie created by the owner of the listening history data. This playlist is updated each month based on how much the owner is subjectively enjoying songs. So, the amount of time a song stays on the playlist is a metric of how enjoyable the song is to the owner, to some extent. There is also information in the Miscellaneous tab about yearly listening statistics for songs, albums, and artists.


**Accreditations**

I would like to acknowledge the Shiny app Living in the Lego World. Significant portions of the user interface were modeled after this app. Full citation: Kaplan, A (2019). Living in the Lego World. https://shiny.rstudio.com/gallery/lego-world.html


## How to use the app.R file



