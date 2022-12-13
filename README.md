SAAM Description
================

## Shiny app

The app itself can be found here:

[https://mollyc.shinyapps.io/saam/]

## Data Set

The 'data set' is more of a database, as there are four separate Google Sheets with possible data sources. Most of the data is stored in the "Songs" Google Sheet, though one of the requested items for the Music Database is to make a display of the songs that were featured in the `Bourgeoisie' playlist, and that information is stored in the "Miscellaneous" Google Sheet.

There are many variables in each of the files. Common variables acorss each file are: Song Name, Artist, Album, Hours (or some form of this), Play Count (or some form of this). Other variables are Date Added, Max Rank, Hour Differential, Play Differential, Total (all-time) Hours, Total (all-time) Plays, and others. There are more than 2000 songs in the database, and more are added each month.

## Short Project Description

- There is **a lot** of data and even to a trained individual who knows what they are looking for, it can be overwhelming.
- An R Shiny app makes the experience much more user friendly and much more exploratory. 
  - The user could, for example, search for statistics for a single song they like without having to know much of anything on how R or data filtering works.
- An R Shiny app also helps with managing all of the data sets at once. Since all of the hard work is done behind the visage of the app, users will not have to worry about which data set they need to look at to get the information they want: we take care of this in the server based on their user interface (UI) input.


## Topics Explored Using the Data Set

-Make a searchable database of listening statistics from August 2013 - present

-Add some nice graphics (artist trends, yearly/monthly trends, etc)

-Provide a client-requested interface that can be hosted in a Shiny app and used as a replacement for the current Google Sheets method.

