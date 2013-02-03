#!/usr/bin/env Rscript --interactive

options(stringsAsFactors=FALSE)

library(tcltk)

jobs <- data.frame(username=character(), coname=character(), jobtitle=character())

tt <- tktoplevel()


while (1) {
  username <- readline("Meetup user name: ")
  coname <-   readline("Company name    : ")
  jobtitle <- readline("Job title       : ")
  
  # pull Meetup user
  
  # if it's OK
  if (TRUE) {
    yn <- readline(sprintf("Add %s, %s, %s? (y/n/q) ", username, coname, jobtitle))
    if (tolower(yn) == 'y') {
      jobs <- rbind(jobs, data.frame(username, coname, jobtitle))
      
      # update screen
      tkgrid(tklabel(tt,text=sprintf("%s, %s, %s", username, coname, jobtitle)))
    } else if (tolower(yn) == 'q') {
      break
    }
  }
}