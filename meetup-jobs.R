#!/usr/bin/env Rscript --interactive

options(stringsAsFactors=FALSE)

#library(tcltk)
library(RCurl)
library(rjson)
library(plyr)
library(lubridate)

#tt <- tktoplevel()

api.key = "732e262c3c68e3a265e67145b44207a"

curl = getCurlHandle()
api = "https://api.meetup.com"
groupname = "Data-Science-DC"

page.size = 200 # not sure why this can't be higher...

csvfilename <- "meetup-jobs.csv"

# get our group info
getGroup <- function (api, api.key, curl, groupname) {
  service = "2/groups"
  request.str = "%s/%s?key=%s&sign=true&group_urlname=%s"
  request <- sprintf(request.str, api, service, api.key, groupname)
  group.json <- getURL(request, curl=curl)
  group <- fromJSON(group.json)$results[[1]]
  list(members=group$members, 
       id=group$id)
}
group <- getGroup(api, api.key, curl, groupname)

# pre-get all members. Care about the name and the photo info only, really
null2na <- function(x) if (is.null(x)) NA else x
getSomeMembers <- function(api, api.key, curl, dsdc.id, offset=0, page=page.size) {
  service = "2/members"
  request.str = "%s/%s?key=%s&sign=true&fields=photo&group_id=%d&page=%d&offset=%d"
  request = sprintf(request.str, api, service, api.key, dsdc.id, page=page, offset=offset)
  #print(request)
  members.json <- getURL(request, curl=curl)
  members <- fromJSON(members.json)
  df.members <- ldply(members$results, function(m) data.frame(name=m$name, 
                                                              member_id=m$id,
                                                              bio=null2na(m$bio),
                                                              photo=null2na(m$photo$photo_link)
  ))
  df.members
}
members <- ldply(seq.int(from=0, to=floor(group$members/page.size)),
                 function(o) getSomeMembers(api, api.key, curl, group$id, offset=o, page=page.size))

#jobs <- data.frame(username=character(), coname=character(), jobtitle=character())

# new data flow: watch the CSV file. when it changes, pull it, pull any new 
# photo URLs, and rebuild the HTML
last_updated <- as.POSIXct(Sys.time() - years(1))
photos <- data.frame(username=character(0), url=character(0)) # map from Meetup name to URL
while (1) {
  mod_date <- file.info(csvfilename)$mtime
  
  if (mod_date > last_updated) {
    last_updated <- mod_date
    
    announcements <- read.csv(csvfilename)
    
    unknown_names <- setdiff(announcements$username, photos$username)
    
    for (name in unknown_names) {
      rows <- which(members$name == name)
      if (length(rows) == 1) {
        # have a single name -- get the URL (could be NA)
        message("Adding ", name, " to photos DF")
        photos <- rbind(photos, data.frame(username=name, url=members$photo[[rows]]))
      } else if (length(rows) == 0) {
        warning("Name ", name, " is not found -- skipping")
      } else {
        warning("Name ", name, " is not unique -- skipping")
      }
    }
    
    export <- join(announcements, photos, by='username', type='left')
    print(export)
    # 
  }
  
  Sys.sleep(5)
}

# while (1) {
#   username <- readline("Meetup user name: ")
#   coname <-   readline("Company name    : ")
#   jobtitle <- readline("Job title       : ")
#   
#   if (username == "") break
#   
#   # is this a Meetup user?
#   rows <- agrep(username, members$name)
#   if (length(rows) > 1) {
#     message("Do you mean one of these?")
#     print(members[rows,c("name", "bio")])
#     thisrow <- readline("Row name/number: ")
#     if (!is.na(thisrow) && thisrow %in% rownames(members[rows,])) {
#       rows <- which(thisrow == rownames(members))
#     }
#   }
#   if (length(rows) == 1) {
#     username <- members$name[rows]
#     photourl <- members$url[rows]
#   } else {
#     message("No matching user!")
#     next
#   }
#   
#   # if it's OK
#   if (TRUE) {
#     yn <- readline(sprintf("Add %s, %s, %s? (y/n/q) ", username, coname, jobtitle))
#     if (tolower(yn) == 'y') {
#       #jobs <- rbind(jobs, data.frame(username, coname, jobtitle))
#       
#       # can we pull a photo?
#       
#       # update screen
#       tkgrid(tklabel(tt,text=sprintf("%s, %s, %s", username, coname, jobtitle)))
#     } else if (tolower(yn) == 'q') {
#       break
#     }
#   }
# }