#!/usr/bin/env Rscript --interactive

options(stringsAsFactors=FALSE)

#library(tcltk)
library(RCurl)
library(rjson)
library(plyr)
library(lubridate)
library(whisker)

#tt <- tktoplevel()

api.key = "732e262c3c68e3a265e67145b44207a"

curl = getCurlHandle()
api = "https://api.meetup.com"
groupname = "Data-Science-DC"

page.size = 200 # not sure why this can't be higher...

csvfilename <- "meetup-jobs.csv"
htmlfilename <- "meetup-jobs.html"

header <- '
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="refresh" content="3">
<style>
td.name {font-family:serif; font-weight:bold;}
td.url {font-family:monospace;}
#logo {position: fixed; top:0px; right:10px;}
</style>
</head>
<body>
<div id="logo">
<img src="http://datacommunitydc.org/blog/wp-content/uploads/2012/08/DC2logo-cluster-name-100px.png">
</div>
<table border="0">
<!-- <tr>
  <th></th>
  <th>Meetup name</th>
  <th>URL</th>
  <th>Announcement</th>
</tr> -->
'
rowspec <- '<tr>
  <td class="icon"><img src="%s" width="64" height="64"></td>
  <td class="name">%s</td>
  <td class="url">%s</td>
  <td class="announcement">%s</td>
    </tr>'
footer <- "
</table>
</body>
</html>
"

membersfile = 'members.Rdata'
if (!file.exists(membersfile) || file.info(csvfilename)$mtime < as.POSIXct(Sys.time() - hours(1))) {
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
  save(members, file=membersfile)
} else load(membersfile)

#jobs <- data.frame(username=character(), coname=character(), jobtitle=character())

# new data flow: watch the CSV file. when it changes, pull it, pull any new 
# photo URLs, and rebuild the HTML
last_updated <- as.POSIXct(Sys.time() - years(1))
photos <- data.frame(username=character(0), url=character(0)) # map from Meetup name to URL
while (1) {
  csvinfo <- file.info(csvfilename)
  
  if (csvinfo$mtime > last_updated && csvinfo$size > 0) {
    last_updated <- csvinfo$mtime
    
    announcements <- read.csv(csvfilename)
    if (nrow(announcements) == 0) next
    
    unknown_names <- setdiff(announcements$username, photos$username)
    
    for (name in unknown_names) {
      rows <- which(members$name == name)
      if (length(rows) == 1) {
        # have a single name -- get the URL (could be NA)
        
        photourl <- members$photo[[rows]]
        if (is.na(photourl) || photourl=='') photourl <- "http://www.echoinggreen.org/sites/default/files/styles/thumbnail/public/default_images/no_fellow.png"
      } else if (length(rows) == 0) {
        message("Name ", name, " is not found -- skipping")
        photourl <- "http://www.echoinggreen.org/sites/default/files/styles/thumbnail/public/default_images/no_fellow.png"
      } else {
        message("Name ", name, " is not unique -- skipping")
        photourl <- "http://www.echoinggreen.org/sites/default/files/styles/thumbnail/public/default_images/no_fellow.png"
      }
      message("Adding ", name, "=", photourl, " to photos DF")
      photos <- rbind(photos, data.frame(username=name, photo=photourl))
    }
    
    export <- join(announcements, photos, by='username', type='left')
    export <- export[seq.int(nrow(export),1),] # new stuff at the top
    #print(export)
    
    html <- file(htmlfilename, "w")
    
    cat(header, file=html)
    a_ply(export, 1, function(rr) cat(sprintf(rowspec, 
                                              URLencode(rr$photo), 
                                              whisker.escape(if (rr$displayname=='') rr$user else rr$displayname),
                                              whisker.escape(rr$url),
                                              whisker.escape(rr$announcement)),
                                      file=html))
    cat(footer, file=html)
    
    close(html)
    # 
  }
  
  Sys.sleep(5)
}
