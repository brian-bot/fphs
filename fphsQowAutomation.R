require(bridger)
bridgeLogin()
require(synapseClient)
synapseLogin()
require(RCurl)
require(rjson)

## ENUMERATE THE TABLES THAT ARE AVAILABLE IN SYNAPSE
tts <- synTableQuery('SELECT DISTINCT originalTable FROM syn5807323')
q <- synQuery('SELECT id, name FROM table WHERE parentId=="syn5518047"')
q <- q[ q$table.name %in% tts@values$originalTable, ]

## GO TO BRIDGE TO SEE WHAT HAS BEEN SCHEDULED AND USE THAT TO REFER BACK TO TABLES ABOVE
sch <- bridgeRestGET('/v3/scheduleplans')

qowSchedules <- lapply(sch$items, function(x){
  if( !grepl("qow", tolower(x$label), fixed = TRUE) ){
    return(NULL)
  } else{
    return(data.frame(guid = x$strategy$schedule$activities[[1]]$survey$guid,
                endDate = as.Date(x$strategy$schedule$endsOn),
                stringsAsFactors=FALSE))
  }
})
qowSchedules <- do.call(rbind, qowSchedules)
## KEEP ONLY THOSE WHICH HAVE COMPLETED
qowSchedules <- qowSchedules[ qowSchedules$endDate < Sys.Date(), ]

## LOOK THROUGH SCHEDULES - AND COUNT THE NUMBER THAT WE WRITE TO BRIDGE
nn <- as.integer(0)
for(y in 1:nrow(qowSchedules)){
  x <- qowSchedules$guid[y]
  publishDate <- qowSchedules$endDate[y]+1
  if(weekdays(publishDate) != "Monday"){
    message(paste("Day of the week is", weekdays(publishDate), "- omitting results"))
    return(NULL)
  }
  aa <- bridgeRestGET(paste0('/v3/surveys/', x, '/revisions/published'))
  allOptions <- sapply(aa$elements[[1]]$constraints$enumeration, "[[", "value")
  
  ## GRAB THE APPROPRIATE SYNAPSE TABLE
  idx <- grep(paste0("-", aa$identifier, "-"), q$table.name, fixed=TRUE)
  if(length(idx) != 1){
    ## A KNOWN CASE WITH TWO VERSIONS
    if(aa$identifier == "qow17"){
      dd1 <- synTableQuery(paste0('SELECT * FROM ', q$table.id[ q$table.name == "fphs-qow17-v1" ]))@values
      dd2 <- synTableQuery(paste0('SELECT * FROM ', q$table.id[ q$table.name == "fphs-qow17-v2" ]))@values
      dd1$qowq1 <- dd1$qow17q1
      dd1$qow17q1 <- NULL
      dd <- rbind(dd1, dd2)
    } else{
      next
    }
  } else{
    dd <- synTableQuery(paste0('SELECT * FROM ', q$table.id[idx]))@values
  }
  dd <- dd[ !grepl("test_user", dd$dataGroups, fixed=TRUE), ]
  dd[[aa$elements[[1]]$identifier]] <- sub('[', '', sub(']', '', sub('["', '', sub('"]', '', dd[[aa$elements[[1]]$identifier]], fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE)
  ## BECAUSE THE APPS DO WEIRD THINGS WITH YES/NO ANSWERS
  dd[[aa$elements[[1]]$identifier]][ dd[[aa$elements[[1]]$identifier]] == 'true' ] <- "Yes"
  dd[[aa$elements[[1]]$identifier]][ dd[[aa$elements[[1]]$identifier]] == 'false' ] <- "No"
  dd[[aa$elements[[1]]$identifier]] <- factor(dd[[aa$elements[[1]]$identifier]], levels = allOptions)
  
  ## SPLIT BY FOOTBALL PLAYER AND CONTROL
  football_player <- table(dd[grepl("football_player", dd$dataGroups, fixed=TRUE), aa$elements[[1]]$identifier])
  football_player <- football_player/sum(football_player)*100
  control <- table(dd[!grepl("football_player", dd$dataGroups, fixed=TRUE), aa$elements[[1]]$identifier])
  control <- control/sum(control)*100
  
  ## PUT IN JSON FORMAT
  res <- list(date = as.character(publishDate),
              data = list(
                question = aa$elements[[1]]$prompt,
                football_player = lapply(as.list(1:length(football_player)), function(z){list(answer=names(football_player)[z], percent=round(unname(football_player)[z], 0))}),
                control = lapply(as.list(1:length(control)), function(z){list(answer=names(control)[z], percent=round(unname(control)[z], 0))})
              ))
  
  # return(res)
  allDone <- bridgeRestPOST('/v3/reports/weekly-survey', body=toJSON(res))
  nn <- nn+1
  cat(nn)
}

## UPDATE THE TRACKING TABLE IN SYNAPSE (ADD A ROW)
updateTrackr <- data.frame(runDate = as.character(Sys.Date()),
                           numQOWs = nn,
                           stringsAsFactors=FALSE)

reallyAllDone <- synStore(Table("syn7070736", updateTrackr))
