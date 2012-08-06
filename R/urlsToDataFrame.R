#' Function for parsing urls and specified nodes into data frames
#' 
#' The primary use for this function is to scrape an "atbats" table and the corresponding "pitch" 
#' (ie, Pitch F/X) table for the specified set of URLs. In fact, this function is used as the core 
#' functionality behind scrapePitchFX. This function provides added flexibility by allowing 
#' one to specify nodes of interest other than "atbat" and "pitch". If the value of the list is NULL,
#' this function will automatically generate the most complete collection of data points.
#' Important: You must have "atbat" AND "pitch" nodes if you want to identify who threw a 
#' particular pitch. Also, if you specify field names for the table, you should be confident that those
#' are the most complete set of fields.
#' 
#' @param urls set of urls for parsing
#' @param tables list of character vectors containing field names for each table. The list names have to correspond to XML nodes of interest within the XML files.
#' @param add.children logical parameter specifying whether to scrape the XML children of the node(s) specified in \code{tables}.
#' @param use.values logical parameter specifying whether to extract XML attributes or values of the node(s).
#' @return Returns a data frames if the length of tables is one. Otherwise, it returns a list of data frames.
#' @export
#' @examples
#' #If it isn't currently baseball season, consider changing the dates below:
#' #Also, this is a small scaled example. Visit my website if you would like to see how to 
#' #build a current and complete database.
#' #mini.urls <- getScoreboardURLs(first.date = Sys.Date() - 10, last.date = Sys.Date())
#' #game.urls <- getPitchFxURLs(mini.urls)
#' #data <- urlsToDataFrame(urls = game.urls)
#' #atbats <- data$atbat
#' #pitches <- data$pitch
#' 

urlsToDataFrame <- function(urls, tables = list(atbat = NULL, pitch = NULL), add.children = FALSE, use.values = FALSE) {
  #Order tables alphabetically. This is important because the atbat node must be parsed first if you want an atbat ID for the pitch table 
  orders <- order(names(tables))
  ordered.tables <- llply(orders, function(x) { tables[[x]] })
  names(ordered.tables) <- names(tables)[orders]
  docs <- NULL
  url.vector <- NULL
  for (i in urls) {
    cat(i, "\n")
    doc <- try_default(xmlParse(i), NULL, quiet = TRUE)
    if (!is.null(doc)) {
      docs <- c(docs, doc) #Keep non-empty documents
      url.vector <- c(url.vector, i) #Keep urls that have content
    }
  }
  #Turn the XML 'documents' into a list of data frames
  ctr <- 1
  frames <- NULL
  for (j in names(ordered.tables)) {
    fields <- unlist(ordered.tables[[ctr]])
    frame <- docsToDataFrame(docs = docs, node = j, fields = fields, urls = url.vector, add.children = add.children, use.values = use.values)
    if (j == "game") { 
      frame <- attachUrls(frame) #Add a date and URL column to games table (note: other nodes can be named "game", but don't have relevant info)
      frames$game <- frame
    }
    if (any(j == c("player", "coach", "umpire"))) {
      names(frame) <- gsub("url", "url_player", names(frame))
      if (j == "player") frames$player <- frame
      if (j == "coach") frames$coach <- frame
      if (j == "umpire") frame$umpire <- frame
    }
    if (j == "runner") frames$runner <- frame
    if (j == "pitch" & any(names(tables) == "atbat")) {
      frame$num <- frames$pitch #attach 'num' column to 'pitch' df (generated from the atbat node)
      frames$pitch <- frame
      #Add balls and strikes!!!!
    }
    if (j == "atbat") {
      if (any(names(tables) == "pitch")) { #Different handling for frames, since frame is a list of two in this case
        frames$pitch <- frame$atbat_id #Vector to be added to 'pitch' df to link with 'atbat' df
        frames$atbat <- frame$final #Keep the atbats df
      } else {
        frames$atbat <- frame
      }
    } 
    ctr <- ctr + 1
  }
  if (length(tables) > 1) {
    return(frames)
  } else {
    return(frame)
  }
}

#' Turn XML documents into a Data Frames
#' 
#' This function adds NAs to missing attributes.
#'
#' @param docs XML documents
#' @param node XML node of interest
#' @param fields "Comlpete" set of field names for the data frame
#' @param urls character vector of URLs used to build data frame
#' @param add.children logical parameter specifying whether to scrape the XML children of the node(s) specified in \code{tables}.
#' @param use.values logical parameter specifying whether to extract XML attributes or values of the node(s).
#' @return Returns a data frame.

docsToDataFrame <- function(docs, node, fields, urls, add.children = FALSE, use.values = FALSE) {
  nodes <- llply(docs, function(x) { 
    getNodeSet(x, paste("//", node, sep=""))
  })
  attributes <- llply(nodes, function(x) { 
    if (length(x) > 0) { #Check that each node has at least some info
      llply(x, function(y) { 
        xmlAttrs(y) #Grab all the attributes from each node
      })
    }
  })
  if (add.children == TRUE) { #Scrape XML children
    children <- llply(nodes, function(x) {
                  llply(x, function(y) {
                    child <- xmlChildren(y)
                    if (node == "Player"){ #The children "faced" and "atbats" (of "Player") are obsolete since they can be derived from the Pitch F/X data
                      child[c(-grep("faced", names(child)),-grep("atbats", names(child)))]
                    }
                  })
                })
    attrs <- llply(children, function(x) {
                llply(x, function(y) {
                  llply(y, function(z) {
                    child.attr <- xmlAttrs(z)
                  })
                })
              })
    attrs <- llply(attrs, function(x) { list(unlist(x)) }) #restructure attrs into a similar form to attributes
    attributes <- mapply(function(x, y) { list(unlist(append(x, y))) }, attributes, attrs) #Combine tags from both
    attributes <- llply(attributes, list) #Return to a identical form that attributes was originally
  } 
  if (is.null(fields)) { #If field names aren't provided, find them.
    namez <- llply(attributes, function(x) { 
      llply(x, function(y) {
        names(y)
      })
    })
    fields <- unique(unlist(namez))
  }
  data <- llply(attributes, function(x) { #Add missing tags to a particular node and fill them with NAs
    llply(x, function(y) { 
      if (length(y) > 0) {
        adjust(y, fields) 
      }
    }) 
  })
  final <- ldply(data, function(x) { #Coerce all the data from a list of lists to one big dataframe
    ldply(x, function(y) { 
      y
      }) 
  })
  #Create url column to identify where the observation originated.
  counts <- llply(nodes, function(x) { length(x) })
  url.column <- rep(urls, counts)
  final$url <- url.column
  if (node == "atbat") {
    final <- createInnings(final) 
    atbat.id <- createAtbatID(nodes)
    return(list(final = final, atbat_id = atbat.id))
  } else {
    return(final)
  }
}

#' "Adjust" attributes to match the entire set
#' 
#' This function adds NAs to missing attributes.
#'
#' @param info XML attributes from a particular node.
#' @param tags "complete" set of attribute names.

#Adjust function used inside of UrlsToDataFrame
adjust <- function(info, tags){ #Adds NAs wherever a tag is missing
  x <- names(info)
  y <- tags
  z <- match(x, y)
  w <- z[!is.na(z)] #get rid of elements in info that doesn't match tags (allows fields to be flexible)
  a <- rep(NA, length(tags))
  if (length(w) < length(z)) {
    relevant.info <- info[which(!is.na(z))]
    a[w] <- relevant.info
  } else {
    a[z] <- info
  }
  names(a) <- tags
  return(a)
}

#' Create columns to match an atbat with a inning (and side of that inning)
#'
#' @param atbats data frame with attributes from the atbat node
#' @return returns the original data frame with two additional columns

createInnings <- function(atbats) {
  atbats <- atbats[order(atbats[,"url"]),]
  atbats$o <- as.integer(atbats$o)
  inning <- with(atbats, tapply(o, INDEX = url, function(x) { swap <- diff(c(x,-1)) #-1 is added to account for walk-offs/quirky endings
                                                 logic <- swap < 0 #Return true whenever we switch sides of an inning
                                                 places <- which(logic) #Which indices are TRUE?
                                                 b <- c(places[1], diff(places)) #Atbats for each side of an inning
                                                 if(length(b)/2 != floor(length(b)/2)) b <- c(b, 0) #Add zero if the bottom inning was not played
                                                 atbatsPerSide <- matrix(b, ncol = 2, byrow = T) #Column1 = "Top", Column2 = "Bottom"
                                                 innings <- dim(atbatsPerSide)[1] #num of innings in the game
                                                 counts <- apply(atbatsPerSide, 1, sum) #num of atbats per inning
                                                 return(rep(1:innings, counts)) #inning ID
  }))
  innings <- as.numeric(unlist(inning))
  top.inning <- with(atbats, tapply(o, INDEX = url, function(x) { swap <- diff(c(x,-1)) #-1 is added to account for walk-offs/quirky endings
                                                      logic <- swap < 0 #Return true whenever we switch sides of an inning
                                                      places <- which(logic) #Which indices are TRUE?
                                                      b <- c(places[1], diff(places)) #Atbats for each side of an inning
                                                      if(length(b)/2 != floor(length(b)/2)) b <- c(b, 0) #Add zero if the bottom inning was not played
                                                      atbatsPerSide <- matrix(b, ncol = 2, byrow = T) #Column1 = "Top", Column2 = "Bottom"
                                                      z <- apply(atbatsPerSide, 1, function(x) { rep(c("Y", "N"), x)} )
                                                      return(unlist(z))
  }))
  top.innings <- as.character(unlist(top.inning))
  return(cbind(atbats, inning = innings, top_inning = top.innings))
}
  
#' Assign each pitch an atbat ID
#'
#' @param nodes XML nodes from a set of URLs. These nodes should be from the "atbat" node.
#' @return returns a vector contains an atbat ID for each pitch thrown

createAtbatID <- function(nodes) {
  p.per.ab <- llply(nodes, function(x) { 
    llply(x, function(y) { 
      sum(as.numeric(names(xmlChildren(y)) == "pitch")) 
    })
  })
  atbat.records <- llply(p.per.ab, function(x) {
    if (length(x) > 0) mapply(rep, 1:length(x), x)
  })
  return(unlist(atbat.records, use.names=FALSE))
}

#' Add columns with relevant "~/miniscoreboard.xml", "~/inning/inning_all.xml" and "~/player.xml" 
#' file names to games table.
#'
#' @param df data frame with all "game" attributes from "~/miniscoreboard.xml" files.
#' @return returns the original data frame with the proper url columns attached at the end.
#'

attachUrls <- function(df) {
  names(df) <- gsub("url", "url_scoreboard", names(df))
  branch <- gsub("miniscoreboard.xml", "", df$url_scoreboard) #common branch among urls
  df$url <- paste(branch, paste("gid_", df$gameday_link, sep = ""), "/inning/inning_all.xml", sep = "") #files with pitchf/x info
  df$url_scores <- gsub("inning_all.xml", "inning_Scores.xml", df$url) #files with scoring details
  df$url_player <- gsub("/inning/inning_all.xml", "/players.xml", df$url) #files with player information and statistics
  df$date <- sapply(str_split(df$gameday_link, "_"), function(x) { paste(x[2], x[3], x[4], sep = "/") })
  return(df)
}

#' Add columns with relevant pitch count to the 'pitch' data frame.
#'
#' @param df data frame with all "pitch" attributes from "~/inning/inning_all.xml" files.
#' @return returns the original data frame with the proper pitch count columns attached at the end.

# addPitchCount <- function(df) {
#   with(df, tapply(type, INDEX = ))
# }

# #replace pitches with df
# balls <- as.numeric(pitches$type == "B")
# strikes <- as.numeric(pitches$type == "S")
# #contact <- as.numeric(pitches$type == "X")
# pitches <- cbind(pitches, balls, strikes, contact)
# stuff <- dlply(idata.frame(pitches[,c("url", "num", "type", "balls", "strikes")]), c("url", "num"), function(x) { 
#   cbind(cumsum(x$balls), cumsum(x$strikes)) })
# head(stuff)
# #NOTE: "S" includes fouls, "X" means it is in play!
# pitches[pitches$type == "X",c("des")]