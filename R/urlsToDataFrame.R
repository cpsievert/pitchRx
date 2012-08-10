#' Parse XML files into data frame(s)
#' 
#' This function takes on a list of XML file names (ie, urls) and parses them into an appropriate amount of data frames.
#' 
#' This function can coerce either XML attributes or XML values into a data frame. The node(s) of interest need to be
#' specified as the name(s) of \code{tables}. 
#' 
#' When \code{use.values = FALSE}, the length of \code{tables} is equal to the number of data frames returned and 
#' the values of \code{tables} are the fields for each data frame. If a particular value is \code{NULL}, 
#' the function will automatically determine the most complete set of fields and fill in \code{NA}s where 
#' information is missing. If \code{add.children = TRUE}, it is recommended that \code{tables} values be 
#' \code{NULL} since child attributes will also be incorporated as fields (with the relevant node as 
#' the suffix name). 
#' 
#' When \code{use.values = TRUE}, the values of \code{tables} is ignored. The XML children of the specified node
#' are the fields. If the children are inconsistent, missing values are filled with \code{NA}s.
#' 
#' @param urls set of urls for parsing
#' @param tables list of character vectors with appropriate names. The list names should correspond to XML nodes of interest within the XML files.
#' @param add.children logical parameter specifying whether to scrape the XML children of the node(s) specified in \code{tables}.
#' @param use.values logical parameter specifying whether to extract XML attributes or values of the node(s).
#' @return Returns a data frames if the length of tables is one. Otherwise, it returns a list of data frames.
#' @export
#' @examples
#' #Get Josh Hamilton's stats going into a game played on July 18th, 2009:
#' url <- "http://gd2.mlb.com/components/game/mlb/year_2009/month_07/day_18/gid_2009_07_18_minmlb_texmlb_1/batters/285078.xml"
#' urlsToDataFrame(url, tables = list(Player = NULL), add.children = TRUE)
#' 
#' #Find XML files with twitter data
#' branch <- "http://gd2.mlb.com/components/game/mlb/twitter/"
#' doc <- htmlParse(branch)
#' nodes <- getNodeSet(doc, "//a")
#' values <- sapply(nodes, xmlValue)
#' extensions <- str_extract_all(values, "([a-z]+)InsiderTweets.xml.([0-9]+)")
#' twitter.urls <- paste(branch, extensions[llply(extensions, length) > 0], sep = "")
#' #Parse the files into a data frame
#' tweets <- urlsToDataFrame(urls = twitter.urls, tables = list(status = NULL), use.values = TRUE)

urlsToDataFrame <- function(urls, tables = list(), add.children = FALSE, use.values = FALSE) {
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
  if (use.values == TRUE) return(docsToDFs(docs))
  ctr <- 1
  frames <- NULL
  for (j in names(ordered.tables)) {
    fields <- unlist(ordered.tables[[ctr]])
    frame <- docsToDataFrame(docs = docs, node = j, fields = fields, urls = url.vector, add.children = add.children, use.values = use.values)
    if (j == "game") { 
      frame <- attachUrls(frame) #Add a date and URL column to games table (note: other nodes can be named "game", but don't have relevant info)
      frames$game <- frame
    }
    if (j %in% c("player", "coach", "umpire")) {
      names(frame) <- gsub("url", "url_player", names(frame))
      if (j == "player") frames$player <- frame
      if (j == "coach") frames$coach <- frame
      if (j == "umpire") frame$umpire <- frame
    }
    if (j == "runner") frames$runner <- frame
    if (j == "pitch" & ("atbat" %in% names(tables))) {
      frame$num <- frames$pitch #attach 'num' column to 'pitch' df (generated from the atbat node)
      frame$count <- addPitchCount(frame)
      frames$pitch <- frame
    }
    if (j == "atbat") {
      if ("pitch" %in% names(tables)) { #Different handling for frames, since frame is a list of two in this case
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

#' Turns a list of XML documents into a single data frame.
#' 
#' This function will determine the most complete amount of fields among all 
#' XML documents and fill NAs where information is missing.
#'
#' @param docs XML documents
#' @return returns a data frame

docsToDFs <- function(docs) {
  frames <- llply(docs, function(x) { xmlToDataFrame(x) }) #should I add better functionality to handle children better?
  all.fields <- unique(unlist(llply(frames, names)))
  missing <- llply(frames, function(x) { all.fields[!(all.fields %in% names(x))] })
  assemble <- function(x, y) {
    if (length(y) > 0) {
      x[y] <- NA
      list(x)
    } else {
      list(x)
    }
  }
  dfs <- mapply(assemble, frames, missing)
  data <- ldply(dfs, identity)
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
  counts <- llply(nodes, function(x) { length(x) }) #Create url column to identify where the observation originated.
  url.column <- rep(urls, counts)
  final$url <- url.column
  if (node == "atbat") { 
    final <- createInnings(final) 
    atbat.id <- createAtbatID(nodes) #Identify which atbat each pitch belongs to (eventually, use to link together as 'num')
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
#' @return returns all present info matching the tags criteria

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

#' Add columns with relevant "~/miniscoreboard.xml", "~/inning/inning_all.xml" and "~/player.xml" 
#' file names to games table.
#'
#' @param df data frame with all "game" attributes from "~/miniscoreboard.xml" files.
#' @return returns the original data frame with the proper url columns attached at the end.

attachUrls <- function(df) {
  names(df) <- gsub("url", "url_scoreboard", names(df))
  branch <- gsub("miniscoreboard.xml", "", df$url_scoreboard) #common branch among urls
  df$url <- paste(branch, paste("gid_", df$gameday_link, sep = ""), "/inning/inning_all.xml", sep = "") #files with pitchf/x info
  df$url_scores <- gsub("inning_all.xml", "inning_Scores.xml", df$url) #files with scoring details
  df$url_player <- gsub("/inning/inning_all.xml", "/players.xml", df$url) #files with player information and statistics
  df$date <- sapply(str_split(df$gameday_link, "_"), function(x) { paste(x[2], x[3], x[4], sep = "/") })
  return(df)
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

#' Add columns with relevant pitch count to the 'pitch' data frame.
#'
#' @param df data frame with all "pitch" attributes from "~/inning/inning_all.xml" files.
#' @return returns the original data frame with the proper pitch count columns attached at the end.
#' @export

addPitchCount <- function(df) {
  df$balls <- as.numeric(df$type == "B")
  df$strikes <- as.numeric(df$type == "S")
  counts <- dlply(idata.frame(df[,c("url", "num", "type", "balls", "strikes")]), c("url", "num"), function(x) { 
    n <- nrow(x) 
    cbind(cumsum(c(0, x$balls[-n])), pmin(cumsum(c(0, x$strikes[-n])), 2)) 
  })
  counts <- llply(counts, as.data.frame)
  counts <- ldply(counts, rbind)
  return(paste(counts[,2], counts[,3], sep = "-"))
}