#' Parse XML files into data frame(s)
#' 
#' This function takes on a list of XML files (ie, urls) and shapes them into a data frame or list of data frames
#' 
#' \code{urlsToDataFrame} coerces either XML attributes or XML values into a data frame. The XML nodes (aka, tags) of interest 
#' need to be specified as the name(s) of the \code{tables} parameter. The values of each \code{tables} parameter should be a 
#' character vector that defines the field names for the respective data frame. These field names should match XML attributes or tags.
#' 
#' When \code{use.values = FALSE}, the length of \code{tables} is equal to the number of data frames returned and 
#' the values of \code{tables} are the fields for each data frame. If a particular value of \code{tables} is \code{NULL}, 
#' the function will automatically determine the most complete set of fields and fill in \code{NA}s where 
#' information is missing. If \code{add.children = TRUE}, \code{tables} values should be \code{NULL} since 
#' child attributes will be used for naming convention (with the relevant node as the suffix name). 
#' 
#' When \code{use.values = TRUE}, the value(s) of \code{tables} are ignored. The XML children of the specified node
#' are the fields. If the children are inconsistent, missing values are filled with \code{NA}s.
#' 
#' @param urls set of urls for parsing
#' @param tables list of character vectors with appropriate names. The list names should correspond to XML nodes of interest within the XML files.
#' @param add.children logical parameter specifying whether to scrape the XML children of the node(s) specified in \code{tables}.
#' @param use.values logical parameter specifying whether to extract XML attributes or values of the node(s).
#' @return Returns a data frames if the length of tables is one. Otherwise, it returns a list of data frames.
#' @export
#' @examples
#' \dontrun{Obtain "batting" stats going into a game played on May 6th, 2008:
#' data(urls)
#' dir <- gsub("players.xml", "batters/", 
#'             urls$url_player[1000])
#' doc <- htmlParse(dir)
#' nodes <- getNodeSet(doc, "//a")
#' values <- gsub(" ", "", 
#'                sapply(nodes, xmlValue))
#' ids <- values[grep("[0-9]+", values)]
#' filenames <- paste(dir, ids, sep="")
#' stats <- urlsToDataFrame(filenames, 
#'                          tables=list(Player=NULL), 
#'                          add.children=TRUE)}

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
    is.frame <- is.data.frame(frame)
    if (is.frame) {
      frame <- list(frame)
      attr(frame, "names") <- j
      frames <- c(frames, frame) #frames should ALWAYS be a list after 1st iteration
    } else { #frame can also be a list
      frames <- c(frames, frame) 
    }
    ctr <- ctr + 1
  }
  if (length(frames) == 1) {
    return(frames[[1]]) #Return just the df, instead of a list of one df
  } else {
    return(frames)
  }
}

# Turns a list of XML documents into a single data frame.
# 
# This function will determine the most complete amount of fields among all 
# XML documents and fill NAs where information is missing.

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

# Turn XML documents into a Data Frames
# 
# This function adds NAs to missing attributes.
#
# @param docs XML documents
# @param node XML node of interest
# @param fields "Comlpete" set of field names for the data frame
# @param urls character vector of URLs used to build data frame
# @param add.children logical parameter specifying whether to scrape the XML children of the node(s) specified in \code{tables}.
# @param use.values logical parameter specifying whether to extract XML attributes or values of the node(s).
# @return Returns a data frame.

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
        #if (node == "Player"){ #The children "faced" and "atbats" (of "Player") are obsolete since they can be derived from the Pitch F/X data
        #  child[c(-grep("faced", names(child)),-grep("atbats", names(child)))]
        #}
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
    return(list(atbat = final, atbat_id = atbat.id))
  } else {
    return(final)
  }
}

# "Adjust" attributes to match the entire set
# 
# This function adds NAs to missing attributes.
#
# @param info XML attributes from a particular node.
# @param tags "complete" set of attribute names.
# @return returns all present info matching the tags criteria

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

# Assign each pitch an atbat ID
#
# @param nodes XML nodes from a set of URLs. These nodes should be from the "atbat" node.
# @return returns a vector contains an atbat ID for each pitch thrown

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

# Create columns to match an atbat with a inning (and side of that inning)
#
# @param atbats data frame with attributes from the atbat node
# @return returns the original data frame with two additional columns

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
