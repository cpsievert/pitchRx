#' Calculate strikezone boundaries
#' 
#' Strikezone boundaries calculated according to Mike Fast's specifications
#' 
#' @param data PITCHf/x orginally entered into \code{animateFX}
#' @param facets variables used for faceting (passed along from \code{layer})
#' @param strikeFX logical parameter indicating whether the function is called from strikeFX
#' @references \url{http://www.baseballprospectus.com/article.php?articleid=14572}
#' @return Returns a list of boundaries for both right handed batters and left handed batters
#' 
getStrikezones <- function(data, facets, strikeFX = FALSE) {
  h <- ldply(str_split(data$b_height, "-"), function(x) { as.numeric(x) })
  h[,2] <- h[,2]/12
  data$heights <- h[,1] + h[,2]
  bounds <- ddply(data, c("stand", facets), summarize, height=mean(heights))
  righty <- as.numeric(bounds$stand == "Batter Stands: R")
  lefty <- as.numeric(bounds$stand == "Batter Stands: L")
  bounds$top <- righty*(2.6 + bounds$height*0.136) + lefty*(2 + bounds$height*0.229)
  bounds$bottom <- righty*(0.92 + bounds$height*0.136) + lefty*(0.35 + bounds$height*0.229)
  bounds$left <- righty*-1.03 + lefty*-1.20
  bounds$right <- righty + lefty*0.81
  if (strikeFX) { #adjust vertical pitch locations
    data2 <- join(bounds, data, by = "stand", type = "inner")
    data2$R <- as.numeric(data$stand == "Batter Stands: R")
    data2$L <- as.numeric(data$stand == "Batter Stands: L")
    data2$tops <- data2$R*(2.6 + data$heights*0.136) + data2$L*(2 + data$heights*0.229)
    data2$bottoms <- data2$R*(0.92 + data$heights*0.136) + data2$L*(0.35 + data$heights*0.229)
    #data2.R <- subset(data2, R = 1)
    #lefts <- righty*-1.03 + lefty*-1.20
    #rights <- righty + lefty*0.81
    data3 <- ddply(data2, .(stand), summarize, 
                                pz2 = bottoms + (pz - bottom)*((tops - bottoms)/(top - bottom)))
    return(list(data3$pz2, bounds))
  } else return(bounds)
}
