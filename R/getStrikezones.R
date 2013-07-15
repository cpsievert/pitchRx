# Calculate strikezone boundaries
# 
# Strikezone boundaries calculated according to Mike Fast's specifications
# 
# @param data PITCHf/x orginally entered into \code{animateFX}
# @param facets variables used for faceting (passed along from \code{layer})
# @param strikeFX logical parameter indicating whether the function is called from strikeFX
# @references \url{http://www.baseballprospectus.com/article.php?articleid=14572}
# @importFrom plyr join
# @return Returns a list of boundaries for both right handed batters and left handed batters
# 
getStrikezones <- function(data, facets, strikeFX = FALSE) {
  heights=NULL #ugly hack to comply with R CMD check
  h <- ldply(strsplit(as.character(data$b_height), "-"), function(x) { as.numeric(x) })
  h[,2] <- h[,2]/12
  data$heights <- h[,1] + h[,2]
  bounds <- ddply(data, c("stand", facets), summarize, height=mean(heights))
  righty <- as.numeric(bounds$stand == "R")
  lefty <- as.numeric(bounds$stand == "L")
  bounds$top <- righty*(2.6 + bounds$height*0.136) + lefty*(2 + bounds$height*0.229)
  bounds$bottom <- righty*(0.92 + bounds$height*0.136) + lefty*(0.35 + bounds$height*0.229)
  bounds$left <- righty*-1.03 + lefty*-1.20
  bounds$right <- righty + lefty*0.81
  if (strikeFX) { #adjust vertical pitch locations
    joinby <- unique(c("stand", facets))
    data2 <- join(data, bounds, by = joinby, type = "inner")
    data2$R <- as.numeric(data$stand == "R")
    data2$L <- as.numeric(data$stand == "L")
    data2$tops <- with(data2, R*(2.6 + heights*0.136) + L*(2 + heights*0.229))
    data2$bottoms <- with(data2, R*(0.92 + heights*0.136) + L*(0.35 + heights*0.229))
    pz2 <- with(data2, R*(bottoms + (pz - bottom)*(tops - bottoms)/(top - bottom)) + 
                        L*(bottoms + (pz - bottom)*(tops - bottoms)/(top - bottom)))
    return(list(pz2, bounds))
  } else return(bounds)
}
