#' Visualize PITCHf/x strikezones
#' 
#' A suite of bivariate plots with "px" on the horizontal axis and "pz" on the vertical axis.
#'
#' @param data PITCHf/x data to be visualized.
#' @param geom plotting geometry. Current choices are: "point", "hex", "bin" and "tile" 
#' @param contour logical. Should contour lines be included?
#' @param point.size Size of points (when geom="point")
#' @param point.alpha plotting transparency parameter (when geom="point").
#' @param color variable used to define coloring scheme.
#' @param fill variable used to define subplot scheme (when geom="subplot2d").
#' @param layer list of other ggplot2 (layered) modifications.
#' @param model Either a \link{gamObject} or a call to fit a model via \link{gam} or \link{bam}.
#' Note that the horizontal and vertical location of the pitch MUST be included as covariates named "px" and "pz", respectively.
#' Relevant factor variables must also be included as covariates in order to produce facetted or differenced plot(s).
#' If this option is used, the geometry must be either "hex", "tile" or "bin". If a non-valid geometry is used, the geometry will be forced to "tile".
#' @param model.save logical. Save the fitted \code{model}? If TRUE, the relevant model object is \link{assign}ed to the global environment
#' @param density1 List of length one. The name should correspond to a variable in \code{data}. The value should correspond to an (observed) value of that variable.
#' @param density2 Similar to \code{density1}. If \code{density1 != density2}, the relevant estimates are automatically differenced.
#' @param limitz limits for horizontal and vertical axes. 
#' @param adjust logical. Should vertical locations be adjusted according to batter height?
#' @param draw_zones logical. Should strikezones be included?
#' @param parent is the function being called from a higher-level function? (experimental)
#' @param ... extra options passed onto geom commands
#' @return Returns a ggplot2 object.
#' @export
#' @import mgcv
#' @import ggplot2
#' @import hexbin
#' @importFrom MASS kde2d
#' @examples
#' data(pitches)
#' p <- strikeFX(pitches, geom="tile", layer=facet_grid(.~stand))
#' p+theme(aspect.ratio=1)
#' \dontrun{
#' strikeFX(pitches, geom="hex", density1=list(des="Called Strike"), density2=list(des="Ball"), 
#'          draw_zones=FALSE, contour=TRUE, layer=facet_grid(.~stand))
#'          
#' noswing <- subset(pitches, des %in% c("Ball", "Called Strike"))
#' noswing$strike <- as.numeric(noswing$des %in% "Called Strike")
#' library(mgcv)
#' m1 <- bam(strike ~ s(px, pz, by=factor(stand)) + 
#'                factor(stand), data=noswing, family = binomial(link='logit'))
#' strikeFX(noswing, model=m1, layer=facet_grid(.~stand))
#' 
#' #If sample size is an issue, try increasing the binwidths
#' strikeFX(noswing, model=m1, layer=facet_grid(.~stand), binwidth=c(.5,.5))
#' m2 <- mgcv::bam(strike ~ s(px, pz, by=factor(stand)) + s(px, pz, by=factor(top_inning)) + 
#'            factor(stand) + factor(top_inning), data=noswing, family = binomial(link='logit'))
#' strikeFX(noswing, geom="bin", model=m2, density1=list(top_inning="Y"), 
#'          density2=list(top_inning="N"), layer=facet_grid(.~stand), binwidth=c(.5, .5))
#' }
#' 

strikeFX <- function(data, geom = "point", contour=FALSE, point.size=3, point.alpha=1/3, color = "pitch_types", fill = "des", layer = list(), model, model.save=TRUE, density1=list(), density2=list(), limitz=c(-2, 2, 0.5, 4.5), adjust=FALSE, draw_zones=TRUE, parent=FALSE, ...){ 
  px=pz_adj=..density..=top=bottom=right=left=x=y=z=NULL #ugly hack to comply with R CMD check
  if (any(!geom %in% c("point", "bin", "hex", "tile", "subplot2d"))) warning("Current functionality is designed to support the following geometries: 'point', 'bin', 'hex', 'tile', 'subplot2d'.")
  if ("pitch_type" %in% names(data)) { #Add descriptions as pitch_types
    data$pitch_type <- factor(data$pitch_type)
    types <- data.frame(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Forkball"))
    data <- join(data, types, by = "pitch_type", type="inner")
  } 
  if (!"b_height" %in% names(data)) {
    warning("pitchRx assumes the height of each batter is recorded as 'b_height'. Since there is no such column, we will assume each batter has a height of 6'2''")
    data$b_height <- "6-2"
  }
  if (!color %in% names(data)) {
    #warning(paste(color, "is the variable that defines coloring but it isn't in the dataset!"))
    color <- ""
  }
  locations <- c("px", "pz")
  FX <- data[complete.cases(data[,locations]),] #get rid of records missing the necessary parameters
  for (i in locations) FX[,i] <- as.numeric(FX[,i])
  if (parent) { #ugly workaround for shiny implementation
    layers <- NULL
    for (i in layer)
      layers <- list(layers, eval(i)) 
  } else {
    layers <- layer
  }
  facets <- getFacets(layer=layers)
  boundaries <- getStrikezones(FX, facets, strikeFX = TRUE) 
  if (adjust) {
    FX$pz_adj <- boundaries[[1]] #adjusted vertical locations
  } else {
    FX$pz_adj <- FX$pz # "adjusted" vert locations
  }
  FX <- join(FX, boundaries[[2]], by="stand", type="inner")
  for (i in locations) FX[,i] <- as.numeric(FX[,i])
  #Recycled plot formats
  labelz <- labs(x = "Horizontal Pitch Location", y = "Height from Ground")
  legendz <- theme(legend.position = c(0.25,0.05), legend.direction = "horizontal")
  xrange <- xlim(limitz[1:2])
  yrange <- ylim(limitz[3:4])
  if (draw_zones) {
    white_zone <- geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="white")
    black_zone <- geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="grey20")
  } else {
    white_zone <- NULL
    black_zone <- NULL
  }
  #As of version 0.7, we assume the model is begin fit via gam/bam
  #Most of this is modeled after ?mgcv::vis.gam (view is 'px' and 'pz', non-facets are conditioned upon)
  #Does it make sense to look at differenced grids given a pre-fitted model????
  if (!missing(model)) {
    #figure out whether model argument is a fitted model or if it has yet to be fitted
    classy <- tryCatch(class(model), error = function(e) NULL)
    isfit <- !is.null(classy)
    if (!isfit) { #fit the model
      form <- as.list(substitute(model))
      fit <- fitModel(FX, form, model.save)
    } else {
      fit <- model
    }
    #create a 2D grid (eventually passed to the ?predict.gam method)
    x.grid <- seq(limitz[1], limitz[2], 0.05) #add option for grid granularity?
    y.grid <- seq(limitz[3], limitz[4], 0.05) 
    grid <- expand.grid(px=x.grid, pz=y.grid)
    #extract covariates included in gam/bam (see ?gamObject)
    vars <- names(fit$var.summary)
    #Summaries for factors have a single value. Other types have 3 values and we want to condition on the 2nd value (see var.summary of ?gamObject)
    var_summary <- lapply(fit$var.summary, function(x) { if (length(x) == 1) x else x[2] })
    factors <- vars[sapply(fit$var.summary, length) == 1]
    ind <- vars %in% c("px", "pz")
    extras <- vars[!ind] #we need to either condition on or facet by these variables
    if (!all(c("px", "pz") %in% vars)) warning("The horizontal location and vertical locations of each pitch should included as covariates in your model as 'px' and 'pz'.")

    #check if the facetting scheme makes sense for the model. if not, give warning and throw out facet call
    if (!is.null(facets)){ #facets exist
      if (length(factors) == 0){ #facets not possible!
        warning("Facet layer will be ignored since no factor variables were put into the model.")
        facets <- NULL
        id <- sapply(lapply(layer, class), function(x) which("facet" %in% x)) 
        layer[id > 0] <- NULL #get rid of the facet layer
      } else { #factors variables do exist
        ind <- facets %in% factors 
        idx <- which(!ind) #which facet variables are not included?
        if (length(idx) > 0) {
          warning(paste0("Facet layer will be ignored since the following variables included in the model as factors: ", facets[idx]))
          facets <- NULL
          id <- sapply(lapply(layer, class), function(x) which("facet" %in% x)) 
          layer[id > 0] <- NULL #get rid of the facet layer
        }
      }
    }
    #'fixed' variables can have multiple 'conditioned' values (px, pz are also here since they are part of the 'view')
    fixed <- c(facets, c("px", "pz"))
    if (length(density1) > 0) fixed <- c(fixed, names(density1))
    if (length(density2) > 0) fixed <- c(fixed, names(density2))
    #'given' variables have one 'conditioned' value (should be the mode for factors and closest obs. to the median for numerics)
    givens <- var_summary[!names(var_summary) %in% fixed]
    for (i in seq_along(givens)) {
      message(paste("Conditioning on:", names(givens[i]), " == ", givens[[i]]))
      grid[names(givens)[i]] <- givens[[i]]
    }
    #now make predictions for each unique value of the facetting variables
    if (!is.null(facets)) {
      sheet <- NULL
      vals <- unique(FX[facets])
      val.names <- names(FX[facets])
      for (i in seq_len(dim(vals)[1])) {
        greed <- suppressWarnings(cbind(grid, vals[i,]))
        names(greed) <- c(names(grid), val.names)
        if (identical(density1, density2)) { #probabilities are not differenced
          greed$z <- predict(fit, greed, type="response") #add an option to look at se!?! see ?predict.gam
        } else { #probabilities are differenced
          greed1 <- greed
          greed1[names(density1)] <- density1[[1]]
          z1 <- predict(fit, greed1, type="response")
          greed2 <- greed
          greed2[names(density2)] <- density2[[1]]
          z2 <- predict(fit, greed2, type="response")
          greed$z <- z1 - z2
        }
        sheet <- rbind(sheet, greed)
      }
    } else { #no facets
      if (identical(density1, density2)) { #probabilities are not differenced
        grid$z <- predict(fit, grid, type="response") #add an option to look at se!?! see ?predict.gam
      } else { #probabilities are differenced
        grid1 <- grid
        grid1[names(density1)] <- density1[[1]]
        z1 <- predict(fit, grid1, type="response")
        grid2 <- grid
        grid2[names(density2)] <- density2[[1]]
        z2 <- predict(fit, grid2, type="response")
        grid$z <- z1 - z2
      }
      grid$z <- predict(fit, grid, type="response")
      sheet <- grid
    }
    #for (i in factors) sheet[[i]] <- factor(sheet[[i]])
    p <- plotDensity(sheet, boundaries, contour, geom, ...)
    p <- p + white_zone
    if (any(sheet$z < 0)) col_scale <- scale_fill_gradient2(midpoint=0) else col_scale <- NULL
    return(p+labelz+xrange+yrange+layers+col_scale)
  }
  if (geom %in% "subplot2d") { #special handling for subplotting
    if (!require(ggsubplot)) {
      message("The 'subplot2d' geom requires library(ggsubplot)!")
      return(NULL)
    }
    require(ggsubplot)
    return(ggplot(data=FX)+labelz+xrange+yrange+
             geom_subplot2d(aes(x=px, y=pz_adj, 
                    subplot = geom_bar(aes_string(x=fill, fill = fill))), ...)+
             black_zone+layers)
  }
  if (geom %in% c("bin", "hex", "tile")) { #special handling for (2D) density geometries
    if (identical(density1, density2)) { #densities are not differenced
      FX1 <- subsetFX(FX, density1)
      t <- ggplot(data=FX1, aes(x=px, y=pz_adj))+labelz+xrange+yrange
      if (geom %in% "bin") t <- t + geom_bin2d(...) 
      if (geom %in% "hex") t <- t + geom_hex(...)
      if (geom %in% "tile") t <- t + stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)
      #Contours and strikezones are drawn last
      if (contour) t <- t + geom_density2d()
      t <- t + white_zone
      return(t+layers)
    } else { #densities are differenced (note that diffDensity handles the subsetting)
      if (!is.null(facets)) {
        stuff <- dlply(FX, facets, function(x) { diffDensity(x, density1, density2, limz=limitz) } )
        densities <- ldply(stuff)
      } else densities <- diffDensity(FX, density1, density2, limz=limitz)
    }
    t2 <- plotDensity(densities, boundaries, contour, geom, ...)
    t2 <- t2 + black_zone
    return(t2+scale_fill_gradient2(midpoint=0)+labelz+xrange+yrange+layers)
  }
  if (geom %in% "point") {
    p <- ggplot(data=FX, aes(ymax=top, ymin=bottom, xmax=right, xmin=left)) + legendz + labelz + xrange + yrange + scale_size(guide = "none") + scale_alpha(guide="none")
    p <- p + black_zone
    if (color == "") {
      point_mapping <- aes_string(x = "px", y="pz_adj")
    } else {
      point_mapping <- aes_string(x = "px", y="pz_adj", colour = color)
    }
    if (contour) p <- p + geom_density2d(point_mapping, colour="black")
    p <- p + geom_point(mapping=point_mapping, size=point.size, alpha=point.alpha, ...)
    return(p + layers)
  }
}

#fit a model and return the predicted values for a (possibly differenced) 2D grid
fitModel <- function(dat, expr, model.save=TRUE) {
  fun <- as.character(expr[[1]])
  expr[[1]] <- NULL
  expr$data <- dat #overwrite data argument
  fit <- do.call(fun, expr)
  if (model.save){
    suffix <- gsub(":| ", "-", format(Sys.time(), "%m %d %X")) #month-day-hour-minute-second
    filename <- paste0("model-", suffix, ".rds")
    saveRDS(fit, file=filename)
    message(paste0("Saved model fit as: ", filename))
    Sys.sleep(1) #ensure we get unique filenames
  }
  fit
  #meanFun(fit, grid)
}

# #use fitted model to find mean response over a 2D grid
# meanFun <- function(fit, grid, se=FALSE) {
#   pred.grid <- predict(fit, grid, type="response", se)
#   data.frame(x=grid[,1], y=grid[,2], z=pred.grid)
# }

#Plot a 2D density with pre-computed heights
plotDensity <- function(dens, bounds, contour, geom, ...){
  px=pz=pz_adj=..density..=top=bottom=right=left=x=y=z=NULL #ugly hack to comply with R CMD check
  common <- intersect(names(dens), names(bounds[[2]]))
  if (length(common) == 0) { #Artifically create a variable (stand) if none exists (required for joining and thus drawing strikezones)
    nhalf <- dim(dens)[1]/2
    if (nhalf == floor(nhalf)) {
      dens$stand <- c(rep("R", nhalf), rep("L", nhalf))
    } else {
      dens$stand <- c(rep("R", nhalf), rep("L", nhalf), "L")
    }
  }
  dens.df <- suppressMessages(join(dens, bounds[[2]], type="inner")) #defaults to join "by" all common variables
  p <- ggplot(data=dens.df)
  if (geom %in% "hex") {
    p <- p + stat_summary_hex(aes(x=px, y=pz, z=z), ...)
  } else {
    p <- p + stat_summary2d(aes(x=px, y=pz, z=z), ...) 
  }
  if (contour) p <- p + stat_contour(aes(x=px, y=pz, z=z)) #passing binwidth here throws error
  return(p)
}


# Special subset handling for density estimates
# 
# @param data PITCHf/x data
# @param density either density1 or density2 passed on from \link{strikeFX}

subsetFX <- function(data, density) {
  if (length(density) == 1) {
    index <- data[,names(density)] %in% density
    subset(data, index)
  } else {
    if (length(density) > 1) warning("The length of each density parameter should be 0 or 1.")
    data
  }
}

# Differenced 2D Kernel Density Estimates
#
# Computes differenced 2D Kernel Density Estimates using MASS::kde2d
#
# Computes two densities on the same support and subtracts them according to the \code{density} expression.
# 
# 
# @param data PITCHf/x data
# @param density1 either density1 or density2 passed on from \link{strikeFX}.
# @param density2 either density1 or density2 passed on from \link{strikeFX}.
# @param limz MASS::kde2d paramaters passed from \link{strikeFX}.
# @return Returns a data frame with differenced density estimates as column z.

diffDensity <- function(data, density1, density2, limz){
  data1 <- subsetFX(data, density1)
  data2 <- subsetFX(data, density2)
  est1 <- kde2d(data1[,"px"], data1[,"pz_adj"], n=100, lims=limz)
  grid <- expand.grid(px = est1$x, pz = est1$y)
  est2 <- kde2d(data2[,"px"], data2[,"pz_adj"], n=100, lims=limz)
  grid["z"] <- as.vector(est1$z) - as.vector(est2$z)
  return(grid)
}
