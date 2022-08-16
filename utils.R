#library("plyr")

pairwise.t.test.with.t.and.df <- function (x, g, p.adjust.method = p.adjust.methods, pool.sd = !paired, 
                                           paired = FALSE, alternative = c("two.sided", "less", "greater"), 
                                           ...) 
{
  if (paired & pool.sd) 
    stop("pooling of SD is incompatible with paired tests")
  DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
  g <- factor(g)
  p.adjust.method <- match.arg(p.adjust.method)
  alternative <- match.arg(alternative)
  if (pool.sd) {
    METHOD <- "t tests with pooled SD"
    xbar <- tapply(x, g, mean, na.rm = TRUE)
    s <- tapply(x, g, sd, na.rm = TRUE)
    n <- tapply(!is.na(x), g, sum)
    degf <- n - 1
    total.degf <- sum(degf)
    pooled.sd <- sqrt(sum(s^2 * degf)/total.degf)
    compare.levels <- function(i, j) {
      dif <- xbar[i] - xbar[j]
      se.dif <- pooled.sd * sqrt(1/n[i] + 1/n[j])
      t.val <- dif/se.dif
      if (alternative == "two.sided") 
        2 * pt(-abs(t.val), total.degf)
      else pt(t.val, total.degf, lower.tail = (alternative == 
                                                 "less"))
    }
    compare.levels.t <- function(i, j) {
      dif <- xbar[i] - xbar[j]
      se.dif <- pooled.sd * sqrt(1/n[i] + 1/n[j])
      t.val = dif/se.dif 
      t.val
    }       
  }
  else {
    METHOD <- if (paired) 
      "paired t tests"
    else "t tests with non-pooled SD"
    compare.levels <- function(i, j) {
      xi <- x[as.integer(g) == i]
      xj <- x[as.integer(g) == j]
      t.test(xi, xj, paired = paired, alternative = alternative, 
             ...)$p.value
    }
    compare.levels.t <- function(i, j) {
      xi <- x[as.integer(g) == i]
      xj <- x[as.integer(g) == j]
      t.test(xi, xj, paired = paired, alternative = alternative, 
             ...)$statistic
    }
    compare.levels.df <- function(i, j) {
      xi <- x[as.integer(g) == i]
      xj <- x[as.integer(g) == j]
      t.test(xi, xj, paired = paired, alternative = alternative, 
             ...)$parameter
    }
  }
  PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
  TVAL <- pairwise.table.t(compare.levels.t, levels(g), p.adjust.method)
  if (pool.sd) 
    DF <- total.degf
  else
    DF <- pairwise.table.t(compare.levels.df, levels(g), p.adjust.method)           
  ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, 
              p.adjust.method = p.adjust.method, t.value = TVAL, dfs = DF)
  class(ans) <- "pairwise.htest"
  ans
}
pairwise.table.t <- function (compare.levels.t, level.names, p.adjust.method) 
{
  ix <- setNames(seq_along(level.names), level.names)
  pp <- outer(ix[-1L], ix[-length(ix)], function(ivec, jvec) sapply(seq_along(ivec), 
                                                                    function(k) {
                                                                      i <- ivec[k]
                                                                      j <- jvec[k]
                                                                      if (i > j)
                                                                        compare.levels.t(i, j)               
                                                                      else NA
                                                                    }))
  pp[lower.tri(pp, TRUE)] <- pp[lower.tri(pp, TRUE)]
  pp
}

# # stderror from package 'hasuekeep'
# stderror <- function(data = NULL, measurevar, groupvars = NULL, na.rm = TRUE, conf.interval = 0.95, tonumeric = TRUE) {
#   
#   # convert to datatable and tibble
#   data <- data.table::data.table(data)
#   
#   # function to compute N without NAs
#   length2 <- function(x, na.rm = FALSE) {
#     if (na.rm)
#       sum(!is.na(x))
#     else length(x)
#   }
#   
#   resultsList <- list() # empty list to store results
#   
#   for (i in 1:length(measurevar)) {
#     
#     if (sum( data.frame(data)[, measurevar[i]] %in% c(Inf, -Inf)) > 0) { # if measurvar contains Inf or -Inf, stop the script
#       stop(paste0("\nInf or -Inf is in ", measurevar[i], " variable"))
#     }
#     
#     # compute mean by group
#     datac <- data[, .(unlist(lapply(.SD, length2, na.rm = na.rm)),
#                       unlist(lapply(.SD, mean, na.rm = na.rm)),
#                       unlist(lapply(.SD, sd, na.rm = na.rm))),
#                   by = groupvars, .SDcols = measurevar[i]]
#     
#     setnames(datac, c(groupvars, "N", measurevar[i], "sd")) # rename column names
#     
#     setkeyv(datac, groupvars) # sort table
#     
#     datac[, se := sd / sqrt(N)] # compute standard error
#     
#     ciMult <- stats::qt(conf.interval / 2 + 0.5, unlist(datac$N) - 1)
#     datac[, ci := se * ciMult]
#     
#     if (tonumeric) {
#       # convert columns to numeric class if possible, else, leave as character
#       oldwarning <- getOption("warn")
#       options(warn = -1)
#       for (j in 1:(ncol(datac)-4)) { # exclude last few columns (outcome, sd, se, ci)
#         if (sum(is.na(as.numeric(as.character(datac[[j]])))) == 0) {
#           datac[[j]] <- as.numeric(datac[[j]])
#         } else {
#           datac[[j]] <- as.character(datac[[j]])
#         }
#       }
#       options(warn = oldwarning)
#     }
#     
#     resultsList[[measurevar[i]]] <- data.table::data.table(datac)
#     
#   }
#   
#   if (length(measurevar) == 1) {
#     return(resultsList[[measurevar[1]]])
#   } else {
#     return(resultsList)
#   }
#   
# }
# 
# # normWithin from package 'hausekeep'
# normWithin <- function (data = NULL, idvar, measurevar, betweenvars = NULL, na.rm = TRUE) {
#   # Norms the data within specified groups in a data frame; it normalizes each
#   # subject (identified by idvar) so that they have the same mean, within each group
#   # specified by betweenvars.
#   # norm data (this function will only be used by seWithin, and won't have to be called directly)
#   
#   data <- data.table::data.table(data)
#   setkeyv(data, idvar) # sort by idvar
#   
#   data.subjMean <- data[, .(unlist(lapply(.SD, mean, na.rm = na.rm))), by = c(idvar, betweenvars), .SDcols = measurevar] # compute mean for each subject
#   setnames(data.subjMean, c(idvar, betweenvars,'subjMean'))
#   dataNew <- left_join(data.frame(data), data.frame(data.subjMean))
#   dataNew <- data.table::data.table(dataNew)
#   setkeyv(dataNew, c(idvar, betweenvars)) # sort
#   
#   measureNormedVar <- paste0(measurevar, "Normed")
#   
#   # dataNew <- data.frame(dataNew)
#   # dataNew[, measureNormedVar] <- dataNew[, measurevar] - unlist(data[, "subjMean"]) + mean(data[, measurevar], na.rm = na.rm)
#   
#   dataNew[, (measureNormedVar) := get(measurevar) - subjMean + mean(get(measurevar), na.rm = T)]
#   
#   dataNew$subjMean <- NULL
#   
#   return(data.frame(dataNew))
# }
# 
# 
# # seWithin from package 'hausekeep'
# seWithin <- function (data = NULL, measurevar, betweenvars = NULL, withinvars = NULL, idvar = NULL, na.rm = TRUE, conf.interval = 0.95, shownormed = FALSE) {
#   # within-subjects CI (normed and un-normed versions)
#   ## Summarizes data, handling within-subjects variables by removing inter-subject variability.
#   ## It will still work if there are no within-S variables.
#   ## Gives count, un-normed mean, normed mean (with same between-group mean),
#   ##   standard deviation, standard error of the mean, and confidence interval.
#   ## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
#   ##   data: a data frame.
#   ##   measurevar: the name of a column that contains the variable to be summariezed
#   ##   betweenvars: a vector containing names of columns that are between-subjects variables
#   ##   withinvars: a vector containing names of columns that are within-subjects variables
#   ##   idvar: the name of a column that identifies each subject (or matched subjects)
#   ##   na.rm: a boolean that indicates whether to ignore NA's
#   ##   conf.interval: the percent range of the confidence interval (default is 95%)
#   ##   shownormed: whether to show the normed version of the outcome variable
#   
#   data <- data.frame(data) # convert to data.frame
#   
#   # Check if betweenvars and withinvars are factors
#   factorvars <- sapply(data[, c(betweenvars, withinvars), drop = FALSE], FUN = is.factor)
#   # Ensure that the betweenvars and withinvars are factors
#   if (!all(factorvars)) {
#     nonfactorvars <- names(factorvars)[!factorvars]
#     message("Automatically converting the following non-factors to factors: ", paste(nonfactorvars, collapse = ", "))
#     data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
#   }
#   
#   resultsList <- list() # empty list to store results
#   
#   for (i in 1:length(measurevar)) {
#     
#     # if measurvar contains Inf or -Inf, stop the script
#     if (sum( data[, measurevar[i]] %in% c(Inf, -Inf)) > 0) {
#       stop(paste0("\nInf or -Inf is in ", measurevar[i], " variable"))
#     }
#     
#     # Get the means from the un-normed data
#     datac <- stderror(data, measurevar[i], groupvars = c(betweenvars, withinvars), na.rm = na.rm, conf.interval = conf.interval, tonumeric = FALSE)
#     
#     # Drop all the unused columns (these will be calculated with normed data)
#     datac$sd <- NULL
#     datac$se <- NULL
#     datac$ci <- NULL
#     
#     # Norm each subject's data
#     ndata <- normWithin(data, idvar, measurevar[i], betweenvars, na.rm)
#     
#     # This is the name of the new column
#     measurevar_n <- paste(measurevar[i], "Normed", sep = "")
#     
#     # Collapse the normed data - now we can treat between and within vars the same
#     ndatac <- stderror(ndata, measurevar_n, groupvars = c(betweenvars, withinvars), na.rm = na.rm, conf.interval = conf.interval, tonumeric = FALSE)
#     ndatac <- data.frame(ndatac)
#     
#     # Apply correction from Morey (2008) to the standard error and confidence interval
#     # Get the product of the number of conditions of within-S variables
#     nWithinGroups <- prod(vapply(ndatac[,withinvars, drop = FALSE], FUN = function(x) length(levels(x)), FUN.VALUE = numeric(1)))
#     correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
#     
#     ndatacTbl <- data.table::data.table(ndatac)
#     
#     # Apply the correction factor
#     # setnames(ndatacTbl, c("sd", "se"), c("stdev", "stderror"))
#     # print(ndatacTbl)
#     ndatacTbl[, `:=` (sd = sd * correctionFactor, se = se * correctionFactor, ci = ci * correctionFactor)]
#     # print(ndatacTbl)
#     # setnames(ndatacTbl, c("stdev", "stderror"), c("sd", "se"))
#     
#     # Combine the un-normed means with the normed results
#     merged <- left_join(data.frame(datac), data.frame(ndatacTbl))
#     merged <- mutate_if(merged, is.factor, as.character) #if factor, convert to character
#     merged[order( unlist((merged[, 1])), decreasing =  F), ] #arrange by first column
#     merged <- data.table::data.table(merged)
#     message("Factors have been converted to characters.")
#     
#     # convert columns to numeric class if possible, else, leave as character
#     oldwarning <- getOption("warn")
#     options(warn = -1)
#     for (j in 1:(ncol(merged)-4)) { # exclude last few columns (outcome, sd, se, ci)
#       if (sum(is.na(as.numeric(as.character(merged[[j]])))) == 0) {
#         merged[[j]] <- as.numeric(merged[[j]])
#       } else {
#         merged[[j]] <- as.character(merged[[j]])
#       }
#     }
#     options(warn = oldwarning)
#     
#     # whether to show normed version
#     if (shownormed == FALSE) {
#       # print(measurevar_n)
#       merged[, (measurevar_n) := NULL]
#     }
#     
#     resultsList[[measurevar[i]]] <- data.table::data.table(merged)
#     message(cat("Confidence intervals: ", conf.interval, sep = ""))
#     
#   }
#   
#   if (length(measurevar) == 1) {
#     # print(resultsList[[measurevar[1]]])
#     return(resultsList[[measurevar[1]]])
#   } else {
#     # print(resultsList)
#     return(resultsList)
#   }
#   
# }

# populate_baseline_columns = function(data) {
#   for (i in 1:nrow(data)) {
#     participant = data$participant[i]
#     session = data$session[i]
#     item = data$item_name[i]
#     data$q2_baseline[i] = data$response[data$participant == participant &
#                                           data$session == session &
#                                           data$item_name == item &
#                                           data$stage == '2']
#     data$q3_baseline[i] = data$response[data$participant == participant &
#                                           data$session == session &
#                                           data$item_name == item &
#                                           data$stage == '3']
#   }
# }


summarySE2 <- function (data = NULL, measurevar, groupvars = NULL, na.rm = TRUE, conf.interval = 0.95) {
  library(data.table)
  data <- data.table(data)
  
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) 
      sum(!is.na(x))
    else length(x)
  }
  
  datac <- data[, .(unlist(lapply(.SD, length2, na.rm = na.rm)), 
                    unlist(lapply(.SD, mean, na.rm = na.rm)),
                    unlist(lapply(.SD, sd, na.rm = na.rm))),
                by = groupvars, .SDcols = measurevar]
  names(datac) <- c(groupvars, "N", measurevar, "sd")
  setkeyv(datac, groupvars)
  
  datac[, se := unlist(sd) / sqrt(unlist(N))] #compute standard error
  
  ciMult <- qt(conf.interval / 2 + 0.5, unlist(datac$N) - 1)
  datac[, ci := se * ciMult]
  datac <- data.frame(datac)
  return(datac)
}

normDataWithin2 <- function (data = NULL, idvar, measurevar, betweenvars = NULL, 
                             na.rm = TRUE) {
  library(data.table); library(dplyr)
  data <- data.table(data)
  setkeyv(data, idvar)
  
  data.subjMean <- data[, .(unlist(lapply(.SD, mean, na.rm = na.rm))), by = idvar, .SDcols = measurevar]
  names(data.subjMean) <- c(idvar, 'subjMean')
  data <- merge(data, data.subjMean)
  setkeyv(data, c(idvar, betweenvars))
  
  measureNormedVar <- paste(measurevar, "Normed", sep = "")
  data <- data.frame(data)
  
  data[, measureNormedVar] <- data[, measurevar] - unlist(data[, "subjMean"]) + mean(data[, measurevar], na.rm = na.rm)
  return(data)
}


#normed and un-normed versions
summarySEwithin2 <- function (data = NULL, measurevar, betweenvars = NULL, withinvars = NULL, 
                              idvar = NULL, na.rm = TRUE, conf.interval = 0.95) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- sapply(data[, c(betweenvars, withinvars), drop = FALSE], 
                       FUN = is.factor)
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ", 
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE2(data, measurevar, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin2(data, idvar, measurevar, betweenvars, na.rm)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "Normed", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE2(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                       na.rm=na.rm, conf.interval=conf.interval)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN= function(x) length(levels(x)),
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- unlist(ndatac$sd) * correctionFactor
  ndatac$se <- unlist(ndatac$se) * correctionFactor
  ndatac$ci <- unlist(ndatac$ci) * correctionFactor
  
  # Combine the un-normed means with the normed results
  merged <- merge(datac, ndatac)
  #merged[, 1] <- as.numeric(as.character(merged[, 1]))
  #merged <- merged[order(merged[, 1]), ]
  return(merged)
}


GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}