
### Historic ----
clean.paf <- function(paf) {
  good.states <- c("GOOD")
  # remove unvalid calibrations
  paf$calibrations$calib.time <- round(paf$calibrations$calib.time, -2)
  cal <- ddply(paf$calibrations, .(trial), function(x) {
    # 1) select last LEFT and last RIGHT
    # but ignore small timing
    # differences between L/R (I found 1 ms in id=5)
    last <- max(x$calib.time)
    x <- subset(x, calib.time == last)
    # 2) keep only GOOD
    # ... removed
    # 3) keep better
    better <- min(x$error.avg)
    x$preferred <- (x$error.avg == better)
    if (sum(x$preferred) > 1) {
      # resolve tie
      best <- min(x$error.max[x$preferred])
      x$preferred <- x$preferred & x$error.max == best
      # if still a tie, resolve later, but we have no particular reason
    }
    return(x)
  })
  print(cal)
  cat("\n===")
  # what is the last trial for this calibration?
  cal2 <- unique(cal[,c("calib.time","trial")])
  max.trial <- max(paf$records$trial)
  cal2$trial2 <- c(cal2$trial[-1],max.trial) # [-1] = without +1
  cat("\n");
  cal <- join(cal,cal2); rm(cal2)
  cal <- cal[,c(1,2,8,3:7)]
  cal$trial <- 1 + cal$trial
  print(cal)
  # remove gaze data duplicates - keep better eye only
  cal$used <- F
  for (et in unique(cal$calib.time)) {
    cal1 <- subset(cal, calib.time == et)
    stopifnot(nrow(cal1) %in% 1:2)
    t1 <- cal1$trial[1]; t2 <- cal1$trial2[1]
    n.all <- nrow(paf$records)
    n.records <- nrow(subset(paf$records, trial %in% t1:t2))
    n.records.L <- nrow(subset(paf$records, trial %in% t1:t2 &
                                 eye == "left"))
    n.records.R <- nrow(subset(paf$records, trial %in% t1:t2 &
                                 eye == "right"))
    cat("\n", n.records, "L:", n.records.L, "P:",n.records.R,"out of",nrow(paf$records))
    # LEFT -- more left (only left?), left is better == > keep only left
    if (n.records.L > n.records.R && any(cal1$eye=="LEFT")) {
      paf$records <- subset(paf$records, !(trial %in% t1:t2) |
                              ((trial %in% t1:t2) & eye == "left"))
      cal$used[cal$calib.time == et & cal$eye == "LEFT"] <- T
      if (!cal$rating[cal$calib.time == et & cal$eye == "LEFT"] %in% good.states) {
        warning("Not GOOD enough calibration for trials ",
                t1,"-", cal$trial2[1],"(",
                cal$rating[cal$calib.time == et & cal$eye == "LEFT"],")")
      }
      stopifnot(nrow(paf$records) == n.all - n.records.R)
      next
    }
    # RIGHT -- more right (only right?), right is better == > keep only right
    if (n.records.L < n.records.R && any(cal1$eye=="RIGHT")) {
      paf$records <- subset(paf$records, !(trial %in% t1:t2) |
                              ((trial %in% t1:t2) & eye == "right"))
      cal$used[cal$calib.time == et & cal$eye == "RIGHT"] <- T
      if (!cal$rating[cal$calib.time == et & cal$eye == "RIGHT"] %in% good.states) {
        warning("Not GOOD enough calibration for trials ",
                t1,"-", t2,"(",
                cal$rating[cal$calib.time == et & cal$eye == "RIGHT"],")")
      }
      stopifnot(nrow(paf$records) == n.all - n.records.L)
      next
    }
    # BOTH -- pick better
    if ((n.records.L == n.records.R) && any(cal1$preferred)) {
      if (sum(cal1$preferred)>1) {
        sel <- cal1$eye == "RIGHT"  # just pick RIGHT (if both equally good and available)
      } else {
        sel <- cal1$preferred
      }
      eye1 <- cal1$eye[sel]
      cat("\nOko:",eye1)
      cat("\nbez uprav:",sum(!(paf$records$trial %in% t1:t2)))
      paf$records <- subset(paf$records, !(trial %in% t1:t2) |
                              ((trial %in% t1:t2) & (eye == tolower(eye1))))
      cal$used[cal$calib.time == et & cal$eye == eye1] <- T
      if (!cal$rating[cal$calib.time == et & cal$eye == eye1] %in% good.states) {
        warning("Not GOOD enough calibration for trials ",
                t1,"-", t2,"(",
                cal$rating[cal$calib.time == et & cal$eye == eye1],")")
      }
      cat("\n",nrow(paf$records))
      stopifnot(nrow(paf$records) == n.all - n.records.R)
      next
    }
    warning("We do not know what to pick??? trials=", t1,"-",t2,
            " (we have",nrow(cal1),"options)")
    if (nrow(cal1) == 1) {
      cal$used[cal$calib.time == et] <- T
    } else {
      cal$used[cal$calib.time == et] <- c(T,F)
    }

  }
  paf$calibrations <- subset(cal, used, select=-preferred)
  return(paf)
}

correct.etime <- function(df, lag=50, fps=85) {
  df$etime.original <- df$etime
  m <- lm(etime ~ 1+offset(mtime), data=df)
  etime.predicted <- predict(m, df)
  lag1 <- df$etime - etime.predicted

  df1 <- df[lag1 < lag,]
  if (nrow(df1)<5) {
    warning("Number of non-lagged records below 5.")
  }
  m <- lm(etime ~ offset(mtime), data=df1)
  df$etime <- predict(m, df)
  df$lag <- df$etime - df$etime.original
  mtime0 <- df$frame / fps
  mtime0[df$frame==0] <- NA
  m <- lm(mtime0 ~ (mtime), data=df)
  df$mtime1 <- predict(m, df)
  return(df)
}

update.gaze.times <- function(gdf, edf) {
  # based on event file, we know correspondence: etime <-> mtime1
  # we will set in gaze file: mtime1 <- etime
  # expect only 1 trial
  ut <- unique(gdf$trial)
  stopifnot(length(ut) == 1)
  trial <- ut[1]
  edf1 <- subset(edf, param == trial)
  m <- lm(mtime1 ~ (etime), data=edf1)
  gdf$mtime1 <- predict(m, gdf)
  return(gdf)
}

filter.gaze.data <- function(gdf, start=0.000, end=10.000) {
  gdf <- subset(gdf, mtime1>= start & mtime1 <= end)
  return(gdf)
}

set.true.trajectory.times <- function(gdf, res) {
  # expect only 1 trial
  ut <- unique(gdf$trial)
  stopifnot(length(ut) == 1)
  t1 <- ut[1]
  from <- res$from[which(res$trial == t1)]
  if (length(from) != 1) {
    stop(sprintf("For trial %d results do not match (found %d)",
                 t1,length(from)))
  }
  #to   <- res$to[res$trial == trial]
  #gdf$otime <- gdf$time
  #gdf$from <- from
  gdf$time <- gdf$time + from
  return(gdf)
}
# gdf2 <- ddply(lg1, .(trial), set.true.trajectory.times, subset(results,id==1))

load.results <- function(pattern, ids, path=".") {
  .data <- NULL
  for (i in ids) {
    fn1 <- sprintf(pattern, i)
    fn1 <- file.path(path, fn1) # use platform independent separator
    .data1 <- read.csv(fn1, as.is=T)
    .data1 <- data.frame(id=i, .data1, stringsAsFactors=F)
    .data <- rbind(.data, .data1)
  }
  return(.data)
}

load.tracks <- function(pattern, tids, path=".") {
  .data <- NULL
  for (ti in tids) {
    fn1 <- sprintf(pattern, ti)
    fn1 <- file.path(path, fn1) # use platform idenpendent separator
    .data1 <- read.csv(fn1, as.is=T,sep="\t",header=F)
    no <- (ncol(.data1) - 1)/2
    stopifnot((ncol(.data1) - 1) %% 2 == 0)
    colnames(.data1) <- c("time",
                          sprintf(c("x%d","y%d"), rep(1:no,each=2)))
    .data1 <- data.frame(track=ti, .data1, stringsAsFactors=F)
    .data <- rbind(.data, .data1)
  }
  return(.data)
}

load.gaze.1 <- function(id, results, template=NA, paf=NULL, expected.trials=NULL) {
  if (is.null(paf)) {
    fn = sprintf(template, id)
    paf <- parse.asc.file(fn)
  }
  d00 <- ddply(paf$events, .(param), correct.etime)
  #cat("\nTime mark error in ms:\n")
  rep1 <- ddply(d00, .(frame), summarize, sd=sd(mtime1*1000))
  rep1 <- subset(rep1, frame>0)
  print(rep1)
  if (!is.null(expected.trials) &
      !setequal(unique(d00$param), expected.trials)) {
    stop(sprintf("Error: trial numbers do not match, read:%d [id:%d]",
                 length(unique(d00$param)),
                 id))
  }
  if (any(rep1$sd > 0.1)) {
    stop(sprintf("Weird... errors > 0.1ms [id:%d]", id))
  } else {
    cat("\nAll time mark errors below <0.1 ms\n")
  }
  # match times
  id2 <- id
  r1 <- results[results$id==id2, ]
  if (nrow(r1)==0) {
    stop(sprintf("results: id %d not found",id))
  }
  gdf <- ddply(paf$records, .(trial), update.gaze.times, d00)
  gdf <- gdf[gdf$mtime1 >= 0 & gdf$mtime1 <= 10,]
  gdf <- gdf[gdf$trial > 8,]
  gdf2 <- data.frame(id=id2, trial=gdf$trial, time=gdf$mtime1,
                     x=gdf$x, y=gdf$y, pupil=gdf$pupil)
  gdf2 <- ddply(gdf2, .(trial), set.true.trajectory.times, r1)
  gdf2 <- gdf2[gdf2$time >=2.0 & gdf2$time < 10,]
  return(gdf2)
}
#lg1 <- load.gaze.1(1, results, paf=paf)

print.experiment <- function(x) {
  stopifnot("experiment" %in% class(x))
  cat("\nExperiment",x$name)
  # number of participants
  cat("\n  # of subjects:     ", length(unique(x$result$id)))
  # number of gaze samples
  cat("\n  # of gaze samples: ", nrow(x$gaze))
  # number of tracks
  cat("\n  # of tracks:       ", length(unique(x$result$track)))
  # list conditions
  cat("\n  Conditions:        ",
      paste(sort(unique(as.character(x$result$cond))),sep=","))
}
