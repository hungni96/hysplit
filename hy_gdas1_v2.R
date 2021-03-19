#' HYSPLIT
#' GDAS1 ftp://arlftp.arlhq.noaa.gov/archives/
#' MET DATA 2006-PRESENT
#' CREATE FUNCTION
procTraj_gdas1 <- function(lat = c(21.02),
                           lon = c(105.8),
                           height = c(500),
                           start = "2019-01-20 00:00:00",
                           end = "2019-01-29 18:00:00",
                           name = "hanoi", 
                           hours = 96,
                           met = "D:/GitHub/hysplit/TrajData/",
                           out = "D:/GitHub/hysplit/TrajProc/",
                           hy.path = "C:/hysplit/") {
   # function for gdas1 meteorology files
   # e.g. for 2 receptors  lat = c(21.02,21.00), lon = c(105.8,105.8), height = c(500,500)
   # start, end: 0h, 3h, 6h, 9h, 12h, 15h, 18h, 21h
   # height (m)
   lapply(c("openair", "plyr", "reshape2", "readxl"), require, character.only = TRUE)
   # set working folder
   setwd(paste0(hy.path, "working/"))
   path.files <- paste0(hy.path, "working/")
   bat.file <- paste0(hy.path, "working/test.bat")
   # delete old tdump files
   files <- list.files(path = path.files, pattern = "tdump")
   lapply(files, function(x) file.remove(x))
   # time series
   dates <- seq(as.POSIXct(start, "GMT"), as.POSIXct(end, "GMT"), by = "3 hour")
   # time base
   date.bs <- seq(as.Date(dates[1]-3600*24*31, "GMT"), as.Date(dates[length(dates)]+3600*24*31, "GMT"), by = "1 day")
   dat <- data.frame(date.bs)
   dat$yr <- as.numeric(format(dat$date.bs, "%y"))
   dat$month <- as.numeric(format(dat$date.bs, "%m"))
   dat$mon <- tolower(format(dat$date.bs, "%b"))
   dat$week <- as.numeric(format(dat$date.bs, "%d"))
   dat$week[dat$week <= 7] <- 1
   dat$week[dat$week >= 8 & dat$week <= 14] <- 2
   dat$week[dat$week >= 15 & dat$week <= 21] <- 3
   dat$week[dat$week >= 22 & dat$week <= 28] <- 4
   dat$week[dat$week >= 29] <- 5
   dat$lg <- NA
   for (f in 1:length(dat$week)) {ifelse(dat$week[f] == dat$week[f+1],dat$lg[f] <- 0, dat$lg[f] <- dat$week[f])}
   dat <- dplyr::filter(dat, !is.na(lg) & lg > 0)
   dat <- dat[,-6]
   # list receptor
   rec <- data.frame(lat, lon, height)
   rec.num <- length(rec$lat)
   # run BAT
   for (i in 1:length(dates)) {
      # defines the properties of time
      year <- format(dates[i], "%y") # e.g. chr "19"
      yr <- as.numeric(year) # e.g. num 19
      Year <- format(dates[i], "%Y") # e.g. chr "2019"
      month <- format(dates[i], "%m") # e.g. chr "01"
      mont <- as.numeric(month) # e.g. num 1
      day <- format(dates[i], "%d") # e.g. "num "01"
      hour <- format(dates[i], "%H") # e.g. "num "01"
      # week of month
      if (day %in% 1:7) {week <- 1}
      if (day %in% 8:14) {week <- 2}
      if (day %in% 15:21) {week <- 3}
      if (day %in% 22:28) {week <- 4}
      if (day %in% 29:31) {week <- 5}
      # set date and time
      x <- paste("echo", year, month, day, hour, " >CONTROL")
      write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE)
      # set number receptor
      x <- paste0("echo ",rec.num," >>CONTROL")
      write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      for (j in 1:rec.num) {
         x <- paste("echo", rec$lat[j], rec$lon[j], rec$height[j], " >>CONTROL") # write the coordinates for each point
         write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      }
      # set back or fwrd hours
      x <- paste("echo ", "-", hours, " >>CONTROL", sep = "")
      write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      # vertical motion method 0 = input model data
      # top of model 10000.0 m agl
      # number of meteorology files
      x <- "echo 0 >>CONTROL
            echo 10000.0 >>CONTROL
            echo 5 >>CONTROL"
      write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      # add meteorology files
      # k must be greater than 2, which means the minimum period is 3rd week of January 2006
      k <- which(dat$yr == yr & dat$week == week & dat$month == mont)
      for (t in (k-2):(k+2)) {
         write.table(paste("echo", met, " >>CONTROL"), bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
         x <- paste("echo gdas1.",dat$mon[t], sprintf("%02d", dat$yr[t]), ".w",dat$week[t]," >>CONTROL", sep = "") # e.g.gdas1.jan19.w1 
         write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      }
      # other settings
      x <- "echo ./ >>CONTROL" # point the tdump path as declared above
      write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      x <- paste("echo tdump", year, month, day, hour, " >>CONTROL", sep = "") # set name of tdump files e.g. tdump19011600
      write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      x <- paste0(hy.path,"exec/hyts_std") # set hyst_std file path
      write.table(x, bat.file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      # run test file (test.bat)
      system(paste0(hy.path, 'working/test.bat'))
   }
   # list of tdump files
   results <- Sys.glob("tdump*")
   # create Rcombined file
   output <- file('Rcombined.txt', 'w')
   for (i in results){
      input <- readLines(i) # read tdump file
      input <- input[-c(1:(rec.num+8))] # delete the first declared lines
      writeLines(input, output)
   }
   close(output)
   # read Rcombined.txt
   traj <- read.table(paste0(hy.path, "working/Rcombined.txt"), header = FALSE)
   traj <- subset(traj, select = -c(V2, V7, V8))
   traj <- rename(traj, c(V1 = "receptor", V3 = "year", V4 = "month", V5 = "day", V6 = "hour",
                          V9 = "hour.inc", V10 = "lat", V11 = "lon", V12 = "height", V13 = "pressure"))
   # convert year format
   year <- traj$year[1]
   if (year < 50) traj$year <- traj$year + 2000 else traj$year <- traj$year + 1900
   # create trajectory time (date2)
   traj$date2 <- with(traj, ISOdatetime(year, month, day, hour, min = 0, sec = 0,
                                        tz = "GMT"))
   # create date
   traj$date <- traj$date2 - 3600 * traj$hour.inc
   # create R data
   file.name <- paste(out, name, Year, ".RData", sep = "")
   save(traj, file = file.name)
}
# RUN
procTraj_gdas1(lat = c(21.02),
               lon = c(105.8),
               height = c(500),
               start = "2019-02-01 00:00:00",
               end = "2019-03-31 21:00:00",
               name = "hanoi", 
               hours = 96,
               met = "D:/GitHub/hysplit/TrajData/",
               out = "D:/GitHub/hysplit/TrajProc/",
               hy.path = "C:/hysplit/")
#' IMPORT
traj <- openair::importTraj(site = "hanoi", year = 2019, local = "D:/GitHub/hysplit/TrajProc/")
#' e.g. pm2.5 conc.
pm25.hn <- read.csv("http://dosairnowdata.org/dos/historical/Hanoi/2019/Hanoi_PM2.5_2019_YTD.csv", header = T)
pm25.hn <- subset(pm25.hn,pm25.hn$QC.Name == "Valid" & pm25.hn$Raw.Conc. > 0)
pm25.hn <- pm25.hn[,c(3,11)]
names(pm25.hn) <- c("date","pm2.5")
pm25.hn$date <-  as.POSIXct(strptime(pm25.hn$date, format = "%Y-%m-%d %I:%M %p", tz = "Asia/Ho_Chi_Minh"))
pm25.hn$date <- format(pm25.hn$date, tz = "GMT")
pm25.hn$date <- as.POSIXct(strptime(pm25.hn$date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
pm25.hn$date <- pm25.hn$date - 3600*24*30*9
#' merge
pm.traj <- dplyr::inner_join(pm25.hn, traj, by = "date")
# cwt plot
pl <- openair::trajLevel(pm.traj, statistic ="cwt", pollutant = "pm2.5", col="jet", smooth = FALSE,
                   projection = "conic", orientation = c(90,0,100), parameters = c(40),
                   xlim=c(95,123), ylim=c(5,30), origin = TRUE, map.res = "hires",
                   map.alpha = 0.3, grid.col = "transparent")
data.pl <- pl$data
summary(data.pl)
# map.alpha transpency level
# map.res = "hires" 
# grid.col = "transparent" remove grid
# save plot
png("pl.png", width = 9*1000, height = 9*1000, res = 1000)
openair::trajLevel(pm.traj, pollutant = "pm2.5",
                   statistic = "cwt", col="jet", smooth = TRUE, projection = "conic",
                   parameters = c(45), orientation=c(90,0,110),xlim=c(90,125),ylim=c(15,30))
dev.off()
