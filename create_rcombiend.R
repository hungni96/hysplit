
# hysplit folder
hy.path <- "C:/hysplit/"
# require packages
lapply(c("openair", "plyr", "reshape2", "readxl"), require, character.only = TRUE)
# set working folder
setwd(paste0(hy.path, "working/"))
# list of fdump files
results <- Sys.glob("fdump*")
# create Rcombined file
output <- file('Rcombined.txt', 'w')
for (i in results){
   input <- readLines(i) # read fdump file
   met.num <- as.numeric(substr(input[1], 1, 6))
   rec.num <- as.numeric(substr(input[1], 12, 12))
   input <- input[-c(1:(met.num + rec.num + 3))] # delete the first declared lines
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
out <- "D:/"
file.name <- paste(out, "data_4y.RData", sep = "")
save(traj, file = file.name)

