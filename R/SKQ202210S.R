library(TheSource)
library(data.table)
library(openxlsx)
library(pals)
#library(zoo)

source('R/source.R')

bb3.dir = 'Z:/Data/Seachest/SKQ202210S/Underway/BB3'
acs.dir = 'Z:/Data/Seachest/SKQ202210S/Underway/ACS'
frrf.dir = 'Z:/Data/Seachest/SKQ202210S/Underway/FRRF'
log.dir = 'Z:/Data/Seachest/SKQ202210S/Underway'

log.file = list.files(log.dir, pattern = '.log', full.names = T)
acs.file = list.files(acs.dir, pattern = '.dat', full.names = T)
log = load.logs(log.file)

#chl = readRDS('../NGA-Data-Processing/_rdata/CHL.rdata')
#chl = chl[chl$Cruise == 'SKQ202210S',]
chl = read.xlsx('Data/SKQ202210S_chl_data_KF.xlsx')
chl = chl[chl$Depth < 10,]
chl$Date = conv.time.excel(chl$Date)

pos = readRDS('../NGA-Data-Processing/_rdata/TSG.rdata')


## Parameters
col = c('black', 'red', 'green')
dt = 60 #sec
dt.filter = 300 # sec
acs = list()



for (i in 1:length(acs.file)) {
  message('Loading file ', i, ' of ', length(acs.file))
  temp = load.acs(acs.file[i])
  
  temp$abs$Time = round(as.numeric(temp$abs$Time)/dt) * dt
  
  acs[[i]] = list()
  acs[[i]]$abs = data.frame(Time = unique(temp$abs$Time))
  acs[[i]]$att = data.frame(Time = unique(temp$abs$Time))
  
  message(' Interpolating ABS fields...')
  for (n in names(temp$abs)) {
    acs[[i]]$abs[[n]] = approx(x = temp$abs$Time, y = temp$abs[[n]], xout = acs[[i]]$abs$Time, ties = mean, rule = 2)$y
  }
  
  message(' Interpolating ATT fields...')
  for (n in names(temp$att)) {
    acs[[i]]$att[[n]] = approx(x = temp$att$Time, y = temp$att[[n]], xout = acs[[i]]$att$Time, ties = mean, rule = 2)$y
  }
  
  acs[[i]]$abs$Time = conv.time.unix(acs[[i]]$abs$Time)
  acs[[i]]$att$Time = conv.time.unix(acs[[i]]$att$Time)
}

saveRDS(acs, '_rdata/acs.rds')


acs = readRDS('_rdata/acs.rds')

#### Set states
# 1 = cal
# 2 = sample
# 3 = bad
for (j in 1:length(acs)) {
  
  ## ABS
  acs[[j]]$abs$State = 0
  for (i in 1:nrow(acs[[j]]$abs)) {
    l = which(log$Time < acs[[j]]$abs$Time[i])
    
    if (length(l) > 0) {
      acs[[j]]$abs$State[i] = log$State[max(l)]
    }
  }
  acs[[j]]$abs$State = ma(acs[[j]]$abs$State, 9)
  acs[[j]]$abs$State[acs[[j]]$abs$State != 1 & acs[[j]]$acs[[j]]$abs$State != 2] = 3
  acs[[j]]$abs$State[1:5] = 3
  acs[[j]]$abs$State[(nrow(acs[[j]]$abs)-5):nrow(acs[[j]]$abs)] = 3
  
  
  ## ATT
  acs[[j]]$att$State = 0
  for (i in 1:nrow(acs[[j]]$abs)) {
    l = which(log$Time < acs[[j]]$att$Time[i])
    
    if (length(l) > 0) {
      acs[[j]]$att$State[i] = log$State[max(l)]
    }
  }
  acs[[j]]$att$State = ma(acs[[j]]$att$State, 9)
  acs[[j]]$att$State[acs[[j]]$att$State != 1 & acs[[j]]$acs[[j]]$att$State != 2] = 3
  acs[[j]]$att$State[1:5] = 3
  acs[[j]]$att$State[(nrow(acs[[j]]$att)-5):nrow(acs[[j]]$att)] = 3
}

### Preliminary Plots
for (i in 1:length(acs)) {
  plot(acs[[i]]$abs$Time,
       acs[[i]]$abs$A401,
       pch = '.',
       main = i,
       col = col[acs[[i]]$abs$State],
       cex = 2)
  
  grid(); box()
  mtext(acs[[i]]$abs$Time[1], side = 3, adj = 0)
}


#### Merge
## Keeping 2:17
for (i in 3:17) {
  acs[[2]]$abs = rbind(acs[[2]]$abs, acs[[i]]$abs)
  acs[[2]]$att = rbind(acs[[2]]$att, acs[[i]]$att)
}

abs = acs[[2]]$abs
att = acs[[2]]$att
rm(acs)

abs = abs[order(abs$Time),]
att = att[order(att$Time),]



#### Calculate seawater values
# 1 = cal
# 2 = sample
# 3 = bad
abs.original = abs
abs.cal = abs
abs.cal[abs.cal$State == 2,] = NA
abs.cal$Time = abs$Time


for (i in 2:(ncol(abs.cal) - 1)) {
  abs.cal[,i][abs.cal[,i] > 1] = NA
  abs.cal[,i] = runmed(abs.cal[,i], 601)
  abs.cal[,i] = approx(x = round(as.numeric(abs.cal$Time)/dt.filter),
                       y = abs.cal[,i],
                       xout = round(as.numeric(abs.cal$Time)/dt.filter),
                       na.rm = T,
                       ties = mean,
                       rule = 2)$y
  
  abs[,i] = abs[,i] - abs.cal[,i] # Adjust baseline!
  abs[abs$State == 1,i] = NA
}


plot(abs.cal$Time, abs.cal$A401, pch = '.', ylim = c(0,1))
plot(abs$Time, abs$A401, pch = '.', ylim = c(0, 1))


att.cal = att
att.cal[att.cal$State == 2, 2:ncol(att.cal)] = NA
for (i in 2:(ncol(att.cal) - 1)) {
  att.cal[,i] = runmed(att.cal[,i], k = 101)
  att.cal[,i] = approx(x = att.cal$Time,
                       y = att.cal[,i],
                       xout = att.cal$Time,
                       na.rm = T,
                       ties = median,
                       rule = 2)$y
  
  att[,i] = att[,i] - att.cal[,i] # Adjust baseline!
}




## Plot resulting timeseries
d = seq(min(abs$Time, na.rm = T), max(abs$Time, na.rm = T), by = '1 day')

for (dd in d) {
  l = which(abs(dd - as.numeric(abs$Time)) < 0.5 * 86400)
  
  plot(abs$Time[l],
       abs$A442.6[l],
       col = col[abs$State[l]],
       pch = '.',
       cex = 3,
       main = conv.time.unix(dd))
  grid()
}


## Add location Data
abs$Latitude = approx(pos$Datetime, pos$Latitude, xout = abs$Time)$y
abs$Longitude = approx(pos$Datetime, pos$Longitude, xout = abs$Time)$y
att$Latitude = approx(pos$Datetime, pos$Latitude, xout = att$Time)$y
att$Longitude = approx(pos$Datetime, pos$Longitude, xout = att$Time)$y


map = make.map.nga()

add.map.points(map,
               lon = abs$Longitude, lat = abs$Latitude,
               col = make.pal(att$C461.7, min = 0, max = 1, pal = 'cubicl'),
               pch = 16,
               cex = 0.5)


#### Analysis

## EOF
library(wql)
temp = eof(att[,-c(1, 89:91)] - att.cal[,-c(1,89)], 3)

# Plot EOF response vs wavelength
plot(NULL, NULL, xlim = c(0, 100), ylim = c(-1,1), ylab = 'Weighting', xlab = 'Wavelength')
grid()

for (i in 1:ncol(temp$REOF)) {
  lines(temp$REOF[,i], col = make.pal(i, min = 0, max = 10))
}

# Plot EOF against time
plot(abs$Time, temp$amplitude[,1], type = 'l', xlim = c(make.time(2022, 04, 20), make.time(2022,05,01)), ylim = c(-2,2))
lines(abs$Time, temp$amplitude[,2])


## Line height Chl algorithm
a.LH = abs$A676.2 - (39/65) * abs$A649.9 - (26/65) * abs$A714.2
a.LH = smooth.spline(x = abs$Time[!is.na(a.LH)], y = a.LH[!is.na(a.LH)], spar = 0.5)

plot(conv.time.unix(a.LH$x), a.LH$y / 0.0126, type = 'l', ylim = c(0, 5))
points(chl$Date, chl$`Total_Chl_A.(µg/L)`)



#### FRRF

frrf = load.frrf(frrf.dir)

## Add datetime
for (i in 1:(length(frrf)-1)) {
  file = strsplit(frrf[[i]]$File, split = '/')[[1]]
  file = file[length(file)]
  frrf[[i]]$Datetime = make.time(substr(file, 1, 4),
                                 substr(file, 5, 6),
                                 substr(file, 7, 8),
                                 substr(file, 10, 11),
                                 substr(file, 12, 13),
                                 substr(file, 14, 15), tz = 'GMT')
}


score = rep(NA, length(frrf)-1)

for (i in 1:(length(frrf)-1)) {
  score[i] = mean(frrf[[i]]$A$QR, na.rm = T)
}

for (i in rev(which(score < 20))) {
  frrf[[i]] = NULL
}

saveRDS(frrf, '_rdata/frrf.rds')



for (i in 1:(length(frrf)-1)) {
  k = which.min((as.numeric(frrf[[i]]$Datetime) - as.numeric(pos$Datetime))^2)
  
  if (length(k) == 1) {
    frrf[[i]]$lon = pos$Longitude[k]
    frrf[[i]]$lat = pos$Latitude[k]
  } else {
    frrf[[i]]$lon = 0
    frrf[[i]]$lat = 0
  }
}


map = make.map.nga()

for (i in 1:(length(frrf)-1)) {
  add.map.points(map,
                 lon = frrf[[i]]$lon, lat = frrf[[i]]$lat,
                 col = make.pal(frrf[[i]]$A$Fv.Fm[1], min = 0.3, max = 0.7, pal = 'cubicl'),
                 pch = 16)
}









