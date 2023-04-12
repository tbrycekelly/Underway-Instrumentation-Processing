library(TheSource)
library(data.table)
library(openxlsx)
library(pals)

source('R/source.R')

bb3.dir = 'Z:/Data/Seachest/SKQ202207S/Underway/BB3'
acs.dir = 'Z:/Data/Seachest/SKQ202207S/Underway/ACS'
frrf.dir = 'Z:/Data/Seachest/SKQ202070S/Underway/FRRF'
log.dir = 'Z:/Data/Seachest/SKQ202207S/Underway'

log.file = list.files(log.dir, pattern = '.log', full.names = T)
event.file = list.files(log.dir, pattern = '.txt', full.names = T)
acs.file = list.files(acs.dir, pattern = '.dat', full.names = T)
bb3.file = list.files(bb3.dir, pattern = '.raw', full.names = T)
#bb3.file = list.files(bb3.dir, pattern = '.lvm', full.names = T)

#chl = readRDS('../NGA-Data-Processing/_rdata/CHL.rdata')
#chl = chl[chl$Cruise == 'SKQ202210S',]
#chl = read.xlsx('Data/SKQ202210S_chl_data_KF.xlsx')
#chl = chl[chl$Depth < 10,]
#chl$Date = conv.time.excel(chl$Date)



## Parameters
col = c('grey', 'black', 'red', 'green')
dt = 60 #sec
dt.filter = 120 # sec

## Load Data
load.acs.all(acs.file, dt = dt, outfile = '_rdata/SKQ202207S ACS.rds')
load.bb3.all(bb3.file, dt = dt, outfile = '_rdata/SKQ202207S BB3.rds')

acs = readRDS('_rdata/SKQ202207S ACS.rds')
bb3 = readRDS('_rdata/SKQ202207S BB3.rds')
log = load.logs(log.file)

event = read.csv(event.file[1], sep = '\t', header = F)
event = data.frame(Time = as.POSIXct(event$V1, tz = 'UTC'),
                   Message = event$V2)


#### Setup Flag Sheet

acs.times = acs[[1]]$abs$Time
for (i in 2:length(acs)) {
  acs.times = c(acs.times, acs[[i]]$abs$Time)
}
acs.times = unique(acs.times)
acs.times = acs.times[order(acs.times)]

bb3.times = bb3[[1]]$Time
for (i in 2:length(bb3)) {
  bb3.times = c(bb3.times, bb3[[i]]$Time)
}
bb3.times = unique(bb3.times)
bb3.times = bb3.times[order(bb3.times)]

# Sheet
flag = data.frame(Time = seq(from = min(bb3.times, acs.times, na.rm = T),
                             to = max(bb3.times, acs.times, na.rm = T),
                             by = '60 sec'),
                  ACS = NA,
                  BB3 = NA,
                  Valve = NA,
                  Flag = 0,
                  Message = NA)

flag$ACS = flag$Time %in% acs.times
flag$BB3 = flag$Time %in% bb3.times

for (i in 1:nrow(flag)) {
  l = which(log$Time <= flag$Time[i])
  if (length(l) > 0) {
    flag$Valve[i] = log$State[max(l)]
  }
}

for (i in 1:nrow(event)) {
  l = which(flag$Time == event$Time[i])
  
  if (length(l) > 0) {
    while(!is.na(flag$Message[l])) {
      l = l+1
    }
    flag$Message[l] = event$Message[i]
  }
}

flag$Flag = flag$Valve
# 1 = cal
# 2 = sample
# 3 = bad
# 0 = lab

tmp.file = tempfile(fileext = '.xlsx')
write.xlsx(flag, file = tmp.file)
browseURL(tmp.file)

flag = read.xlsx('Z:/Data/Seachest/SKQ202207S/Underway/SKQ202207S Flag.xlsx')
flag$Time = conv.time.excel(flag$Time)


#### Set states
# 1 = cal
# 2 = sample
# 3 = bad
for (j in 1:length(acs)) {
  
  ## ABS
  acs[[j]]$abs$State = 0
  for (i in 1:nrow(acs[[j]]$abs)) {
    l = which.min(abs(difftime(flag$Time, acs[[j]]$abs$Time[i],units = 'sec')))
    
    if (length(l) > 0) {
      acs[[j]]$abs$State[i] = flag$Flag[l]
    }
  }
  acs[[j]]$abs$State = ma(acs[[j]]$abs$State, 9)
  acs[[j]]$abs$State[acs[[j]]$abs$State != round(acs[[j]]$abs$State)] = 3 # bad
  acs[[j]]$abs$State[is.na(acs[[j]]$abs$State)] = 3 #bad
  
  
  ## ATT
  acs[[j]]$att$State = 0
  for (i in 1:nrow(acs[[j]]$abs)) {
    l = which.min(abs(difftime(flag$Time, acs[[j]]$att$Time[i],units = 'sec')))
    
    if (length(l) > 0) {
      acs[[j]]$att$State[i] = flag$Flag[l]
    }
  }
  acs[[j]]$att$State = ma(acs[[j]]$att$State, 9)
  acs[[j]]$att$State[acs[[j]]$att$State != round(acs[[j]]$att$State)] = 3 # bad
  acs[[j]]$att$State[is.na(acs[[j]]$att$State)] = 3 #bad
}

### Preliminary Plots
for (i in 1:length(acs)) {
  plot(acs[[i]]$abs$Time,
       acs[[i]]$abs$A401,
       pch = '.',
       main = i,
       col = col[acs[[i]]$abs$State+1],
       cex = 3)
  
  grid(); box()
  mtext(acs[[i]]$abs$Time[1], side = 3, adj = 0)
  points(flag$Time, rep(0.5, nrow(flag)), col = col[flag$Flag], pch = '.')
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


p = prcomp(na.omit(abs[,c(-1, -ncol(abs))]))



plot(att$Time, att$C400.9, pch = '.', col = col[att$State], ylim = c(0, 2))

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
  abs[,i] = abs[,i] - abs.cal[,i]
  abs[abs$State == 1,i] = NA
}


plot(abs.cal$Time, abs.cal$A401, pch = '.', ylim = c(0,1))
plot(abs$Time, abs$A401, pch = '.', ylim = c(0, 1))

a.LH = abs$A676.2 - (39/65) * abs$A649.9 - (26/65) * abs$A714.2
a.LH = smooth.spline(x = abs$Time[!is.na(a.LH)], y = a.LH[!is.na(a.LH)], spar = 0.5)

plot(conv.time.unix(a.LH$x), a.LH$y / 0.0126, type = 'l', ylim = c(0, 5))
points(chl$Date, chl$`Total_Chl_A.(µg/L)`)





att.cal = att
att.cal[att.cal$State == 2, 2:ncol(att.cal)] = NA
for (i in 2:(ncol(att.cal) - 1)) {
  att.cal[,i] = runmed(att.cal[,i], k = 101)
  att.cal[,i] = approx(x = att.cal$Time, y = att.cal[,i], xout = att.cal$Time, na.rm = T, ties = median, rule = 2)$y
  att[,i] = att[,i] - att.cal[,i]
}




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

pos = readRDS('../NGA-Data-Processing/_rdata/TSG.rdata')

for (i in 1:(length(frrf)-1)) {
  k = which.min((as.numeric(frrf[[i]]$Datetime) - as.numeric(pos$DateTime.UTC.))^2)
  
  frrf[[i]]$lon = pos$Longitude.decimaldegreeseast.[k]
  frrf[[i]]$lat = pos$Latitude.decimaldegreesnorth.[k]
}


map = make.map.nga()

for (i in 1:(length(frrf)-1)) {
  add.map.points(map,
                 lon = frrf[[i]]$lon, lat = frrf[[i]]$lat,
                 col = make.pal(frrf[[i]]$A$Fv.Fm[1], min = 0, max = 1, pal = 'cubicl'),
                 pch = 16)
}




