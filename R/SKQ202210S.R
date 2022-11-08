library(TheSource)
library(data.table)
library(openxlsx)

source('R/source.R')

bb3.dir = 'Z:/Data/Seachest/Underway/SKQ202210S/BB3'
acs.dir = 'Z:/Data/Seachest/Underway/SKQ202210S/ACS'
log.dir = 'Z:/Data/Seachest/Underway/SKQ202210S'

log.file = list.files(log.dir, pattern = '.log', full.names = T)
acs.file = list.files(acs.dir, pattern = '.dat', full.names = T)

log = load.logs(log.file)
#chl = readRDS('../NGA-Data-Processing/_rdata/CHL.rdata')
#chl = chl[chl$Cruise == 'SKQ202210S',]
chl = read.xlsx('Data/SKQ202210S_chl_data_KF.xlsx')
chl = chl[chl$Depth < 10,]
chl$Date = conv.time.excel(chl$Date)


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
    acs[[i]]$abs[[n]] = approx(x = temp$abs$Time, y = temp$abs[[n]], xout = acs[[i]]$abs$Time, ties = median, rule = 2)$y
  }
  
  message(' Interpolating ATT fields...')
  for (n in names(temp$att)) {
    acs[[i]]$att[[n]] = approx(x = temp$att$Time, y = temp$att[[n]], xout = acs[[i]]$att$Time, ties = median, rule = 2)$y
  }
  
  acs[[i]]$abs$Time = conv.time.unix(acs[[i]]$abs$Time)
  acs[[i]]$att$Time = conv.time.unix(acs[[i]]$att$Time)
  
}

for (i in 2:length(acs)) {
  acs[[1]]$abs = rbind(acs[[1]]$abs, acs[[i]]$abs)
  acs[[1]]$att = rbind(acs[[1]]$att, acs[[i]]$att)
}

abs = acs[[1]]$abs
att = acs[[1]]$att
rm(acs)

abs = abs[order(abs$Time),]
att = att[order(att$Time),]

### Noise Filter
for (i in 2:ncol(abs)) {
  abs[,i] = runmed(abs[,i], dt.filter / dt)
}

for (i in 2:ncol(att)) {
  att[,i] = runmed(att[,i], dt.filter / dt)
}


#### Set states

abs$State = 0
for (i in 1:nrow(abs)) {
  l = which(log$Time < abs$Time[i])
  
  if (length(l) > 0) {
    abs$State[i] = log$State[max(l)]
  }
}

att$State = 0
for (i in 1:nrow(abs)) {
  l = which(log$Time < att$Time[i])
  
  if (length(l) > 0) {
    att$State[i] = log$State[max(l)]
  }
}

## Filter
abs = abs[abs$Time < make.time(2022, 7, 21),]
att = att[att$Time < make.time(2022, 7, 21),]

plot(att$Time, att$C400.9, pch = '.', col = att$State, ylim = c(0, 2))

#### Calculate seawater values
abs.cal = abs
abs.cal[abs.cal$State == 2, 2:ncol(abs.cal)] = NA

for (i in 2:(ncol(abs.cal) - 1)) {
  abs.cal[,i] = runmed(abs.cal[,i], 101)
  abs.cal[,i] = approx(x = abs.cal$Time, y = abs.cal[,i], xout = abs.cal$Time, na.rm = T, ties = median, rule = 2)$y
  abs[,i] = abs[,i] - abs.cal[,i]
}

a.LH = abs$A676.2 - (39/65) * abs$A649.9 - (26/65) * abs$A714.2

plot(abs.cal$Time, a.LH / 0.0126, type = 'l', ylim = c(0, 5))
points(chl$Date, chl$`Total_Chl_A.(µg/L)`)


plot(abs.cal$Time, abs$A401[abs$State] - abs.cal$A401, pch = '.', ylim = c(-1, 1))



att.cal = att
att.cal[att.cal$State == 2, 2:ncol(att.cal)] = NA
for (i in 2:(ncol(att.cal) - 1)) {
  att.cal[,i] = runmed(att.cal[,i], k = 101)
  att.cal[,i] = approx(x = att.cal$Time, y = att.cal[,i], xout = att.cal$Time, na.rm = T, ties = median, rule = 2)$y
  att[,i] = att[,i] - att.cal[,i]
}






