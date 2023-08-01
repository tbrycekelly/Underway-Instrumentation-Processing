load.acs.all = function(files, dt, outfile) {
  acs = list()
  
  for (i in 1:length(files)) {
    message('Loading file ', i, ' of ', length(files))
    temp = load.acs(files[i])
    
    temp$abs$Time = round(as.numeric(temp$abs$Time)/dt) * dt
    
    acs[[i]] = list()
    acs[[i]]$abs = data.frame(Time = unique(temp$abs$Time))
    acs[[i]]$att = data.frame(Time = unique(temp$abs$Time))
    
    message(' Interpolating ABS fields...')
    for (n in names(temp$abs)) {
      acs[[i]]$abs[[n]] = approx(x = temp$abs$Time,
                                 y = temp$abs[[n]],
                                 xout = acs[[i]]$abs$Time,
                                 ties = mean,
                                 rule = 2)$y
    }
    
    message(' Interpolating ATT fields...')
    for (n in names(temp$att)) {
      acs[[i]]$att[[n]] = approx(x = temp$att$Time,
                                 y = temp$att[[n]],
                                 xout = acs[[i]]$att$Time,
                                 ties = mean,
                                 rule = 2)$y
    }
    
    acs[[i]]$abs$Time = conv.time.unix(acs[[i]]$abs$Time)
    acs[[i]]$att$Time = conv.time.unix(acs[[i]]$att$Time)
  }
  
  saveRDS(acs, outfile)
  message('ACS data saved to ', outfile)
}


load.acs = function(file, verboes = T) {
  
  start.time =  strsplit(file, split = '_')[[1]][3]
  start.time = make.time(substr(start.time, 1, 4),
                         substr(start.time, 5, 6),
                         substr(start.time, 7, 8),
                         substr(start.time, 9, 10),
                         substr(start.time, 11, 12),
                         substr(start.time, 13, 14),
                         tz = 'UTC')
  #start.time = file.info(file)$ctime ## inital guess
  data = as.data.frame(fread(file, skip = 99, verbose = F, showProgress = F))
  
  ## Get wavelengths
  wavelengths = names(data)[-1]
  wavelengths = wavelengths[grep(x = wavelengths, pattern = 'C')]
  wavelengths = as.numeric(gsub('C', '', wavelengths))
  
  att = data[,c(1, grep(x = names(data), pattern = 'C'))]
  abs = data[,c(1, grep(x = names(data), pattern = 'A'))]
  
  att$Time = (att$`Time(ms)` - min(att$`Time(ms)`))/1000 + start.time
  att$`Time(ms)` = NULL
  
  abs$Time = att$Time
  abs$`Time(ms)` = NULL
  
  ## Noise Filter (15 samples)
  for (i in 2:ncol(abs)) {
    abs[,i] = runmed(abs[,i], 15)
  }
  for (i in 2:ncol(att)) {
    att[,i] = runmed(att[,i], 15)
  }
  
  list(abs = abs, att = att, wavelengths = wavelengths)
}


load.bb3.all = function(files, dt, outfile) {
  bb3 = list()
  
  for (i in 1:length(files)) {
    message('Loading file ', i, ' of ', length(files))
    temp = load.bb3(files[i])
    
    temp$Time = round(as.numeric(temp$Time)/dt) * dt
    
    message(' Interpolating fields...')
    bb3[[i]] = data.frame(Time = unique(temp$Time))
    
    for (n in names(temp)) {
      bb3[[i]][[n]] = approx(x = temp$Time,
                                 y = temp[[n]],
                                 xout = bb3[[i]]$Time,
                                 ties = mean,
                                 rule = 2)$y
    }
    
    bb3[[i]]$Time = conv.time.unix(bb3[[i]]$Time)
  }
  
  saveRDS(bb3, outfile)
  message('BB3 data saved to ', outfile)
}


load.bb3 = function(file, verbose = T) {
  
  bb3 = as.data.frame(fread(file, fill = T))
  bb3$Time = paste(bb3$V1, bb3$V2)
  bb3$Time = strptime(bb3$Time, format = '%m/%d/%y %H:%M:%S', tz = 'UTC')
  
  bb3 = data.frame(Time = bb3$Time, C470 = bb3$V4, C532 = bb3$V6, C650 = bb3$V8, QC = bb3$V9)
  
  ## Noise Filter (5 sec)
  bb3$C470 = runmed(bb3$C470, 5)
  bb3$C532 = runmed(bb3$C532, 5)
  bb3$C650 = runmed(bb3$C650, 5)
  
  bb3
}



load.logs = function(files) {
  log = as.data.frame(fread(files[1]))
  
  for (i in 2:length(files)) {
    log = rbind(log, as.data.frame(fread(files[i])))
  }
  
  log$Time = as.POSIXct(paste(log$V1, log$V2), tz = 'UTC')
  ## return
  
  data.frame(Time = log$Time, State = log$V4)
}