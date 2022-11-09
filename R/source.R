load.acs = function(file, verboes = T) {
  
  start.time = file.info(file)$ctime ## inital guess
  data = as.data.frame(fread(file, skip = 99))
  
  ## Get wavelengths
  wavelengths = names(data)[-1]
  wavelengths = wavelengths[grep(x = wavelengths, pattern = 'C')]
  wavelengths = as.numeric(gsub('C', '', wavelengths))
  
  att = data[,c(1, grep(x = names(data), pattern = 'C'))]
  abs = data[,c(1, grep(x = names(data), pattern = 'A'))]
  
  att$Time = att$`Time(ms)`/1000 + start.time - max(att$`Time(ms)`/1000)
  att$`Time(ms)` = NULL
  
  abs$Time = att$Time
  abs$`Time(ms)` = NULL
  
  ## Noise Filter
  for (i in 2:ncol(abs)) {
    abs[,i] = runmed(abs[,i], 15)
  }
  for (i in 2:ncol(att)) {
    att[,i] = runmed(att[,i], 15)
  }
  
  list(abs = abs, att = att, wavelengths = wavelengths)
}


load.logs = function(files) {
  log = fread(files[1])
  
  for (i in 2:length(files)) {
    log = rbind(log, fread(files[i]))
  }
  
  log$Time = as.POSIXct(paste(log$V1, log$V2), tz = 'UTC')
  ## return
  
  data.frame(Time = log$Time, State = log$V4)
}