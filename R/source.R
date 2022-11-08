load.acs = function(file, verboes = T) {
  
  start.time = file.info(file)$ctime ## inital guess
  acs.data = as.data.frame(fread(file, skip = 99))
  
  ## Get wavelengths
  wavelengths = names(acs.data)[-1]
  wavelengths = wavelengths[grep(x = wavelengths, pattern = 'C')]
  wavelengths = as.numeric(gsub('C', '', wavelengths))
  
  acs.att = acs.data[,c(1, grep(x = names(acs.data), pattern = 'C'))]
  acs.abs = acs.data[,c(1, grep(x = names(acs.data), pattern = 'A'))]
  
  acs.att$Time = acs.att$`Time(ms)`/1000 + start.time - max(acs.att$`Time(ms)`/1000)
  acs.att$`Time(ms)` = NULL
  
  acs.abs$Time = acs.att$Time
  acs.abs$`Time(ms)` = NULL
  
  list(abs = acs.abs, att = acs.att, wavelengths = wavelengths)
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