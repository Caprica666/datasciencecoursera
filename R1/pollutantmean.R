##
## pollutantmean
## @param directory character vector of length 1 with the location of the CSV files
## @param pollutant character vector of length 1 with the name of the pollutant
##                  for which we will calculate the mean; either "sulfate" or "nitrate"
## @param id        integer vector indicating the monitor ID numbers to be used
## @return the mean of the pollutant across all monitors list in the 'id' vector (ignoring NA values)
##
pollutantmean <- function(directory, pollutant, id = 1:332)
{
  if ((pollutant != "sulfate") && (pollutant != "nitrate"))
  {
      stop("pollutant must be either sulfate or nitrate")
  }
  data = vector()
  for (i in id)
  {
    fname = paste(directory, "/", sep = "", formatC(i, width = 3, format = "d", flag = "0"), ".csv")
    monitordata = tryCatch(
      {
        monitordata = read.table(fname, sep=",", header=TRUE)
      },
    warning = function(w)
      {
        warning(paste("WARNING Monitor ", i, ": ", w))
      },
    error = function(e)
      {
        warning(paste("WARNING Monitor ", i, ": ", e))
      })
    pollutantdata = monitordata[pollutant]
    validdata = pollutantdata[!is.na(pollutantdata)]
    data = c(data, validdata)
  }
  pollutantmean = mean(data)
}
