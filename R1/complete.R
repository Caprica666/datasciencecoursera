complete = function(directory, id)
{
  complete = matrix(nrow = length(id), ncol = 2)
  colnames(complete) = c("id", "nobs")
  i = 1;
  for (monitor in id)
  {
    fname = paste(directory, "/", sep = "", formatC(monitor, width = 3, format = "d", flag = "0"), ".csv")
    validdata = tryCatch(
      {
        monitordata = read.table(fname, sep=",", header=TRUE)
        numvalid = sum(complete.cases(monitordata))
      },
      warning = function(w)
      {
        warning(paste("WARNING Monitor ", monitor, ": ", w))
      },
      error = function(e)
      {
        warning(paste("WARNING Monitor ", monitor, ": ", e))
      })
    complete[i, 1] = monitor
    complete[i, 2] = numvalid
    i = i + 1
  }
  as.data.frame(complete)
}