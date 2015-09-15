##
## corr
## @param directory character vector of length 1 with the location of the CSV files
## @param threshold number of completely observed observations (on all variables)
##                  required to compute the correlation between nitrate and sulfate
##                  (the default is 0)
## @return numeric vector of correlations
##
corr <- function(directory, threshold = 0)
{
  filelist = list.files(directory, "*.csv", full.names = TRUE, no.. = TRUE)
  numcomplete = 0
  corr = vector()
  for (fname in filelist)
  {
    data = read.table(fname, header = TRUE, sep = ",")
    valid = complete.cases(data)
    numcomplete = sum(valid)
    if (sum(valid) >= threshold)
    {
      tmp = cor(data$nitrate[valid], data$sulfate[valid])
      corr = c(corr, tmp)
    }
  }
  corr
}