library(LaF)
  
filenam = "train.txt"
sep = "\t"
chunklen = 1000000 # you can adjust this
  
df = read.csv(filenam, nrows=min(chunklen,1000), sep=sep, header=F)
ctypes = sapply(df, function(x) ifelse(is.integer(x),"integer",ifelse(is.numeric(x),"double","string")))
lf = laf_open_csv(filenam, column_types=ctypes, sep=sep, skip=min(chunklen,1000))
  
# initialise the counts
isnumcol = sapply(df, is.numeric)
csums = colSums(df[,isnumcol],na.rm = T)
ccnts = colSums(is.na(df[,isnumcol])==F)

repeat {
    df = next_block(lf, columns = 1:ncol(lf), nrows = chunklen)
    if (nrow(df) == 0) break
    csums = csums + colSums(df[,isnumcol],na.rm = T)
    ccnts = ccnts + colSums(is.na(df[,isnumcol])==F)
    cat('.')
}
