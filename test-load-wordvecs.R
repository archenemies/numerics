
#prefix = "../wordvecs/gn300-wk"
prefix = "../wordvecs/gn300-wk-short"

header_fn = paste0(prefix, ".hdr")
wordvec_fn = paste0(prefix, ".wvs")
table_fn = paste0(prefix, ".tab")

header = as.numeric(read.table(header_fn))
vector_length = header[1]
vocab_size = header[2]

wvconn = file(wordvec_fn, "rb")
wvs = readBin(wvconn, numeric(), size=4, n=vocab_size*vector_length)
dim(wvs) <- c(vocab_size, vector_length)

wtab = read.table(table_fn, quote=NULL,
  col.names=c("index", "orig.index", "word"))

wvtab = cbind(wtab, wv=wvs, awv=abs(wvs))

#structure(list(wtab, ), class=

# this takes too long
#wvs = matrix(wvs_raw, ncol=vector_length)
