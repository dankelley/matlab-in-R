## data format p9 doc
file <- file("1.mat", "rb")
seek(file, 0, "end")
fileSize <- seek(file, 0, "start")
b <- readBin(file, what="raw", n=fileSize)
descriptiveText <- format(paste(readBin(b[1:116], what="character",n=116), collapse=""))
cat("descriptiveText:", descriptiveText, "\n")
endianIndicator <- readBin(b[127:128], what="character", n=1, size=2)
endian <- if (endianIndicator == "MI") "swap" else "little"
# page 1-7
version <- readBin(b[125:126], what="integer", n=1, size=2, signed=FALSE, endian=endian)
if (version != 256)
    stop("version should be 256, but it is ", version)
# page 1-7
subsysDataOffset <- readBin(b[121:124], what="character", n=1, size=4, signed=FALSE, endian=endian)
if (subsysDataOffset != '0000' && subsysDataOffset != '    ')
    stop("subsysDataOffset should be 4 zeros, or 4 blanks, but it is '", subsysDataOffset)
o <- 129
dataType <- readBin(b[o+0:3], "integer", endian=endian, signed=FALSE)
o <- o + 4
cat("dataType", dataType, "\n")
miTypes <- c("miINT8", "miUINT8", "miINT16", "miUINT16", "miINT32",
             "miUINT32", "miSINGLE", "type8", "miDOUBLE", "type10",
             "type11", "miINT64", "miUINT64", "miMATRIX", "miCOMPRESSED",
             "miUTF8", "miUTF16", "miUTF32")
cat("type is", miTypes[dataType], "\n")
numberOfBytes <- readBin(b[o+0:3], "integer", signed=FALSE, endian=endian)
o <- o + 4
cat("numberOfBytes", numberOfBytes, "\n")
#readBin(buf[o+4+seq(0, numberOfBytes - 1)], what="double", size=4"
data  <- b[o + 0:(-1 + numberOfBytes)]

