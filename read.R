library(oce)
debug <- 1
## data format p9 doc

##infile <- "2.mat"
##infile <- "list_two_matrices.mat"
infile <- "vector.mat"                 # 3 values, in vector named 'vec'

miTypes <- c("miINT8",                 # 1.
             "miUINT8",                # 2.
             "miINT16",                # 3.
             "miUINT16",               # 4.
             "miINT32",                # 5.
             "miUINT32",               # 6.
             "miSINGLE",               # 7.
             "type8",                  # 8.
             "miDOUBLE",               # 9.
             "type10",                 # 10.
             "type11",                 # 11.
             "miINT64",                # 12.
             "miUINT64",               # 13.
             "miMATRIX",               # 14.
             "miCOMPRESSED",           # 15.
             "miUTF8",                 # 16.
             "miUTF16",                # 17.
             "miUTF32")                # 18.
## classes defined in Table 1-3, page 1-16.
classes <- c("mxCELL_CLASS",           # 1. Cell array
             "mxSTRUCT_CLASS",         # 2. Structure
             "mxOBJECT_CLASS",         # 3. Object
             "mxCHAR_CLASS",           # 4. Character array
             "mxSPARSE_CLASS",         # 5. Sparse array
             "mxDOUBLE_CLASS",         # 6. Double precision array
             "mxSINGLE_CLASS",         # 7. Single precision array
             "mxINT8_CLASS",           # 8. 8-bit, signed integer
             "mxUINT8_CLASS",          # 9. 8-bit, unsigned integer
             "mxINT16_CLASS",          # 10. 16-bit, signed integer
             "mxUINT16_CLASS",         # 11. 16-bit, unsigned integer
             "mxINT32_CLASS",          # 12. 32-bit, signed integer
             "mxUINT32_CLASS",         # 13. 32-bit, unsigned integer
             "mxINT64_CLASS",          # 14. 64-bit, signed integer
             "mxUINT64_CLASS"          # 15. 64-bit, unsigned integer
             )

oceDebug(debug, "inFile=", infile, "\n")
file <- file(infile, "rb")
seek(file, 0, "end")
fileSize <- seek(file, 0, "start")
oceDebug(debug, "filesize=",fileSize, "\n")
b <- readBin(file, what="raw", n=fileSize)
cat(infile, "contains:\n", paste(rep('-', 70), collapse=''), '\n')
for (i in 1:length(b))
    cat(readBin(b[i],"character",1,1))
cat('\n', paste(rep('-', 70), collapse=''), '\n')
descriptiveText <- format(paste(readBin(b[1:116], what="character",n=116), collapse=""))
oceDebug(debug, "descriptiveText:", descriptiveText, "\n")
endianIndicator <- readBin(b[127:128], what="character", n=1, size=2)
endian <- if (endianIndicator == "MI") "swap" else "little"
## page 1-7
version <- readBin(b[125:126], what="integer", n=1, size=2, signed=FALSE, endian=endian)
if (version != 256)
    stop("version should be 256, but it is ", version)
## page 1-7
subsysDataOffset <- readBin(b[121:124], what="character", n=1, size=4, signed=FALSE, endian=endian)
if (subsysDataOffset != '0000' && subsysDataOffset != '    ')
    stop("subsysDataOffset should be 4 zeros, or 4 blanks, but it is '", subsysDataOffset)
o <- 129
dataType <- readBin(b[o+0:3], "integer", endian=endian, signed=FALSE)
o <- o + 4
if (dataType < 1 || dataType > 18)
    stop("dataType code must be between 1 and 18, but it is ", dataType)
oceDebug(debug, "dataType is", dataType, "(", miTypes[dataType], ")\n")

numberOfBytes <- readBin(b[o+0:3], "integer", signed=FALSE, endian=endian)
o <- o + 4
oceDebug(debug, "numberOfBytes", numberOfBytes, " ... proceeding to examine data\n")
##readBin(buf[o+4+seq(0, numberOfBytes - 1)], what="double", size=4"
## read the data, item by item
while (o < fileSize - 4) {
    oceDebug(debug, "\no=", o, "; b[(-3):3]=", b[o+(-3):3], "\n")
    dataType <- readBin(b[o + 0:3], "integer", n=1, size=4, endian=endian, signed=FALSE)
    o <- o + 4
    if (dataType < 1 || dataType > 18)
        stop("unknown dataType index ", dataType)
    miType <- miTypes[dataType]
    if (miType == "miUINT32") { # array flags (p1-15 to p1-18)
        oceDebug(debug, "array flags\n")
        numberOfBytes <- readBin(b[o + 0:3], "integer", n=1, size=4, endian=endian, signed=FALSE)
        o <- o + 4
        oceDebug(debug, "numberOfBytes is ", numberOfBytes, "\n")
        print(b[o+0:4])
        flags <- b[o + 2]
        ## p 1-15 shows class at b[o+4], but it does not state byte
        ## ordering, and I find that b[o] gives the expected value in test data.
        class <- readBin(b[o], "integer", size=1, n=1)
        if (class < 1 || class > length(classes))
            stop("class must be between 1 and ", length(classes), " but it is ", class)
        oceDebug(debug, "class=", class, "(", classes[class], ")\n")
        if (class == 6) {
            oceDebug(debug, "double-precision array\n")
        } else {
            warning("no idea what this class (", class, ") means")
        }
        o <- o + numberOfBytes
        oceDebug(debug, "after getting class, o=", o, "\n")
    } else if (miType == "miINT32") { # dimension
        oceDebug(debug, "dataType is ", dataType, "(", miTypes[dataType], ")\n")
        numberOfBytes <- readBin(b[o + 0:3], "integer", n=1, size=4, endian=endian, signed=FALSE)
        o <- o + 4
        oceDebug(debug, "numberOfBytes is ", numberOfBytes, "\n")
        dim <- readBin(b[o + 0:(-1 + numberOfBytes)], "integer", size=4, n=numberOfBytes/4, endian=endian, signed=FALSE)
        oceDebug(debug, "dim=", dim, "\n")
        o <- o + numberOfBytes
        oceDebug(debug, "after getting dim, o=", o, "\n")
    } else {
        oceDebug(debug, "dataType is ", dataType, "(", miTypes[dataType], ")\n")
        numberOfBytes <- readBin(b[o + 0:3], "integer", n=1, size=4, endian=endian, signed=FALSE)
        o <- o + 4
        oceDebug(debug, "numberOfBytes is ", numberOfBytes, "\n")
        data <- readBin(b[o + 0:(-1 + numberOfBytes)], "integer", size=4, n=numberOfBytes/4, endian=endian, signed=FALSE)
        oceDebug(debug, "data=", data, "\n")
        o <- o + numberOfBytes
    }
}
