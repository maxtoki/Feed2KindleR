# TODO: add download of html page...
# url <- "http://www.hardballtimes.com/the-fascinating-enigma-of-the-non-roster-invitee/"

# download.file(url, "destfile.html" , quiet = FALSE, mode = "w",
#               cacheOK = TRUE,
#               extra = getOption("download.file.extra"))

removescripts <- function(
  url = "destfile.html"
  , dest = "destfile2.html"
){
# strips html page of scripts
  require(XML)
  require(xml2)
  doc <- htmlParse(url,useInternalNode=TRUE)
  r <- xmlRoot(doc)
  removeNodes(xpathSApply(r, "//script"))
  removeNodes(xpathSApply(r, "//head"))
  saveXML(r, dest)
}

workimages <- function(
  url = "destfile2.html"
  , dest = "destfile3.html"
  , folder = 'img'
){
  # downloads images from html page and update src entries to look locally
  require(XML)
  require(dplyr)
  require(stringr)
  doc <- htmlParse(url,useInternalNode=TRUE)
  r <- xmlRoot(doc)
  img <- xpathSApply(r, "//img")
  urls <- sapply(img, function(x) xmlAttrs(x)["src"])
  imgnames <- sapply(urls, function(x){
    spl <- str_split(x, "/")
    nm <- spl[[1]][length(spl[[1]])]
    return(nm)
  })
  
  sapply(1:length(img), function(i){
    ond <- img[[i]]
    nnd <- ond
    xmlAttrs(ond)["src"] <- paste(folder, imgnames[i], sep="/")
    replaceNodes(ond, nnd)
  })
  
  sapply(1:length(img), function(i){
    # TODO: make it work for relative links too
    if(substr(urls[i], 1, 4)=="http" & str_count(imgnames[i], fixed("?")) == 0) 
      download.file(urls[i], paste(folder, imgnames[i], sep="/") , quiet = FALSE, mode="wb")
    })
  
  saveXML(r, dest)
}
