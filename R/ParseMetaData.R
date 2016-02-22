# RSS / ATOM parser

feed.parse <- function(
  url = "http://www.feedforall.com/sample.xml"
  # "http://rotofeed.blogspot.com/feeds/posts/default"
){
  require(xml2)
  
  page <- read_xml(url) #html(paste(readLines(url), collapse = " "))
  
  # check if it's rss or atom
  feedtype <- NA
  if(page %>% xml_find_all("/feed") %>% length() > 0) feedtype <- "atom"
  if(page %>% xml_find_all("/rss") %>% length() > 0) feedtype <- "rss"
  if(is.na(feedtype)) stop("feed type not recognized")  
  # page <- read_xml(url)
  
  # parse
  if(feedtype=="rss"){
    # parse rss file
    sitenode <- page %>% xml_find_one("channel")
    
    feedname <- sitenode %>% xml_find_one("title") %>% xml_text
    feeddescr <- sitenode %>% xml_find_one("description") %>% xml_text
    
    articlenodes <- page %>% xml_find_all("//item")
    
    feedarticles <- lapply(articlenodes, function(x){
      arttitle <- x %>% xml_find_one("title") %>% xml_text
      artlink <- x %>% xml_find_one("link") %>% xml_text
      artdate <- x %>% xml_find_one("pubDate") %>% xml_text
      #...description?
      artdata <- data.frame(title=arttitle, link=artlink, pubdate=artdate)
      return(artdata)
    })
    feedarticles <- do.call("rbind", feedarticles)
    
  } else {
    # parse atom file
    sitenode <- page %>% xml_find_one("feed")
    
    feedname <- sitenode %>% xml_find_one("title") %>% xml_text
    feeddescr <- sitenode %>% xml_find_one("subtitle") %>% xml_text
    
    articlenodes <- page %>% xml_find_all("//entry")
    
    feedarticles <- lapply(articlenodes, function(x){
      arttitle <- x %>% xml_find_one("title") %>% xml_text
      artlink <- x %>% xml_find_one("link") %>% xml_attr("href")
      artdate <- x %>% xml_find_one("updated") %>% xml_text
      #...description?
      artdata <- data.frame(title=arttitle, link=artlink, pubdate=artdate)
      return(artdata)
    })
    feedarticles <- do.call("rbind", feedarticles)
    
  }
  
  return(list(feed=feedname, description=feeddescr, articles=feedarticles))
}

# tht <- feed.parse("http://www.hardballtimes.com/feed/")
