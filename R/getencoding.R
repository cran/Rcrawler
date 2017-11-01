#' Getencoding
#'
#' This function  retreives the encoding charset of web page based on HTML tags and HTTP header
#' @param url character, the web page url.
#' @return
#' return the encoding charset as character
#' @author salim khalil
#'
#' @import httr xml2
#' @export
Getencoding <- function(url) {

  pag<-tryCatch(GET(url, user_agent("Mozilla/5.0 (Windows NT 6.3; WOW64; rv:42.0) Gecko/20100101 Firefox/42.0"),timeout(5)) , error=function(e) NULL)
  if (!is.null(pag)){
    head<-pag$headers$`content-type`
    enc<-tryCatch(gsub("(.*)=(.*)","\\2",regmatches(head, gregexpr('charset=([^\']*)', head))[[1]] ), error=function(e) NA)
      page<-NA
      if(length(enc)!=0){
        if(!is.na(enc)){
          page<-as.character(content(pag, type = "htmlTreeParse", as="text", encoding = enc))
        }
      }
      if (is.na(page)){
      page<-as.character(content(pag, type = "htmlTreeParse", as="text", encoding = "UTF-8"))
      }
      if (is.na(page)){
        page<-as.character(content(pag, type = "htmlTreeParse", as="text", encoding = "ISO-8859-1"))
      }
      if(!is.na(page)) {
      if (grepl("<meta http-equiv=\"Content-Type\"", page)) {
        enc<-xml_attr(xml_find_one(read_html(page),"//meta[@http-equiv=\'Content-Type\']"), "content")
        enc<-gsub(".*charset=([^\']*)","\\1",enc )
        #enc<-regmatches(page, gregexpr('charset=([^\'>]*)', page))[[1]][1]
        #enc<-gsub("(.*)=([^\'\"]*)\"","\\2",enc )
        } else if (grepl("<meta charset=", page)){

          enc<-regmatches(page, gregexpr('charset=([^\'>/]*)', page))[[1]][1]
          enc<-gsub("(.*)=\"([^\">]*)\"","\\2",enc )
        }
      }
      enc<-toupper(enc)
      enc<-gsub("^\\s+|\\s+$", "", enc)

  } else {
      enc ="NULL"
  }
     return (enc)
}
