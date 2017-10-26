#loading needed libraries
library('rvest')
library('stringr')


# this function will give us the list of political articles urls of the given page
# params : $url 
# return : list of urls

getArticlesUrls <- function(base_link){
  
  #Reading the HTML code from the website
  base_link_html <- read_html(base_link)
  
  #Using CSS selectors to scrap the pages urls
  articles <- html_nodes(base_link_html,'.section_title')
  
  #getting articles links as html_node
  article_links <- html_nodes(articles, 'a')
  
  article_links <- as.vector(html_attr(article_links, 'href'))
  
  #generate fixed links
  generated_links <- c()
  fixed_link <- "http://www.hespress.com"
  
  for(i in 1:length(article_links)){
    generated_links <- append(generated_links, paste(fixed_link,article_links[i], sep = ""))
  }
  
  return(generated_links)
}


# this function will generate a set of page links wich contains articles that we will scrap later to extract comments
# params : no_params :D 
# return : vector of links

generatePagesLinks <- function(){
  base_link <- "http://www.hespress.com/politique/index."
  generated_pages_links <- c()
  for (i in 1:60) {
    generated_pages_links <- append(generated_pages_links, paste(base_link, paste(i, ".html", sep = ""), sep = ""))
  }
  return(generated_pages_links)
}

# this function will scrap comments from HTML code andset it on data frame
# params : url of thearticle
# returns : set of comments in french only

scrap_comments <- function(url){
  #Reading the HTML code from the website
  #download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  
  webpage <- read_html(url)
  
  #Using CSS selectors to scrap the comments and scores section
  comments_html <- html_nodes(webpage,'.comment_text')
  scores_html <- html_nodes(webpage,'.result')
  
  if (length(comments_html) != 0) {
    #Converting the comments and scores to text
    comments <- html_text(comments_html)
    comments <- trimws(comments)
    comments <- str_replace(comments,"\r\n","")
    comments <- gsub("[\r\n]", " ", comments) # replace line breaks
    sapply(comments, function(x){})
    
    res <- as.vector(as.numeric(html_text(scores_html)))
    # affect pos for postive scores, neg for negative 
    scores <- c()
    for(i in 1:length(res)){
      if(res[i] >= 0) #postive
        scores <- append(scores,"pos")
      else if(res[i] < 0) #negative
        scores <- append(scores,"neg")
      # else if(is.null(res[i]) || res[i] == '' || is.null(res[i]))
      #   scores <- append(scores, "na")
    }
    
    # create a dataframe containing each comment with his score for better respresentation
    df <- data.frame(scores, comments)
    
    # now after getting comment we filter them to get only french comments by using the filter below
    df <- df[!is_arabic(df$comments),]
    
    return(df)
  }

}


# this function check if the comment is in arabic or not
# params : text
# return : boolean True if is arabic & false if it's not
is_arabic <- function(text) {
  grepl("[\u0600-\u06ff]|[\u0750-\u077f]|[\ufb50-\ufc3f]|[\ufe70-\ufefc]", text, perl=FALSE)
}

is_english <- function(text){
  grepl("and", text, perl = FALSE)
}

setCommentsToFile <- function(df){
  write.table(df, file = "/home/pacman/hespress_medium/data.csv",sep = ",", row.names = FALSE, append = T, col.names = FALSE)
  #write.csv(df, file = "/home/pacman/hespress_medium/MyData.csv", append = TRUE)
}

# main function
# 1- get the pages urls 
page_links <- generatePagesLinks()

# 2- get the articles urls from the above pages
article_urls <- c()
for(i in 1:length(page_links)){
  article_urls <- append(article_urls, getArticlesUrls(page_links[i])) 
}

# now that we have links of articles well setted, we'll proceed our scrapper to get the shit done
for(i in 1:length(article_urls)){
  comments_df <- scrap_comments(article_urls[i])
  setCommentsToFile(comments_df)
}
