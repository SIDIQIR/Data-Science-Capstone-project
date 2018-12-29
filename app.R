

library(shiny)
library(tm)
library(ngram)
library(quanteda)
library(tidyr)

#rm(list = ls())

#Loading all three text files

#f = setwd("C:\\Coursera\\Data_Science_Capstone\\final\\en_US\\final\\Nextword")

set.seed(12345)
#blogs <- readLines(con ="en_US.blogs.txt", encoding = "UTF-8", warn = F)
#news <- readLines(con ="en_US.news.txt", encoding = "UTF-8", warn=F)
#twitter <- readLines(con = "en_US.twitter.txt", encoding = "UTF-8", warn = F)

#b = length(blogs)*0.01
#n = length(news)*0.1
#t = length(twitter)*0.01

# Sampling 1 to 10 percent of data depend on the file size

#blogs1 <- sample(blogs,b)
#news1 <- sample(news,n)
#twitter1 <- sample(twitter,t)


#text <- c(blogs1,news1,twitter1)

#Processing/cleaning our text data

cleantext1 <- function(tx){
  badwords <- readLines('badwords.txt', warn = F)
  t <- gsub(pattern=';|\\.|!|\\?', x=tx, replacement='')
  t <- gsub(pattern="[^[:alpha:]]", x=t, replacement = ' ')
  t <- removePunctuation(t, replace = " ")
  t <- removeWords(t,badwords)
  t <- tolower(t)
  t <- gsub(pattern="\\W*\\b\\w{1}\\b", x=t, replacement=' ')
  t <- gsub(pattern="\\s+", x=t, replacement=' ')
  t <- trimws(t)
  return (t)
}


#Creating ngram function

ngram1 <- function(x,y){
  
  c1 = c('w1', 'w2','w3','w4',"w5",'w6','w7','w8','w9')
  x1 <- dfm(x, ngrams = y, concatenator = " ")
  df1 <- as.data.frame(as.matrix(docfreq(x1)))
  dfs6 <- sort(rowSums(df1), decreasing = TRUE)
  df6 <- data.frame(Term = names(dfs6), Frequency = dfs6)
  df6 <- df6 %>% separate(Term, c1[1:y], " ")
  return (df6)
}



#cleantext <- cleantext1(text)



#ng2 <- ngram1(cleantext,2)
#ng3 <- ngram1(cleantext,3)
#ng4 <- ngram1(cleantext,4)

#saveRDS(ng2, file = "ng2.rds")
#saveRDS(ng3, file = "ng3.rds")
#saveRDS(ng4, file = "ng4.rds")
ng2 <- readRDS(file = "ng2.rds")
ng3 <- readRDS(file = "ng3.rds")
ng4 <- readRDS(file = "ng4.rds")



# Predicting next word

prediction <- function(ww1 ="") {
  
  
  ww <- cleantext1(ww1)
  wwsp = strsplit(ww, "\\s+")[[1]]
  z1 = tail(wwsp, n=1)
  y1 = tail(wwsp, n=2)
  w1 = tail(wwsp, n=3)
  
  i=1
  
  stop =FALSE
  
  if (length(wwsp)>=3){
    
    for (i in 1:length(ng4$w1)){
      
      if(any(w1[3]==ng4[i,3])==TRUE){
        
        print(ng4[i,4]) 
        stop = TRUE
        break
        
        
      }
      if (stop){break}
    }  
    
  } else if (length(wwsp)==2){
    
    for (i in 1:length(ng3$w1)){
      
      if(any(y1[2]==ng3[i,2])==TRUE){
        
        print(ng3[i,3]) 
        stop = TRUE
        break
      } 
      
      if (stop){break} 
    }
  } else if (length(wwsp)==1){
    
    for (i in 1:length(ng2$w1)){
      
      if(any(z1==ng2[i,1] )==TRUE){
        
        print(ng2[i,2]) 
        stop = TRUE
        break
      } 
      
      
      if (stop){break} 
    }
    
  } 
}



ui <- shinyUI(fluidPage(
  
  headerPanel(title = "Capstone Project Data Science"),
  
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", label = h3("Please enter a word or a sentence"), "these recommendations are easy to follow and except for - adding some herbs to your rinse", width = "250px", height = "200px"),
      #textInput("text", label = h2("Please enter a word or multiple words"), value = "First Cubs game ever! Wrigley field is gorgeous"),
      submitButton(text = "Predict next Word"),
      h3('Author: Rahim S' ),
      h3('Date: 12 28 2018')),
    
    
    mainPanel(
      
      h3("Next word prediction using N-gram" ),
      h2(),
      h4('Next word is ...'),
      verbatimTextOutput("Word", placeholder = T)
      
      
      
      
    ))
  ))

server<- shinyServer(function(input, output) {
  
  
  predictedWord <- reactive({prediction(input$text)})
  
  output$Word <- renderPrint({predictedWord()})
  
  
})

shinyApp(ui = ui, server = server)


