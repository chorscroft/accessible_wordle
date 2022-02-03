library(shiny)
library(magrittr)
library(ggplot2)

ui<-fluidPage(
  ## Input from Wordle "Share" button
  textInput("wordleIn",""),
  ## Press Go button to generate text and image
  actionButton("go","Go"),
  ## Text output will appear here
  verbatimTextOutput("result",placeholder = TRUE),
  ## Image will appear here
  mainPanel(
    plotOutput("plot",width = "200px",height = "200px",)
  )
)

server <- function(input,output){
  ## Get text output
  output$result <- renderText({
    ## Split wordle output into lines
    wordle_lines<-unlist(strsplit(input$wordleIn," "))
    
    ## Get first line of wordle output
    outputText<-paste(wordle_lines[1],wordle_lines[2],wordle_lines[3],"\n")
    
    ## For each line count the number of right and wrong letters
    currentline<-1
    for(wordle_line in wordle_lines[5:length(wordle_lines)]){
      ## count green letters
      green<-unlist(strsplit(wordle_line,""))=="🟩"
      ## Generate text output
      if (sum(green)==1){
        green_text<-paste0(sum(green)," letter in the right place")
      } else {
        green_text<-paste0(sum(green)," letters in the right place")
      }
      ## count yellow letters
      yellow<-unlist(strsplit(wordle_line,""))=="🟨"
      ## Generate text output
      if (sum(yellow)==1){
        yellow_text<-paste0(", ",sum(yellow)," letter in the wrong place")
      } else if (sum(yellow)>0){
        yellow_text<-paste0(", ",sum(yellow)," letters in the wrong place")
      } else {
        yellow_text=""
      }
      ## Final text output for this line
      outputText<-paste0(outputText,"\n","Line ",currentline,": ", green_text, yellow_text)
      ## Iterate line
      currentline<-currentline+1
    }
    # Output the text
    paste(outputText)
  }) %>%
    bindEvent(input$go)
  
  ## Get image output
  output$plot <- renderPlot({
    ## change wordle output emoji colours into words
    wordle_lines<-unlist(strsplit(input$wordleIn," "))
    long_wordle<-unlist(strsplit(wordle_lines[5:length(wordle_lines)],""))
    long_wordle[long_wordle=="🟩"]<-"green"
    long_wordle[long_wordle=="🟨"]<-"yellow"
    long_wordle[long_wordle=="⬛"]<-"black"
    
    ## plot the image
    ggplot(,aes(rep(c(1:5),length(wordle_lines)-4),rep(c((length(wordle_lines)-4):1),each=5))) +
      geom_point(col=long_wordle,pch=15,cex=10) + 
      xlim(0,6) +
      ylim(0,length(wordle_lines)-3) +
      theme_void()
    
  }) %>%
    bindEvent(input$go)
  
}

shinyApp(ui,server)
