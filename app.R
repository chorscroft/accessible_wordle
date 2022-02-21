library(shiny)
library(magrittr)
library(ggplot2)

ui<-fluidPage(
  titlePanel("Accessible wordle output"),
  mainPanel(
    p("1. Click SHARE on Wordle"),
    p("2. Paste into the text box"),
    p("3. Press Go"),
    p("4. Copy the text and the image into your social media of choice"),

    ## Input from Wordle "Share" button
    textInput("wordleIn",""),
    ## Press Go button to generate text and image
    actionButton("go","Go"),
    ## Text output will appear here
    verbatimTextOutput("result",placeholder = TRUE),
    ## Image will appear here
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
      ## count yellow letters
      yellow<-unlist(strsplit(wordle_line,""))=="🟨"
      if (sum(green)==0 & sum(yellow)==0){
        green_text<-"No letters guessed correctly"
        yellow_text<-""
      } else if (sum(green)==5){
        green_text<-paste0("Word guessed correctly!")
        yellow_text<-""
      } else if (sum(yellow)==5){
        green_text<-paste0("All letters in the wrong place")
        yellow_text<-""
      } else {
        if (sum(green)==0){
          green_text<-""
        } else {
          pos_green<-which(green)
          green_text<-switch(sum(green),
                             paste0("letter ",pos_green," is in the right place"),
                             paste0("letters ",pos_green[1]," and ",pos_green[2]," are in the right place"),
                             paste0("letters ",pos_green[1],", ",pos_green[2],", and ",pos_green[3]," are in the right place"),
                             paste0("letters ",pos_green[1],", ",pos_green[2],", ",pos_green[3],", and ",pos_green[4]," are in the right place"))
        }
        if (sum(green)>0 & sum(yellow)>0){
          green_text<-paste0(green_text,"; ")
        }
        if (sum(yellow)==0){
          yellow_text<-""
        } else {
          pos_yellow<-which(yellow)
          yellow_text<-switch(sum(yellow),
                              paste0("letter ",pos_yellow," is in the wrong place"),
                              paste0("letters ",pos_yellow[1]," and ",pos_yellow[2]," are in the wrong place"),
                              paste0("letters ",pos_yellow[1],", ",pos_yellow[2],", and ",pos_yellow[3]," are in the wrong place"),
                              paste0("letters ",pos_yellow[1],", ",pos_yellow[2],", ",pos_yellow[3],", and ",pos_yellow[4]," are in the wrong place"))
        }
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
    long_wordle[long_wordle=="🟨"]<-"gold"
    long_wordle[long_wordle=="⬛"]<-"black"
    
    ## plot the image
    ggplot(, aes(rep(c(1:5),length(wordle_lines)-4),rep(c((length(wordle_lines)-4):1),each=5))) + 
      geom_tile(aes(fill = long_wordle, size=2, width=0.95,height=0.95),show.legend = FALSE) +
      scale_fill_manual(breaks=c("green","gold","black"),values = c("green3","gold","black")) +
      coord_equal(expand = FALSE) +
      theme_void()
    
  }) %>%
    bindEvent(input$go)
  
}

shinyApp(ui,server)
