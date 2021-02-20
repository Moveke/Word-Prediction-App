#Front end user interface code for the word prediction
#Shiny application.
suppressWarnings(library(shiny))

shinyUI(fluidPage(
  
  #Do some additional preformatting of the app user interface, including
  #setting background color, font style and font size.
  tags$head(tags$style(
    HTML('
        body, input, button, select { 
            font-family: "Helvetica";
            font-size: 30px;
            font-weight: bold;
            background-color: #CBFAFF;
        }')
  )),  
  
  #Make the text of the input phrase typed in by the user nice and large.
  tags$style("#phrase {font-size:30px;height:30px;}"),
  
  # Application title
  titlePanel(HTML("Word Prediction Application")),

  #Sidebar with a radiobutton panel to select the prediction context of
  #blogs, news or twitter.
  sidebarLayout(
    sidebarPanel(
      h3("Choose document type prediction (context):"),
      radioButtons("doc_type", "Document Type:",
                   c("Blogs" ="blogs",
                     "News" = "news",
                     "Twitter" = "twitter")),
      p("After selecting the prediction type above, enter a phrase of
        words in the text box in the main panel and then push the 'Predict!'
        button.  The predicted word following your phrase will then
        be shown.", style='font-size:16px'),
      p(" "),
      p("A presentation providing more details about this app is provided at the following link:",
        style='font-size:16px'), 
      tags$a(href="http://rpubs.com/pinion87/wordprediction", "Prediction App", 
             style='font-size:16px')     
    ),
  
    #In the main panel, show the text box that allows the user to type
    #the input phrase, the button to execute the prediction and also show
    #the predicted word upon completion.  
    mainPanel(
      titlePanel(HTML("Next Word Prediction")),
      h3("Please enter a test phrase and then hit the predict button"),
      textAreaInput("phrase", "Test phrase:", height= "100%", width= "200%"),
      actionButton("predictButton", "Predict!",style='font-size:30px'),
      h3("Predicted word ('NA' means no additional words found):"),
      textOutput("nText")
              
      )
    )
))
