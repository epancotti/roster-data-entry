library(shiny)
library(shinyjs) #enable reset
library(rdrop2)
library(tidyverse)


server<- function(input, output, session) {
    # store temporary values
    tempvalues <- reactiveValues()
    # load token to get access to dropbox account
    drop_auth(rdstoken = "droptoken.rds")
    #user data
    
    dataReactive <- reactive({
      tibble(firstname=input$firstname,
                  lastname=input$lastname,
                  middlename=input$middlename,
                  district=input$district,
                  streetnumber=input$streetnumber,
                  streettype=input$streettype,
                  school=input$school,
             city=input$city,
             state=input$state,
             zip=input$zip,
             grade=input$grade,
             dob=input$dob,
             status=input$status,
             pic=tempvalues$current_file)
      
    })
    
    # function to load next pic
    next_pic<-function(){
        # get the name of the next image
        list_of_files<-drop_dir("todo") 
        # check whether there any pics left
        if (length(list_of_files$name)>0){
        # save name of current file
        tempvalues$current_file<-list_of_files$name[1]
        # download the next image
        myImage <- reactive({
            # content of to do folder
            drop_download(paste("todo/", tempvalues$current_file,sep=""),overwrite = TRUE)
        })
        # send image to ui
        output$image <- renderImage({
            myImage()
            list(src =  tempvalues$current_file,alt="No pic loaded.",width="400px")
        }, deleteFile = FALSE)
        # Update status
        output$status <- renderText({ 
            paste("Last action: Loaded pic:", tempvalues$current_file, "at", Sys.time())
        })
        }
        else{
          # Update status
          output$status <- renderText({ 
            paste("No more images")
          })
        }
        
    }  
    # Load first pic
    next_pic()
    
    # SUBMIT BUTTON
    observeEvent(input$submit, {
        # load the existing data
        df<-drop_read_csv('ep_demo.csv')
        # append  the new user data
        temp_data <- dataReactive()
        temp_data<-rbind(df,temp_data)
        # Update data file
        write.csv(temp_data, 'ep_demo.csv', row.names=FALSE)
        drop_upload('ep_demo.csv')
      # Update status
        output$status <- renderText({ 
            paste("Last action: Saved data for pic:", tempvalues$current_file, "at ",Sys.time())
        })
        
        
    })
    
    # LOAD NEXT BUTTON
    observeEvent(input$load, {
        # Move pic file
        drop_move(paste("todo/", tempvalues$current_file,sep=""), paste("done/", tempvalues$current_file,sep=""))
        # Load pic
        next_pic()
        # Reset inputs
        reset("form")
    })
    
    # RESET (move files back, only for testing)
    observeEvent(input$reset, {
        # get a list of all files in done folder
        a<-drop_dir("done") 
        # move all files
        for (p in     a$name) {
            # Move file
            drop_move(paste("done/",p,sep=""), paste("todo/", p,sep=""))
        }
    })
    
}


ui <- fluidPage( 
    useShinyjs(),
    titlePanel("Enter Roster Data"), 
    
    sidebarLayout(
        sidebarPanel(  tags$style(HTML("#form { height: 90vh; overflow-y: auto; font-size: 10px,margin-bottom: 5px;} 
                                       .form-group{margin-bottom: 5px; font-size: 10px}" )),
            div(
                id = "form",
                textInput("firstname", "First Name", ""),
                textInput("lastname", "Last Name", ""),
                textInput("middlename", "Middle Name (or initial)", ""),
                textInput("district", "Community Name", ""),
                numericInput("streetnumber", "Street Number", ""),
                textInput("streetname", "Street Name", ""),
                textInput("streettype", "Street Suffix (St., Rd., etc.", ""),
                textInput("school", "School", ""),
                textInput("city", "City", ""),
                textInput("state", "State (2 letters, like MA)", ""),
                numericInput("zip", "Zip Code", ""),
                textInput("grade", "Grade", ""),
                dateInput("dob", "Date of Birth",  "2000-01-1"),
                textInput("status", "Status", ""),
                actionButton("load", "Load next", class = "btn-primary"),
                actionButton("submit", "Submit", class = "btn-primary"),
                actionButton("reset", "Reset (move files back)", class = "btn-primary"),
                
                
            )
        ),
        
        mainPanel(
            textOutput("status"),
            br(),
            imageOutput("image")
        )
    )
)


shinyApp(ui, server)