#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(googledrive)
library(googlesheets4)
library(tidyr)
library(dplyr)

#Paths to loop over 
jpegpaths <- 
    as.data.frame(drive_ls("https://drive.google.com/drive/u/0/folders/1vUhW32OZ78YELqNRgGgte3U7rCzob06t"))

jpegpaths <- jpegpaths %>%
    separate(name, into = c("pdfnum", "page"), sep = " - 1985 ", extra = "merge") %>%
    separate(page, into = c("page", NA), sep = "\\.", extra = "merge") 

    #Eventually this will get defined in the app
    path <- paste0(c("http://drive.google.com/uc?export=view&id=",jpegpaths$id[1]), collapse = "")

#Which fields get saved at the end 
fieldsAll <- c("firstname", "lastname", "middlename", "district", "streetnumber", "streetname",
               "streettype", "school", "city", "state", "zip", "grade", "dob", "status", "submit")

#Save the results to a file (change to google sheet?)
savedata <- function(data) {
    write.csv(x = data, file = file.path(responsesDir, rosterdataentry), 
              row.names = FALSE, quote = TRUE)
    
}

#Directory where responses get stored 
responsesDir <- file.path("responses")


# Define UI with external image call
ui <- fluidPage(
    titlePanel("Enter Roster Data"), 
    
    sidebarLayout(
        sidebarPanel(
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
                dateInput("dob", "Date of Birth", ""),
                textInput("status", "Status", ""),
                actionButton("submit", "Submit", class = "btn-primary"),
 
                )
        ),
    
         mainPanel(
                htmlOutput("picture")
         )
    )
)

# Define server with information needed to hotlink image
server <- function(input, output) {
    
    output$picture <- renderText({
            c('<img src="',
                path, 
                '" height = "800" width = "1000">'
            )
})}



# Run the application 
shinyApp(ui = ui, server = server)
