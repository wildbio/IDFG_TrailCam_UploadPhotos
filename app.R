# Remove existing objects from global environment
objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")

# Install/load required packages
dependencies<-c("rlang","shiny","shinyBS","shinydashboard","shinycssloaders","shinyjs",
                "tidyverse")

for(i in 1:length(dependencies)){
        if(dependencies[i] %in% installed.packages()==FALSE){
                install.packages(dependencies[i])
                require(dependencies[i],character.only=TRUE)
        } else{
                update.packages(dependencies[i])
                require(dependencies[i],character.only=TRUE)
        }
}

mycss <- "
.slick-prev {
left: 2%;
z-index: 1;
}
.slick-next {
right: 2%;
}
#plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
.radio label {
font-size: 18px;
}

.checkbox label {
font-size: 18px;
}

.shiny-split-layout > div {
overflow: visible;
}

"

# UI
ui <- dashboardPage(title = "Photo Upload App",
                    dashboardHeader(
                            # Set height of dashboardHeader
                            tags$li(class = "dropdown",
                                    tags$style(".main-header {max-height: 70px}"),
                                    tags$style(".main-header .logo {
                       height: 70px; 
                       line-height: 75px !important;
                       padding: 0 0px}")
                            ),
                            # Use image in title
                            title = "Photo Upload App",
                            titleWidth = "200px",
                            tags$li(a(href = 'http://idfg.idaho.gov',
                                      img(src = 'idfglogo.png',
                                          title = "Idaho Fish and Game", height = "50px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown")
                    ),
                    dashboardSidebar(disable=TRUE),
                    dashboardBody(
                            fluidPage(
                                    fluidRow(
                                            column(width = 12, align = "center",
                                                   
                                                   h1(HTML("<center><b>Welcome to the <br>
                                    <font face=\"Gadugi\" size = 8 color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>Photo Upload App</b></font>
                                    <br>for remote camera projects</center></b>")),
                                                   br(),
                                                   wellPanel(h4(HTML("This app uploads renamed photos to the IDFG Data 
                                                   Team's cloud storage account on Microsoft Azure. 
                                                   <br><b>Please only use this app if requested to do so 
                                                                     by someone on the Data Team</b>")))
                                                   ),
                                            column(width = 4, align = "left",
                                                   wellPanel(
                                                           h3(HTML("<b>1. Upload Speed</b>")),
                                                           h4(HTML("<a href=\"https://www.speedtest.net/\">Check upload speed here</a> and enter into box below")),
                                                           numericInput("upload_speed",HTML("<font size = 4>Upload Speed (Mbps)</font>"), 
                                                                        value = 50, min = 1, max = 500),
                                                           uiOutput("upload_time"))
                                                   ),
                                            column(width = 4, align = "left",
                                                   wellPanel(
                                                           h3(HTML("<b>2. Project/photo details</b>")),
                                                           selectInput(inputId = "project", label = HTML("<font size=4>Project</font>"),
                                                                       choices = c("", "EOE2020 (Ecology of everything)" = "EOE2020",
                                                                                   "CAMLN2020 (Caribou mountain lions)" = "CAMLN2020",
                                                                                   "SWWLF2020 (Statewide wolf)" = "SWWLF2020",
                                                                                   "SWUNG2020 (Statewide ungulate)" = "SWUNG2020")),
                                                           selectizeInput(inputId = "region", label = HTML("<font size=4>Region</font>"),
                                                                          choices = c("",1:7),
                                                                          options = list(create = TRUE)),
                                                           h4(tags$b('Choose folder of photos to upload')),
                                                           uiOutput(outputId = "loc_note"),
                                                           actionButton(inputId = "chooseFolderButton", label = HTML("<font size = 4>Click to Choose Folder</font>"), 
                                                                        style="color: #fff; background-color: #6faade; border-color: #5491c7; width: 100%"),
                                                           uiOutput("selectedFolder"),
                                                           br(),
                                                           textInput(inputId = "password", label = HTML("<font size=4>Password</font>")),
                                                           h4(tags$b('Ready to upload?')),
                                                           actionButton('upload',HTML('<font size = 4><b>Upload Photos</b></font>'), 
                                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%"))
                                                   ),
                                            column(width = 4, align = "left",
                                                   wellPanel(
                                                           h3(HTML("<b>3. Upload Confirmation</b>")),
                                                           uiOutput(outputId = "upload_message") %>% withSpinner(color="#3c8dbc"))
                                                   )# column
                                    ) # fluidrow
                            ) # fluidpage
                    ) # dashboard body
) # dashboard page



server <- function(input, output, session) {
        output$upload_time <- renderUI({
                req(input$upload_speed)
                speed <- reactive({
                        round(input$upload_speed/2,2)
                })
                time_min <- reactive({
                        round((15000/speed())/60,2)
                })
                time_20cams <- reactive({
                        round((time_min()*20)/60,2)
                })
                list(h4(HTML(paste0("Time to upload <b>20,000 photos</b> (about one camera) at half-bandwidth (", speed()," Mbps): <b>",
                                    time_min(), " minutes</b>"))),
                     h4(HTML(paste0("Approximate time to upload <b>20 cameras</b> at half-bandwidth (", speed()," Mbps): <b>",
                                    time_20cams(), " hours</b>")))
                )
                
        })
        
        region <- reactive({
                req(input$region)
                paste0("R",input$region)
        })
        
        output$loc_note <- renderUI({
                req(input$region)
                h5(HTML(paste0("Select the folder <b>", 
                               input$project,"\\",input$project,"_rename\\",region(),
                               "</b> created by the SD Card Processing App on your external hard drive")))
        })
        
        # Choose folder with original photos
        observeEvent(input$chooseFolderButton, {
                orig_folder <- choose.dir("")
                if(!is.na(orig_folder)){
                        output$selectedFolder <- renderUI({
                                checkboxGroupInput(inputId = "selectedFolder", label = "",
                                                   choices = orig_folder, selected = orig_folder)
                        })
                } else {"Folder selection cancelled."}
        })
        
        output$upload_message <- renderUI({
                if (input$upload == 0){
                        return()
                }
                else {
                        isolate({ 
                                shiny::withProgress(
                                        value = 0, {
                                                region <- reactive({
                                                        req(input$region)
                                                        paste0("R",input$region)
                                                })
                                                
                                                pdir <- reactive({
                                                        req(input$selectedFolder)
                                                        rdir <- dirname(input$selectedFolder)
                                                        dirname(rdir)
                                                })
                                                
                                                # Create upload log file
                                                upload_log <- file.path(pdir(), 
                                                                        paste0(input$project,"_",region(),"_UploadLog.txt"))
                                                lapply(upload_log, function(x) if(!file.exists(x)) file.create(x))
                                                
                                                incProgress(2/10, message = "Uploading to the cloud...")
                                                cloud <- paste0("\"https://idfgupload.blob.core.windows.net/idfg/",input$project,"/",region(),
                                                                "?st=2019-10-30T16%",input$password,"%3A57Z&se=2032-10-31T16%",input$password,"%3A00Z&sp=racwdl&sv=2018-03-28&sr=c&sig=3ObVsb2Rb9gmdBW0Rvh%2FmyhLuB0w5ZoLwpmjVaR%2B2cI%3D\"")
                                                half_bandwidth <- reactive({
                                                        round(input$upload_speed/2,0)
                                                })
                                                speed <- paste0("--cap-mbps ", half_bandwidth())
                                                push_photos <- paste("azcopy sync", input$selectedFolder, cloud, speed)
                                                
                                                sysout <- file(upload_log, "a")
                                                sink(sysout,append = T)
                                                upld<-system(push_photos, intern = T)
                                                print(paste("~~~~~~~~~~~~~~~~~~~~~~Photo Upload Log ",Sys.time()," ~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
                                                print(upld)
                                                sink()
                                                close(sysout)
                                                
                                                log_cloud <- paste0("\"https://idfgupload.blob.core.windows.net/idfg/",input$project,"/log_files?st=2019-10-30T16%",
                                                                    input$password,"%3A57Z&se=2032-10-31T16%", input$password,
                                                                    "%3A00Z&sp=racwdl&sv=2018-03-28&sr=c&sig=3ObVsb2Rb9gmdBW0Rvh%2FmyhLuB0w5ZoLwpmjVaR%2B2cI%3D\"")
                                                csv_cloud <- paste0("\"https://idfgupload.blob.core.windows.net/idfg/",input$project,"/metadata_csvs?st=2019-10-30T16%",
                                                                  input$password,"%3A57Z&se=2032-10-31T16%", input$password,
                                                                  "%3A00Z&sp=racwdl&sv=2018-03-28&sr=c&sig=3ObVsb2Rb9gmdBW0Rvh%2FmyhLuB0w5ZoLwpmjVaR%2B2cI%3D\"")
                                                log_files <- paste0(pdir(),"/*.txt")
                                                csv_files <- paste0(pdir(),"/*.csv")
                                                push_err <- paste("azcopy copy", log_files, log_cloud, speed)
                                                push_csv <- paste("azcopy copy", csv_files, csv_cloud, speed)
                                                upld_err<-system(push_err, intern = T)
                                                upld_csv<-system(push_csv, intern = T)
                                                
                                                if(T %in% str_detect(upld, "Failed: 0") | F %in% str_detect(upld,"Copy Transfers: 0") | F %in% str_detect(upld,"Error")){
                                                        list(wellPanel(
                                                                style = "background:#d0e2f2",
                                                                h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>SUCCESS!</b></font></center>")),
                                                                br(),
                                                                h4(HTML("<center><b>You uploaded all your photos to the cloud!</b></center>")),
                                                                br(),
                                                                h4(HTML("Feel free to <a href=\"mailto:amanda.carr@idfg.idaho.gov\">contact us</a> 
                                                                     with any questions"))
                                                                )
                                                        )
                                                } else {
                                                        list(wellPanel(
                                                                style = "background:#d0e2f2",
                                                                h1(HTML("<center><font color=\"#3c8dbc\" style = \"text-shadow: 1.5px 1.5px #4d3a7d\"><b>WELL SHUCKS</b></font></center>")),
                                                                br(),
                                                                h4(HTML("<center><b>Something went wrong during the upload</b></center>")),
                                                                br(),
                                                                h4(HTML(paste0("<br><br>Please check ",upload_log, " for information regarding this error, 
                                                                                            and/or <a href=\"mailto:amanda.carr@idfg.idaho.gov\">send us the file</a> for troubleshooting")))
                                                                )
                                                             
                                                        )
                                                } # else upload fail
                                        } # shiny with progress
                                ) # shiny with progress
                        }) # isolate
                } #else
        }) # renderUI
        
        
        
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

