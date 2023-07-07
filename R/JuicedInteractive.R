#This file implements a sample dashboard
#modified: December 2017
#author:   J. Kuckartz - joost.kuckartz@arcadis.com

#' Interactive dashboard
#' 
#' Display data from a data.table in an interactive graph. This function displays a sample dashboard
#' with many buttons lacking exact functionality. It's intention is for the code to be
#' modified to fit a dashboard designed for a specific application.
#' 
#' @param alldata The data.table to interactively display.
#' @param categorycolumnname String with column name to use for the different categories axis (default=\code{"CATEGORY"}).
#' @param plotsettings The settings for the plot. Has to be a modification of the returned list from \code{\link{getdefaultplotsettings}} 
#' default=\code{(NULL}, which plots according to the default settings as returned by \code{\link{getdefaultplotsettings}}).
#' @param stoponlastaction Set to \code{FALSE} to keep the interactive plot running when the last item in the dropdown item has been handled.
#' Default=\code{TRUE}, which closes the plot after the last item has been handled.
#' @family plot functions
#' @export
sampledashboard <- function(alldata,
                            plotsettings=NULL,
                            categorycolumnname="CATEGORY",
                            stoponlastaction=TRUE) {
  
  #general functions
  #function to get data for one approach
  getdisplaydata <- function(thedata,dropdownselection) {
    dispdata=thedata[get(categorycolumnname)==dropdownselection,]     #extract data for this category
    dispdata=setorderv(dispdata,plotsettings$xaxis)                   #sort by x axis
    dispdata[,SHOWID:=c(1:nrow(dispdata))]                            #create unique identifier
    message(paste0("Displaying category ",dropdownselection))
    return(dispdata)
  }
  
  #prepare data
  allcategories=sort(unique(alldata[,get(categorycolumnname)]))
  
  #make graphical interface (i.e. client, webpage)
  ui <- fluidPage(
    fillPage(
      tags$head(
        tags$style(type = "text/css",
                   ".topblock {float: left; padding-right: 10px;}",
                   ".topblocklower {float: left; padding-right: 10px; margin-bottom: -10px;}",
                   ".toprow {width: 100%;}",
                   ".clearrow {clear: both;}",
                   ".infotxt {min-height: 20px;}",
                   ".input100 {width: 100px;}",
                   ".input50 {width: 50px;}",
                   ".form-control {display: inline;}",
                   "#bottom {clear: both; display: inline-block; width: 100%; }",
                   "#thegraph {height: 85vh !important; }",
                   "#hoverinfo { position: fixed; bottom: 10px; right: 10px; width: 200px; border: 1px solid #000000; }",
                   "#buttonthumbup {border-color: #92d050;}",
                   "#buttonthumbupban {border-color: #c9d050;}",
                   "#buttonthumbdown {border-color: #7200ff;}",
                   "#buttonsearch {margin-bottom: 6px;}",
                   "#buttonclear {margin-bottom: 6px;}"
        )),
      div(class = "toprow",
          div(class = "topblock",
              h4("Buttonsection 1:"),
              actionButton("buttonplus", label=NULL, title="Plus", icon=icon("plus")),
              actionButton("buttonmin", label=NULL, title="Minus", icon=icon("minus")),
              actionButton("buttoncross", label=NULL, title="Cross", icon=icon("remove")),
              actionButton("buttoncheck", label=NULL, title="Check", icon=icon("check"))
          ),
          div(class = "topblock",
              h4("Buttonsection 2:"),
              actionButton("buttonthumbup", label=NULL, title="Thumbs Up", icon=icon("thumbs-o-up")),
              actionButton("buttonthumbupban", label=div(icon("thumbs-o-up"),icon("ban")), title="Thumbs Up Ban"),
              actionButton("buttonthumbdown", label=NULL, title="Thumbs Down", icon=icon("thumbs-o-down"))
          ),
          div(class = "topblocklower",
              h4("Dropdownsection with buttons:"),
              div(class = "topblock",
                  htmlOutput("reactivedropdownselector")                         #in htmlOutput because dropdown contains reactive variable
              ),
              div(class = "topblock",
                  actionButton("buttonleftarrow", label=NULL, title="Previous", icon=icon("arrow-left")),
                  actionButton("buttontable", label=NULL, title="View data", icon=icon("table")),
                  actionButton("buttonrightarrow", label=NULL, title="Next", icon=icon("arrow-right"))
              )
          ),
          div(class = "topblock",
              h4("Textsection with buttons:"),
              "Field 1: ",
              tags$input(id="inputtextfield1",type="text",class="input100 form-control"),
              " Field 2: ",
              tags$input(id="inputtextfield2",type="text",class="input50 form-control"),
              actionButton("buttonsearch", label=NULL, title="Search", icon=icon("search")),
              actionButton("buttonclear", label=NULL, title="Clear", icon=icon("ban")),
              span(class = "infotxt",
                   htmlOutput("inlinetext", inline=TRUE)
              )
          ),
          div(class = "clearrow")
      ),
      div(class = "infotxt",
          htmlOutput("underbuttoninfotext")
      ),
      div(id = "bottom",
          plotlyOutput("thegraph")
      ),
      htmlOutput("hoverinfo")
    )
  )
  #make server
  server <- function(input, output, session) {
    #store variables that need server action on change (initial values)
    v <- reactiveValues(
      plotdata = getdisplaydata(alldata,allcategories[1]),
      underbuttoninfodisplaytext = "default underbutton text",
      inlinedisplaytext = "default inline text",
      reactivedropdownitems = allcategories             #the reactivedropdownitems are also modified reactively (in reactivedropdownselector)
    )
    
    #create graphical interface elements that dynamically change (use 'htmlOutput("name")' in the ui section)
    output$reactivedropdownselector <- renderUI({               #here reactivedropdownidentifier is defined as it can be modified through button clicks
      selectInput("reactivedropdownidentifier", NULL, choices=v$reactivedropdownitems, selected=allcategories[1])
    })
    output$underbuttoninfotext <- renderText({
      paste0("Some display text: ",v$underbuttoninfodisplaytext)
    })
    output$inlinetext <- renderText({
      v$inlinedisplaytext
    })
    
	  #function to call when to update the display text. Can also be done inline using 'v$underbuttoninfodisplaytext='.
    updatedisplaytext <- function(texttodisplay) {
      v$underbuttoninfodisplaytext = texttodisplay
    }
    #function to call when to update the inline text. Can also be done inline using 'v$inlinedisplaytext='.
    updateinlinetext <- function(texttodisplay) {
      v$inlinedisplaytext = texttodisplay
    }
	
    selectnextindropdown <- function() {
      curselected=match(input$reactivedropdownidentifier,v$reactivedropdownitems)
      if (curselected<length(v$reactivedropdownitems)) {
        newdropdownselectedtext=v$reactivedropdownitems[curselected+1]
        updateSelectInput(session, "reactivedropdownidentifier", NULL, selected=newdropdownselectedtext)
      } else {
        message("No more reactivedropdownitems, this is the last.")
        if (stoponlastaction) {
          stopApp()
        }
      }
    }
	
    selectpreviousindropdown <- function() {
      curselected=match(input$reactivedropdownidentifier,v$reactivedropdownitems)
      if (curselected>1) {
        newdropdownselectedtext=v$reactivedropdownitems[curselected-1]
        updateSelectInput(session, "reactivedropdownidentifier", NULL, selected=newdropdownselectedtext)
      } else {
        message("Cannot select previous, this is the first")
      }
    }
  
    #deal with events
    observeEvent(input$buttonplus, {
      message("Clicked Plus Button")
	    updatedisplaytext("Clicked Plus Button")
    })
    observeEvent(input$buttonmin, {
      message("Clicked Min Button")
	    updatedisplaytext("Clicked Min Button")
    })
    observeEvent(input$buttonleftarrow, {              #Previous dropdown item button click
      message("Clicked Left Arrow Button")
	    updatedisplaytext("Clicked Left Arrow Button")
	    selectpreviousindropdown()
    })
    observeEvent(input$buttonrightarrow, {             #Next dropdown item button click
      message("Clicked Right Arrow Button")
	    updatedisplaytext("Clicked Right Arrow Button")
	    selectnextindropdown()
    })
    observeEvent(input$buttoncross, {                  #Cross button click
      message("Clicked Cross Button for ",input$reactivedropdownidentifier)
	    updatedisplaytext(paste0("Clicked Cross Button for ",input$reactivedropdownidentifier))
      selectnextindropdown()
    })
    observeEvent(input$buttoncheck, {                  #Check done button click
      message("Clicked Check Button for ",input$reactivedropdownidentifier)
	    updatedisplaytext(paste0("Clicked Check Button for ",input$reactivedropdownidentifier))
      selectnextindropdown()
    })
    observeEvent(input$buttontable, {                  #Table button click
	    message("Clicked Table Button for ",input$reactivedropdownidentifier,". Data displayed in main screen.")
	    updatedisplaytext(paste0("Clicked Check Button for ",input$reactivedropdownidentifier,". Data displayed in main screen."))
      View(v$plotdata)
    })
    observeEvent(input$buttonthumbup, {                #Thumb up button click
      message("Clicked Thumb Up Button")
	    updatedisplaytext("Clicked Thumb Up Button")
      selectnextindropdown()
    })
    observeEvent(input$buttonthumbupban, {             #Thumb up ban button click
      message("Clicked Thumb Up Ban Button")
	    updatedisplaytext("Clicked Thumb Up Ban Button")
      selectnextindropdown()
    })
    observeEvent(input$buttonthumbdown, {              #Thumb down button click
      message("Clicked Thumb Down Button")
	    updatedisplaytext("Clicked Thumb Down Button")
      selectnextindropdown()
    })
    observeEvent(input$buttonsearch, {                 #Search button click
	    message("Clicked Search Button with field 1 = ",input$inputtextfield1," and field 2 = ",input$inputtextfield2)
	    updatedisplaytext("Clicked Search Button")
	    updateinlinetext("Clicked Search Button")
    })
    observeEvent(input$buttonclear, {                  #Clear button click
    	message("Clicked Clear Button")
	    updatedisplaytext("Clicked Clear Button")
	    updateTextInput(session, "inputtextfield1", value = "")
      updateTextInput(session, "inputtextfield2", value = "")
      updateinlinetext("")
    })
    
	  #This event is called every time the dropdown item changes. Also when updateSelectInput() is called!
    observeEvent(input$reactivedropdownidentifier, {   #Dropdown for reactivedropdownitems changed
      v$plotdata=getdisplaydata(alldata,input$reactivedropdownidentifier)
      updatedisplaytext("Dropdown item changed")
    })
    #This event is called when data is selected.
    observeEvent(event_data("plotly_selected", source = "thegraph"), {              #graph points selection
      d <- event_data("plotly_selected", source = "thegraph")
      if (!is.null(d)) {
        if (length(d)) {
          d=as.data.table(d)
          message("Selected ",nrow(d)," data points")
          updatedisplaytext(paste0("Selected ",nrow(d)," data points"))
        }
      }
    })
    #This event catches the close button and stops the app, which allows other functions below to continue.
    session$onSessionEnded(function() {
      stopApp()
    })
    
    #This ensures that the hoverinfo htmlOutput gets rendered with a specific text.
    output$hoverinfo = renderText({                                                 #display hover text (doesn't need an 'observeEvent' function)
      d <- event_data("plotly_hover", source = "thegraph")
      if (!is.null(d)) {
        if (length(d)) {
          showdata=v$plotdata[SHOWID==d$key,]
          plotsettings$function_hovertext(showdata, plotsettings)
        }
      } else {
        "Hover on a point to get the information here."
      }
    })
	
    #render the plot
    output$thegraph <- renderPlotly({
	    plotscatter(v$plotdata,plotsettings=plotsettings,returnplotly=TRUE)
    })
  }
  #invoke display
  app=shinyApp(ui, server)
  runApp(app)
}