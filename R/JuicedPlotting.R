#This file implements generic plotting functions
#modified: February 2021
#author:   J. Kuckartz - jkuckartz1984@hotmail.com




#' Plot settings
#'
#' Get the default plotsettings for use in plotting functions. Settings contain the following elements:
#' \describe{
#' \item{\code{xaxis}}{This sets the column from the data.table to be used for the x axis.
#' A string containing only one column name.
#'  Default=\code{"x"}.
#' }
#' \item{\code{xaxisrangemode}}{This sets the x axis range setting.
#' If \code{"normal"}, all data is displayed. If \code{"tozero"}, the zero axis
#' is always shown. If \code{"nonnegative"}, the x axis range never goes below zero.
#' Default=\code{"normal"}
#' }
#' \item{\code{yaxes}}{This sets the columns from the data.table to display on the y axis.
#' Can be more than one string containing the column names.
#' If \code{graphtype="candlestick"} this needs to be of at least length 4,
#' containing the column information for 'open', 'close', 'high', 'low', in that order. Any
#' additional columns will be added as a regular scatter trace.
#' If \code{graphtype="piechart"} only the first provided column is used.
#' If \code{graphtype="treemap"} only the first provided column is used.
#' Default=\code{c("y")}.
#' }
#' \item{\code{yaxesvisible}}{This sets the visibility settings for the y axes.
#' Must be of same length as \code{plotsettings$yaxes},
#' and must be strings containing either \code{YES}, \code{NO}, or \code{legendonly}.
#' Default=\code{c("YES")}.
#' }
#' \item{\code{yaxeslinecolors}}{This sets the main display colors for the y axes.
#' Must be of same length as \code{plotsettings$yaxes},
#' and must be strings containing a color indication.
#' If \code{graphtype="scatter"} this setting is used for line colors.
#' If \code{graphtype="scatterstack"} this setting is used for line colors.
#' Fill colors will be automatically made based on this color.
#' If \code{graphtype="histogram"} this setting is used for bar colors.
#' If \code{graphtype="candlestick"} this setting is omitted.
#' If \code{graphtype="piechart"} this setting is omitted.
#' If \code{graphtype="treemap"} this setting is omitted.
#' Default=\code{c("#6C9DDC")}.
#' }
#' \item{\code{yaxesmarkercolors}}{This sets the columns from the data.table to use
#' as data source for the color of the markers on the representative y axis.
#' Must be of same length as \code{plotsettings$yaxes}
#' and be more than one string containing the column names, \code{NA} where
#' no markers are to be displayed, or \code{"linecolor"} if the marker needs to take the
#' same color as the line.
#' Works together with \code{plotsettings$markercolorsettings},
#' where the data in those columns are associated with colors.
#' If \code{graphtype="scatter"} this setting is used for marker colors.
#' If \code{graphtype="scatterstack"} this setting is used for marker colors.
#' If \code{graphtype="histogram"} this setting is used omitted.
#' If \code{graphtype="candlestick"} this setting is omitted.
#' If \code{graphtype="piechart"} this setting is used for pie colors.
#' If \code{graphtype="treemap"} this setting is used for block colors.
#' Default=\code{c("linecolor")}.
#' }
#' \item{\code{yaxesmarkersymbols}}{This sets the columns from the data.table to use
#' as data source for the symbols of the markers on the representative y axis.
#' Must be of same length as \code{plotsettings$yaxes}.
#' Can be more than one string containing the column names, or \code{NA} where
#' no symbols are to be displayed. Works together with \code{plotsettings$markersymbolsettings},
#' where the specific symbols for columns are associated.
#' If \code{graphtype="scatter"} this setting is used for marker colors.
#' If \code{graphtype="scatterstack"} this setting is used for marker colors.
#' If \code{graphtype="histogram"} this setting is used omitted.
#' If \code{graphtype="candlestick"} this setting is omitted.
#' If \code{graphtype="piechart"} this setting is omitted.
#' If \code{graphtype="treemap"} this setting is omitted.
#' Default=\code{c(NA)}.
#' }
#' \item{\code{yaxesmarkersizes}}{This sets the size of the marker (if displayed). Default=\code{8}.
#' Can be a single value, making all markers equal in size, or the same length as \code{plotsettings$yaxes}
#' allowing each y axis to have a different size marker. Missing values will use the first
#' provided marker size.
#' }
#' \item{\code{yaxesnames}}{This sets the name of the y axes, as to be displayed in the
#' legend. Also allows legend grouping by name. Must be of same length as \code{plotsettings$yaxes},
#' and must be strings containing the name or \code{NA} to use the same name as
#' provided in \code{plotsettings$yaxes}.
#' Default=\code{c(NA)}.
#' }
#' \item{\code{yaxisrangemode}}{This sets the y axis range setting.
#' If \code{"normal"}, all data is displayed.
#' If \code{"tozero"}, the zero axis is always shown.
#' If \code{"nonnegative"}, the y axis range never goes below zero.
#' Default=\code{"normal"}. This setting works globally, and not for each y axis individually.
#' }
#' \item{\code{displaytitle}}{This sets the graph title to a specified text. Must be a string
#' or \code{NULL} to display no title. Default=\code{NULL}.
#' }
#' \item{\code{displaylegend}}{This sets the visibility of a legend in the graph
#' (either \code{TRUE} or \code{FALSE}). Default=\code{TRUE}.
#' }
#' \item{\code{graphtype}}{This sets the graph type. Currently supported are
#' \code{"scatter"}  (will call \code{\link{plotscatter}}),
#' \code{"scatterstack"}  (will call \code{\link{plotscatter}}),
#' \code{"histogram"} (will call \code{\link{plothistogram}}),
#' \code{"candlestick"} (will call \code{\link{plotcandlestick}}),
#' \code{"piechart"} (will call \code{\link{plotpiechart}}), and
#' \code{"treemap"} (will call \code{\link{plottreemap}}).
#' Checks for \code{graphtype} happen in the dashboard, not in the \code{plot-} functions.
#' Default=\code{"scatter"}.
#' }
#' \item{\code{graphmode}}{This sets the mode of the graph, and is dependent
#' on what \code{graphtype} is chosen. Default=\code{NA}.
#' If \code{graphtype="scatter"} this setting can be a vector with the same length as
#'   \code{yaxes} and will be either \code{"lines"}, \code{"lines+markers"} or \code{"markers"}
#'   for the specific y axis data to be plotted in that mode. If the length is insufficient,
#'   it will default to the \code{NA} setting for that y axis.
#'   When \code{NA} is provided, it will use the \code{"lines+markers"} setting.
#' If \code{graphtype="scatterstack"} this setting is the same as \code{graphtype="scatter"}
#'   but \code{"none"} can be added to hide the lines.
#'   When \code{NA} is provided, it will use the \code{"none"} setting.
#' If \code{graphtype="histogram"} this setting is used for the \code{barmode} setting of
#'   the plot layout. Can be either \code{"group"}, \code{"stack"} or \code{"overlay"}.
#'   When \code{NA} is provided, it will use the \code{"group"} setting.
#' If \code{graphtype="candlestick"} this setting can be a vector with the same length as
#'   \code{yaxes} and will be either \code{"lines"}, \code{"lines+markers"} or \code{"markers"}
#'   for the specific y axis data to be plotted in that mode. If the length is insufficient,
#'   it will default to the \code{NA} setting for that y axis.
#'   When \code{NA} is provided, it will use the \code{"lines+markers"} setting.
#' If \code{graphtype="piechart"} this setting is omitted.
#' If \code{graphtype="treemap"} this setting is omitted.
#' }
#' \item{\code{markercolorsettings}}{This associates the column names of the data.table with the content in that column, and the color to use.
#' The association of data with colors only happens when \code{yaxesmarkercolors} is provided.
#' Consists of a list of list, in which the interior list contains:
#' \describe{
#'   \item{columnnames}{String vector of data.table column name that contains the data to be mapped.}
#'   \item{columncontent}{Vector of discrete data points to be found in this column of the data.table.
#'   If only two values are provided and they are numeric, then they are assumed to be
#'   the minimum and maximum value for a color ramp to be generated.
#'   Leaving this list item empty (NULL or NA) then a color ramp is automatically assumed, mapping
#'   the minimum and maximum value of the data.}
#'   \item{columncolors}{The color to use for each specific data point, or the various colors
#'   (minimum 2) of the color ramp to generate. To map discrete data points this list item must be of the same
#'   length as \code{columncontent} and can be a color name, a hex color,
#'   or \code{"linecolor"} (which takes the same color as the line,  specified in \code{plotsettings$yaxeslinecolors}).}
#' }
#' The default is a list too long to report here, but can be found by simply executing this function.
#' }
#' \item{\code{markersymbolsettings}}{This associates the column names of the data.table with the content in that column, and the symbol to use.
#' The association of data with colors only happens when \code{yaxesmarkersymbols} is provided.
#' Consists of a list of list, in which the interior list contains:
#' \describe{
#'   \item{columnnames}{String vector of data.table column name that contains the data to be mapped.}
#'   \item{columncontent}{Vector of discrete data points to be found in this column of the data.table.}
#'   \item{columnsymbols}{The symbol to use for each specific data point. Must be of the same length as \code{columncontent}.}
#' }
#' }
#' \item{\code{datafix_keepmissingvalues}}{This informs the plotting functions what to do with missing values. If \code{TRUE},
#' missing values are kept and can cause gaps in the display. If \code{FALSE}, missing values are removed which will create a
#' continuous display. Default=\code{TRUE}.
#' }
#' \item{\code{function_hovertext}}{This informs the plotting functions which function to use to create the hover text format.
#' Set to \code{NULL} or to a function that returns \code{NULL} to not display any hover text.
#' Default=\code{getdefaulthovertext}.
#' }
#' \item{\code{function_labeltext}}{This informs the plotting functions which function to use to create the label text format.
#' Set to \code{NULL} or to a function that returns \code{NULL} to not display any label text.
#' Default=\code{getdefaultlabeltext}, which simply returns \code{NULL} to avoid default label display.
#' }
#' \item{\code{labelposition}}{This sets the positioning method of the label text.
#' If \code{graphtype="scatter"} or \code{graphtype="scatterstack"} or \code{graphtype="treemap"} this setting can be a combination
#'   of \code{"top"}, \code{ "middle"} or \code{"bottom"} with \code{"left"}, \code{"center"} or \code{"right"}.
#' If \code{graphtype="histogram"} or \code{graphtype="piechart"} this setting can be either of \code{"inside"},
#'   \code{"outside"}, \code{"auto"} or \code{"none"}.
#' If \code{graphtype="candlestick"} this setting is only used for the additional scatter traces that follow the candlestick traces.
#' Default=\code{NA}, which takes either \code{"middle center"} or \code{"auto"} depending on \code{graphtype}.
#' }
#' }
#' @return List with default plot settings.
#' @export
#' @family plot functions
#' @export
getdefaultplotsettings <- function() {
  plotsettings=NULL

  plotsettings$xaxis="x"
  plotsettings$xaxisrangemode="normal"
  plotsettings$yaxes=c("y")
  plotsettings$yaxesvisible=c("YES")
  plotsettings$yaxeslinecolors=c("#6C9DDC")
  plotsettings$yaxesmarkercolors=c("linecolor")          #column name / "linecolor" to follow yaxeslinecolors / NA to not display markers
  plotsettings$yaxesmarkersymbols=c(NA)                  #column name / "linecolor" to follow yaxeslinecolors / NA to not display markers
  plotsettings$yaxesmarkersizes=8
  plotsettings$yaxesnames=c(NA)
  plotsettings$yaxisrangemode="normal"

  plotsettings$displaytitle=NULL
  plotsettings$displaylegend=TRUE
  plotsettings$graphmode=NA
  plotsettings$graphtype="scatter"



  plotsettings$markercolorsettings=list(list(
    columnnames=c("STRINGCOL"),                                   #column names
    columncontent=c("GOOD", "WARNING", "ALARM"),                  #column content to map to colors
    columncolors=c("linecolor","#FFA500","#FF0000")               #colors associated with content
  ),list(
    columnnames=c("BOOLCOL"),                                     #column names
    columncontent=c(TRUE, FALSE),                                 #column content to map to colors
    columncolors=c("linecolor","#FF0000")                         #colors associated with content
  ),list(
    columnnames=c("BOOLCOLINV"),                                  #column names
    columncontent=c(TRUE, FALSE),                                 #column content to map to colors
    columncolors=c("#FF0000","linecolor")                         #colors associated with content
  ),list(
    columnnames=c("RANGECOL"),                                    #column names
    columncontent=NULL,                                           #column content to map to colors
    columncolors=c("#FF0000","#00FF00")                           #colors associated with content
  ))

  plotsettings$markersymbolsettings=list(list(
    columnnames=c("STRINGCOL"),                                   #column names
    columncontent=c("ACTIVE", "INACTIVE"),                        #column content to map to symbols
    columnsymbols=c("o","x")                                      #symbols associated with content
  ),list(
    columnnames=c("BOOLCOL"),                                     #column names
    columncontent=c(TRUE, FALSE),                                 #column content to map to symbols
    columnsymbols=c("x","o")                                      #symbols associated with content
  ))

  plotsettings$datafix_keepmissingvalues=TRUE

  plotsettings$function_hovertext=getdefaulthovertext
  plotsettings$function_labeltext=getdefaultlabeltext
  plotsettings$labelposition=NA

  return(plotsettings)
}

#' Make plotsettings
#'
#' Simplified function to create automatic plotsettings for a range of yaxes names.
#' @param yaxesnames Vector with strings representing columns to display on the y axes.
#' @param sethuecolors Set to \code{FALSE} to keep each y axis in the default color.
#' Default=\code{TRUE}, which makes a unique line color for each y axis.
#' @return List with default plot settings as returned from \code{\link{getdefaultplotsettings}},
#' but with the default settings applied to more than one y axis.
#' @family plot functions
#' @export
makeplotsettings <- function(yaxesnames,sethuecolors=TRUE) {
  defset=getdefaultplotsettings()
  rl=length(yaxesnames)
  if (sethuecolors) {
    defset$yaxeslinecolors=gethuecolors(rl)
  } else {
    defset$yaxeslinecolors=rep(defset$yaxeslinecolors[1],rl)
  }
  defset$yaxes=yaxesnames
  defset$yaxesvisible=rep(defset$yaxesvisible[1],rl)
  defset$yaxesmarkercolors=rep(defset$yaxesmarkercolors[1],rl)
  defset$yaxesmarkersymbols=rep(defset$yaxesmarkersymbols[1],rl)
  defset$yaxesnames=rep(NA,rl)
  return(defset)
}

#####################################
# PLOT ASSISTING FUNCTIONS
#####################################

#' Generate hue colors
#'
#' Generate a range of hues (colors) for graphs
#' @param n The amount of colors to generate
#' @return Vector containing n amount of colors
#' @family plot assists
#' @export
gethuecolors <- function(n) {
  hues = seq(15, 375, length = n + 1)
  output = hcl(h = hues, l = 65, c = 100)[1:n]
  return(output)
}

#' Generate palette colors
#'
#' Generate a range of palette based colors. Uses the 70 unique colors from the RColorBrewer
#' library and recycles them to match the number of observations.
#' @param observations character vector or number vector
#' @return Data.table with the columns \code{OBSERVATIONS} and \code{COLOR}.
#' @family plot assists
#' @export
#' @examples observations =  1:100
#' DT_test = merge(
#'     x=data.table(BLABLA = observations),
#'     y=ut_create_colors(observations),
#'     by.x="BLABLA",
#'     by.y="OBSERVATIONS"
#'   )
#'
#'   table(
#'     data.table(
#'       table(DT_test$COLOR)
#'     )[order(-N)]$N
#'   )
getpalettecolors <- function(observations){
  require(RColorBrewer)
  ### 70 unique colors
  dt_kleuren = unique(
    data.table(COLOR=c(
      RColorBrewer::brewer.pal(8, "Accent"),
      RColorBrewer::brewer.pal(8, "Dark2"),
      RColorBrewer::brewer.pal(12, "Paired"),# RColorBrewer::display.brewer.pal(8, "Paired")
      RColorBrewer::brewer.pal(9, "Pastel1"),
      RColorBrewer::brewer.pal(8, "Pastel2"),
      RColorBrewer::brewer.pal(9, "Set1"),
      RColorBrewer::brewer.pal(8, "Set2"),
      RColorBrewer::brewer.pal(12, "Set3")
    ))
  )
  dt_kleuren$NUMMER = 1:nrow(dt_kleuren)

  dt_observations_color = data.table(OBSERVATIONS= unique(observations))
  dt_observations_color$NUMMER = 1:nrow(dt_observations_color) %% nrow(dt_kleuren)
  dt_observations_color[NUMMER==0,NUMMER:=nrow(dt_kleuren)]
  dt_observations_color = merge(
    x =  dt_observations_color,
    y = dt_kleuren,
    by="NUMMER"
  )
  dt_observations_color=dt_observations_color[,.(OBSERVATIONS,   COLOR)]
  dt_observations_color= unique(dt_observations_color)
  return(dt_observations_color)
}

#' Make colors
#'
#' Make the color settings for each data point in a graph.
#' @param plotdata The data to use in the plot.
#' @param plotsettings The settings to use in the plot.
#' @param whichaxis The integer value that indicates the axis used.
#' @return The color settings for each data point (list with a named vector \code{color}).
#' @family plot functions
plot_makecolors <- function(plotdata, plotsettings, whichaxis) {
  yaxislinecolor=plotsettings$yaxeslinecolors[whichaxis]                  #get yaxis line color
  yaxismarkercolorname=plotsettings$yaxesmarkercolors[whichaxis]          #get yaxis name for color
  if (is.na(yaxismarkercolorname)) {
    yaxismarkercolorname=NULL
  }
  #setup colors
  markercolors=NULL
  if (!is.null(yaxismarkercolorname)) {
    if (yaxismarkercolorname=="linecolor") {
      markercolors=yaxislinecolor
    } else {
      whichsetting=grep(yaxismarkercolorname,plotsettings$markercolorsettings, fixed = TRUE)[1]   #get the correct color setting based on column name
      if (!is.na(whichsetting)) {                                         #in case the column provided is not found
        whichsetting=plotsettings$markercolorsettings[[whichsetting]]

        #check if a colorramp is to be used: content is either NA or NULL or numeric with lenght 2
        usecolorramp=anyNA(whichsetting$columncontent) | is.null(whichsetting$columncontent) | (length(whichsetting$columncontent)==2 && is.numeric(whichsetting$columncontent))
        if (usecolorramp) {
          if (length(whichsetting$columncontent)==2 && is.numeric(whichsetting$columncontent)) {
            rampminval=min(whichsetting$columncontent)
            rampmaxval=max(whichsetting$columncontent)
          } else {
            rampminval=min(plotdata[,get(yaxismarkercolorname)])
            rampmaxval=max(plotdata[,get(yaxismarkercolorname)])
          }
          crfunc=colour_ramp(whichsetting$columncolors)                                     #make ramp
          minmaxmap=linearcalc(plotdata[,get(yaxismarkercolorname)],rampminval,rampmaxval)  #set the range values between 0 and 1
          markercolors=crfunc(minmaxmap)                                                    #make colors out of them
        } else {
          usecolors=str_replace_all(whichsetting$columncolors,"linecolor",yaxislinecolor)   #wherever "linecolor" is specified, use linecolor
          markercolors=mapvalues(plotdata[,get(yaxismarkercolorname)],whichsetting$columncontent,usecolors, warn_missing = FALSE)
        }
      }
    }
  }
  themarkers=NULL
  if (!is.null(yaxismarkercolorname)) {                                   #if colorname is NULL, then we will not display markers
    themarkers=list()
    themarkers$color=markercolors
  }
  return(themarkers)
}

#' Make markers
#'
#' Make the marker settings for each data point in a graph.
#' @param plotdata The data to use in the plot.
#' @param plotsettings The settings to use in the plot.
#' @param whichaxis The integer value that indicates the axis used.
#' @return The marker settings for each data point.
#' @family plot functions
plot_makemarkers <- function(plotdata, plotsettings, whichaxis) {
  #first make colors
  themarkers=plot_makecolors(plotdata, plotsettings, whichaxis)           #specific function that makes colors

  #now make symbols
  if (!is.null(themarkers)) {                                             #if no colors returned then we will not display anything
    yaxismarkersymbolname=plotsettings$yaxesmarkersymbols[whichaxis]      #get yaxis name for marker symbol
    if (is.na(yaxismarkersymbolname)) {
      yaxismarkersymbolname=NULL
    }
    markersymbols=NULL
    if (!is.null(yaxismarkersymbolname)) {
      whichsetting=grep(yaxismarkersymbolname,plotsettings$markersymbolsettings, fixed = TRUE)[1]
      if (!is.na(whichsetting)) {                                         #in case the column provided is not found
        whichsetting=plotsettings$markersymbolsettings[[whichsetting]]
        markersymbols=mapvalues(plotdata[,get(yaxismarkersymbolname)],whichsetting$columncontent,whichsetting$columnsymbols, warn_missing = FALSE)
      }
    }
    #now make sizes
    markersize=plotsettings$yaxesmarkersizes[whichaxis]                   #get marker size
    if (is.na(markersize))
      markersize=plotsettings$yaxesmarkersizes[1]

    themarkers$symbol=markersymbols
    themarkers$size=markersize
    themarkers$sizemin=markersize
  }
  return(themarkers)
}

#' Get hover text
#'
#' Make the hover text for each data point in a graph. This is the default function, other specialized
#' functions can be written for specific data. This function is provided as a variable in the plotsettings
#' used in plotting.
#'
#' The function has two purposes. First, it provides the hover text in the graph for a specific axis. Second,
#' it provides the text to display in an information field (if available).
#' @param plotdata The data to use in the plot.
#' @param plotsettings The settings to use in the plot (see \code{\link{getdefaultplotsettings}}).
#' @param whichaxis The integer value that indicates the y axis from the plotsettings is used.
#' Default=\code{0}, which does not specify a specific y axis.
#' @return The hover text for each data point.
#' @family plot functions
#' @export
getdefaulthovertext <- function(plotdata, plotsettings, whichaxis=0) {
  hasmultiaxes=length(plotsettings$yaxes)>1

  #prep variables
  addittext=NULL

  #get text for title
  titletext=NULL
  if (whichaxis>0) {
    yaxisnamename=plotsettings$yaxesnames[whichaxis]
    if (!is.na(yaxisnamename)) {
      titletext=paste0("<b>", yaxisnamename, "</b><br>")
    }
  }

  #get text from x axis ready
  disptext=paste0(plotsettings$xaxis, ": ", plotdata[,get(plotsettings$xaxis)], "<br>")
  #get text from y axes ready
  for (i in 1:length(plotsettings$yaxes)) {
    yaxisname=plotsettings$yaxes[i]
    thistext=paste0(yaxisname, ": ", plotdata[,get(yaxisname)], "<br>")
    if (whichaxis==i) {
  	  disptext=paste0(disptext, thistext)
  	} else {
  	  addittext=paste0(addittext, thistext)
  	}
  }

  #combine into final hover text
  if (hasmultiaxes & whichaxis>0) {
    disptext=paste0(titletext,"<b>Axes data:</b><br>",disptext,"<b>Additional data:</b><br>")
  }
  disptext=paste0(titletext,disptext,addittext)
  return(disptext)
}

#' Get label text
#'
#' Make the label text for each data point in a graph. This is the default function, other specialized
#' functions can be written for specific data. This function is provided as a variable in the plotsettings
#' used in plotting.
#' @param plotdata The data to use in the plot.
#' @param plotsettings The settings to use in the plot (see \code{\link{getdefaultplotsettings}}).
#' @param whichaxis The integer value that indicates the y axis from the plotsettings is used.
#' Default=\code{0}, which does not specify a specific y axis.
#' @return The label text for each data point.
#' @family plot functions
#' @export
getdefaultlabeltext <- function(plotdata, plotsettings, whichaxis=0) {
  return(NULL)
}

#' Update plotsettings
#'
#' Update the plotsettings based on the column names in the data. It checks if the provided settings
#' are possible to display. This function is always called from the plot functions.
#' @param plotsettings The plotsettings to check.
#' @param datacolnames The column names present in the data.
#' @return Updated plotsettings with axes removed that are not present in the data. If none of the axes
#' are present (either x-axis or y-axes), the function will return \code{NULL}.
#' @family plot functions
updateplotsettings <- function(plotsettings, datacolnames) {
  if (!plotsettings$xaxis %in% datacolnames) {
    message("Plotsetting x axis '",plotsettings$xaxis,"' not in data!")
    return(NULL)
  }
  extractor=plotsettings$yaxes %in% datacolnames
  if (all(extractor==FALSE)) {
    message("All plotsetting y axes not in data!")
    return(NULL)
  }
  if (any(extractor==FALSE)) {                                    #looks like there's a column not in the data
    plotsettings$yaxes=plotsettings$yaxes[extractor]
    plotsettings$yaxeslinecolors=plotsettings$yaxeslinecolors[extractor]
    plotsettings$yaxesmarkercolors=plotsettings$yaxesmarkercolors[extractor]
    plotsettings$yaxesmarkersymbols=plotsettings$yaxesmarkersymbols[extractor]
    plotsettings$yaxesnames=plotsettings$yaxesnames[extractor]
    plotsettings$yaxesvisible=plotsettings$yaxesvisible[extractor]
  }
  return(plotsettings)
}

#' Plot a scatterplot
#'
#' Plots a scatterplot with columns from a data.table.
#' @param datatable The data.table in which the data is stored to plot.
#' @param plotsettings The settings to use in the plot. Default=\code{NULL}, which loads default plotsettings
#' from \code{\link{getdefaultplotsettings}}.
#' @param returnplotly Set to \code{TRUE} to return the plotly variable to be used or modified in another setting (such as in
#' a dashboard). Default=\code{FALSE}, which displays the plot.
#' @param plotlysource Source attribute as defined in \code{\link[plotly]{plot_ly}}). Default=\code{"A"}.
#' @return The plotly variable if \code{returnplotly=TRUE} or an invisible \code{NULL}. It will always return \code{NULL} and
#' print a message if the plotsettings do not match the data.
#' @family plot functions
#' @export
plotscatter <- function(datatable,
                        plotsettings=NULL,
                        returnplotly=FALSE,
                        plotlysource="A") {
  if (is.null(plotsettings)) {
    plotsettings=getdefaultplotsettings()
  }
  #check if axes in data
  plotsettings=updateplotsettings(plotsettings=plotsettings, datacolnames=colnames(datatable))
  if (is.null(plotsettings)) {
    message("The provided plotsettings do not match the columns in the datatable. Cannot display the plot.")
	  return(NULL)
  }

  #make plot
  p <- plot_ly(type = 'scatter', mode="lines+markers", source=plotlysource)

  #add some general settings
  yaxisstackgroup=""
  if (plotsettings$graphtype=="scatterstack") {
    yaxisstackgroup="one"   #name does not matter it will stack all traces
  }

  isnavalue=-0.01
  for (i in 1:length(plotsettings$yaxes)) {                           #loop over all axes to plot
    #get graphmode
    yaxisgraphmode=plotsettings$graphmode[i]
    if (is.na(yaxisgraphmode)) {
      if (plotsettings$graphtype=="scatterstack") {
        yaxisgraphmode="none"                                         #default is to display nothing in a stack
      } else {
        yaxisgraphmode="lines+markers"                                #default is to display lins+markers in a scatter
      }
    }
    #get easy variables for current plot setting
    yaxisname=plotsettings$yaxes[i]                                   #get yaxis name
    yaxislinecolor=plotsettings$yaxeslinecolors[i]                    #get yaxis line color
    if (is.na(yaxislinecolor)) yaxislinecolor=plotsettings$yaxeslinecolors[1]
    yaxisnamename=plotsettings$yaxesnames[i]
    if (is.na(yaxisnamename)) yaxisnamename=yaxisname

    #setup y axis visibility
    yaxisvisibility=plotsettings$yaxesvisible[i]                      #prepare visibility of plot
    if (is.na(yaxisvisibility)) {  #missing setting
      yaxisvisibility=TRUE
    } else if (yaxisvisibility=="YES") {
      yaxisvisibility=TRUE
    } else if (yaxisvisibility=="NO") {
      yaxisvisibility=FALSE
    }

    #setup line color


    #prepare trace data with fix for missing values (NA only at the moment)
  	if (plotsettings$datafix_keepmissingvalues) {
  	  rowindices=c(1:nrow(datatable))                                 #get all indices
      tracey=ifelse(!is.na(datatable[,get(yaxisname)]),datatable[,get(yaxisname)],isnavalue)  #fix for NA values
  	} else {
      rowindices=which(!is.na(datatable[,get(yaxisname)]))            #get the indices where data is not NA
      tracey=datatable[rowindices,get(yaxisname)]
    }
    tracedata=datatable[rowindices,]
    tracex=datatable[rowindices,get(plotsettings$xaxis)]
    tracekey=NULL
    if ("SHOWID" %in% colnames(tracedata)) {
      tracekey=tracedata[,SHOWID]
    }

    #prepare specific plot items depending on mode
    theline=NULL
    themarkers=NULL
    if (str_detect(yaxisgraphmode,"marker")) {
      themarkers=plot_makemarkers(tracedata, plotsettings, i)
    }
    if (str_detect(yaxisgraphmode,"line")) {
      theline=list(color = yaxislinecolor)
    }

    #prepare text items for this axis
    thehovertext=NULL
    thelabeltext=NULL
    if (is.function(plotsettings$function_hovertext))
      thehovertext=plotsettings$function_hovertext(tracedata, plotsettings, i)
    if (is.function(plotsettings$function_labeltext))
      thelabeltext=plotsettings$function_labeltext(tracedata, plotsettings, i)
    yaxistextposition=plotsettings$labelposition
    if (is.na(yaxistextposition))
      yaxistextposition=NULL

    p <- add_trace(p,
                   mode = yaxisgraphmode,
                   data = tracedata,
                   stackgroup = yaxisstackgroup,
                   x = tracex,
                   y = tracey,
                   key = tracekey,
                   name = yaxisnamename,
                   legendgroup = yaxisnamename,
                   line = theline,
                   marker = themarkers,
                   text = thelabeltext,
                   textposition = yaxistextposition,
                   hoverinfo = "text",
                   hovertext = thehovertext,
                   visible = yaxisvisibility
    )
  }
  p <- layout(p,
              title = plotsettings$displaytitle,
              xaxis = list(title=plotsettings$xaxis, rangemode=plotsettings$xaxisrangemode),
              yaxis = list(rangemode=plotsettings$yaxisrangemode),
              showlegend = plotsettings$displaylegend,
              hovermode="closest"
  )
  p$elementId <- NULL #fix for annoying warnings (hopefully)

  if (returnplotly) {
    return(p)
  } else {
    return(methods::show(p))
  }
}

#' Plot a candlestick chart
#'
#' Plots a candlestick chart with columns from a data.table. For candlesticks to display,
#' four yaxes columns are needed. The first four in \code{plotsettings$yaxes} represent
#' the open, close, high, and low data (in that order). Any further yaxes will be displayed
#' based on the representative \code{plotsettings$graphmode}, or its default as lines.
#' @param datatable The data.table in which the data is stored to plot.
#' @param plotsettings The settings to use in the plot. Default=\code{NULL}, which loads default plotsettings
#' from \code{\link{getdefaultplotsettings}}.
#' @param returnplotly Set to \code{TRUE} to return the plotly variable to be used or modified in another setting (such as in
#' a dashboard). Default=\code{FALSE}, which displays the plot.
#' @param plotlysource Source attribute as defined in \code{\link[plotly]{plot_ly}}). Default=\code{"A"}.
#' @return The plotly variable if \code{returnplotly=TRUE} or an invisible \code{NULL}. It will always return \code{NULL} and
#' print a message if the plotsettings do not match the data.
#' @family plot functions
#' @export
plotcandlestick <- function(datatable,
                            plotsettings=NULL,
                            returnplotly=FALSE,
                            plotlysource="A") {
  if (is.null(plotsettings)) {
    plotsettings=getdefaultplotsettings()
  }
  #check if axes in data
  plotsettings=updateplotsettings(plotsettings=plotsettings, datacolnames=colnames(datatable))
  if (is.null(plotsettings)) {
    message("The provided plotsettings do not match the columns in the datatable. Cannot display the plot.")
    return(NULL)
  }
  if (length(plotsettings$yaxes)<4) {
    message("The provided plotsettings yaxes do not match candlestick data. Cannot display the plot.")
    return(NULL)
  }

  #make plot
  #p <- plot_ly(type = 'candlestick', source=plotlysource)
  p <- plot_ly(source=plotlysource)

  #here comes the information that represents candlesticks
  tracex=datatable[,get(plotsettings$xaxis)]
  yaxisname=plotsettings$yaxes[1]
  yaxisnamename=plotsettings$yaxesnames[1]
  if (is.na(yaxisnamename)) yaxisnamename=yaxisname
  traceopen=datatable[,get(yaxisname)]
  yaxisname=plotsettings$yaxes[2]
  traceclose=datatable[,get(yaxisname)]
  yaxisname=plotsettings$yaxes[3]
  tracehigh=datatable[,get(yaxisname)]
  yaxisname=plotsettings$yaxes[4]
  tracelow=datatable[,get(yaxisname)]

  hovertxt=""
  # There seems to be an issue with hovertext on candles... Doesn't display the text.
  # hovertxt=paste0("<b>",yaxisnamename,"</b><br>",
  #                "Open: ",traceopen,"<br>",
  #                "Close: ",traceclose,"<br>",
  #                "Low: ", tracelow,"<br>",
  #                "Close: ",traceclose)

  p <- add_trace(p,
                 type='candlestick',
                 data = datatable,
                 x = tracex,
                 open = traceopen,
                 close = traceclose,
                 high = tracehigh,
                 low = tracelow,
                 text = hovertxt,
                 hoverinfo = "x+y",
                 name = yaxisnamename
  )

  #if more information: add additional traces to candlestick data
  if (length(plotsettings$yaxes)>4) {
    isnavalue=-0.01
    for (i in 5:length(plotsettings$yaxes)) {                           #loop over all axes to plot
      #get graphmode
      yaxisgraphmode=plotsettings$graphmode[i]
      if (is.na(yaxisgraphmode)) yaxisgraphmode="lines"

      #get easy variables for current plot setting
      yaxisname=plotsettings$yaxes[i]                                   #get yaxis name
      yaxislinecolor=plotsettings$yaxeslinecolors[i]                    #get yaxis line color
      if (is.na(yaxislinecolor)) yaxislinecolor=plotsettings$yaxeslinecolors[1]
      yaxisnamename=plotsettings$yaxesnames[i]
      if (is.na(yaxisnamename)) yaxisnamename=yaxisname

      #setup y axis visibility
      yaxisvisibility=plotsettings$yaxesvisible[i]                      #prepare visibility of plot
      if (is.na(yaxisvisibility)) {  #missing setting
        yaxisvisibility=TRUE
      } else if (yaxisvisibility=="YES") {
        yaxisvisibility=TRUE
      } else if (yaxisvisibility=="NO") {
        yaxisvisibility=FALSE
      }

      #setup line color
      theline=list(color = yaxislinecolor)

      #prepare trace data with fix for missing values (NA only at the moment)
      if (plotsettings$datafix_keepmissingvalues) {
        rowindices=c(1:nrow(datatable))                                 #get all indices
        tracey=ifelse(!is.na(datatable[,get(yaxisname)]),datatable[,get(yaxisname)],isnavalue)  #fix for NA values
      } else {
        rowindices=which(!is.na(datatable[,get(yaxisname)]))            #get the indices where data is not NA
        tracey=datatable[rowindices,get(yaxisname)]
      }
      tracedata=datatable[rowindices,]
      tracex=datatable[rowindices,get(plotsettings$xaxis)]
      tracekey=NULL
      if ("SHOWID" %in% colnames(tracedata)) {
        tracekey=tracedata[,SHOWID]
      }

      #prepare markers for this axis
      themarkers=plot_makemarkers(tracedata, plotsettings, i)

      #prepare text items for this axis
      thehovertext=NULL
      thelabeltext=NULL
      if (is.function(plotsettings$function_hovertext))
        thehovertext=plotsettings$function_hovertext(tracedata, plotsettings, i)
      if (is.function(plotsettings$function_labeltext))
        thelabeltext=plotsettings$function_labeltext(tracedata, plotsettings, i)
      yaxistextposition=plotsettings$labelposition
      if (is.na(yaxistextposition))
        yaxistextposition=NULL

      p <- add_trace(p,
                     data = tracedata,
                     mode = yaxisgraphmode,
                     stackgroup = yaxisstackgroup,
                     x = tracex,
                     y = tracey,
                     key = tracekey,
                     name = yaxisnamename,
                     legendgroup = yaxisnamename,
                     line = theline,
                     marker = themarkers,
                     text = thelabeltext,
                     textposition = yaxistextposition,
                     hoverinfo = "text",
                     hovertext = thehovertext,
                     visible = yaxisvisibility
      )

      if (yaxisgraphmode=="markers") {
        p<-add_markers(p,
                       data = tracedata,
                       x = tracex,
                       y = tracey,
                       key = tracekey,
                       name = yaxisnamename,
                       legendgroup = yaxisnamename,
                       marker = themarkers,
                       text = thelabeltext,
                       textposition = yaxistextposition,
                       hoverinfo = "text",
                       hovertext = thehovertext,
                       visible = yaxisvisibility
        )
      } else {
        p <- add_trace(p,
                       type = "scatter",
                       mode = yaxisgraphmode,
                       data = tracedata,
                       x = tracex,
                       y = tracey,
                       key = tracekey,
                       name = yaxisnamename,
                       legendgroup = yaxisnamename,
                       line = theline,
                       marker = themarkers,
                       text = thelabeltext,
                       textposition = yaxistextposition,
                       hoverinfo = "text",
                       hovertext = thehovertext,
                       visible = yaxisvisibility
        )
      }
    }
  }
  p <- layout(p,
              title = plotsettings$displaytitle,
              xaxis = list(title=plotsettings$xaxis, rangemode=plotsettings$xaxisrangemode, rangeslider = list(visible = FALSE)),
              yaxis = list(rangemode=plotsettings$yaxisrangemode),
              showlegend = plotsettings$displaylegend,
              hovermode="closest"
  )
  p$elementId <- NULL #fix for annoying warnings (hopefully)

  if (returnplotly) {
    return(p)
  } else {
    return(methods::show(p))
  }
}

#' Plot a scatterplot
#'
#' Plots a scatterplot with columns from a data.table.
#' @param datatable The datatable in which the data is stored to plot.
#' @param xaxiscolumn String with column name to use for the x axis (default=\code{"x"}).
#' @param yaxiscolumn String with column name to use for the y axis (default=\code{"y"}).
#' @param namecolumn String with column name to use to display name information, meaning that each data point has
#' a name as specified in this column. Default=\code{NULL}, which does not use point names.
#' @param linkxaxiscolumn String with column name to use for the x axis, drawing linkage lines from the representing
#' \code{xaxiscolumn} and \code{yaxiscolumn}. Default=\code{NULL}, which draws no linkage lines between related points.
#' @param linkyaxiscolumn String with column name to use for the y axis, drawing linkage lines from the representing
#' \code{xaxiscolumn} and \code{yaxiscolumn}. Default=\code{NULL}, which draws no linkage lines between related points.
#' @param linknamecolumn String with column name to use to display name information for linkage points. Default=\code{NULL},
#' which uses the settings as specified in \code{namecolumn}.
#' @param plotsettings The settings to use in the plot. Default=\code{NULL}, which loads default plotsettings
#' from \code{\link{getdefaultplotsettings}}.
#' @param returnplotly Set to \code{TRUE} to return the plotly variable to be used or modified in another setting (such as in
#' a dashboard). Default=\code{FALSE}, which displays the plot.
#' @param plotlysource Source attribute as defined in \code{\link[plotly]{plot_ly}}). Default=\code{"A"}.
#' @return The plotly variable or an invisible \code{NULL}.
#' @family plot functions
#' @export
plotscatterwithlink <- function(datatable,
                        xaxiscolumn="x",
                        yaxiscolumn="y",
                        namecolumn=NULL,
                        linkxaxiscolumn=NULL,
                        linkyaxiscolumn=NULL,
                        linknamecolumn=NULL,
                        plotsettings=NULL,
                        returnplotly=FALSE,
                        plotlysource="A") {
  if (xaxiscolumn %in% colnames(datatable) & yaxiscolumn %in% colnames(datatable)) {
    if (is.null(plotsettings)) {
      plotsettings=getdefaultplotsettings()
    }
    thehoverinfo=NULL
    thetext=NULL
    if (!is.null(namecolumn)) {
      thehoverinfo="text"
      thetext=paste0(datatable[,get(namecolumn)],"<br>",
                     "x axis: ",datatable[,get(xaxiscolumn)],"<br>",
                     "y axis: ",datatable[,get(yaxiscolumn)])
    }
    p <- plot_ly(type = 'scatter', mode="lines+markers", source=plotlysource)
    p <- add_markers(p,                                             #base points
                     data = datatable,
                     x=datatable[,get(xaxiscolumn)],
                     y=datatable[,get(yaxiscolumn)],
                     color = I(plotsettings$markercolors[1]),
                     marker =  list (size = plotsettings$yaxesmarkersizes,
                                     sizemin = plotsettings$yaxesmarkersizes),
                     name = namecolumn,
                     showlegend = plotsettings$showlegend,
                     hoverinfo = thehoverinfo,
                     text = thetext
    )
    linklines=NULL
    if (!is.null(linkxaxiscolumn) & !is.null(linkyaxiscolumn)) {    #plot the link points and link lines
      thehoverinfo=NULL
      thetext=NULL
      if (!is.null(linknamecolumn)) {
        thehoverinfo="text"
        thetext=datatable[,get(linknamecolumn)]
      }
      p <- add_markers(p,                                           #link points
                       data = datatable,
                       x=datatable[,get(linkxaxiscolumn)],
                       y=datatable[,get(linkyaxiscolumn)],
                       color = I(plotsettings$markercolors[2]),
                       name = linknamecolumn,
                       marker =  list (size = plotsettings$yaxesmarkersizes,
                                       sizemin = plotsettings$yaxesmarkersizes),
                       showlegend = plotsettings$showlegend,
                       hoverinfo = thehoverinfo,
                       text = thetext
      )
      lineobj=list(type="line", line=list(color=plotsettings$linecolor, width=1),xref="x",yref="y")
      linklines=list()
      for (i in 1:nrow(datatable)) {                                #make many lines
        x0=datatable[i,get(xaxiscolumn)]
        y0=datatable[i,get(yaxiscolumn)]
        x1=datatable[i,get(linkxaxiscolumn)]
        y1=datatable[i,get(linkyaxiscolumn)]
        if (!is.na(x0) & !is.na(x1) & !is.na(y0) & !is.na(y1)) {
          lineobj[["x0"]]=datatable[i,get(xaxiscolumn)]
          lineobj[["y0"]]=datatable[i,get(yaxiscolumn)]
          lineobj[["x1"]]=datatable[i,get(linkxaxiscolumn)]
          lineobj[["y1"]]=datatable[i,get(linkyaxiscolumn)]
          linklines=c(linklines,list(lineobj))
        }
      }
    }
    p <-
      layout(p,
             title = plotsettings$displaytitle,
             xaxis = list(title=xaxiscolumn, rangemode="tozero"),
             yaxis = list(title=yaxiscolumn, rangemode="tozero"),
             showlegend = plotsettings$displaylegend,
             hovermode = "closest",
             shapes = linklines
      )
    p$elementId <- NULL #fix for annoying warnings (hopefully)

    if (returnplotly) {
      return(p)
    } else {
      return(methods::show(p))
    }
  }
}

#' Plot a histogram
#'
#' Plots a histogram with columns from a data.table.
#' @param datatable The datatable in which the data is stored to plot.
#' @param plotsettings The settings to use in the plot. Default=\code{NULL}, which loads default plotsettings
#' from \code{\link{getdefaultplotsettings}}.
#' @param returnplotly Set to \code{TRUE} to return the plotly variable to be used or modified in another setting (such as in
#' a dashboard). Default=\code{FALSE}, which displays the plot.
#' @param plotlysource Source attribute as defined in \code{\link[plotly]{plot_ly}}). Default=\code{"A"}.
#' @return The plotly variable or an invisible \code{NULL}.
#' @family plot functions
#' @export
plothistogram <- function(datatable,
                          plotsettings=NULL,
                          returnplotly=FALSE,
                          plotlysource="A") {
  if (is.null(plotsettings)) {
    plotsettings=getdefaultplotsettings()
  }
  #check if axes in data
  plotsettings=updateplotsettings(plotsettings=plotsettings, datacolnames=colnames(datatable))
  if (is.null(plotsettings)) {
    message("The provided plotsettings do not match the columns in the datatable. Cannot display the plot.")
    return(NULL)
  }

  xaxiscolumn=plotsettings$xaxis

  p <- plot_ly(source=plotlysource)
  for (i in 1:length(plotsettings$yaxes)) {                           #loop over all axes to plot
    #get easy variables for current plot setting
    yaxisname=plotsettings$yaxes[i]                                   #get yaxis name
    yaxisnamename=plotsettings$yaxesnames[i]
    if (is.na(yaxisnamename)) yaxisnamename=yaxisname

    yaxislinecolor=plotsettings$yaxeslinecolors[i]                    #get yaxis line color
    if (is.na(yaxislinecolor)) yaxislinecolor=plotsettings$yaxeslinecolors[1]

    #prepare text items for this axis
    thehovertext=NULL
    thelabeltext=NULL
    if (is.function(plotsettings$function_hovertext))
      thehovertext=plotsettings$function_hovertext(datatable, plotsettings, i)
    if (is.function(plotsettings$function_labeltext))
      thelabeltext=plotsettings$function_labeltext(datatable, plotsettings, i)
    yaxistextposition=plotsettings$labelposition
    if (is.na(yaxistextposition))
      yaxistextposition=NULL

    p <- add_trace(p,
                   type = 'bar',
                   data = datatable,
                   x=datatable[,get(xaxiscolumn)],
                   y=datatable[,get(yaxisname)],
                   marker =  list (color = yaxislinecolor),
                   name=yaxisnamename,
                   text = thelabeltext,
                   textposition = yaxistextposition,
                   hoverinfo = "text",
                   hovertext = thehovertext
    )
  }

  yaxistitle=""
  if (length(plotsettings$yaxes)==1) {
    yaxistitle=plotsettings$yaxes[1]
  }

  graphbarmode=plotsettings$graphmode
  if (is.na(graphbarmode)) graphbarmode="group"
  p <-
    layout(p,
           title = plotsettings$displaytitle,
           xaxis = list(title = xaxiscolumn, tickangle = -45),
           yaxis = list(title = yaxistitle),
           margin = list(b = 100),
           showlegend = plotsettings$displaylegend,
           hovermode = "closest",
           barmode = graphbarmode
    )
  p$elementId <- NULL #fix for annoying warnings (hopefully)

  if (returnplotly) {
    return(p)
  } else {
    return(methods::show(p))
  }
}

#' Plot a pie chart
#'
#' Plots a pie chart with columns from a data.table.
#' @param datatable The datatable in which the data is stored to plot.
#' @param plotsettings The settings to use in the plot. Default=\code{NULL}, which loads default plotsettings
#' from \code{\link{getdefaultplotsettings}}.
#' @param returnplotly Set to \code{TRUE} to return the plotly variable to be used or modified in another setting (such as in
#' a dashboard). Default=\code{FALSE}, which displays the plot.
#' @param plotlysource Source attribute as defined in \code{\link[plotly]{plot_ly}}). Default=\code{"A"}.
#' @param donuthole Set to a value between 0 and 1 to create a donut pie chart.
#' @return The plotly variable or an invisible \code{NULL}.
#' @family plot functions
#' @export
plotpiechart <- function(datatable,
                         plotsettings=NULL,
                         returnplotly=FALSE,
                         plotlysource="A",
                         donuthole=0) {
  if (is.null(plotsettings)) {
    plotsettings=getdefaultplotsettings()
  }
  #check if axes in data
  plotsettings=updateplotsettings(plotsettings=plotsettings, datacolnames=colnames(datatable))
  if (is.null(plotsettings)) {
    message("The provided plotsettings do not match the columns in the datatable. Cannot display the plot.")
    return(NULL)
  }

  xaxiscolumn=plotsettings$xaxis

  p <- plot_ly(source=plotlysource)

  for (i in 1:1) {                           #loop over all axes to plot (only one possible for pie chart)
    #get easy variables for current plot setting
    yaxisname=plotsettings$yaxes[i]                                   #get yaxis name
    yaxisnamename=plotsettings$yaxesnames[i]
    if (is.na(yaxisnamename)) yaxisnamename=yaxisname

    thecolors=plot_makecolors(datatable,plotsettings,i)

    #prepare text items for this axis
    thehovertext=NULL
    thelabeltext=NULL
    if (is.function(plotsettings$function_hovertext))
      thehovertext=plotsettings$function_hovertext(datatable, plotsettings, i)
    if (is.function(plotsettings$function_labeltext))
      thelabeltext=plotsettings$function_labeltext(datatable, plotsettings, i)
    yaxistextposition=plotsettings$labelposition
    if (is.na(yaxistextposition))
      yaxistextposition=NULL

    p <- add_trace(p,
                   type='pie',
                   data = datatable,
                   labels=datatable[,get(xaxiscolumn)],
                   values=datatable[,get(yaxisname)],
                   marker =  list (color = thecolors$color),
                   hole = donuthole,
                   name=yaxisnamename,
                   text = thelabeltext,
                   textposition = yaxistextposition,
                   hoverinfo = "text",
                   hovertext = thehovertext
    )
  }

  yaxistitle=""
  if (length(plotsettings$yaxes)==1) {
    yaxistitle=plotsettings$yaxes[1]
  }
  p <-
    layout(p,
           title = plotsettings$displaytitle,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           margin = list(b = 100),
           showlegend = plotsettings$displaylegend,
           hovermode = "closest"
    )
  p$elementId <- NULL #fix for annoying warnings (hopefully)

  if (returnplotly) {
    return(p)
  } else {
    return(methods::show(p))
  }
}

#' Plot density functions
#'
#' Plot density functions based on data from a data.table. Simplified plotting function,
#' without the use of any plotsettings from \code{\link{getdefaultplotsettings}}.
#' Uses the \code{\link[stats]{density}} function.
#' @param datatable The data.table with the data to plot density functions from.
#' @param columns The columns to plot density functions for. Default=\code{NULL}, which
#' plots the density functions for all columns.
#' @return Nothing, just plots the density functions.
#' @family plot functions
#' @export
plotdensityfunctions <- function(datatable,columns=NULL, returnplotly=FALSE, withlegend=TRUE) {
  p=NULL
  if (is.null(columns)) {
    columns=colnames(datatable)
  } else {
    columns = columns[columns %in% colnames(datatable)]   #filter the actual columns there
  }
  if (length(columns)) {
    dt_colors = getpalettecolors(sort(columns))
    p=plot_ly(type="scatter", mode="lines")
    for (thecol in sort(columns)) {
      kleur = dt_colors[OBSERVATIONS==thecol]$COLOR
      densfun <- density(datatable[,get(thecol)], n=2^12, na.rm=TRUE)
      p=add_trace(p,
                  x = densfun$x, y = densfun$y,
                  name = thecol, legendgroup=thecol,
                  showlegend=withlegend,
                  color = I(kleur)
                  )
    }
  }
  return(if(returnplotly==FALSE){methods::show(p)}else{p})
}

#' Plot ecdf functions
#'
#' Plot empirical cumulative distribution functions based on data from a data.table.
#' Uses the \code{\link[stats]{ecdf}} function. Simplified plotting function,
#' without the use of any plotsettings from \code{\link{getdefaultplotsettings}}.
#' @param datatable The data.table with the data to plot ecdf functions from.
#' @param columns The columns to plot ecdf functions for. Default=\code{NULL}, which
#' plots the density functions for all columns.
#' @param quantilerange The range to plot, between 0 and 1, at least two values.
#' Default=\code{c(0,1)}, which plots the full range.
#' @param inpercentage Set to \code{TRUE} to display percentages between 0 and 100.
#' Default=\code{FALSE}, which displays factors between 0 and 1.
#' @return Nothing, just plots the ecdf functions.
#' @family plot functions
#' @export
plotecdffunctions <- function(datatable,columns=NULL,quantilerange=c(0,1),inpercentage=FALSE,returnplotly=FALSE, withlegend=TRUE) {
  p=NULL
  if (is.null(columns)) {
    columns=colnames(datatable)
  } else {
    columns = columns[columns %in% colnames(datatable)]   #filter the actual columns there
  }
  if (length(columns)) {                                                      #if column names are there
    plotmin=NULL
    plotmax=NULL
    for (colname in columns) {                                                #loop over all of them
      qdat=quantile(datatable[,get(colname)],probs=quantilerange,na.rm=TRUE)  #get quantiles
      plotmin=min(plotmin,qdat)
      plotmax=max(plotmax,qdat)
    }
    xvals=seq(plotmin,plotmax,length.out = 2000)
    dt_colors = getpalettecolors(sort(columns))
    p=plot_ly(type="scatter", mode="lines")
    for (colname in sort(columns)) {
      ecdffun <- ecdf(datatable[,get(colname)])
      yvals=ecdffun(xvals)
      if (inpercentage) yvals=yvals*100
      kleur = dt_colors[OBSERVATIONS==colname]$COLOR
      p=add_trace(p,x = xvals, y = 100*yvals, mode = "lines", name = colname, legendgroup=colname, showlegend=withlegend,color = I(kleur))
    }
    p <- layout(p,
                xaxis = list(rangemode="tozero"),
                yaxis = list(range=c(0,100))
    )
  }
  return(if(returnplotly==FALSE){methods::show(p)}else{p})
}


