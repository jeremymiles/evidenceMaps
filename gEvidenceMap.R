
#' @param data The data file.
#' @param name The id variable that will be displayed as a label on 
#'   the final chart.
#' @param link The link that clicking on the bubble will take you to. 
#' @param xAxisVariable The name (quoted) of the variable representing the
#'   x (horizontal) in the data.
#' @param yAxisVariable The name (quoted) of the variable representing 
#'   the y (vertical) variable.
#' @param size The variable representing the count - the size of bubbles.
#' @param xAxisLabels
#' @param yAxisLabels
#' @param cat The categorical variables representing the colors.
#' @param catColors Colors to be used for the categories in the evidence map
#'   in alphabetical order. Color names can be taken from: 
#'   https://www.w3schools.com/tags/ref_colornames.asp. If colors are not specified,
#'   Google Charts will select them.
#' @param title Title for the chart



GoogleEvidenceMap <- function(data, name, link, xAxisVariable, yAxisVariable, size, 
                              xAxisLabels = NULL,
                              yAxisLabels = NULL,
                              catVar,
                              catColors = NULL,
                              title = "Bubble Plot",
                              fileName = "BubbleChart.html") {

  if(!(xAxisVariable %in% names(data))) {
    stop("xAxisVariable variable ", xAxisVariable, " not in data.")
  }
  
  
  if(!(name %in% names(data))) {
    stop("Name variable ", name, " not found in data.")
  }
  
  if(!(yAxisVariable %in% names(data))) {
    stop("yAxisVariable variable ", yAxisVariable, " not in data.")
  }
  


dNew <- data.frame(id = data[name])  


  # get number of yAxisVariable labels, for check
  yLabelsMatch <- tryCatch(all.equal(sort(yAxisLabels), 
                              sort(unique(data[[yAxisVariable]]))))
    if (yLabelsMatch != TRUE) { # no match
    stop("Y axis labels provided and Y Axis variables don't match.\n",
         "Y Axis Labels:\n", paste(sort(yAxisLabels), collapse = " "), "\n",
         "Y Axis Values in data\n",
         paste(sort(unique(data[[yAxisVariable]])), collapse = " "))
  }
  rm(yLabelsMatch)
  
  xLabelsMatch <- tryCatch(all.equal(sort(xAxisLabels), 
                                     sort(unique(data[[xAxisVariable]]))))
  if (xLabelsMatch != TRUE) { # no match
    stop("X axis labels provided and X Axis variables don't match.\n",
         "X Axis Labels:\n", paste(sort(xAxisLabels), collapse = " "), "\n",
         "X Axis Values in data\n",
         paste(sort(unique(data[[xAxisVariable]])), collapse = " "))
  }
  rm(xLabelsMatch)
  
  if(!is.null(catColors) & 
     length(catColors) != length(unique(data[[catVar]]))) {
    stop("Wrong number of category color labels supplied")
  }
  
  dNew$xAxisVariable <- data[[xAxisVariable]]
  dNew$yAxisVariable <- data[[yAxisVariable]]
  dNew$catVar <- data[[catVar]]
  
  # if labels are not supplied, create them alphabetically
  if (is.null(xAxisLabels)) {
    xAxisLabels <- unique(data[[outcomes]])
  }
  if (is.null(yAxisLabels)) {
    xAxisLabels <- unique(data[[yAxisVariable]])
  }

  dNew$link <- data[[link]]
  
  if (is.character(dNew$xAxisVariable)) {
  dNew$numericOutcome <- factor(dNew$xAxisVariable, 
                              levels = xAxisLabels, 
                              ordered = TRUE) %>% as.numeric
  } else {
    dNew$numericOutcome <- dNew$xAxisVariable
  }
  
  if (is.character(dNew$yAxisVariable)) {
    dNew$numericIntervention <- factor(dNew$yAxisVariable, 
                                  levels = yAxisLabels, 
                                  ordered = TRUE) %>% as.numeric
  } else {
    dNew$numericintervention <- dNew$yAxisVariable
  }
  
  dNew$name <- data[[name]]
  
  dNew$size <- data[[size]]

# Remove anything with missing data
nMissing <- apply(dNew,
                  1, function(x) {
                    sum(is.na(x))
                  })
if (max(nMissing) > 0) {
  warning("Some missing data found. These lines were removed from the data")
}
dNew <- dNew[nMissing == 0, ]

# Need to find duplicates, and jitter them
# Create table of counts
countTab <- as.data.frame(table(dNew$numericOutcome,
                                dNew$numericIntervention))

# loop through the table, 'cos I can't think of a better way.
for (i in 1:nrow(countTab)) {
  if(countTab$Freq[i] > 1) { #got a dupe
    matchData <- dNew[dNew$numericOutcome == countTab$Var1[i] &
           dNew$numericIntervention == countTab$Var2[i], ]
    shifts <- seq(from = -0.2, to = 0.2, length.out = nrow(matchData))
    matchData$numericIntervention <- matchData$numericIntervention + shifts
    dNew[dNew$numericOutcome == countTab$Var1[i] &
           dNew$numericIntervention == countTab$Var2[i], ]$numericIntervention <-
      matchData$numericIntervention
    }
}

dJSHeader <- paste0("['", name,"', '",
                    xAxisVariable, "', ",
                    "'", yAxisVariable, "', '",
                    catVar, "', '",
                    size, "', '",
                    link, "', ",
                    "{type: 'string', role: 'tooltip'}],")


dJSTable <- paste0("['", dNew$name,  "', ",
                   dNew$numericOutcome, ", ",
                   dNew$numericIntervention, ", ",
                   "'", dNew$catVar, "', ",
                   dNew$size, ", ",
                   "'", dNew$link, "', ",
                   "'", dNew$name, "'",  # names repeats, 'cos it's a tooltip (== hovertext)
                   # except this currently doesn't work with Google
                   # charts API when using bubble plots.
                   "],")




# Now put together HTML

html1 <- "<html>
  <head>
  <script type='text/javascript' src='https://www.gstatic.com/charts/loader.js'></script>
  <script type='text/javascript'>
  google.charts.load('current', {'packages':['corechart']});
google.charts.setOnLoadCallback(drawChart);

function drawChart() {
  
  var data = google.visualization.arrayToDataTable(["

#JS table here


html2 <- "      ]);\n\n"

html3 <- "      var options = {"
html4 <- paste0("title: '", title, "',\n")
html5 <- paste0("hAxis: {title: '", xAxisVariable, "',\n ticks: [")

# Sort out labels for the x-axis (xAxisVariable)
xAxisNums <- 1:length(unique(dNew$xAxisVariable))
# Do the zeroth first
html6 <- paste0("{v: 0, f: ''},")
# loop through all nums
html6 <- "{v: 0, f: ''},\n"
for (i in xAxisNums) {
  html6 <- paste0(html6,
                  "{v: ", i, ", f: '", xAxisLabels[i], "'},\n")
}
i <- i + 1
html6 <- paste0(html6, "{v: ", i, ", f: ' ", "'},\n
                ]
				},")
rm(i)           

html7 <- paste0("vAxis: {title: '", yAxisVariable, "',\n ticks: [")
# Sort out labels for the y-axis (yAxisVariable)
yAxisNums <- 1:length(unique(dNew$yAxisVariable))
# Do the zeroth first
html8 <- paste0("{v: 0, f: ''},")
# loop through all nums
html8 <- "{v: 0, f: ''},\n"
for (i in yAxisNums) {
  html8 <- paste0(html8,
                  "{v: ", i, ", f: '", yAxisLabels[i], "'},\n")
}
i <- i + 1
html8 <- paste0(html8, "{v: ", i, ", f: '", "'},\n")
rm(i)           
html9 <- paste0("]
				},
                bubble: {textStyle: {fontSize: 11}},")
if(!is.null(catColors)) {
  html10 <- "series: {"
  for(i in 1:(length(unique(dNew$catVar)))) {
    html10 <- paste0(html10, "'", unique(dNew$catVar)[i],
                     "': {color: '", catColors[i], "'},\n")
      }
}

html10 <- paste0(html10, "\n 		}};")

## Last bit!! (which we don't really need to understand)
html11 <- "

      var chart = new google.visualization.BubbleChart(document.getElementById('chart_div'));
chart.draw(data, options);
////////////////// Click Handler added from https://stackoverflow.com/questions/6205621/how-to-add-links-in-google-chart-api
var selectHandler = function(e) {
window.location = data.getValue(chart.getSelection()[0]['row'], 5 );
}

// Add our selection handler.
google.visualization.events.addListener(chart, 'select', selectHandler);
//}
///////////////////
}
</script>
</head>
<body>
<div id='chart_div' style='width: 1200px; height: 800px;'></div>
</body>
</html>"

# put them all together
allHtml <- c(html1, 
             dJSHeader,
             dJSTable,
             html2, 
             html3, 
             html4, 
             html5, 
             html6, 
             html7,
             html8,
             html9,
             html10, html11)

write.table(allHtml, file=fileName,
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)

browseURL(fileName)

}