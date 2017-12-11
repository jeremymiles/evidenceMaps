
#' @param data The data file.
#' @param name The id variable that will be displayed as a label on 
#'   the final chart.
#' @param link The link that clicking on the bubble will take you to. 
#' @param outcome The name (quoted) of the variable representing the
#'   outcomes in the data.
#' @param intervention The name (quoted) of the variable representing 
#'   the intervention variable.
#' @param size The variable representing the count - the size of bubbles.
#' @param outcomeLabels
#' @param interventionLabels
#' @param cat The categorical variables representing the colors.
#' @param catColors Colors to be used for the categories in the evidence map
#'   in alphabetical order. Color names can be taken from: 
#'   https://www.w3schools.com/tags/ref_colornames.asp. If colors are not specified,
#'   Google Charts will select them.
#' @param title Title for the chart



GoogleEvidenceMap <- function(data, name, link,outcome, intervention, size, 
                              outcomeLabels = NULL,
                              interventionLabels = NULL,
                              catVar,
                              catColors = NULL,
                              title = "Bubble Plot",
                              fileName = "BubbleChart.html") {
  

    
  dNew <- data.frame(id = data[name])  
  
  # remove when function
  outcomeLabels <- c("CE1", "CE2", "PC1", "PC2", "AE1", "AE2")
  interventionLabels <- c("ActiveSurveillance", "SLNBiopsy",
    "Hormone", "Lumpectomy", "LumpectomyRT", "Mastectomy")
  
  if(!is.null(outcomeLabels) & 
     length(outcomeLabels) != length(unique(data[[outcome]]))) {
    stop("Wrong number of outcome variable labels supplied")
  }
  
  if(!is.null(interventionLabels) & 
     length(interventionLabels) != length(unique(data[[intervention]]))) {
    stop("Wrong number of intervention variable labels supplied")
  }

  if(!is.null(catColors) & 
     length(catColors) != length(unique(data[[catVar]]))) {
    stop("Wrong number of category color labels supplied")
  }
  
  dNew$outcome <- data[[outcome]]
  dNew$intervention <- data[[intervention]]
  dNew$catVar <- data[[catVar]]
  
  # if labels are not supplied, create them alphabetically
  if (is.null(outcomeLabels)) {
    outcomeLabels <- unique(data[[outcomes]])
  }
  if (is.null(interventionLabels)) {
    outcomeLabels <- unique(data[[intervention]])
  }

  dNew$link <- data[[link]]
  
  if (is.character(dNew$outcome)) {
  dNew$numericOutcome <- factor(dNew$outcome, 
                              levels = outcomeLabels, 
                              ordered = TRUE) %>% as.numeric
  } else {
    dNew$numericOutcome <- dNew$outcome
  }
  
  if (is.character(dNew$intervention)) {
    dNew$numericIntervention <- factor(dNew$intervention, 
                                  levels = interventionLabels, 
                                  ordered = TRUE) %>% as.numeric
  } else {
    dNew$numericintervention <- dNew$intervention
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
# Creat table of counts
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
                    outcome, "', ",
                    "'", intervention, "', '",
                    catVar, "', '",
                    size, "', '",
                    link, "', ",
                    "{type: 'string', role: 'tooltip'}],")


dJSTable <- paste0("['", dNew$name,  "', ",
                   dNew$numericOutcome, ", ",
                   dNew$numericIntervention, ", ",
                   "'", dNew$catVar, "', ",
                   dNew$size, ", ",
                   "'http://", dNew$link, "', ",
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
html5 <- paste0("hAxis: {title: '", outcome, "',\n ticks: [")

# Sort out labels for the x-axis (outcome)
xAxisNums <- 1:length(unique(dNew$outcome))
# Do the zeroth first
html6 <- paste0("{v: 0, f: ''},")
# loop through all nums
html6 <- "{v: 0, f: ''},\n"
for (i in xAxisNums) {
  html6 <- paste0(html6,
                  "{v: ", i, ", f: '", outcomeLabels[i], "'},\n")
}
i <- i + 1
html6 <- paste0(html6, "{v: ", i, ", f: ' ", "'},\n
                ]
				},")
rm(i)           

html7 <- paste0("vAxis: {title: '", intervention, "',\n ticks: [")
# Sort out labels for the y-axis (intervention)
yAxisNums <- 1:length(unique(dNew$intervention))
# Do the zeroth first
html8 <- paste0("{v: 0, f: ''},")
# loop through all nums
html8 <- "{v: 0, f: ''},\n"
for (i in xAxisNums) {
  html8 <- paste0(html8,
                  "{v: ", i, ", f: '", interventionLabels[i], "'},\n")
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

setwd("D:/Documents/susanne/shiny")
data <- readxl::read_excel("newMockData.xlsx") %>% as.data.frame

GoogleEvidenceMap(data = data, 
                  name = "names",
                  link = "ID",
                  outcome = "outcomes",
                  intervention = "interventions",
                  catVar = "confidence",
                  size = "SR",
                  title = "Bubble plot",
                  catColors = c("green", "papayawhip", "slategrey", "mediumseagreen"),
                  fileName = "bubbleChart.html")
