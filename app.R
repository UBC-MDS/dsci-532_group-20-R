# Import packages
library(tidyverse, quietly = TRUE)
library(plotly)
library(reticulate)
use_virtualenv("532")
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)

# Import functions from data_wrangling script
source_python('data_wrangling.py')

columns <- list('Reservations', 'Average daily rate per person', 'Adults', 
'Children','Babies', 'Required parking spaces', 'Booking changes', 'Special requests')

app <- Dash$new(external_stylesheets= dbcThemes$BOOTSTRAP)
server <- app$server #to deploy the app. 

# Setup app layout and front-end
jumbotron <- dbcJumbotron(
        dbcContainer(
            list(
                htmlH1("Super Hotel Management", className="display-3"),
                htmlP(
                    "This is an interactive dashboard based on the data from the Hotel Booking Demand dataset",
                    className="lead",
                )
            )
        ),
    fluid=TRUE,
)

info_area <- dbcContainer(
    list(
        dbcRow(
            list(
                dbcCol(
                    list(
                        htmlBr(),
                        htmlBr(),
                        htmlH4("Select Feature for Main Plot"),
                        htmlBr(),
                        dccDropdown(
                            id="y-axis-dropdown",
                            options= lapply(columns, function(x) {
                                list(label = x, value = x)
                            }
                            ),
                            value=columns[1],
                            multi=FALSE,
                            searchable=FALSE,
                            clearable=FALSE,
                        ),
                        htmlBr(),
                        htmlBr(),

                        htmlH4("Select Hotel Type"),
                        htmlBr(),
                        dccRadioItems(
                            id = 'hotel-type-selection',
                            options = list(list("label" = "All Hotels", "value" = "All"),
                                           list("label" = "Resort Hotel", "value" = "Resort"),
                                           list("label" = "City Hotel", "value" = "City")
                            ),
                            value = "All",
                            labelStyle = list("display" = "block")
                        ),
                        htmlBr(),
                        htmlBr(),
                        htmlH4("Select Weeks"),
                        htmlBr(),
                        dccRangeSlider(
                            id = "week-selection",
                            min = 1,
                            max = 53,
                            value = list(1, 53),
                            marks = list("1" = "", "9" = "Spring", "22" = "Summer", "35" = "Fall", "48" = "Winter", "53" = "")
                        )
                    ) 
                ),
                dbcCol(
                    list(
                        dbcRow(
                            list(
                                dccGraph(
                                    id="lines",
                                    style=list("border-width" = "0", "width" = "100%", "height" = "500px")
                                    )
                                )
                            )
                        ),
                        dbcRow(
                            list(
                                dccGraph(
                                    id = "hist1",
                                    style=list("border-width" = "0", "width" = "50%", "height" = "500px")
                                        ),
                                dccGraph(
                                    id = "hist2",
                                    style=list("border-width" = "0", "width" = "50%", "height" = "500px")
                            )
                        )
                    )
                )
            )
        )
    )
)

app$layout(htmlDiv(list(jumbotron, htmlBr(), info_area)))

# Callbacks and back-end
app$callback(
    output("lines", "figure"),
    list(input("hotel-type-selection", "value"),
    input("week-selection", "value"),
    input("y-axis-dropdown", "value")),
# Function to plot the main plot using selected hotel type and y variables
    function(hotel_type, x_col, y_col) {
        df <- main_plot(hotel_type, x_col, y_col)
        title_text <- paste(y_col, " for each week in the year")

        chart <- ggplot(df, aes(x = `Arrival week`,
                            y = y_col)) +
            geom_line() +
            ggtitle(title_text) + 
            xlab("Week numbers") + 
            ylab(y_col)
        ggplotly(chart)
    }
)

app$callback(
    output("hist1", "figure"),
    list(input("hotel-type-selection", "value"),
    input("week-selection", "value")),
# Function to plot the bottom left plot using selected hotel type
    function(hotel_type, weeks){
        df <- left_plot(hotel_type, weeks)

        chart <- ggplot(df, aes(x = `Country of origin`)) +
            geom_histogram() +
            ggtitle("Top 20 Home Countries of Guests") + 
            xlab("Countries") + 
            ylab("Reservations")
        ggplotly(chart)
    }
)

app$callback(
    output("hist2", "figure"),
    list(input("hotel-type-selection", "value"),
    input("week-selection", "value")),
# Function to plot the bottom right plot using selected hotel type
    function(hotel_type, weeks){
    df <- right_plot(hotel_type, weeks)
 
    chart <- ggplot(df, aes(x = `Number of Nights of Stay`)) +
                geom_histogram() +
                ggtitle("Length of Guests Stay") + 
                xlab("Number of nights of stay") + 
                ylab("Percent of Reservations")
            ggplotly(chart)
    }
)

app$run_server(debug = T)
