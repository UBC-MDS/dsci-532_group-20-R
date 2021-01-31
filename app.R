# Import packages
library(tidyverse, quietly = TRUE)
library(reticulate)
library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)

columns <-
  list(
    'Reservations',
    'Average daily rate',
    'Adults',
    'Children',
    'Babies',
    'Required parking spaces',
    'Booking changes',
    'Special requests'
  )

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

top_plot <-
  function(hotel_type = "All",
           x_col = list(1, 53),
           y_col = "Reservations") {
    df <- main_plot(hotel_type, x_col, y_col)
    title_text <- paste(y_col, " for each week in the year")
    chart <- ggplot(df, aes(x = `Arrival week`,
                            y = !!sym(y_col))) +
      geom_line(color = "orange") +
      ggtitle(title_text) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 25)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      xlab("Week numbers") +
      ylab(y_col) +
      theme_bw() +
      theme(
        text = element_text(size = 13),
        plot.title = element_text(size = 22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")
      )
    ggplotly(chart)
  }

bottom_left_plot <- function(hotel_type = "All",
                             weeks = list(1, 53)) {
  df <- left_plot(hotel_type, weeks)
  histogram_l <-
    ggplot(df, aes(x = reorder(`Country of origin`,-counts), y = counts)) +
    geom_bar(stat = "identity", fill = "orange") +
    ggtitle("Top 10 Countries of Guests") +
    xlab("Countries") +
    ylab("Reservations") +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 15),
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black")
    )
  ggplotly(histogram_l)
}

bottom_right_plot <- function(hotel_type = "All",
                              weeks = list(1, 53)) {
  df <- right_plot(hotel_type, weeks)
  histogram_r <-
    ggplot(df,
           aes(x = `Number of Nights of Stay`, y = `Percent of Reservations`)) +
    geom_bar(stat = "identity", fill = "orange") +
    ggtitle("Length of Guests Stay") +
    xlim(0, 10) +
    xlab("Number of nights of stay") +
    ylab("Percent of Reservations") +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      plot.title = element_text(size = 15),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black")
    )
  ggplotly(histogram_r)
}

############### NEW DATA WRANGLING FUNCTIONS #######################
getdata <- function(hotel_type = "All",
                    weeks = c(1, 53)) {
  hotels <- read_csv("data/processed/clean_hotels.csv")
  hotel_trim <- hotels
  hotel_trim <-
    do.call(data.frame, lapply(hotel_trim, function(x)
      replace(x, is.infinite(x), NA)))
  colnames(hotel_trim) <- str_replace_all(colnames(hotel_trim), c("\\." = " "))
  hotel_trim %>% drop_na()
  if (hotel_type == "Resort") {
    hotel_trim <- hotel_trim %>%
      filter(`Hotel type` == "Resort")
  }
  if (hotel_type == "City") {
    hotel_trim <- hotel_trim %>%
      filter(`Hotel type` == "City")
  }
  hotel_trim <- hotel_trim %>%
    filter(`Arrival week` >= weeks[1]) %>%
    filter(`Arrival week` <= weeks[2])
  hotel_trim
}

main_plot <-
  function(hotel_type = "All",
           weeks = c(1, 53),
           y_col = "Reservations") {
    df <- getdata(hotel_type, weeks)
    reservations_weekly <- df %>%
      group_by(`Arrival week`) %>%
      summarise(Reservations = n() / length(unique(df$`Arrival year`)))
    prices_weekly <- df %>% 
      group_by(`Arrival week`) %>%
      summarise(`Average daily rate` = mean(`Average daily rate`))
    parking_weekly <- df %>%
      group_by(`Arrival week`) %>%
      summarise(`Required parking spaces` = sum(`Required parking spaces`) /
                  length(unique(df$`Arrival year`)))
    adults_weekly <- df %>%
      group_by(`Arrival week`) %>%
      summarise(Adults = sum(Adults) / length(unique(df$`Arrival year`)))
    children_weekly <- df %>%
      group_by(`Arrival week`) %>%
      summarise(Children = sum(Children) / length(unique(df$`Arrival year`)))
    babies_weekly <- df %>%
      group_by(`Arrival week`) %>%
      summarise(Babies = sum(Babies) / length(unique(df$`Arrival year`)))
    changes_weekly <- df %>%
      group_by(`Arrival week`) %>%
      summarise(`Booking changes` = sum(`Booking changes`) / length(unique(df$`Arrival year`)))
    special_weekly <- df %>%
      group_by(`Arrival week`) %>%
      summarise(`Special requests` = sum(`Special requests`) / length(unique(df$`Arrival year`)))

    # TODO: fix infinities problem in `Average rate per person`
    # it is causing `prices weekly` to break
    data_weekly <- left_join(reservations_weekly, prices_weekly)
    data_weekly <- left_join(data_weekly, parking_weekly)
    data_weekly <- left_join(data_weekly, adults_weekly)
    data_weekly <- left_join(data_weekly, children_weekly)
    data_weekly <- left_join(data_weekly, babies_weekly)
    data_weekly <- left_join(data_weekly, changes_weekly)
    data_weekly <- left_join(data_weekly, special_weekly)
    data_weekly
  }

left_plot <- function(hotel_type = "All",
                      weeks = c(1, 53)) {
  df <- getdata(hotel_type, weeks)
  top_10_countries <- df %>%
    group_by(`Country of origin`) %>%
    summarise(counts = n()) %>%
    arrange(desc(counts))
  top_10_countries[1:10,]
}

right_plot <- function(hotel_type = "All",
                       weeks = c(1, 53)) {
  df <- getdata(hotel_type, weeks)
  df <- df %>%
    mutate(`Total Nights of Stay` = `Weekend nights` + `Week nights`)
  stay_nights <- data.frame(table(df$`Total Nights of Stay`))
  stay_nights <- stay_nights %>%
    mutate(`Percent of Reservations` = Freq / sum(Freq) * 100)
  colnames(stay_nights) <-
    c("hotel", "Number of Nights of Stay", "Percent of Reservations")
  stay_nights
}
#####################################################################

top_graph <- dccGraph(id = "lines",
                      figure = top_plot(),
                      style = list(width = 700))

bottom_left_graph <- dccGraph(
  id = "hist_l",
  figure = bottom_left_plot(),
  style = list(width = 350, height = 250)
)

bottom_right_graph <- dccGraph(
  id = "hist_r",
  figure = bottom_right_plot(),
  style = list(width = 350, height = 250)
)

# Setup app layout and front-end
jumbotron <- dbcJumbotron(dbcContainer(list(
  htmlH1("Super Hotel Management", className = "display-3"),
  htmlP(
    "This is an interactive dashboard based on the data from the Hotel Booking Demand dataset",
    className = "lead",
  )
)),
fluid = TRUE)


info_area <- dbcContainer(dbcRow(list(dbcCol(
  list(
    htmlH4("Select Feature for Main Plot"),
    htmlBr(),
    dccDropdown(
      id = "y-axis-dropdown",
      options = map(columns, function(x) {
        list(label = x, value = x)
      }),
      value = 'Reservations',
      multi = FALSE,
      searchable = FALSE,
      clearable = FALSE
    ),
    htmlBr(),
    htmlBr(),
    htmlH4("Select Hotel Type"),
    htmlBr(),
    dccRadioItems(
      id = 'hotel-type-selection',
      options = list(
        list("label" = " All Hotels", "value" = "All"),
        list("label" = " Resort Hotel", "value" = "Resort"),
        list("label" = " City Hotel", "value" = "City")
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
      marks = list(
        "1" = "",
        "9" = "Spring",
        "22" = "Summer",
        "35" = "Fall",
        "48" = "Winter",
        "53" = ""
      )
    )
  ),
  md = 4
),
dbcCol(
  list(dbcRow(list(top_graph)),
       dbcRow(list(
         dbcCol(bottom_left_graph),
         dbcCol(bottom_right_graph)
       )))
))))

app$layout(htmlDiv(list(jumbotron, htmlBr(), info_area)))

# Callbacks and back-end
app$callback(
  output = list(id = "lines", property = "figure"),
  params = list(
    input(id = "hotel-type-selection", property = "value"),
    input(id = "week-selection", property = "value"),
    input(id = "y-axis-dropdown", property = "value")
  ),
  # Function to plot the main plot using selected hotel type and y variables
  function(hotel_type, x_col, y_col) {
    top_plot(hotel_type = hotel_type,
             x_col = x_col,
             y_col = y_col)
  }
)


app$callback(
  output = list(id = "hist_l", property = "figure"),
  params = list(
    input(id = "hotel-type-selection", property = "value"),
    input(id = "week-selection", property = "value")
  ),
  # Function to plot the bottom left plot using selected hotel type
  function(hotel_type, weeks) {
    bottom_left_plot(hotel_type = hotel_type, weeks = weeks)
  }
)

app$callback(
  output = list(id = "hist_r", property = "figure"),
  params = list(
    input(id = "hotel-type-selection", property = "value"),
    input(id = "week-selection", property = "value")
  ),
  # Function to plot the bottom right plot using selected hotel type
  function(hotel_type, weeks) {
    bottom_right_plot(hotel_type = hotel_type, weeks = weeks)
  }
)

app$run_server(debug = T)
