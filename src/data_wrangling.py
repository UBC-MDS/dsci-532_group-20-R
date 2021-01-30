import pandas as pd
import numpy as np

# Read in data from processed data
def getdata(hotel_type="All", weeks=[1, 53]):
    hotels = pd.read_csv("data/processed/clean_hotels.csv")
    hotel_trim = hotels.copy()

    # filter based on hotel type selection
    hotel_trim = hotel_trim.replace([np.inf, -np.inf], np.nan).dropna()
    if hotel_type == "Resort":
        hotel_trim = hotel_trim[hotel_trim["Hotel type"] == "Resort"]
    if hotel_type == "City":
        hotel_trim = hotel_trim[hotel_trim["Hotel type"] == "City"]
    # filter based on weeks selected
    hotel_trim = hotel_trim[hotel_trim["Arrival week"].between(weeks[0], weeks[1])]

    return hotel_trim


# Dataframe for main plots:
def main_plot(hotel_type="All", weeks=[1, 53], y_col="Reservations"):
    df = getdata(hotel_type, weeks)
    reservations_weekly = (
        df.groupby("Arrival week")["Hotel type"].count()
        / df["Arrival year"].value_counts().count()
    )
    prices_weekly = df.groupby("Arrival week")["Average daily rate per person"].mean()
    parking_weekly = (
        df.groupby("Arrival week")["Required parking spaces"].sum()
        / df["Arrival year"].value_counts().count()
    )
    adults_weekly = (
        df.groupby("Arrival week")["Adults"].sum()
        / df["Arrival year"].value_counts().count()
    )

    # canceled_weekly = df.groupby("Arrival week")["Canceled"].count() / df["Arrival year"].value_counts().count()
    children_weekly = (
        df.groupby("Arrival week")["Children"].sum()
        / df["Arrival year"].value_counts().count()
    )
    babies_weekly = (
        df.groupby("Arrival week")["Babies"].sum()
        / df["Arrival year"].value_counts().count()
    )
    changes_weekly = (
        df.groupby("Arrival week")["Booking changes"].sum()
        / df["Arrival year"].value_counts().count()
    )
    special_weekly = (
        df.groupby("Arrival week")["Special requests"].sum()
        / df["Arrival year"].value_counts().count()
    )

    data_weekly = pd.merge(reservations_weekly, prices_weekly, on="Arrival week")
    data_weekly = pd.merge(data_weekly, parking_weekly, on="Arrival week")
    data_weekly = pd.merge(data_weekly, adults_weekly, on="Arrival week")
    # data_weekly = pd.merge(data_weekly, canceled_weekly, on="Arrival week")
    data_weekly = pd.merge(data_weekly, children_weekly, on="Arrival week")
    data_weekly = pd.merge(data_weekly, babies_weekly, on="Arrival week")
    data_weekly = pd.merge(data_weekly, changes_weekly, on="Arrival week")
    data_weekly = pd.merge(data_weekly, special_weekly, on="Arrival week")
    data_weekly = data_weekly.rename(
        columns={"Hotel type": "Reservations"}
    ).reset_index()

    return data_weekly


# Dataframe for countries plot:
def left_plot(hotel_type="All", weeks=[1, 53]):
    df = getdata(hotel_type, weeks)
    top_10_countries = (
        df.groupby("Country of origin")
        .size()
        .reset_index(name="counts")
        .sort_values(by="counts", ascending=False)[:10]
    )
    return top_10_countries


# Dataframe for Stay Length plot:
def right_plot(hotel_type="All", weeks=[1, 53]):
    df = getdata(hotel_type, weeks)
    df["Total Nights of Stay"] = df["Weekend nights"] + df["Week nights"]
    num_nights = list(df["Total Nights of Stay"].value_counts().index)
    num_bookings = list(df["Total Nights of Stay"].value_counts())
    rel_bookings = (
        df["Total Nights of Stay"].value_counts() / sum(num_bookings) * 100
    )  # convert to percent
    stay_nights = pd.DataFrame(
        {
            "hotel": "Both Hotels",
            "Number of Nights of Stay": num_nights,
            "Percent of Reservations": rel_bookings,
        }
    )
    return stay_nights
