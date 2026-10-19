# Builds the tables in data/processed/ from the files in data/raw/.
# Run it once whenever the raw files change:  Rscript R/build_tables.R
# Every definition the posts rely on is set here, once.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(jsonlite)
})

root <- "~/UCI/projects/rearview-notes/data"
raw  <- function(f) file.path(root, "raw", f)
out  <- function(f) file.path(root, "processed", f)

# ---- Rules -----------------------------------------------------------------

TZ             <- "America/Los_Angeles"  # the export is UTC; everything is read in local time
SCHEDULED_MIN  <- 60   # over this from order to pickup, the wait was booked: left out of order time
SHIFT_GAP_MIN  <- 60   # over this from one drop-off to the next pickup starts a new shift
MAX_TRIP_HOURS <- 6    # over this from pickup to drop-off, the row is set aside as broken
STRIDE_MAX_MI  <- 1000 # a day past this is a broken Stride row

WEEKDAYS <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

# The export's timestamps, read as UTC. Fractions of a second are dropped, as the site does.
parse_utc <- function(x) as.POSIXct(substr(x, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
minutes   <- function(from, to) as.numeric(difftime(to, from, units = "mins"))
local_date <- function(t) as.Date(format(t, "%Y-%m-%d", tz = TZ))
local_hour <- function(t) as.integer(format(t, "%H", tz = TZ))
weekday_of <- function(d) WEEKDAYS[as.integer(format(d, "%u"))]
r2 <- function(x) round(x, 2)

# ---- Orders ----------------------------------------------------------------

export <- read_csv(raw("dasher_delivery_information.csv"), col_types = cols(.default = "c"))

orders <- export |>
  transmute(
    store    = trimws(STORE_NAME),
    items    = as.integer(TOTAL_ITEM_COUNT),
    subtotal = as.numeric(SUBTOTAL_IN_CENTS) / 100,
    created  = parse_utc(ORDER_CREATED_TIME),
    pickup   = parse_utc(ACTUAL_PICKUP_TIME),
    delivery = parse_utc(ACTUAL_DELIVERY_TIME),
    key      = paste(ORDER_CREATED_TIME, ACTUAL_PICKUP_TIME, ACTUAL_DELIVERY_TIME, STORE_NAME)
  )

n_raw <- nrow(orders)
orders <- orders |>
  filter(!is.na(created), !is.na(pickup), !is.na(delivery),
         delivery >= pickup, pickup >= created,
         minutes(pickup, delivery) <= MAX_TRIP_HOURS * 60) |>
  distinct(key, .keep_all = TRUE) |>   # an exact repeat would overlap itself and read as a stack
  select(-key)
n_set_aside <- n_raw - nrow(orders)

# ---- Trips: orders in the car at the same time ----------------------------
# Sorted by pickup, an order starts a new trip only if every order before it was already
# dropped off. Otherwise it overlaps one of them and joins that trip.

orders <- orders |>
  arrange(pickup) |>
  mutate(
    prev_drop = lag(cummax(as.numeric(delivery)), default = -Inf),
    trip_id   = cumsum(as.numeric(pickup) >= prev_drop)
  ) |>
  select(-prev_drop)

trips <- orders |>
  group_by(trip_id) |>
  summarise(
    start      = min(pickup),
    end        = max(delivery),
    size       = n(),
    same_store = if (n() > 1) as.integer(n_distinct(store) == 1) else NA_integer_,
    .groups = "drop"
  ) |>
  arrange(start)

# ---- Shifts: trips joined until a gap of more than an hour ----------------

trips <- trips |>
  mutate(
    prev_end = lag(cummax(as.numeric(end)), default = -Inf),
    gap      = (as.numeric(start) - prev_end) / 60,
    shift_id = cumsum(gap > SHIFT_GAP_MIN)
  ) |>
  select(-prev_end, -gap)

shifts <- trips |>
  group_by(shift_id) |>
  summarise(start = min(start), end = max(end), trips = n(), orders = sum(size), .groups = "drop") |>
  mutate(
    # A shift belongs to the day it started, even if it runs past midnight.
    # A break long enough to start a new shift after midnight makes that a new day.
    day        = local_date(start),
    weekday    = weekday_of(day),
    start_hour = local_hour(start),
    waits      = trips - 1,
    minutes    = r2(minutes(start, end))
  )

day_of <- select(shifts, shift_id, day, weekday)

# ---- Waits: no order in the car, from one trip's last drop-off to the next trip's first pickup

waits <- trips |>
  group_by(shift_id) |>
  arrange(start, .by_group = TRUE) |>
  mutate(next_start = lead(start), next_trip = lead(trip_id)) |>
  ungroup() |>
  filter(!is.na(next_start)) |>
  transmute(
    shift_id,
    hour           = local_hour(end),   # when the wait began
    minutes        = r2(minutes(end, next_start)),
    after_trip_id  = trip_id,
    before_trip_id = next_trip
  ) |>
  left_join(day_of, by = "shift_id") |>
  mutate(wait_id = row_number()) |>
  select(wait_id, shift_id, day, weekday, hour, minutes, after_trip_id, before_trip_id)

# ---- Stores: kind and town from the map's lookups -------------------------

KIND <- c(
  fast_food_restaurant = "Fast food", hamburger_restaurant = "Fast food", chicken_restaurant = "Fast food",
  pizza_restaurant = "Fast food", sandwich_shop = "Fast food", bagel_shop = "Fast food", deli = "Fast food",
  meal_takeaway = "Fast food", meal_delivery = "Fast food", food_court = "Fast food",
  cafe = "Coffee & drinks", coffee_shop = "Coffee & drinks", tea_house = "Coffee & drinks",
  bubble_tea_store = "Coffee & drinks", juice_shop = "Coffee & drinks", cat_cafe = "Coffee & drinks",
  dog_cafe = "Coffee & drinks",
  bakery = "Bakery & sweets", ice_cream_shop = "Bakery & sweets", dessert_shop = "Bakery & sweets",
  dessert_restaurant = "Bakery & sweets", donut_shop = "Bakery & sweets", candy_store = "Bakery & sweets",
  chocolate_shop = "Bakery & sweets", confectionery = "Bakery & sweets", acai_shop = "Bakery & sweets",
  grocery_store = "Grocery", supermarket = "Grocery", wholesaler = "Grocery", warehouse_store = "Grocery",
  butcher_shop = "Grocery", food_store = "Grocery",
  convenience_store = "Convenience", gas_station = "Convenience", liquor_store = "Convenience",
  cigar_shop = "Convenience",
  pharmacy = "Pharmacy", drugstore = "Pharmacy",
  bar = "Bar", pub = "Bar", wine_bar = "Bar", bar_and_grill = "Bar",
  restaurant = "Restaurant", fine_dining_restaurant = "Restaurant", buffet_restaurant = "Restaurant",
  steak_house = "Restaurant", diner = "Restaurant", cafeteria = "Restaurant"
)
# Google's place type, folded into a few kinds. The suffixes carry the long tail.
kind_of <- function(type) {
  t <- tolower(type)
  ifelse(is.na(t), NA_character_,
  ifelse(t %in% names(KIND), unname(KIND[t]),
  ifelse(grepl("_restaurant$", t), "Restaurant",
  ifelse(grepl("_store$|_shop$", t) | t == "store", "Retail", "Other"))))
}
# The town in an address: the last part with no digits that is not a country.
COUNTRY <- "^(usa|united states|canada|australia|new zealand|uk|united kingdom|[A-Z]{2,3})$"
town_of <- function(address) {
  vapply(address, function(a) {
    if (is.na(a)) return(NA_character_)
    parts <- trimws(strsplit(a, ",")[[1]])
    parts <- parts[nzchar(parts) & !grepl("[0-9]", parts) & !grepl(COUNTRY, parts, ignore.case = TRUE)]
    if (length(parts)) tail(parts, 1) else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

places <- fromJSON(raw("places.json"))
stores <- tibble(
  store   = names(places),
  type    = vapply(places, function(p) if (is.null(p$type)) NA_character_ else p$type, character(1)),
  address = vapply(places, function(p) if (is.null(p$address)) NA_character_ else p$address, character(1)),
  lat     = vapply(places, function(p) if (is.null(p$lat)) NA_real_ else p$lat, numeric(1)),
  lng     = vapply(places, function(p) if (is.null(p$lng)) NA_real_ else p$lng, numeric(1))
) |>
  transmute(store, kind = kind_of(type), town = town_of(address), store_lat = lat, store_lng = lng)

# ---- Order table -----------------------------------------------------------

orders_out <- orders |>
  left_join(select(trips, trip_id, shift_id, size), by = "trip_id") |>
  left_join(day_of, by = "shift_id") |>
  group_by(trip_id) |>
  mutate(pickup_order = rank(pickup, ties.method = "first"),
         drop_order   = rank(delivery, ties.method = "first")) |>
  ungroup() |>
  left_join(stores, by = "store") |>
  mutate(
    order_id       = row_number(),
    hour           = local_hour(pickup),
    scheduled      = as.integer(minutes(created, pickup) > SCHEDULED_MIN),
    retail         = as.integer(subtotal == 0),
    order_time     = r2(minutes(created, delivery)),
    before_pickup  = r2(minutes(created, pickup)),
    pickup_to_door = r2(minutes(pickup, delivery))
  ) |>
  transmute(order_id, trip_id, shift_id, day, weekday, hour, store, kind, town, store_lat, store_lng,
            trip_size = size, pickup_order, drop_order, scheduled, retail, items,
            order_time, before_pickup, pickup_to_door)

trips_out <- trips |>
  left_join(day_of, by = "shift_id") |>
  transmute(trip_id, shift_id, day, weekday, hour = local_hour(start), size, same_store,
            minutes = r2(minutes(start, end)), minutes_per_order = r2(minutes(start, end) / size))

shifts_out <- shifts |>
  select(shift_id, day, weekday, start_hour, trips, orders, waits, minutes)

# ---- Days: a driver's day, with Stride's miles for that date ---------------

stride <- read_csv(raw("stride_mileage.csv"), col_types = cols(.default = "c")) |>
  transmute(day   = as.Date(Date, format = "%m/%d/%Y"),
            miles = suppressWarnings(as.numeric(gsub("[^0-9.]", "", Distance)))) |>
  filter(!is.na(day), miles > 0) |>
  group_by(day) |> summarise(miles = round(sum(miles), 2), .groups = "drop") |>
  filter(miles <= STRIDE_MAX_MI)

days_out <- trips_out |>
  group_by(day, weekday) |>
  summarise(orders = sum(size), trips = n(), stacked_trips = sum(size > 1),
            active_minutes = r2(sum(minutes)), .groups = "drop") |>
  left_join(shifts_out |> group_by(day) |> summarise(dash_minutes_rebuilt = round(sum(minutes))), by = "day") |>
  left_join(stride, by = "day")

# ---- Write -----------------------------------------------------------------

dir.create(file.path(root, "processed"), showWarnings = FALSE)
write_csv(orders_out, out("orders.csv"))
write_csv(trips_out,  out("trips.csv"))
write_csv(waits,      out("waits.csv"))
write_csv(shifts_out, out("shifts.csv"))
write_csv(days_out,   out("days.csv"))

cat(sprintf("%d orders (%d set aside), %d trips, %d shifts, %d waits, %d days (%d with miles)\n",
            nrow(orders_out), n_set_aside, nrow(trips_out), nrow(shifts_out), nrow(waits),
            nrow(days_out), sum(!is.na(days_out$miles))))
cat(sprintf("kind for %d orders, town for %d\n", sum(!is.na(orders_out$kind)), sum(!is.na(orders_out$town))))
