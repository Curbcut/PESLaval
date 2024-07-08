z <- sf::read_sf("data/axe3/locations/parc_espace_vert_20240607_PG.shp")
z$area <-  cc.buildr::get_area(z)
z <- z[z$area >= 2000, ]

tt <- ttm()

DBs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DB", geo_format = "sf")
z <- sf::st_transform(z, 32618)
DBs <- sf::st_transform(DBs, 32618)

access <- function(x) {
  park <- z[x, ]
  inters <- sf::st_intersection(DBs, park)$GeoUID
  tt$from[tt$to %in% inters]
}

which_have_access <- lapply(seq_along(z$geometry), access)

which_have_access <- table(unlist(which_have_access))
parks_access <- tibble::tibble(GeoUID = names(which_have_access),
                               parks = as.vector(which_have_access))

parks_access <- cc.buildr::merge(parks_access, DBs[c("GeoUID", "geometry")])
parks_access["parks"] |> plot()
