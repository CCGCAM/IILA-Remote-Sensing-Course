# Read a Sentinel-2 L2A point time series from Element 84 Earth Search STAC.
# The course ships a verified CSV so students can work without a live connection.
stac_band_assets <- c(B02="blue", B03="green", B04="red",
                      B05="rededge1", B06="rededge2", B07="rededge3",
                      B08="nir", B8A="nir08", B11="swir16", B12="swir22")

search_s2_items <- function(lon, lat, start, end, max_cloud=25) {
  stopifnot(requireNamespace("jsonlite", quietly=TRUE))
  bbox <- paste(c(lon-0.01,lat-0.01,lon+0.01,lat+0.01),collapse=",")
  dates <- utils::URLencode(paste0(start,"T00:00:00Z/",end,"T23:59:59Z"),
                            reserved=TRUE)
  url <- paste0("https://earth-search.aws.element84.com/v1/search?",
                "collections=sentinel-2-c1-l2a&bbox=",bbox,
                "&datetime=",dates,"&limit=100")
  items <- jsonlite::fromJSON(url,simplifyVector=FALSE)$features
  items[vapply(items,function(x) {
    cloud <- x$properties[["eo:cloud_cover"]]
    !is.null(cloud) && is.finite(cloud) && cloud <= max_cloud
  },logical(1))]
}

one_per_fortnight <- function(items,start,max_items=14) {
  date <- as.Date(vapply(items,function(x) substr(x$properties$datetime,1,10),""))
  cloud <- vapply(items,function(x) x$properties[["eo:cloud_cover"]],numeric(1))
  fortnight <- floor(as.numeric(date-as.Date(start))/14)
  groups <- split(seq_along(items),fortnight)
  chosen <- vapply(groups,function(index) index[which.min(cloud[index])],integer(1))
  chosen <- chosen[order(date[chosen])]
  items[head(chosen,max_items)]
}

read_stac_pixel <- function(href,point) {
  raster <- terra::rast(paste0("/vsicurl/",href))
  here <- terra::project(point,terra::crs(raster))
  as.numeric(terra::extract(raster,here)[1,2])
}

extract_s2_item <- function(item,point) {
  scl <- read_stac_pixel(item$assets$scl$href,point)
  if (!is.finite(scl) || !scl %in% c(4,5)) return(NULL)
  values <- vapply(stac_band_assets,function(key)
    read_stac_pixel(item$assets[[key]]$href,point),numeric(1))
  if (!all(is.finite(values)) || any(values < 0 | values > 1)) return(NULL)
  data.frame(date=as.Date(substr(item$properties$datetime,1,10)),
             item_id=item$id,
             cloud_cover=item$properties[["eo:cloud_cover"]],
             SCL=scl,t(values),check.names=FALSE)
}

fetch_stac_series <- function(lon=-62.12,lat=-32.71,
                              start="2024-10-01",end="2025-03-31",
                              max_cloud=25,max_items=14) {
  stopifnot(requireNamespace("terra",quietly=TRUE))
  point <- terra::vect(data.frame(lon=lon,lat=lat),
                       geom=c("lon","lat"),crs="EPSG:4326")
  items <- one_per_fortnight(search_s2_items(lon,lat,start,end,max_cloud),
                             start,max_items)
  rows <- lapply(items,function(item) {
    tryCatch(extract_s2_item(item,point),error=function(e) {
      message("Skipping ",item$id,": ",conditionMessage(e))
      NULL
    })
  })
  rows <- Filter(Negate(is.null),rows)
  if (length(rows)<3) stop("Fewer than three clear observations were read from STAC.")
  series <- do.call(rbind,rows)
  series <- series[order(series$date),]
  series$NDVI <- with(series,(B08-B04)/(B08+B04))
  series$NDRE <- with(series,(B08-B05)/(B08+B05))
  series$NDMI <- with(series,(B08-B11)/(B08+B11))
  rownames(series) <- NULL
  series
}
