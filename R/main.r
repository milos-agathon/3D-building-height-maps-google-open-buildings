# 1. PACKAGES
#------------

install.packages("pacman")

pacman::p_load(
    rgee,
    terra,
    sf,
    exactextractr,
    tidyverse,
    rayshader,
    osmdata
)

# 2. AUTHENTICATE AND START
#--------------------------

rgee::ee_Authenticate()
rgee::ee_Initialize(
    user = "INSERT YOUR GMAIL ADDRESS",
    drive = TRUE
)

# 3. BUFFER
#----------

lat <- 19.0623444
lon <- 72.8650271

cents <- sf::st_as_sf(
    x = data.frame(
        lat = lat,
        lon = lon
    ),
    coords = c(
        "lon", "lat"
    ),
    crs = "EPSG:4326"
)

circle <- sf::st_buffer(
    cents,
    dist = units::set_units(
        1, km
    )
) |>
    sf::st_set_crs("EPSG:4326")

# 4. DEFINE BOUNDS
#-----------------

city_bbox <- sf::st_bbox(
    circle
)

xmin <- city_bbox[["xmin"]]
xmax <- city_bbox[["xmax"]]
ymin <- city_bbox[["ymin"]]
ymax <- city_bbox[["ymax"]]

city_bounds <- ee$Geometry$Rectangle(
    c(
        west = xmin,
        south = ymin,
        east = xmax,
        north = ymax
    ),
    geodetic = TRUE,
    proj = "EPSG:4326"
)

# 5. CONNECT TO OPEN BUILDING DATA
#---------------------------------

open_buildings_temporal <-
    ee$ImageCollection(
        "GOOGLE/Research/open-buildings-temporal/v1"
    )$
        filterBounds(
        city_bounds
    )

rgee::ee_print(open_buildings_temporal)

# 6. CHOOSE BUILDING HEIGHT & PRESENCE
#-------------------------------------

variable_names <- c(
    "building_height",
    "building_presence"
)

building_bands <- open_buildings_temporal$select(
    variable_names
)$toBands()

rgee::ee_print(building_bands)

string_id <- "3b_EPSG_32643_2023_06_30_"

band_names <- paste0(
    string_id,
    variable_names
)

building_data <- building_bands$select(
    band_names
)$clip(
    city_bounds
)

rgee::ee_print(building_data)

# 7. LOAD DATA
#-------------

buildings_raster <- rgee::ee_as_rast(
    image = building_data,
    dsn = "mumbai_buildings.tif",
    maxPixels = 17 * 1e6,
    region = city_bounds,
    via = "drive"
)

terra::writeRaster(
    x = buildings_raster,
    filename = "mumbai_buildings.tif",
    overwrite = TRUE
)

buildings_raster <- terra::rast("mumbai_buildings.tif")

# 8. RASTER LAYERS
#-----------------

names(buildings_raster)

new_band_names <- sub(
    string_id,
    "", names(buildings_raster)
)

names(buildings_raster) <- new_band_names

for(
    lyr in names(buildings_raster)){
        assign(
            lyr, buildings_raster[[lyr]]
        )
    }

building_presence
building_height

# 8. FILTER HEIGHT
#-----------------

confidence_level <- .7

mask_raster <- terra::ifel(
    terra::init(
        building_presence,
        runif
    ) >= confidence_level,
    NA, building_presence
)

building_height_masked <- terra::mask(
    building_height, mask_raster
)

# 9. GET BUILDING POLYGONS
#-------------------------

open_buildings_poly <-
    ee$FeatureCollection(
        "GOOGLE/Research/open-buildings/v3/polygons"
    )$filterBounds(
        city_bounds
    )

rgee::ee_print(open_buildings_poly)

open_buildings_sf <- rgee::ee_as_sf(
    open_buildings_poly,
    maxFeatures = 5000
)

open_buildings_sf <- subset(
    open_buildings_sf,
    confidence >= confidence_level
)

# 10. LARGEST HEIGHT PER POLYGON
#-------------------------------

height <- exactextractr::exact_extract(
    x = building_height_masked,
    y = open_buildings_sf,
    fun = "max"
)

building_height_sf <- cbind(
    open_buildings_sf,
    height
) |>
    sf::st_as_sf()

# 12. OSM ROADS
#--------------

roads <- city_bbox |>
    osmdata::opq() |>
    osmdata::add_osm_feature(
        key = "highway"
    ) |>
    osmdata::osmdata_sf()

city_roads <- roads$osm_lines |>
    sf::st_intersection(circle) |>
    sf::st_cast("MULTILINESTRING")

# 13. MAP
#--------

theme_for_the_win <- function() {
    theme_minimal() +
        theme(
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = c(.25, .1),
            legend.title = element_text(
                size = 8, color = "grey80",
                vjust = -20
            ),
            legend.text = element_text(
                size = 7, color = "grey80"
            ),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(
                fill = cols[1]
            ),
            plot.margin = unit(
                c(
                    t = -1, r = -1,
                    b = -1, l = -1
                ), "cm"
            )
        )
}

cols <- hcl.colors(
    6, "Inferno"
)

map1 <- ggplot() +
    geom_sf(
        data = building_height_sf,
        aes(fill = height),
        color = NA,
        linewidth = 0
    ) +
    geom_sf(
        data = city_roads,
        linewidth = .2,
        color = "white"
    ) +
    scale_fill_gradientn(
        name = "",
        colors = cols,
        na.value = cols[1]
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barheight = unit(
                .2,
                units = "cm"
            ),
            barwidth = unit(
                5,
                units = "cm"
            ),
            label.position = "bottom",
            title.position = "top",
            label.hjust = .5,
            drop = FALSE
        )
    ) +
    theme_for_the_win()

# 14. 3D MAP
#-----------

map2 <- ggplot() +
    geom_sf(
        data = building_height_sf,
        aes(fill = height),
        color = NA,
        linewidth = 0
    ) +
    # geom_sf(
    #     data = city_roads,
    #     linewidth = .2,
    #     color = "white"
    # ) +
    scale_fill_gradientn(
        name = "",
        colors = cols,
        na.value = cols[1]
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barheight = unit(
                .2,
                units = "cm"
            ),
            barwidth = unit(
                5,
                units = "cm"
            ),
            label.position = "bottom",
            title.position = "top",
            label.hjust = .5,
            drop = FALSE
        )
    ) +
    theme_for_the_win() +
    theme(legend.position = "none")

rayshader::plot_gg(
    ggobj = map2,
    width = 7,
    height = 7,
    windowsize = c(
        600, 600
    ),
    background = cols[1],
    scale = 150,
    solid = FALSE,
    shadow = TRUE,
    shadow_intensity = 1,
    phi = 87,
    theta = 0,
    zoom = .55
)

rayshader::render_camera(
    phi = 45,
    theta = 0,
    zoom = .5
)

rayrender::render_path(
    lat = city_roads,
    altitude = 1,
    extent = building_height_sf,
    zscale = 1,
    linewidth = 2,
    color = "white"
)

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/brown_photostudio_02_4k.hdr"
hdri_file <- basename(u)

download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)

file_name <- "3d_mumbai_buildings.png"

rayshader::render_highquality(
    filename = file_name,
    preview = TRUE,
    light = FALSE,
    environment_light = hdri_file,
    intensity_env = 1.25,
    ground_material = rayrender::diffuse(
        color = cols[1]
    ),
    line_radius = 2,
    interactive = FALSE,
    parallel = TRUE,
    width = 4000,
    height = 4000
)
