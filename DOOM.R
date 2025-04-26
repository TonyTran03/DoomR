
##### Doom E1M1 ####
#  By Tony Tran  

library(ggplot2)


######## utility functions #########

read_vertexes <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  seek(con, where = lump_info$offset, origin = "start")
  
  # each vertex is two 2-byte signed integers → 4 bytes
  n_bytes_per_vertex <- 4
  n_vertices <- lump_info$size %/% n_bytes_per_vertex
  raw_ints <- readBin(con,
                      what = integer(),
                      n = n_vertices * 2,
                      endian = "little",
                      signed = TRUE,
                      size = 2
  )
  
  close(con)
  # reshape into 2-col matrix: [x, y]
  verts <- matrix(raw_ints, ncol = 2, byrow = TRUE)
  colnames(verts) <- c("x", "y")
  as.data.frame(verts)
}

read_segs <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  seek(con, where = lump_info$offset, origin = "start")
  # each SEG record has 6 fields of 2 bytes each → 12 bytes
  n_bytes_per_seg <- 12 
  n_segs <- lump_info$size %/% n_bytes_per_seg
  raw_ints <- readBin(con,
                      what = integer(),
                      n = n_segs * 6,
                      size = 2,
                      endian = "little",
                      signed = FALSE
  )
  close(con)
  segs_mat <- matrix(raw_ints, ncol = 6, byrow = TRUE)
  colnames(segs_mat) <- c("v1", "v2", "angle", "linedef", "direction", "offset")
  as.data.frame(segs_mat)
}

# Code is borrowed from https://rosettacode.org/wiki/Ray-casting_algorithm#
#point <- function(x,y) 
  #list(x=x, y=y)

# pts = list(p1, p2, ... )... coords
# segs = list(c(1,2), c(2,1) ...) indices
#createPolygon <- function(pts, segs) {
  #pol <- list()
  #for(pseg in segs) {
    #pol <- c(pol, list(list(A=pts[[pseg[1]]], B=pts[[pseg[2]]])))
  #}
  #pol
#}




###### Parsing File ######
wad_path   <- "C:/Users/tonyt/OneDrive/Desktop/DoomR/DOOM.WAD"
con        <- file(wad_path, "rb")
magic      <- readChar(con, nchars = 4, useBytes = TRUE)   # "IWAD" or "PWAD"
num_lumps  <- readBin(con, integer(), size = 4, endian = "little")
dir_offset <- readBin(con, integer(), size = 4, endian = "little")

# seek to lump directory (the table of content)
seek(con, where = dir_offset, origin = "start")

# build lump_table
lump_table <- data.frame(
  offset = integer(num_lumps),
  size   = integer(num_lumps),
  name   = character(num_lumps),
  stringsAsFactors = FALSE
)
for (i in seq_len(num_lumps)) {
  lump_table$offset[i] <- readBin(con, integer(), size = 4, endian = "little")
  lump_table$size[i]   <- readBin(con, integer(), size = 4, endian = "little")
  lump_table$name[i]   <- gsub("\\x00", "",
    readChar(con, nchars = 8, useBytes = TRUE)
  )
}
close(con)

# Find the eight map lumps for E1M1
map_idx  <- which(lump_table$name == "E1M1")
map_data <- lump_table[(map_idx + 1):(map_idx + 8), ]

# sanity check lump ordering
#expected <- c("THINGS","LINEDEFS","SIDEDEFS",
#              "VERTEXES","SEGS","SSECTORS","NODES","SECTORS")
#stopifnot(all(map_data$name == expected))
#print("hi")


vertex_lump <- map_data[4, ]  # VERTEXES
segs_lump   <- map_data[5, ]  # SEGS


##### Reading geometry data #####
vertices <- read_vertexes(wad_path, vertex_lump)
segs     <- read_segs(  wad_path, segs_lump)

cat("Loaded", nrow(vertices), "vertices and", nrow(segs), "segments.\n")


#Recreating the map to verify the data. We can now use the data to raytrace the map.
walls <- data.frame(
  x1 = vertices$x[segs$v1 + 1],
  y1 = vertices$y[segs$v1 + 1],
  x2 = vertices$x[segs$v2 + 1],
  y2 = vertices$y[segs$v2 + 1]
)

cat("Constructed", nrow(walls), "wall segments.\n")
ggplot(walls) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2)) + 
  coord_equal() +
  theme_void()



#print(walls)
# _____
# I will start on thing #114 (1024, -3264)
player <- list(
  x = 1024,      # X coordinate in map space
  y = -3264,      # Y coordinate in map space
  angle = atan2(-96, -96)  # gives you -135 degrees
)

fov <- pi / 2   # 90 field of view
n_rays <- 320 

# Creating a sequence (e.g. 1, 2, 3,...,) from the top and bottom of my 
#   cone pov separated by number of rows 
angles <- seq(
  player$angle - fov/2,
  player$angle + fov/2,
  length.out = n_rays
)
# We are making the direction for every ray
rays <- data.frame(
  dx = cos(angles),
  dy = sin(angles)
)




ggplot(rays) +
  geom_segment(
    aes(x = player$x, y = player$y, 
        xend = player$x + 50*dx,  # scale a bit to show rays
        yend = player$y + 50*dy),
    color = "blue"
  ) +
  geom_point(aes(x = player$x, y = player$y), color = "red", size = 3) +  # player position
  coord_fixed() +
  theme_minimal()



#### Ray Casting Point in polygon detection #### 
# point_in_polygon <- function(polygon, p) {
#   count <- 0
#   for(side in polygon) {
#     if ( ray_intersect_segment(p, side) ) {
#       count <- count + 1
#     }
#   }
#   if ( count %% 2 == 1 )
#     "INSIDE"
#   else
#     "OUTSIDE"
# }

# ray_intersect_segment <- function(p, side) {
#   eps <- 0.0001
#   a <- side$A
#   b <- side$B
#   if ( a$y > b$y ) {
#     a <- side$B
#     b <- side$A
#   }
#   if ( (p$y == a$y) || (p$y == b$y) ) {
#     p$y <- p$y + eps
#   }
#   if ( (p$y < a$y) || (p$y > b$y) )
#     return(FALSE)
#   else if ( p$x > max(a$x, b$x) )
#     return(FALSE)
#   else {
#     if ( p$x < min(a$x, b$x) )
#       return(TRUE)
#     else {
#       if ( a$x != b$x )
#         m_red <- (b$y - a$y) / (b$x - a$x)
#       else
#         m_red <- Inf
#       if ( a$x != p$x )
#         m_blue <- (p$y - a$y) / (p$x - a$x)
#       else
#         m_blue <- Inf
#       return( m_blue >= m_red )
#     }
#   }
# }







intersect_ray_segment <- function(px, py, dx, dy, x1, y1, x2, y2) {
  sx <- x2 - x1
  sy <- y2 - y1
  
  D <- dx * sy - dy * sx
  
  if (D == 0) {
    # Parallel lines
    return(NULL)
  }
  Dt = (x1-px)*sy - (y1-py)(sx)
  Du = dx*(y1-py) - (dy)(x1-px)
  # t is the scaling for length of ray hits the wall (t>= 0)
  t = Dt/D
  # u is the scaling for where the ray hits u:(0,1)
    #we have to know if ray hit is inside the wall segment.
  u = Du/D
  

  if(t >=0 && u >= 0 && u<= 1){
    #(x,y) = (px,py) + t (dx,dy)
    intersection_x = px + t*dx
    intersection_y = py + t*dy
    
    distance = sqrt((intersection_x-px)^2 + (intersection_y-py)^2 )
    
    
    return (
      list(x = intersection_x, y = intersection_y, dist = distance)
    )
  }
  
  else{
    return(NULL)
  }
}

