##### Doom E1M1 ####
# By Tony Tran

library(ggplot2)


######## WAD readers ########

read_wad_name <- function(con) {
  bytes <- readBin(con, what = raw(), n = 8)
  rawToChar(bytes[bytes != as.raw(0)], multiple = FALSE)
}

read_lump_directory <- function(wad_path) {
  con <- file(wad_path, "rb")
  on.exit(close(con))

  magic <- readChar(con, nchars = 4, useBytes = TRUE)
  num_lumps <- readBin(con, integer(), size = 4, endian = "little")
  dir_offset <- readBin(con, integer(), size = 4, endian = "little")

  seek(con, where = dir_offset, origin = "start")

  lump_table <- data.frame(
    offset = integer(num_lumps),
    size = integer(num_lumps),
    name = character(num_lumps),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(num_lumps)) {
    lump_table$offset[i] <- readBin(con, integer(), size = 4, endian = "little")
    lump_table$size[i] <- readBin(con, integer(), size = 4, endian = "little")
    lump_table$name[i] <- read_wad_name(con)
  }

  list(magic = magic, lumps = lump_table)
}

read_vertexes <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_vertices <- lump_info$size %/% 4
  raw_ints <- readBin(
    con,
    what = integer(),
    n = n_vertices * 2,
    endian = "little",
    signed = TRUE,
    size = 2
  )

  verts <- matrix(raw_ints, ncol = 2, byrow = TRUE)
  colnames(verts) <- c("x", "y")
  as.data.frame(verts)
}

read_linedefs <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_linedefs <- lump_info$size %/% 14
  raw_ints <- readBin(
    con,
    what = integer(),
    n = n_linedefs * 7,
    size = 2,
    endian = "little",
    signed = FALSE
  )

  linedefs <- matrix(raw_ints, ncol = 7, byrow = TRUE)
  colnames(linedefs) <- c(
    "v1", "v2", "flags", "special_type", "sector_tag",
    "right_sidedef", "left_sidedef"
  )
  as.data.frame(linedefs)
}

read_sidedefs <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_sidedefs <- lump_info$size %/% 30
  sidedefs <- data.frame(
    x_offset = integer(n_sidedefs),
    y_offset = integer(n_sidedefs),
    upper_texture = character(n_sidedefs),
    lower_texture = character(n_sidedefs),
    middle_texture = character(n_sidedefs),
    sector = integer(n_sidedefs),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n_sidedefs)) {
    sidedefs$x_offset[i] <- readBin(con, integer(), size = 2, signed = TRUE, endian = "little")
    sidedefs$y_offset[i] <- readBin(con, integer(), size = 2, signed = TRUE, endian = "little")
    sidedefs$upper_texture[i] <- read_wad_name(con)
    sidedefs$lower_texture[i] <- read_wad_name(con)
    sidedefs$middle_texture[i] <- read_wad_name(con)
    sidedefs$sector[i] <- readBin(con, integer(), size = 2, signed = FALSE, endian = "little")
  }

  sidedefs
}

read_segs <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_segs <- lump_info$size %/% 12
  raw_ints <- readBin(
    con,
    what = integer(),
    n = n_segs * 6,
    size = 2,
    endian = "little",
    signed = FALSE
  )

  segs <- matrix(raw_ints, ncol = 6, byrow = TRUE)
  colnames(segs) <- c("v1", "v2", "angle", "linedef", "direction", "offset")
  as.data.frame(segs)
}

read_ssectors <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_ssectors <- lump_info$size %/% 4
  raw_ints <- readBin(
    con,
    integer(),
    n = n_ssectors * 2,
    size = 2,
    signed = FALSE,
    endian = "little"
  )

  ssectors <- matrix(raw_ints, ncol = 2, byrow = TRUE)
  colnames(ssectors) <- c("num_segs", "first_seg_index")
  as.data.frame(ssectors)
}

read_nodes <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_nodes <- lump_info$size %/% 28
  nodes <- data.frame(
    x = integer(n_nodes),
    y = integer(n_nodes),
    dx = integer(n_nodes),
    dy = integer(n_nodes),
    bbox0_top = integer(n_nodes),
    bbox0_bottom = integer(n_nodes),
    bbox0_left = integer(n_nodes),
    bbox0_right = integer(n_nodes),
    bbox1_top = integer(n_nodes),
    bbox1_bottom = integer(n_nodes),
    bbox1_left = integer(n_nodes),
    bbox1_right = integer(n_nodes),
    right_child = integer(n_nodes),
    left_child = integer(n_nodes)
  )

  for (i in seq_len(n_nodes)) {
    nodes[i, 1:12] <- readBin(
      con,
      integer(),
      n = 12,
      size = 2,
      signed = TRUE,
      endian = "little"
    )
    nodes$right_child[i] <- readBin(con, integer(), size = 2, signed = FALSE, endian = "little")
    nodes$left_child[i] <- readBin(con, integer(), size = 2, signed = FALSE, endian = "little")
  }

  nodes
}

read_sectors <- function(wad_path, lump_info) {
  con <- file(wad_path, "rb")
  on.exit(close(con))
  seek(con, where = lump_info$offset, origin = "start")

  n_sectors <- lump_info$size %/% 26
  sectors <- data.frame(
    floor_height = integer(n_sectors),
    ceiling_height = integer(n_sectors),
    floor_texture = character(n_sectors),
    ceiling_texture = character(n_sectors),
    light_level = integer(n_sectors),
    special_type = integer(n_sectors),
    tag = integer(n_sectors),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n_sectors)) {
    sectors$floor_height[i] <- readBin(con, integer(), size = 2, signed = TRUE, endian = "little")
    sectors$ceiling_height[i] <- readBin(con, integer(), size = 2, signed = TRUE, endian = "little")
    sectors$floor_texture[i] <- read_wad_name(con)
    sectors$ceiling_texture[i] <- read_wad_name(con)
    sectors$light_level[i] <- readBin(con, integer(), size = 2, signed = TRUE, endian = "little")
    sectors$special_type[i] <- readBin(con, integer(), size = 2, signed = FALSE, endian = "little")
    sectors$tag[i] <- readBin(con, integer(), size = 2, signed = FALSE, endian = "little")
  }

  sectors
}


######## Map assembly ########

wad_path <- file.path(getwd(), "DOOM.WAD")
if (!file.exists(wad_path)) {
  stop("Could not find DOOM.WAD in the current working directory: ", getwd())
}

wad <- read_lump_directory(wad_path)
lump_table <- wad$lumps

map_name <- "E1M1"
map_idx <- which(lump_table$name == map_name)
if (length(map_idx) != 1) {
  stop("Could not find map lump ", map_name)
}

map_data <- lump_table[(map_idx + 1):(map_idx + 10), ]
names_by_lump <- setNames(seq_len(nrow(map_data)), map_data$name)

linedefs <- read_linedefs(wad_path, map_data[names_by_lump["LINEDEFS"], ])
sidedefs <- read_sidedefs(wad_path, map_data[names_by_lump["SIDEDEFS"], ])
vertices <- read_vertexes(wad_path, map_data[names_by_lump["VERTEXES"], ])
segs <- read_segs(wad_path, map_data[names_by_lump["SEGS"], ])
ssectors <- read_ssectors(wad_path, map_data[names_by_lump["SSECTORS"], ])
nodes <- read_nodes(wad_path, map_data[names_by_lump["NODES"], ])
sectors <- read_sectors(wad_path, map_data[names_by_lump["SECTORS"], ])

cat(
  "Loaded",
  nrow(vertices), "vertices,",
  nrow(linedefs), "linedefs,",
  nrow(segs), "segs,",
  nrow(ssectors), "subsectors,",
  nrow(nodes), "nodes, and",
  nrow(sectors), "sectors.\n"
)

no_sidedef <- 65535
get_sector_for_sidedef <- function(side_index) {
  if (is.na(side_index) || side_index == no_sidedef) {
    return(NA_integer_)
  }
  sidedefs$sector[side_index + 1]
}

front_sidedef <- integer(nrow(segs))
back_sidedef <- integer(nrow(segs))
front_sector <- integer(nrow(segs))
back_sector <- integer(nrow(segs))

for (i in seq_len(nrow(segs))) {
  linedef <- linedefs[segs$linedef[i] + 1, ]

  if (segs$direction[i] == 0) {
    front_sidedef[i] <- linedef$right_sidedef
    back_sidedef[i] <- linedef$left_sidedef
  } else {
    front_sidedef[i] <- linedef$left_sidedef
    back_sidedef[i] <- linedef$right_sidedef
  }

  front_sector[i] <- get_sector_for_sidedef(front_sidedef[i])
  back_sector[i] <- get_sector_for_sidedef(back_sidedef[i])
}

seg_info <- cbind(
  segs,
  front_sidedef = front_sidedef,
  back_sidedef = back_sidedef,
  front_sector = front_sector,
  back_sector = back_sector
)

seg_info$front_floor <- sectors$floor_height[seg_info$front_sector + 1]
seg_info$front_ceiling <- sectors$ceiling_height[seg_info$front_sector + 1]
seg_info$back_floor <- ifelse(
  is.na(seg_info$back_sector),
  NA,
  sectors$floor_height[seg_info$back_sector + 1]
)
seg_info$back_ceiling <- ifelse(
  is.na(seg_info$back_sector),
  NA,
  sectors$ceiling_height[seg_info$back_sector + 1]
)

walls <- data.frame(
  x1 = vertices$x[seg_info$v1 + 1],
  y1 = vertices$y[seg_info$v1 + 1],
  x2 = vertices$x[seg_info$v2 + 1],
  y2 = vertices$y[seg_info$v2 + 1],
  linedef = seg_info$linedef,
  direction = seg_info$direction,
  front_sector = seg_info$front_sector,
  back_sector = seg_info$back_sector,
  front_floor = seg_info$front_floor,
  front_ceiling = seg_info$front_ceiling,
  back_floor = seg_info$back_floor,
  back_ceiling = seg_info$back_ceiling
)

cat("Constructed", nrow(walls), "wall segments with sector heights.\n")

map_plot <- ggplot(walls) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2)) +
  coord_equal() +
  theme_void()


######## Player and rays ########

player <- list(
  x = 1024,
  y = -3264,
  z = 41,
  angle = atan2(-96, -96)
)

fov <- pi / 2
n_rays <- 640

angles <- seq(
  player$angle - fov / 2,
  player$angle + fov / 2,
  length.out = n_rays
)

rays <- data.frame(
  dx = cos(angles),
  dy = sin(angles)
)

ray_plot <- ggplot(rays) +
  geom_segment(
    aes(
      x = player$x,
      y = player$y,
      xend = player$x + 50 * dx,
      yend = player$y + 50 * dy
    ),
    color = "blue"
  ) +
  annotate("point", x = player$x, y = player$y, color = "red", size = 3) +
  coord_fixed() +
  theme_minimal()


######## Sector-aware ray render ########

intersect_ray_segment <- function(px, py, dx, dy, x1, y1, x2, y2) {
  sx <- x2 - x1
  sy <- y2 - y1
  denominator <- dx * sy - dy * sx

  if (abs(denominator) < 1e-9) {
    return(NULL)
  }

  t <- ((x1 - px) * sy - (y1 - py) * sx) / denominator
  u <- (dx * (y1 - py) - dy * (x1 - px)) / denominator

  if (t >= 0 && u >= 0 && u <= 1) {
    intersection_x <- px + t * dx
    intersection_y <- py + t * dy

    return(list(
      x = intersection_x,
      y = intersection_y,
      dist = sqrt((intersection_x - px)^2 + (intersection_y - py)^2)
    ))
  }

  NULL
}

screen_width <- 640
screen_height <- 480
half_screen <- screen_height / 2
projection_scale <- (screen_width / 2) / tan(fov / 2)

project_z <- function(world_z, corrected_distance) {
  half_screen - ((world_z - player$z) * projection_scale / corrected_distance)
}

is_subsector <- function(index) {
  bitwAnd(index, 0x8000) != 0
}

get_subsector_index <- function(index) {
  bitwAnd(index, 0x7FFF)
}

is_point_on_front_side <- function(px, py, node) {
  ((px - node$x[1]) * node$dy[1] - (py - node$y[1]) * node$dx[1]) <= 0
}

find_player_subsector <- function(node_index, px, py, nodes) {
  node_index <- as.integer(node_index[1])

  if (is_subsector(node_index)) {
    return(get_subsector_index(node_index))
  }

  node <- nodes[node_index + 1, ]
  if (is_point_on_front_side(px, py, node)) {
    find_player_subsector(node$left_child, px, py, nodes)
  } else {
    find_player_subsector(node$right_child, px, py, nodes)
  }
}

get_subsector_sector <- function(ssector_index, seg_info, ssectors) {
  entry <- ssectors[ssector_index + 1, ]
  first_seg <- entry$first_seg_index + 1
  seg_info$front_sector[first_seg]
}

root_node <- nrow(nodes) - 1
player_subsector <- find_player_subsector(root_node, player$x, player$y, nodes)
player_sector <- get_subsector_sector(player_subsector, seg_info, ssectors)
cat("Player starts in subsector", player_subsector, "sector", player_sector, "\n")

make_wall_span <- function(x, corrected_distance, z_low, z_high,
                           linedef, front_sector, back_sector, part) {
  y_top <- project_z(z_high, corrected_distance)
  y_bottom <- project_z(z_low, corrected_distance)

  top <- max(min(min(y_top, y_bottom), screen_height), 0)
  bottom <- max(min(max(y_top, y_bottom), screen_height), 0)

  shade <- max(45, min(235, 245 - corrected_distance / 5))
  if (part == "lower") {
    shade <- shade * 0.72
  } else if (part == "upper") {
    shade <- shade * 0.86
  }

  data.frame(
    x = x,
    y_top = top,
    y_bottom = bottom,
    corrected_distance = corrected_distance,
    linedef = linedef,
    front_sector = front_sector,
    back_sector = back_sector,
    part = part,
    shade = grDevices::rgb(shade, shade, shade, maxColorValue = 255),
    stringsAsFactors = FALSE
  )
}

make_plane_span <- function(x, x_width, near_distance, far_distance, sector_index, plane) {
  if (is.na(sector_index) || far_distance <= near_distance) {
    return(NULL)
  }

  sector <- sectors[sector_index + 1, ]
  world_z <- if (plane == "floor") sector$floor_height else sector$ceiling_height
  y_near <- project_z(world_z, near_distance)
  y_far <- project_z(world_z, far_distance)

  y_min <- max(min(min(y_near, y_far), screen_height), 0)
  y_max <- max(min(max(y_near, y_far), screen_height), 0)
  base <- if (plane == "floor") 42 else 26

  if (abs(y_near - y_far) < 0.5 || y_max - y_min < 0.5) {
    return(NULL)
  }

  shade <- max(20, min(120, base + 120 / (1 + far_distance / 260)))

  data.frame(
    xmin = x - x_width / 2,
    xmax = x + x_width / 2,
    ymin = y_min,
    ymax = y_max,
    sector = sector_index,
    plane = plane,
    shade = grDevices::rgb(shade, shade, shade, maxColorValue = 255),
    stringsAsFactors = FALSE
  )
}

clip_span_to_windows <- function(y_top, y_bottom, windows) {
  clipped <- data.frame(y_top = numeric(0), y_bottom = numeric(0))

  if (nrow(windows) == 0 || y_bottom - y_top < 0.5) {
    return(clipped)
  }

  for (i in seq_len(nrow(windows))) {
    top <- max(y_top, windows$y_top[i])
    bottom <- min(y_bottom, windows$y_bottom[i])

    if (bottom - top >= 0.5) {
      clipped <- rbind(clipped, data.frame(y_top = top, y_bottom = bottom))
    }
  }

  clipped
}

subtract_span_from_windows <- function(windows, y_top, y_bottom) {
  remaining <- data.frame(y_top = numeric(0), y_bottom = numeric(0))

  if (nrow(windows) == 0 || y_bottom - y_top < 0.5) {
    return(windows)
  }

  for (i in seq_len(nrow(windows))) {
    win_top <- windows$y_top[i]
    win_bottom <- windows$y_bottom[i]

    if (y_bottom <= win_top || y_top >= win_bottom) {
      remaining <- rbind(remaining, windows[i, ])
    } else {
      if (y_top - win_top >= 0.5) {
        remaining <- rbind(remaining, data.frame(y_top = win_top, y_bottom = y_top))
      }
      if (win_bottom - y_bottom >= 0.5) {
        remaining <- rbind(remaining, data.frame(y_top = y_bottom, y_bottom = win_bottom))
      }
    }
  }

  remaining
}

slice_rows <- list()
plane_rows <- list()
column_width <- screen_width / n_rays

for (i in seq_len(n_rays)) {
  ray_dx <- rays$dx[i]
  ray_dy <- rays$dy[i]
  ray_hits <- data.frame(
    wall_index = integer(0),
    dist = numeric(0),
    corrected_distance = numeric(0)
  )

  for (j in seq_len(nrow(walls))) {
    wall <- walls[j, ]
    res <- intersect_ray_segment(
      px = player$x,
      py = player$y,
      dx = ray_dx,
      dy = ray_dy,
      x1 = wall$x1,
      y1 = wall$y1,
      x2 = wall$x2,
      y2 = wall$y2
    )

    if (!is.null(res)) {
      corrected_distance <- res$dist * cos(angles[i] - player$angle)

      if (corrected_distance > 1e-6) {
        ray_hits <- rbind(ray_hits, data.frame(
          wall_index = j,
          dist = res$dist,
          corrected_distance = corrected_distance
        ))
      }
    }
  }

  if (nrow(ray_hits) > 0) {
    ray_hits <- ray_hits[order(ray_hits$corrected_distance), ]
    x_p <- ((i - 0.5) / n_rays) * screen_width
    visible_windows <- data.frame(y_top = 0, y_bottom = screen_height)
    current_sector <- player_sector
    interval_start <- 1

    for (hit_idx in seq_len(nrow(ray_hits))) {
      if (nrow(visible_windows) == 0) {
        break
      }

      hit <- ray_hits[hit_idx, ]
      wall <- walls[hit$wall_index, ]
      spans <- list()
      interval_end <- hit$corrected_distance

      plane_rows[[length(plane_rows) + 1]] <- make_plane_span(
        x_p,
        column_width,
        interval_start,
        interval_end,
        current_sector,
        "floor"
      )
      plane_rows[[length(plane_rows) + 1]] <- make_plane_span(
        x_p,
        column_width,
        interval_start,
        interval_end,
        current_sector,
        "ceiling"
      )

      if (is.na(wall$back_sector)) {
        spans[[length(spans) + 1]] <- make_wall_span(
          x_p,
          hit$corrected_distance,
          wall$front_floor,
          wall$front_ceiling,
          wall$linedef,
          wall$front_sector,
          wall$back_sector,
          "solid"
        )
      } else {
        next_sector <- if (!is.na(current_sector) && current_sector == wall$front_sector) {
          wall$back_sector
        } else if (!is.na(current_sector) && current_sector == wall$back_sector) {
          wall$front_sector
        } else {
          wall$back_sector
        }

        if (!isTRUE(all.equal(wall$front_floor, wall$back_floor))) {
          spans[[length(spans) + 1]] <- make_wall_span(
            x_p,
            hit$corrected_distance,
            min(wall$front_floor, wall$back_floor),
            max(wall$front_floor, wall$back_floor),
            wall$linedef,
            wall$front_sector,
            wall$back_sector,
            "lower"
          )
        }

        if (!isTRUE(all.equal(wall$front_ceiling, wall$back_ceiling))) {
          spans[[length(spans) + 1]] <- make_wall_span(
            x_p,
            hit$corrected_distance,
            min(wall$front_ceiling, wall$back_ceiling),
            max(wall$front_ceiling, wall$back_ceiling),
            wall$linedef,
            wall$front_sector,
            wall$back_sector,
            "upper"
          )
        }

        current_sector <- next_sector
        interval_start <- interval_end
      }

      for (span in spans) {
        if (is.null(span)) {
          next
        }

        clipped_spans <- clip_span_to_windows(span$y_top, span$y_bottom, visible_windows)
        if (nrow(clipped_spans) == 0) {
          next
        }

        for (clip_idx in seq_len(nrow(clipped_spans))) {
          clipped_span <- span
          clipped_span$y_top <- clipped_spans$y_top[clip_idx]
          clipped_span$y_bottom <- clipped_spans$y_bottom[clip_idx]
          slice_rows[[length(slice_rows) + 1]] <- clipped_span
        }

        visible_windows <- subtract_span_from_windows(
          visible_windows,
          span$y_top,
          span$y_bottom
        )
      }

      if (is.na(wall$back_sector)) {
        break
      }
    }

    if (nrow(visible_windows) > 0 && !is.na(current_sector)) {
      far_clip <- 2400
      plane_rows[[length(plane_rows) + 1]] <- make_plane_span(
        x_p,
        column_width,
        interval_start,
        far_clip,
        current_sector,
        "floor"
      )
      plane_rows[[length(plane_rows) + 1]] <- make_plane_span(
        x_p,
        column_width,
        interval_start,
        far_clip,
        current_sector,
        "ceiling"
      )
    }
  }
}

slice_data <- do.call(rbind, slice_rows)
plane_data <- do.call(rbind, plane_rows)

######## Connected wall-polygon render ########

camera_transform <- function(x, y) {
  dx <- x - player$x
  dy <- y - player$y

  list(
    forward = dx * cos(player$angle) + dy * sin(player$angle),
    side = -dx * sin(player$angle) + dy * cos(player$angle)
  )
}

clip_to_near_plane <- function(p1, p2, near_clip) {
  if (p1$forward >= near_clip && p2$forward >= near_clip) {
    return(list(p1 = p1, p2 = p2))
  }

  if (p1$forward < near_clip && p2$forward < near_clip) {
    return(NULL)
  }

  t <- (near_clip - p1$forward) / (p2$forward - p1$forward)
  clipped <- list(
    forward = near_clip,
    side = p1$side + t * (p2$side - p1$side)
  )

  if (p1$forward < near_clip) {
    list(p1 = clipped, p2 = p2)
  } else {
    list(p1 = p1, p2 = clipped)
  }
}

screen_x_from_camera <- function(point) {
  screen_width / 2 + point$side * projection_scale / point$forward
}

screen_y_from_camera <- function(world_z, point) {
  half_screen - ((world_z - player$z) * projection_scale / point$forward)
}

make_wall_polygon <- function(wall, z_low, z_high, part, group_id) {
  p1 <- camera_transform(wall$x1, wall$y1)
  p2 <- camera_transform(wall$x2, wall$y2)
  clipped <- clip_to_near_plane(p1, p2, near_clip = 8)

  if (is.null(clipped)) {
    return(NULL)
  }

  p1 <- clipped$p1
  p2 <- clipped$p2
  sx1 <- screen_x_from_camera(p1)
  sx2 <- screen_x_from_camera(p2)

  if (max(sx1, sx2) < 0 || min(sx1, sx2) > screen_width) {
    return(NULL)
  }

  y1_top <- screen_y_from_camera(z_high, p1)
  y1_bottom <- screen_y_from_camera(z_low, p1)
  y2_top <- screen_y_from_camera(z_high, p2)
  y2_bottom <- screen_y_from_camera(z_low, p2)

  if (max(y1_top, y1_bottom, y2_top, y2_bottom) < 0 ||
      min(y1_top, y1_bottom, y2_top, y2_bottom) > screen_height) {
    return(NULL)
  }

  avg_dist <- (p1$forward + p2$forward) / 2
  shade <- max(40, min(230, 245 - avg_dist / 5))
  if (part == "lower") {
    shade <- shade * 0.74
  } else if (part == "upper") {
    shade <- shade * 0.88
  }

  data.frame(
    group = group_id,
    x = c(sx1, sx2, sx2, sx1),
    y = c(y1_top, y2_top, y2_bottom, y1_bottom),
    avg_dist = avg_dist,
    part = part,
    shade = grDevices::rgb(shade, shade, shade, maxColorValue = 255),
    stringsAsFactors = FALSE
  )
}

polygon_rows <- list()
group_id <- 1

for (i in seq_len(nrow(walls))) {
  wall <- walls[i, ]
  wall_parts <- list()

  if (is.na(wall$back_sector)) {
    wall_parts[[length(wall_parts) + 1]] <- list(
      z_low = wall$front_floor,
      z_high = wall$front_ceiling,
      part = "solid"
    )
  } else {
    if (!isTRUE(all.equal(wall$front_floor, wall$back_floor))) {
      wall_parts[[length(wall_parts) + 1]] <- list(
        z_low = min(wall$front_floor, wall$back_floor),
        z_high = max(wall$front_floor, wall$back_floor),
        part = "lower"
      )
    }

    if (!isTRUE(all.equal(wall$front_ceiling, wall$back_ceiling))) {
      wall_parts[[length(wall_parts) + 1]] <- list(
        z_low = min(wall$front_ceiling, wall$back_ceiling),
        z_high = max(wall$front_ceiling, wall$back_ceiling),
        part = "upper"
      )
    }
  }

  for (wall_part in wall_parts) {
    polygon <- make_wall_polygon(
      wall,
      wall_part$z_low,
      wall_part$z_high,
      wall_part$part,
      group_id
    )

    if (!is.null(polygon)) {
      polygon_rows[[length(polygon_rows) + 1]] <- polygon
      group_id <- group_id + 1
    }
  }
}

wall_polygons <- do.call(rbind, polygon_rows)
wall_polygons <- wall_polygons[order(wall_polygons$avg_dist, decreasing = TRUE), ]

render_plot <- ggplot(wall_polygons) +
  geom_polygon(
    aes(
      x = x,
      y = y,
      group = group,
      fill = shade
    ),
    colour = "#111111",
    linewidth = 0.08
  ) +
  scale_fill_identity() +
  coord_fixed(
    ratio = 1,
    xlim = c(0, screen_width),
    ylim = c(screen_height, 0),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black", colour = NA),
    plot.background = element_rect(fill = "black", colour = NA),
    legend.position = "none"
  )

print(render_plot)
ggsave(
  "doom_render_debug.png",
  plot = render_plot,
  width = 8,
  height = 6,
  dpi = 120
)
cat("Saved render to", normalizePath("doom_render_debug.png"), "\n")


######## BSP traversal debug ########

is_point_on_front_side <- function(px, py, node) {
  ((px - node$x[1]) * node$dy[1] - (py - node$y[1]) * node$dx[1]) <= 0
}

is_subsector <- function(index) {
  bitwAnd(index, 0x8000) != 0
}

get_subsector_index <- function(index) {
  bitwAnd(index, 0x7FFF)
}

draw_ssector <- function(ssector_index, walls, seg_info, ssectors, verbose = FALSE) {
  entry <- ssectors[ssector_index + 1, ]
  num_segs <- entry$num_segs
  start_idx <- entry$first_seg_index

  for (i in 0:(num_segs - 1)) {
    seg_index <- start_idx + i
    wall <- walls[seg_index + 1, ]
    seg <- seg_info[seg_index + 1, ]

    if (verbose) {
      cat(sprintf(
        "Drawing SEG %d (linedef %d, front sector %d): (%d,%d) -> (%d,%d)\n",
        seg_index,
        seg$linedef,
        seg$front_sector,
        wall$x1,
        wall$y1,
        wall$x2,
        wall$y2
      ))
    }
  }
}

traverse_bsp <- function(node_index, px, py, walls, seg_info, nodes, ssectors,
                         verbose = FALSE) {
  node_index <- as.integer(node_index[1])

  if (is_subsector(node_index)) {
    ssector_index <- get_subsector_index(node_index)

    if (ssector_index >= 0 && ssector_index < nrow(ssectors)) {
      if (verbose) {
        cat(sprintf("Reached SSECTOR %d\n", ssector_index))
      }
      draw_ssector(ssector_index, walls, seg_info, ssectors, verbose)
      return(ssector_index)
    } else {
      cat(sprintf("Invalid SSECTOR index: %d\n", ssector_index))
    }
    return(integer(0))
  }

  if (node_index < 0 || node_index >= nrow(nodes)) {
    cat(sprintf("Invalid NODE index: %d\n", node_index))
    return(integer(0))
  }

  node <- nodes[node_index + 1, ]

  if (is_point_on_front_side(px, py, node)) {
    draw_order <- c(
      traverse_bsp(node$left_child, px, py, walls, seg_info, nodes, ssectors, verbose),
      traverse_bsp(node$right_child, px, py, walls, seg_info, nodes, ssectors, verbose)
    )
  } else {
    draw_order <- c(
      traverse_bsp(node$right_child, px, py, walls, seg_info, nodes, ssectors, verbose),
      traverse_bsp(node$left_child, px, py, walls, seg_info, nodes, ssectors, verbose)
    )
  }

  draw_order
}

root_node <- nrow(nodes) - 1
cat("Traversing BSP from root node", root_node, "\n")
bsp_draw_order <- traverse_bsp(
  root_node,
  player$x,
  player$y,
  walls,
  seg_info,
  nodes,
  ssectors,
  verbose = FALSE
)
cat("BSP traversal visited", length(bsp_draw_order), "subsectors.\n")
