# ---------------------------------------------------------------------------
# Builds the rewindr hex logo and the pkgdown favicons.
#
# The motif is the package in one picture: a row of data-frame snapshots on a
# timeline, the way rewindr records the data after every step. The scrubber
# knob has been dragged back to the snapshot where rows went missing, which
# is outlined in coral and has lost rows. A rewind symbol starts the track.
# Below, a light "rewind" and a heavy "r", and neu-data.com along the
# lower-right edge in the style of the other Neudata stickers.
#
# Run from the package root:  source("data-raw/make_logo.R")
# Needs ragg, systemfonts, magick and jsonlite, and the Segoe UI font.
# ---------------------------------------------------------------------------

library(grid)

systemfonts::register_variant("SegoeLight", "Segoe UI", weight = "light")
systemfonts::register_variant("SegoeHeavy", "Segoe UI", weight = "heavy")
systemfonts::register_variant("SegoeSemi", "Segoe UI", weight = "semibold")

W <- 518
H <- 600
CX <- 259
CY <- 300

al <- function(col, a) {
  m <- grDevices::col2rgb(col)
  grDevices::rgb(m[1, ], m[2, ], m[3, ], alpha = a * 255, maxColorValue = 255)
}

# bg: sticker background; rim: border; ink: cards, track and name;
# ok: healthy rows; bad: the divergent snapshot; knob: the scrubber.
p <- list(bg = "#0E3A46", rim = "#04242F", ink = "#F4EFE3",
          ok = "#7FD1B9", bad = "#FF6F59", knob = "#FFC857")

hex_pts <- function(r, cx = CX, cy = CY) {
  a <- seq(90, 390, by = 60)[1:6] * pi / 180
  list(x = cx + r * cos(a), y = cy + r * sin(a))
}

rline <- function(x0, x1, y0, y1 = y0, col, lwd) {
  grid.lines(c(x0, x1), c(y0, y1), default.units = "native",
             gp = gpar(col = col, lwd = lwd, lineend = "round"))
}

# Rounded rectangle drawn as a polygon (grid.roundrect misbehaves with
# native units).
rrect <- function(x, y, w, h, fill, r = 8, col = NA, lwd = 1) {
  r <- min(r, w / 2, h / 2)
  a <- seq(0, pi / 2, length.out = 12)
  cx <- c(x + w / 2 - r, x - w / 2 + r, x - w / 2 + r, x + w / 2 - r)
  cy <- c(y + h / 2 - r, y + h / 2 - r, y - h / 2 + r, y - h / 2 + r)
  px <- c(cx[1] + r * cos(a), cx[2] + r * cos(a + pi / 2),
          cx[3] + r * cos(a + pi), cx[4] + r * cos(a + 3 * pi / 2))
  py <- c(cy[1] + r * sin(a), cy[2] + r * sin(a + pi / 2),
          cy[3] + r * sin(a + pi), cy[4] + r * sin(a + 3 * pi / 2))
  grid.polygon(px, py, default.units = "native", gp = gpar(fill = fill, col = col, lwd = lwd))
}

# One data-frame snapshot: a card with a header bar and `n` rows.
snapshot <- function(x, y, n, row_col, alpha, outline = NA) {
  w <- 66; h <- 92
  rrect(x, y, w, h, al(p$ink, 0.10 * alpha + 0.04), r = 10, col = outline, lwd = if (is.na(outline)) 1 else 7)
  rrect(x, y + h / 2 - 14, w - 16, 9, al(p$ink, 0.75 * alpha), r = 4.5)
  ys <- y + h / 2 - 33 - (seq_len(n) - 1) * 14
  for (yy in ys) rline(x - w / 2 + 12, x + w / 2 - 12, yy, col = al(row_col, alpha), lwd = 6)
}

draw_logo <- function() {
  grid.newpage()
  pushViewport(viewport(xscale = c(0, W), yscale = c(0, H)))

  o <- hex_pts(299)
  grid.polygon(o$x, o$y, default.units = "native", gp = gpar(fill = p$rim, col = NA))
  i <- hex_pts(284)
  grid.polygon(i$x, i$y, default.units = "native", gp = gpar(fill = p$bg, col = NA))

  # snapshots along the timeline; the fourth is where rows went missing
  xs <- CX + (-2:2) * 80
  cy <- 410
  rows <- c(4, 4, 4, 2, 2)
  alpha <- c(0.55, 0.7, 0.85, 1, 0.45)
  for (k in 1:5) {
    bad <- k == 4
    snapshot(xs[k], cy, rows[k], if (k >= 4) p$bad else p$ok, alpha[k],
             outline = if (bad) p$bad else NA)
  }
  # the two lost rows, as dashed ghosts in the divergent card
  for (yy in cy + 46 - 33 - (2:3) * 14) {
    grid.lines(c(xs[4] - 21, xs[4] + 21), c(yy, yy), default.units = "native",
               gp = gpar(col = al(p$bad, 0.6), lwd = 4, lty = "22", lineend = "round"))
  }

  # timeline track with ticks, and the knob dragged back to the fourth card
  ty <- 330
  rline(xs[1] - 18, xs[5] + 18, ty, col = al(p$ink, 0.35), lwd = 7)
  rline(xs[1] - 18, xs[4], ty, col = p$knob, lwd = 7)
  for (k in 1:5) rline(xs[k], xs[k], ty - 9, ty + 9, col = al(p$ink, 0.6), lwd = 4)
  grid.circle(xs[4], ty, r = unit(17, "native"), default.units = "native",
              gp = gpar(fill = p$knob, col = p$bg, lwd = 6))

  # rewind symbol |<<: back to an earlier state
  tri <- function(x, y, s) {
    grid.polygon(c(x, x + s, x + s), c(y, y + s * 0.62, y - s * 0.62), default.units = "native",
                 gp = gpar(fill = p$knob, col = NA))
  }
  grid.lines(c(CX - 38, CX - 38), c(262 - 22, 262 + 22), default.units = "native",
             gp = gpar(col = p$knob, lwd = 10, lineend = "butt"))
  tri(CX - 30, 262, 34)
  tri(CX + 4, 262, 34)

  # wordmark: light "rewind" + heavy "r", centred together
  size <- 76
  w1 <- convertWidth(grobWidth(textGrob("rewind", gp = gpar(fontfamily = "SegoeLight", fontsize = size))),
                     "native", valueOnly = TRUE)
  w2 <- convertWidth(grobWidth(textGrob("r", gp = gpar(fontfamily = "SegoeHeavy", fontsize = size))),
                     "native", valueOnly = TRUE)
  start <- CX - (w1 + w2 + 3) / 2
  grid.text("rewind", start, 168, just = c("left", "centre"), default.units = "native",
            gp = gpar(fontfamily = "SegoeLight", fontsize = size, col = al(p$ink, 0.85)))
  grid.text("r", start + w1 + 3, 168, just = c("left", "centre"), default.units = "native",
            gp = gpar(fontfamily = "SegoeHeavy", fontsize = size, col = p$bad))

  grid.text("neu-data.com", 386, 116, rot = 30, default.units = "native",
            gp = gpar(fontfamily = "SegoeSemi", fontsize = 15, col = al(p$ink, 0.7)))
  popViewport()
}

# High-resolution master, downsampled so edges are smooth.
render_master <- function(file) {
  ragg::agg_png(file, width = W * 4, height = H * 4, res = 288, background = "transparent")
  draw_logo()
  invisible(grDevices::dev.off())
  magick::image_read(file)
}

logo <- render_master(tempfile(fileext = ".png"))
dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
magick::image_write(magick::image_resize(logo, paste0(W, "x", H)), "man/figures/logo.png", format = "png")

# Favicons, cut from the master so they match it exactly.
dir.create("pkgdown/favicon", showWarnings = FALSE, recursive = TRUE)
square <- function(size) {
  img <- magick::image_resize(logo, paste0("x", size))
  magick::image_extent(img, paste0(size, "x", size), color = "none", gravity = "center")
}
magick::image_write(square(180), "pkgdown/favicon/apple-touch-icon.png", format = "png")
magick::image_write(square(96), "pkgdown/favicon/favicon-96x96.png", format = "png")
magick::image_write(square(192), "pkgdown/favicon/web-app-manifest-192x192.png", format = "png")
magick::image_write(square(512), "pkgdown/favicon/web-app-manifest-512x512.png", format = "png")
magick::image_write(magick::image_join(square(16), square(32), square(48)),
                    "pkgdown/favicon/favicon.ico", format = "ico")
b64 <- function(path) {
  raw <- readBin(path, "raw", file.info(path)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
}
writeLines(sprintf(paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" width="96" height="96" viewBox="0 0 96 96">',
  '<image width="96" height="96" href="%s"/></svg>'), b64("pkgdown/favicon/favicon-96x96.png")),
  "pkgdown/favicon/favicon.svg")
writeLines(c(
  "{",
  '  "name": "rewindr",',
  '  "short_name": "rewindr",',
  '  "icons": [',
  '    {"src": "/web-app-manifest-192x192.png", "sizes": "192x192", "type": "image/png", "purpose": "maskable"},',
  '    {"src": "/web-app-manifest-512x512.png", "sizes": "512x512", "type": "image/png", "purpose": "maskable"}',
  "  ],",
  sprintf('  "theme_color": "%s",', p$rim),
  sprintf('  "background_color": "%s",', p$bg),
  '  "display": "standalone"',
  "}"
), "pkgdown/favicon/site.webmanifest")
