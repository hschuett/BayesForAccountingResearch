theme_prodgray <- function(font_size = 9, font_family = "", line_size = .1,
                           rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  half_line <- font_size / 2
  small_size <- rel_small * font_size
  dark_gray   <- "#4C4C4C"  # grey30

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = dark_gray,
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_line(color = dark_gray, size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(color = dark_gray, size = small_size),
      axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      axis.text.y       = element_text(margin = margin(r = small_size / 4), hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_line(color = dark_gray, size = line_size),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title.x      = element_text(
        margin = margin(t = half_line / 2),
        vjust = 1
      ),
      axis.title.x.top  = element_text(
        margin = margin(b = half_line / 2),
        vjust = 0
      ),
      axis.title.y      = element_text(
        angle = 90,
        margin = margin(r = half_line / 2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line / 2),
        vjust = 0
      ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = ggplot2::element_rect(colour=dark_gray, fill=NA, size=0.5),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_blank(),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(3.5, 3.5, 3.5, 3.5),

      complete = TRUE
    )
}

save_fig <- function(fig, figname, w = 6, h = 3.2, pth = "../out/figs/"){
  file_path = paste0(pth, figname)
  ggsave(paste0(file_path, ".pdf"), fig, width = w, height = h)

  devEMF::emf(paste0(file_path, ".emf"), width = w, height = h,
              emfPlus = T, emfPlusFont = F, emfPlusFontToPath = T, emfPlusRaster = T)
  print(fig)
  invisible(dev.off())

  ragg::agg_png(paste0(file_path, ".png"),
                width = w, height = h, res = 300, units = "in")
  print(fig)
  invisible(dev.off())

  # return(paste0(file_path, ".pdf"))
}

desc_row <- function(x, n){
  drow <-  data.frame(
    var = n,
    mean = mean(x),
    sd   = sd(x),
    q5 = quantile(x, 0.05),
    q25 = quantile(x, 0.25),
    q50 = quantile(x, 0.50),
    q75 = quantile(x, 0.75),
    q95 = quantile(x, 0.95)
  )
  return(drow)
}
