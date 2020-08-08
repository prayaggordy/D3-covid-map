library(ggplot2)
library(ggthemes)
suppressWarnings(suppressMessages(library(grid)))
suppressWarnings(suppressMessages(library(gridExtra)))
library(bbplot)
suppressWarnings(suppressMessages(library(scales)))

theme_silver_chips_minimal <- function(base_family = "Helvetica", base_size = 11.5, plot_title_family = base_family,
																				plot_title_size = 22, plot_title_face = "bold", plot_title_margin = 10,
																				subtitle_family = base_family, subtitle_size = 15, subtitle_face = "plain",
																				subtitle_margin = 15, strip_text_family = base_family, strip_text_size = 12,
																				strip_text_face = "plain", caption_family = base_family,
																				caption_size = 11, caption_face = "italic", caption_margin = 10,
																				axis_text_size = base_size, axis_title_family = subtitle_family,
																				axis_title_size = 12, axis_title_face = "plain", axis_title_margin = 8,
																				plot_margin = margin(30, 30, 30, 30), grid_col = "#cccccc",
																				grid = TRUE, axis_col = "#cccccc", axis = FALSE, ticks = FALSE) {
	ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size) +
		theme(legend.title = element_blank(),
					legend.background = element_blank(),
					legend.key = element_blank(),
					legend.position = "bottom",
					legend.direction = "horizontal",
					legend.box = "vertical",
					legend.margin = margin(0.75, 0.75, 0.75, 0.75))
	if (inherits(grid, "character") | grid == TRUE) {
		ret <- ret + theme(panel.grid = element_line(color = grid_col, size = 0.2),
											 panel.grid.major = element_line(color = grid_col, size = 0.2),
											 panel.grid.minor = element_line(color = grid_col, size = 0.15))
		if (inherits(grid, "character")) {
			if (regexpr("X", grid)[1] < 0)
				ret <- ret + theme(panel.grid.major.x = element_blank())
			if (regexpr("Y", grid)[1] < 0)
				ret <- ret + theme(panel.grid.major.y = element_blank())
			if (regexpr("x", grid)[1] < 0)
				ret <- ret + theme(panel.grid.minor.x = element_blank())
			if (regexpr("y", grid)[1] < 0)
				ret <- ret + theme(panel.grid.minor.y = element_blank())
		}
	}
	else {
		ret <- ret + theme(panel.grid = element_blank())
	}
	if (inherits(axis, "character") | axis == TRUE) {
		ret <- ret + theme(axis.line = element_line(color = "#2b2b2b", size = 0.15))
		if (inherits(axis, "character")) {
			axis <- tolower(axis)
			if (regexpr("x", axis)[1] < 0) {
				ret <- ret + theme(axis.line.x = element_blank())
			}
			else {
				ret <- ret + theme(axis.line.x = element_line(color = axis_col, size = 0.15))
			}
			if (regexpr("y", axis)[1] < 0) {
				ret <- ret + theme(axis.line.y = element_blank())
			}
			else {
				ret <- ret + theme(axis.line.y = element_line(color = axis_col, size = 0.15))
			}
		}
		else {
			ret <- ret + theme(axis.line = element_line(color = axis_col, size = 0.15))
		}
	}
	else {
		ret <- ret + theme(axis.line = element_blank())
	}
	if (!ticks) {
		ret <- ret + theme(axis.ticks = element_blank())
	}
	else {
		ret <- ret + theme(axis.ticks = element_line(size = 0.15),
											 axis.ticks.length = grid::unit(5, "pt"))
	}
	ret <- ret + theme(axis.text = element_text(size = axis_text_size, margin = margin(t = 0)),
										 axis.title.x = element_text(size = axis_title_size, family = axis_title_family, face = axis_title_face, margin = margin(t = axis_title_margin, r = 0, b = 0, l = 0)),
										 axis.title.y = element_text(size = axis_title_size, family = axis_title_family, face = axis_title_face, margin = margin(t = 0, r = axis_title_margin, b = 0, l = 0)),
										 strip.text = element_text(hjust = 0, size = strip_text_size, face = strip_text_face, family = strip_text_family),
										 panel.spacing = grid::unit(2, "lines"),
										 plot.title = element_text(hjust = 0, size = plot_title_size, margin = margin(b = plot_title_margin), family = plot_title_family, face = plot_title_face),
										 plot.subtitle = element_text(hjust = 0, size = subtitle_size, margin = margin(b = subtitle_margin), family = subtitle_family, face = subtitle_face),
										 plot.caption = element_text(hjust = 1, size = caption_size, margin = margin(t = caption_margin), family = caption_family, face = caption_face),
										 plot.margin = plot_margin)
	ret
}

scale_color_silver_chips <- function () {
	colors_df <- data.frame(name = c("celeste", "gold", "tumbleweed", "copper red", "bittersweet"), value = c("#9CF6F6", "#F3C98B", "#DAA588", "#C46D5E", "#F56960"))
	colors <- deframe(colors_df)
	max_n <- length(colors)
	f <- manual_pal(values)
	attr(f, "max_n") <- max_n
	discrete_scale(aesthetics = "color", palette = scales::hue_pal(), f)
}

theme_silver_chips <- function(base_size = 18, base_family = "sans") {
	(theme_foundation(base_size = base_size, base_family = base_family) +
	 	theme(line = element_line(colour = "black"),
	 				rect = element_rect(fill = "#F0F0F0", linetype = 0, colour = NA),
	 				text = element_text(colour = "#3C3C3C"),
	 				axis.text = element_text(size = 13, colour = "#3C3C3C", face = "bold"),
	 				axis.ticks = element_blank(),
	 				axis.line = element_blank(),
	 				axis.title = element_text(size = 18, colour = "#3C3C3C", face = "bold", vjust = 1.5),
	 				legend.title = element_blank(),
	 				legend.background = element_rect(fill = "#F0F0F0", size = 0.5, linetype = "dotted"),
	 				legend.position = "bottom",
	 				legend.direction = "horizontal",
	 				legend.box = "vertical",
	 				legend.margin = margin(0.75, 0.75, 0.75, 0.75),
	 				panel.grid = element_line(colour = NULL),
	 				panel.grid.major = element_line(colour = "#D2D2D2"),
	 				panel.grid.minor = element_blank(),
	 				plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
	 				plot.margin = unit(c(1, 1, 1, 1), "lines"),
	 				panel.background = element_rect(fill = "#F0F0F0"),
	 				plot.background = element_rect(fill = "#F0F0F0"),
	 				panel.border = element_rect(colour = "#F0F0F0"),
	 				strip.background = element_rect()))
}

gray_footer <- function(init_plot, left, right) {
	grid.newpage()

	l <- textGrob(paste('  ', left, sep = ''), x = unit(0, 'npc'), gp = gpar(col = 'white', family = 'sans', fontsize = 9), hjust = 0)
	r <- textGrob(paste(right, '  ', sep = ''), x = unit(1, 'npc'), gp = gpar(col = 'white', family = 'sans', fontsize = 9), hjust = 1)

	f <- grobTree(rectGrob(gp = gpar(fill = '#5B5E5F', lwd = 0)), l, r)

	grid.arrange(init_plot, f, heights = unit(c(0.94, 0.06), c('npc', 'npc')))
}

chips_logo_text <- function(p, source_name = NA, logo_text = "twitter.com/PrayagGordy") {
	if (is.na(source_name)) {
		g <- grid::grobTree(gp = gpar(fontsize = 7, hjust = 1, vjust = 1),
												grid::textGrob("twitter.com/PrayagGordy", x = 0.996, hjust = 1, gp = grid::gpar(fontsize = 8, col = "#888888", fontface = "bold", fontfamily = "Helvetica")))
	} else {
		g <- grid::grobTree(grid::textGrob(source_name, x = 0.004, hjust = 0, gp = grid::gpar(fontsize = 8, col = "#888888", fontface = "bold", fontfamily = "Helvetica")),
												grid::textGrob(logo_text, x = 0.996, hjust = 1, gp = grid::gpar(fontsize = 8, col = "#888888", fontface = "bold", fontfamily = "Helvetica")))
	}

	grid.arrange(p, g, ncol = 1, heights = c(30, 1))
}

left_align <- function(plot_name, pieces){
	grob <- ggplot2::ggplotGrob(plot_name)
	n <- length(pieces)
	grob$layout$l[grob$layout$name %in% pieces] <- 2
	return(grob)
}

create_footer <- function(source_name, logo_image_path) {
	footer <- grid::grobTree(grid::textGrob(source_name, x = 0.004, hjust = 0, gp = grid::gpar(fontsize = 8)),
													 grid::rasterGrob(png::readPNG(logo_image_path), x = 0.944))
	return(footer)
}

source_footer <- function(p, source_name, side = "left") {
	f <- grid::grobTree(grid::textGrob(source_name, x = 0.01, hjust = 0, gp = grid::gpar(fontsize = 10)))

	grid.arrange(p, f, ncol = 1, heights = c(30, 1))
}

chips_logo_circle <- function(p, source_name = "", width_pixels = 640, height_pixels = 450) {
	img <- "/Users/Prayag/Downloads/Newthreshermasthead.png"
	footer <- create_footer(source_name, img)
	plot_left_aligned <- left_align(p, c("subtitle", "title", "caption"))
	ggpubr::ggarrange(plot_left_aligned, footer, ncol = 1, nrow = 2, heights = c(1, 0.09))
}
