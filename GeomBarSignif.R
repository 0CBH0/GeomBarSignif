GeomBarSignif <- ggproto("GeomBarSignif", Geom,
	required_aes = c("x", "y", "control_index", "signif", "ns", "style"),
  	default_aes = aes(colour = "black",  size=0.3, textsize = 3.88, angle = 0, hjust = 0, vjust = 0.02, ns = FALSE, style = 1, alpha = NA, family = "", fontface = 1, lineheight = 1.2),
	draw_key = ggplot2::draw_key_point,
	draw_panel = function(data, panel_params, coord, check_overlap = FALSE)
	{
		if(length(union(data$group, data$group)) != nrow(data)) data$group <- 1:nrow(data)
		ns <- data$ns
		data$control_index <- data$control_index[data$group]
		data$signif <- data$signif[data$group]
		data <- data[order(data$group),]
		data <- coord$transform(data, panel_params)
		dsi <- 0
		for(i in 1:nrow(data))
		{
			if(data$signif[i] != "NS" | ns[i] == TRUE) dsi <- c(dsi, i)
		}
		ic <- union(data$control_index, data$control_index)
		for(i in 1:length(ic))
		{
			if(length(which(dsi == ic[i])) == 0) next
			dsi <- dsi[-which(dsi == ic[i])]
		}
		dsi <- dsi[-1]
		if(length(dsi) > 0)
		{
			data_sub <- data[dsi,]
			point_x <- matrix(0, nrow(data_sub), 9)
			point_y <- matrix(0, nrow(data_sub), 9)
			sub_class <- union(data_sub$control_index, data_sub$control_index)
			point_y[, 4] <- sapply(data_sub$y+data_sub$vjust, function(x) min(x, 0.96))
			for(i in 1:length(sub_class))
			{
				index <- which(data$control_index == sub_class[i])[-1]
				sub_index <- which(data_sub$control_index == sub_class[i])
				if(length(sub_index) == 0) next
				base_y <- data$y[match(sub_class[i], data$group)]+data$vjust[match(sub_class[i], data$group)]
				point_y[sub_index, 1] <- sapply(base_y+seq(0, length(sub_index)-1,1)*0.04, function(x) min(x, 0.96))
				point_y[sub_index, 2] <- sapply(sub_index, function(x) min(max(point_y[x, 1], point_y[x, 4])+0.02, 0.98))
				for(j in 1:length(sub_index))
				{
					left_index <- index[which(index < data_sub$group[sub_index[j]])]
					if(length(left_index) == 0) next
					left_data_y <- data$y[left_index]+data$vjust[left_index]
					if(max(left_data_y) > point_y[sub_index[j], 2]) point_y[sub_index[j], 2] <- max(left_data_y, point_y[sub_index[j], 1]+0.02, point_y[sub_index[j], 4]+0.02)
				}
			}
			for(i in 1:length(sub_class))
			{
				sub_index <- which(data_sub$control_index == sub_class[i])
				if(length(sub_index) < 2) next
				for(j in 2:length(sub_index))
				{
					if(point_y[sub_index[j], 1] < point_y[sub_index[j-1], 2])
					{
						point_y[sub_index[j], 1] <- min(point_y[sub_index[j-1], 2]+0.02, 0.96)
						point_y[sub_index[j], 2] <- max(point_y[sub_index[j], 1], point_y[sub_index[j], 4])+0.02
					}
				}
			}
			point_y[which(point_y[, 2] < point_y[, 1]+0.02), 2] <- point_y[which(point_y[, 2] < point_y[, 1]+0.02), 1]+0.02
			point_y[which(point_y[, 2] < point_y[, 4]+0.02), 2] <- point_y[which(point_y[, 2] < point_y[, 4]+0.02), 4]+0.02
			point_x[, 1] <- data$x[match(data_sub$control_index, data$group)]
			point_x[, 2] <- data$x[match(data_sub$control_index, data$group)]
			point_x[, 3] <- data_sub$x
			point_y[, 3] <- point_y[, 2]
			point_x[, 4] <- data_sub$x
			point_x[, 9] <- (data_sub$x+data$x[match(data_sub$control_index, data$group)])/2
			point_y[, 9] <- point_y[, 2]
			point_y[, 1]
			data_sub$vjust <- 0.2
			if(length(which(data_sub$signif == "NS")) > 0) 
			{
				data_sub$vjust[which(data_sub$signif == "NS")] = -0.25
				data_sub$textsize[which(data_sub$signif == "NS")] = 2.5
			}
			if(length(which(data_sub$style == 2)) > 0) point_y[which(data_sub$style == 2), c(1, 4)] <- point_y[which(data_sub$style == 2), 2]
			grid::gList(
				grid::textGrob(
					data_sub$signif,
					point_x[, 9], point_y[, 9],
					default.units = "native",
					hjust = 0.5, vjust = data_sub$vjust,
					rot = data_sub$angle,
					check.overlap = check_overlap,
					gp = grid::gpar(
						col = scales::alpha(data_sub$colour, data_sub$alpha),
						fontsize = data_sub$textsize * ggplot2::.pt,
						fontfamily = data_sub$family,
						fontface = data_sub$fontface,
						lineheight = data_sub$lineheight)
				),
				grid::segmentsGrob(
					point_x[,c(2, 1, 4)], point_y[,c(2, 1, 4)],
					default.units = "native",
					point_x[,c(3, 2, 3)], point_y[,c(3, 2, 3)],
					gp = grid::gpar(
						col = scales::alpha(data_sub$colour, data_sub$alpha),
						lty = data_sub$linetype,
						lwd = data_sub$size * ggplot2::.pt
					)
				)
			)
		}
	}
)

geom_barsignif <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, 
	control_index=NULL, signif=NULL, ns = FALSE, style = 1, nudge_x = 0, nudge_y = 0, check_overlap = FALSE, ...)
{
	if (!missing(nudge_x) || !missing(nudge_y))
	{
		if (!missing(position)) stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    		position <- position_nudge(nudge_x, nudge_y)
    	}
	params = list(na.rm = na.rm, ...)
	params = c(params, list(control_index=control_index, signif=signif, ns=ns, style=style, check_overlap=check_overlap))
	layer(geom = GeomBarSignif, mapping = mapping,  data = data, stat = stat, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = params)
}
