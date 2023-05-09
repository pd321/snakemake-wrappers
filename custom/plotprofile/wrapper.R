library(purrr)
library(ggplot2)
library(locfit)
library(cowplot)
library(ggthemes)
library(rlang)

plot_profile <- function(bwtool_out_files,
                         sample_names,
                         bwtool_start,
                         bwtool_end,
                         bwtool_window,
                         x_axis_break_interval,
                         plot_title,
                         plot_colours,
                         ylimit) {

  # Get the mean signal value
  bw_mean_signal_list <- bwtool_out_files %>%
    purrr::map(~readr::read_tsv(.x, col_names = FALSE)) %>%
    purrr::map(~apply(.x, 2, mean, na.rm = TRUE)) %>%
    purrr::map(~predict(locfit(.x ~ lp(seq_along(.x), nn = 0.1, h = 0.8)), seq_along(.x)))
  # Set col names for df
  names(bw_mean_signal_list) <- sample_names
  # Covert to df
  bw_signal_df <- as.data.frame(bw_mean_signal_list, make.names = FALSE)
  print(colnames(bw_signal_df))
  # As position to df
  bw_signal_df$Position <- seq(from = bwtool_start, to = bwtool_end - bwtool_window, by = bwtool_window)
  # Now lets melt this df

  bw_signal_df_melt <- bw_signal_df %>%
    tidyr::gather(key = "Sample", value = "Signal", -Position)
  # Plotting now

  bw_signal_df_melt$Sample <- factor(bw_signal_df_melt$Sample, levels = sample_names)

  profilePlot <- ggplot(data = bw_signal_df_melt, mapping = aes(x = Position, y = Signal, color = Sample)) +
    geom_line(size = 1) +
    geom_vline(xintercept = 0, colour = "grey", linetype = "dashed", size = 0.3) +
    scale_colour_manual(values = plot_colours, name = "") +
    scale_x_continuous(breaks = seq(from = bwtool_start, to = bwtool_end, by = x_axis_break_interval)) +
    ggplot2::labs(x = "", y = "Normalized Signal", title = plot_title)

    if (!is.na(ylimit)) {
      profilePlot <- profilePlot + ggplot2::ylim(0, ylimit)
    }

    profilePlot <- profilePlot +
    cowplot::theme_cowplot() +
    cowplot::panel_border(colour = "black", size = 1.25) +
    ggplot2::theme(
      axis.line = element_blank(),
      legend.position = "right",
      axis.title.y = element_text(size = 20),
      axis.text = element_text(size = 16)
    )

  return(profilePlot)
}


bwtool_out_files <- snakemake@input[["bwtool_out_files"]]
sample_names <- snakemake@input[["bwtool_out_files"]] %>% purrr::map(basename) %>% purrr::map(~gsub("\\..*", "", .x, perl = TRUE)) %>% unlist
input_count <- sum(grepl("input", sample_names, ignore.case = TRUE))
bwtool_start <- -1 * as.integer(snakemake@params[["bwtool_start_end"]])
bwtool_end <- as.integer(snakemake@params[["bwtool_start_end"]])
bwtool_window <- as.integer(snakemake@params[["bwtool_window"]])
x_axis_break_interval <- as.integer(snakemake@params[["profile_x_axis_break"]])
plot_title <- snakemake@params[["plot_title"]]

if (rlang::has_name(snakemake@params, "plot_colours")) {
  plot_colours <- snakemake@params[["plot_colours"]]
} else {
  plot_colours <- ggthemes::wsj_pal()(6)
}

if (rlang::has_name(snakemake@params, "ylimit")) {
  ylimit <- snakemake@params[["ylimit"]]
} else {
  ylimit <- NA
}

profile_plot <- plot_profile(
  bwtool_out_files = bwtool_out_files,
  sample_names = sample_names,
  bwtool_start = bwtool_start,
  bwtool_end = bwtool_end,
  bwtool_window = bwtool_window,
  x_axis_break_interval = x_axis_break_interval,
  plot_title = plot_title,
  plot_colours = plot_colours,
  ylimit = ylimit
)

ggsave(
  filename = snakemake@output[["plot_profile_out"]],
  plot = profile_plot,
  width = 10,
  height = 8
)
