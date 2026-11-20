# Ensure these packages are installed:
# install.packages(c("ggplot2", "hexSticker", "showtext"))

library(ggplot2)
library(hexSticker)
library(showtext)

# 1. Load Fonts (Optional, but good for consistent rendering)
#font_add_google("Noto Sans", "noto_sans")
#showtext_auto()

# 2. Generate Data for Plot
t <- seq(0, 12, length.out = 200)
y0_val <- 0.05
K_val  <- 1
r_val  <- 0.7

logistic_y <- function(time) {
  K_val / (1 + ((K_val - y0_val) / y0_val) * exp(-r_val * time))
}

df_curve <- data.frame(
  t = t,
  y = logistic_y(t)
)

# Data points (simulating noisy observations around the curve)
set.seed(42)
t_obs <- seq(0, 12, by = 0.6) + rnorm(16, 0, 0.3)
df_points <- data.frame(
  t = t_obs,
  y = logistic_y(t_obs) + rnorm(16, 0, 0.05)
)
df_points$y <- pmax(pmin(df_points$y, K_val * 1.05), y0_val * 0.9)

# True inflection point of the logistic curve (where y = K / 2)
t_infl <- log((K_val - y0_val) / y0_val) / r_val
y_infl <- K_val / 2
slope_infl <- (r_val * K_val) / 4


# Tangent line extending from y = y0_val to y = K_val through (t_infl, y_infl)
y_tang <- c(y0_val, K_val)
t_tang <- t_infl + (y_tang - y_infl) / slope_infl
tangent_df <- data.frame(
  t = t_tang,
  y = y_tang
)

# 3. Create the ggplot
p <- ggplot() +
  geom_point(data = df_points, aes(x = t, y = y), shape = 21,
             color = "#557A1A",
             stroke = 0.4,
             fill = "#D2DC46", size = 1.5, alpha = 0.95) +
  geom_line(data = df_curve, aes(x = t, y = y),
            color = "#00008C", linewidth = 0.8, alpha=0.7) +
  geom_line(data = tangent_df, aes(x = t, y = y),
            color = "#D95F02", linewidth = 0.4, linetype = "dashed") +

  # --- Annotations (Single instances only) ---

  geom_segment(aes(x = 0, xend = 5, y = y0_val, yend = y0_val),
               color = "#777777", linetype = "dotted", linewidth = 0.5) +

  geom_segment(aes(x = 5, xend = 12, y = K_val, yend = K_val),
               color = "#777777", linetype = "dotted", linewidth = 0.5) +


  # Smaller mm sizes for svglite / hexSticker
  annotate("text", x = 0.7, y = 0.3, label = expression(y[0]), color = "#00008C", size = 4.0) +
  annotate("text", x = 11.4, y = 0.8, label = expression(K),    color = "#00008C", size = 4.0) +
  annotate("text", x = 7.5, y = 0.48, label = expression(mu[max]), color = "#D95F02", size = 5.0) +

  scale_x_continuous(expand = c(0, 0), limits = c(0, 12)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent", color = NA))

# 4. Generate the Hex Sticker

# Turn off showtext so font sizes aren't inflated in SVG
if ("showtext" %in% loadedNamespaces()) showtext::showtext_auto(FALSE)

# Scale text sizes down by ~72/300 (or ~0.35x) if switching from showtext+PNG to pure vector SVG,
# OR use svglite directly on the sticker object:
s <- sticker(
  p,
  package = "growthrates",
  p_size = 6.4,
  p_color = "#00008C",
  p_fontface = "bold",
  p_y = 1.45,
  s_x = 1.0,
  s_y = 0.8,
  s_width = 1.2,
  s_height = 0.75,
  h_fill = "#D0D3D4",
  h_color = "#00008C",
  h_size = 1.5
)

svglite::svglite("man/figures/logo.svg", width = 1.73, height = 2.0)
print(s)
dev.off()

# ## Using the magick package (ImageMagick in R)
# img <- magick::image_read_svg("man/figures/growthrates_logo.svg")
# img <- magick::image_scale(img, "1200x") # or "1200x" for usethis::use_logo()
# magick::image_write(img, "man/figures/growthrates_logo.png", format = "png")
