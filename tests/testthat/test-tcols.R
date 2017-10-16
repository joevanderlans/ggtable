dapg::reset_session()
library(ggtable)
library(grid)

# find the export path
export_path <- file.path(getwd(), "tests", "export")

# prepare data
text <- rep("This is a text value.", 4)
numeric <- c(1e1, 1e2, 1e3, 1e4)
data <- data.frame(text, numeric_1 = numeric, numeric_2 = numeric)

t <- list()

# test defaults
t[[1]] <- ggtable(data) +
  tlabs(title = "Defaults") +
  tcols(tcol("text"),
        tcol("numeric_1"),
        tcol("numeric_2"))

# test order and selection
t[[2]] <- ggtable(data) +
  tlabs(title = "Order and Selection") +
  tcols(tcol("numeric_2"),
        tcol("text"))

# test header
t[[3]] <- ggtable(data) +
  tlabs(title = "Headers") +
  tcols(tcol("text", "THIS IS A LONG HEADER"),
        tcol("numeric_1", ""),
        tcol("numeric_2", "TWO\nROWS"))

# test format function
t[[4]] <- ggtable(data) +
  tlabs(title = "Format Function") +
  tcols(tcol("text", format_fun = toupper),
        tcol("numeric_1", format_fun = scales::dollar),
        tcol("numeric_2", format_fun = scales::comma))

# test width
t[[5]] <- ggtable(data) +
  tlabs(title = "Widths") +
  tcols(tcol("text", width = unit(2, "inches")),
        tcol("numeric_1", width = unit(3, "inches")),
        tcol("numeric_2", width = unit(4, "inches")))

# test data hjust and x
t[[6]] <- ggtable(data) +
  tlabs(title = "Justification and Position") +
  tcols(tcol("text", hjust = 0, x = 0.1),
        tcol("numeric_1", hjust = 0.5, x = 0.5),
        tcol("numeric_2", hjust = 1, x = 0.9))

# test header hjust and x
t[[7]] <- ggtable(data) +
  tlabs(title = "Header Justification and Position") +
  tcols(tcol("text", header_hjust = 0, header_x = 0.1),
        tcol("numeric_1", header_hjust = 0.5, header_x = 0.5),
        tcol("numeric_2", header_hjust = 1, header_x = 0.9))

# test wrap length
t[[8]] <- ggtable(data) +
  tlabs(title = "Wrap Length") +
  tcols(tcol("text", wrap_len = 20),
        tcol("numeric_1"),
        tcol("numeric_2"))

# export tables
ggtsave(t, file.path(export_path, "tcols.pdf"))
