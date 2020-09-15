# library(demoData)
# library(tidyverse)
# library(metflow2)
#
# path <- file.path(".", "example")
# dir.create(path = path, showWarnings = FALSE)
#
# demo_data <- system.file("metflow2", package = "demoData")
#
# file.copy(from = file.path(demo_data, dir(demo_data)),
#           to = path, overwrite = TRUE, recursive = TRUE)
#
#
# object <- creatMetflowObject(ms1.data = c("batch1.data.csv", "batch2.data.csv"),
#                              sample.information = "sample_info.csv",
#                              path = path)
#
#
#
# object
#
# object <- align_batch(
#   object = object,
#   combine.mz.tol = 15,
#   combine.rt.tol = 30,
#   use.int.tol = FALSE
# )
#
#
# object2 <- filter_peaks(
#   object = object,
#   min.fraction = 0.5,
#   type = "any",
#   min.subject.blank.ratio = 2,
#   according.to = "class",
#   which.group = "QC"
# )
#
#
# object2 <- filter_samples(object = object2,
#                           min.fraction.peak = 0.5)
#
#
# object2 <- imputeMV(object = object2,
#                     method = "knn", colmax = 0.9, rowmax = 0.9)
# object2
#
#
# object3 <- normalizeData(object = object2, method = "svr")
#
#
# object4 <- integrateData(object = object3, method = "qc.mean")
#
# rsd2 <- calculate_rsd(object = object2, slot = "QC")
# rsd4 <- calculate_rsd(object = object4, slot = "QC")
#
#
# dplyr::left_join(rsd2, rsd4, by = c("index", "name")) %>%
#   dplyr::mutate(class = dplyr::case_when(rsd.y < rsd.x ~ "Decrease",
#                                          rsd.y > rsd.x ~ "Increase",
#                                          rsd.y == rsd.y ~ "Equal")) %>%
#   ggplot(aes(rsd.x, rsd.y, colour = class)) +
#   ggsci::scale_color_jama() +
#   geom_abline(slope = 1, intercept = 0) +
#   geom_point() +
#   labs(x = "RSD after normalization", y = "RSD before normalization") +
#   theme_bw()
#
#
#
# object4
#
#
