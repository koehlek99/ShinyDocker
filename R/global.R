lst_qcMetrics <- PTXQC:::getMetricsObjects(FALSE)
df.meta <- PTXQC:::getMetaData(lst_qcMetrics = lst_qcMetrics)
lst_qcMetrics_ord <- gsub("qcMetric_", "", names(lst_qcMetrics[df.meta$.id]))