
formatFileSize = function(table, columns) {
	getFromNamespace("formatColumns", ns = "DT")(table, columns, tplFileSize)
}

formatTimeDiff = function(table, columns) {
	getFromNamespace("formatColumns", ns = "DT")(table, columns, tplTimeDiff)
}

# file size should be in bytes
tplFileSize = function(...) {
  "DTWidget.formatFileSize(data);"
}


# time diff should be in secs
tplTimeDiff = function(...) {
  "DTWidget.formatTimeDiff(data);"
}
