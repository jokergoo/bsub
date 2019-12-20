 
formatFileSize = function(table, columns) {
	getFromNamespace("formatColumns", ns = "DT")(table, columns, tplFileSize)
}

formatTimeDiff = function(table, columns) {
	getFromNamespace("formatColumns", ns = "DT")(table, columns, tplTimeDiff)
}

# file size should be in bytes
tplFileSize = function(cols, ...) {
  sprintf(
    "DTWidget.formatFileSize(this, row, data, %d);",
    cols
  )
}


# time diff should be in secs
tplTimeDiff = function(cols, digits, ...) {
  sprintf(
    "DTWidget.formatTimeDiff(this, row, data, %d);",
    cols
  )
}
