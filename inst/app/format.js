
format_file_size = function(x) {
	if(x < 1024) {
		return x + "Byte";
	} else if(x < 1024*1024) {
		return Math.round(x/1024) + "KB";
	} else if(x < 1024*1024*1024) {
		return Math.round(x/1024/1024) + "MB";
	} else {
		return Math.round(x/1024/1024/1024) + "GB";
	}
}

DTWidget.formatFileSize = function(thiz, row, data, col) {
	var d = parseFloat(data[col]);
	if (isNaN(d)) return;

	$(thiz.api().cell(row, col).node()).html(format_file_size(d));
}

format_time_diff = function(x) {
	var hour = 0;
	var min = 0;

	if(x < 60) {
		return "0:00";
	} else {
		hour = Math.floor(x / 3600);
		min = Math.floor((x - hour*3600)/60);

		if(min < 10) {
			return hour + ":0" + min;
		} else {
			return hour + ":" + min;
		}
	}
}

DTWidget.formatTimeDiff = function(thiz, row, data, col) {
	var d = parseFloat(data[col]);
	if (isNaN(d)) return;

	$(thiz.api().cell(row, col).node()).html(format_time_diff(d));
}
