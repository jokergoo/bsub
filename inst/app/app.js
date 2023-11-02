
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

DTWidget.formatFileSize = function(data) {
	var d = parseFloat(data);
	if (isNaN(d)) return "";

	return format_file_size(d);
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

DTWidget.formatTimeDiff = function(data) {
	var d = parseFloat(data);
	if (isNaN(d)) return "";

	return format_time_diff(d);
}

DTWidget.formatNumber = function(data) {
	var d = parseFloat(data);
	if (isNaN(d)) return "";

	return d;
}

dt_bind_events = function() {
	var styles = {
      'color': '#337ab7',
      'text-decoration': 'underline',
      'cursor': 'pointer'
    };

    $('.dataTable tbody tr :nth-child(4)').on('click', function(event) {
        var job_id = $(this).siblings().slice(0, 1).text();
        Shiny.onInputChange('select_job', 0);
        Shiny.onInputChange('select_job', job_id);
    }).css(styles);

    $('.dataTable tbody tr :nth-child(2)').on('click', function(event) {
    	var job_id = $(this).siblings().slice(0, 1).text();
        Shiny.onInputChange('select_dep', 0);
        Shiny.onInputChange('select_dep', job_id);
    }).css(styles);

    $('.dataTable tbody tr :nth-child(3)').on('click', function(event) {
    	var job_id = $(this).siblings().slice(0, 1).text();
        Shiny.onInputChange('select_dep', 0);
        Shiny.onInputChange('select_dep', job_id);
    }).css("cursor", "pointer");

    $('.dataTable tbody tr :nth-child(1)').each(function() {
    	var status = $(this).siblings().slice(1, 2).text();
    	if(status == "RUN" || status == "PEND") {
    		var job_id = $(this).text();
    		$(this).html("<input type='checkbox' class='job_checkbox' name='job_id_kill' value='"+job_id+"' /> " + job_id)
    	}
    })

    $('.dataTable tbody tr :nth-child(3)').each(function() {
    	var status = $(this).html();
    	$(this).html("<span class='" + status + "'>" + status + "</span");
    })

    $('.dataTable tbody tr :nth-child(9)').addClass("dt-right");
    $('.dataTable thead tr :nth-child(9)').addClass("dt-right");

    var k = $('.dataTable tbody tr :nth-child(3)').filter(function() {
    	var status = $(this).text();
    	return status == "RUN" || status == "PEND"
    }).length;
    if(k == 0) {
    	$("#kill").attr("disabled", true);
    } else {
    	$("#kill").attr("disabled", false);
    }

    Shiny.addCustomMessageHandler("clear_div", function(id) { $('#' + id).empty();});
}



gviz_add_events = function() {

	var nodes = $("svg g.node");
	var styles = {
      'cursor': 'pointer'
    };

	nodes.on("click", function(e) {
		var job_id = $(this).find("text").text();
		Shiny.onInputChange('select_job', 0);
        Shiny.onInputChange('select_job', job_id);
	}).css(styles);

	nodes.on("mouseenter", function(e) {
		ellipse = $(this).find("ellipse");
		ellipse.attr("fill", ellipse.attr("stroke") + "40");
	}).on("mouseleave", function(e) {
		ellipse = $(this).find("ellipse");
		ellipse.attr("fill", ellipse.attr("stroke") + "80");
	})

	$("#dep_info").remove();
	
}


$(function() {
	$("#kill").on("click", function(e) {
		var checkbox = $(".job_checkbox");
		var kill_job_id = "";
		if(checkbox.length) {
			for(var i = 0; i < checkbox.length; i ++) {
				if(checkbox[i].checked) {
					kill_job_id = kill_job_id + checkbox[i].getAttribute("value") + ";";
				}
			}
			kill_job_id = kill_job_id + "@" + Math.random();
		}
		Shiny.onInputChange('kill_job_id', kill_job_id);
	})
});

count_down = function(id) {
	var countDownDate = new Date().getTime() + 5*60*1000;

	var x= setInterval(function() {
		var now = new Date().getTime();

		// Find the distance between now and the count down date
		var distance = countDownDate - now;

		// Time calculations for days, hours, minutes and seconds
		var minutes = Math.floor((distance % (1000 * 60 * 60)) / (1000 * 60));
		var seconds = Math.floor((distance % (1000 * 60)) / 1000);

		// Display the result in the element with id="demo"
		document.getElementById(id).innerHTML = minutes + "m " + seconds + "s ";

		// If the count down is finished, write some text
		if (distance < 0) {
			clearInterval(x);
			document.getElementById(id).innerHTML = "done";
		}
		$("#"+id).text()
	}, 1000)
}

