$(function() {
	$(".lang-toggle").click(function() {
		var language = $(this).data("language");
		$("details.example").hide();
		$("details.example[data-language='"+language+"']").show();
	});

    var defaultLanguage = "C";
    $("details.example[data-language='"+defaultLanguage+"']").show();
});

