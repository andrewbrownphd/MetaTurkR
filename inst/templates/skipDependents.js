$('input:radio[name=${dep1}]').change(function () {
	
	/*Define what to do if the first element is true*/
	var dependents = [
		${deps}
	];
	
	var tf = $('input:radio[name=' + dependents[0][0] + '][value=' + dependents[0][1] + ']').is(':checked');
	skipPattern(dependents,tf);			
});
