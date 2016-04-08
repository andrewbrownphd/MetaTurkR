if($('input[id=assignmentId]').val() == 'ASSIGNMENT_ID_NOT_AVAILABLE') {
	$('#checkSpace').hide();
	$('#submitSpace').show();
};

function honeyFail() {
	$('#review').show();
	$(':input').prop('disabled',true);
	$( ":input:checked" ).prop('disabled',false);
	$('#assignmentId' ).prop('disabled',false);
	$('#checkSpace').hide();
	$('#submitSpace').show();
	$('#submitButton').prop('disabled',false);
	$('#justification').append('<h4> <label for="disagree">' +
		'If you disagree with our answer tell us why.</label>' +
		'<input type="text" class="form-control" id="disagree" placeholder="Optional" />' +
		'</h4>');
	$('#disagree').prop('disabled',false);
	$('input[name=honeypot]').attr('value','fail');
};
