function checkAnswers(){
	var wrong = [];
	var right = [];
	req = true;
	cor = true;
			
	/*Get the answers and pass them in*/
	$.each(ans, function(key, value) {
		/*Is the question answered?*/
		v = $('input:radio[name=' + key +']:checked').val();
		if(v === undefined) {
			req = false;
		} else {
			/*Answered correctly?*/
			if(v != value) {
				cor = false;
				wrong.push(key);
			} else {
				right.push(key);
			}
		};
	});

	if(req) {
		/*If everything answered, give feedback*/
		if(honey) {
			/*If everything filled and correct, set honeypot to pass and submit*/
			if(cor) {
				$('input[name=honeypot]').attr('value','pass');
				document.forms["mturk_form"].submit();
			};

			/*If everything filled, but incorrect answers, run processing*/
			if(!cor) {
				ansProcessing(wrong, right);
				honeyFail();
			};
		};

		if(!honey) {
			ansProcessing(wrong, right);
		};
	} else {		
		/*If not, then alert user*/
		if(!req) {
			alert("Please answer all required questions.");
		};
	};

/*Change the display depending on right or wrong answers*/
function ansProcessing(wrong, right) {
	$.each(wrong, function(key,q) {
		$('input[name=' + q + ']').parent().css({"background-color": "#ff4d4d"});
	});
	$.each(right, function(key,q) {
		$('input[name=' + q + ']').parent().css({"background-color": "#00cc00"});
	});
	$('#justification').show();
};
}
