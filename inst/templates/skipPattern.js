function skipPattern(deps,tf){
	/*For each dependent radio button*/
	$.each(deps, 
		/*Get the index*/
		function(indx) {
			/*As long as it's not the first index*/
			if(indx !== 0) {
				/*Select the defaults and disable the inputs*/
				$('input:radio[name=' + deps[indx][0] + ']').prop('disabled',tf);
				$('input:radio[name=' + deps[indx][0] + '][value=\'' + deps[indx][1] + '\']').prop('checked',tf);
				/*Make sure selected radio is not disabled*/
				if(tf) {
					$('input:radio[name=' + deps[indx][0] + '][value=\'' + deps[indx][1] + '\']').prop('disabled',false);
				}
			};
		}
	)
};
