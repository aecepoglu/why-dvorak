var layouts = [];

function registerLayout(name, layout, isEnabled) {
	layouts.push({
		name: name,
		layout: layout,
		isEnabled: isEnabled !== false
	});

	console.log("registered layout", name);
}

function getFile(path) {
	return new Promise(function(resolve, reject) {
		var xhr = new XMLHttpRequest();
		xhr.onreadystatechange = function() {
			if (xhr.readyState == 4) {
				if (xhr.status == 200) {
					resolve(xhr.responseText);
				} else {
					reject(xhr);
				}
			 }
		};
		xhr.open("GET", path);
		xhr.send();
	});
}

function foo(text, layout) {
	let sameHandCount = 0;
	let sameFingerCount = 0;
	let distanceCount = 0;
	let count = 0;

	let chars = text.toLowerCase().split("");

	var prev;
	chars.forEach((c, index, list) => {
		let cur = layout[c];

		if (!cur) {
			//console.debug("skipped '" + c + "' in " + layout.name);
			return;
		}

		distanceCount += cur.distance;
		count ++;

		if (cur.hand == "any") {
			prev = undefined;
		} else if (prev && prev.hand == cur.hand) {
			sameHandCount ++;

			if (prev.finger == cur.finger) {
				sameFingerCount ++;
			}
		}

		prev = cur;
	});

	let factor = 100 / count;

	return {
		sameHandCount: sameHandCount * factor,
		sameFingerCount: sameFingerCount * factor,
		distanceTravelled: distanceCount * factor
	};
}

function analyze(text) {
	return layouts
		.filter(l => l.isEnabled)
		.map(l => {
			let x = foo(text, l.layout);

			return {
				layout: l.name,
				stats: x
			};
		});
}
