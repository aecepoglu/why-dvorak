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

	let chars = text.split("");

	chars
		.map(c => layout[c])
		.forEach((cur, index, list) => {
			if (!cur) {
				return;
			}

			distanceCount += cur.distance;

			let prev = index > 0
				? list[index - 1]
				: undefined;

			if (prev && prev.hand == cur.hand) {
				sameHandCount ++;

				if (prev.finger == cur.finger) {
					sameFingerCount ++;
				}
			}
		});

	let factor = 100 / chars.length;

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
