var layouts = [];

function registerLayout(name, layout) {
	layouts.push({
		name: name,
		layout: layout
	});

	console.log("registered layout", name, layout);
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

	//console.log("same hand count: " + sameHandCount + ". normalized: " + (sameHandCount / chars.length));
	//console.log("same finger count: " + sameFingerCount + ". normalized: " + (sameFingerCount / chars.length));
	//console.log("distance traveled: " + distanceCount + ". normalized: " + (distanceCount / chars.length));

	return {
		sameHandCount: sameHandCount,
		sameFingerCount: sameFingerCount,
		distanceTravelled: distanceCount,
		length: chars.length
	};
}
