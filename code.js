const STR1 = "during the whole of a dull, dark, and soundless day in the autumn of the year, when the clouds hung oppressivhandy low in the heaven";
//const STR2 = "Sphinx of black quartz, judge my vow. How vexingly quick daft zebras jump! The five boxing wizards jump quickly."
const STR2 = " The Colemak layout was designed with the QWERTY layout as a base, changing the positions of 17 keys while retaining the QWERTY positions of most non-alphabetic characters and many popular keyboard shortcuts, supposedly making it easier to learn than Dvorak layout for people who already type in QWERTY without losing efficiency. It shares several design goals with the Dvorak layout, such as minimizing finger path distance and making heavy use of the home row.[3] 74% of typing is done on the home row compared to 70% for Dvorak and 32% for QWERTY.[4] The Colemak layout lacks a Caps Lock key; an additional Backspace key occupies the position typically occupied by Caps Lock on modern keyboards.[1]\
Coleman states that he designed Colemak to be easy to learn, explaining that Dvorak is hard for QWERTY typists to learn due to it being so different from the QWERTY layout.[5] The layout has attracted media attention as an alternative to Dvorak for improving typing speed and comfort with an alternate keyboard layout.[4][6][7]"


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

	console.log("same hand count: " + sameHandCount + ". normalized: " + (sameHandCount / chars.length));
	console.log("same finger count: " + sameFingerCount + ". normalized: " + (sameFingerCount / chars.length));
	console.log("distance traveled: " + distanceCount + ". normalized: " + (distanceCount / chars.length));
};

console.log("\nfor test 1, dvorak\n------");
foo(STR1, DVORAK);

console.log("\nfor test 1, qwerty\n------");
foo(STR1, QWERTY);

console.log("\nfor test 1, f-tr\n------");
foo(STR1, COLEMAK);

console.log("\nfor test 1, colemak\n------");
foo(STR1, F);

console.log("\nfor test 2, dvorak\n-------");
foo(STR2, DVORAK);

console.log("\nfor test 2, qwerty\n-------");
foo(STR2, QWERTY);

console.log("\nfor test 2, colemak\n-------");
foo(STR2, COLEMAK);

console.log("\nfor test 2, f-tr\n-------");
foo(STR2, F);
