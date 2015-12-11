
function f (x) {
	if (x == 1) {
		1
	} else {
		x * f(x - 1)
	}
}

function main (arg) {
	f(10)
}