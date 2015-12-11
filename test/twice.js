function main (arg) {
	var f = function (x) x;
	f(f)(function (y) y)
}