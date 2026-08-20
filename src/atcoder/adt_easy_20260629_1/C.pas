program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults, math;
const
	NN = 100;
var
	n, i, x, ai, s: int32;
	a: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, x);

	s := 0;
	a := tlist<int32>.create;
	for i := 0 to n-2 do begin
		read(ai);
		inc(s, ai);
		a.add(ai);
		a.exchange(i, random(i+1));
	end;
	readln;
	a.sort;

	// mx = (s - a[0]) / (n-2)
	// mn = (s - a[n-2]) / (n-2)
	// (s - a[0] - a[n-2] + ans) / (n-2) >= x
	// (s - a[0] - a[n-2] + ans) >= (n-2) * x
	// (s - a[0] - a[n-2] + ans) >= (n-2) * x

	if s - a[0] < x then
		writeln(-1)
	else if s - a[n-2] >= x then
		writeln(0)
	else
		writeln(x - s + a[0] + a[n-2]);

	a.free;
end.
