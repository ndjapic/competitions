program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort
uses
	generics.collections, generics.defaults;
const
	NN = 300 * 1000;
var
	n, i, l, r, x: int32;
	ans: int64;
	a: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n);
	a := tlist<int32>.create;

	for i := 1 to n do begin
		read(x);
		a.add(x);
		a.exchange(i-1, random(i));
	end;
	readln;
	a.sort;

	ans := 0;

	x := 0;
	r := 0;
	while (r < n) and (a[r] <= 0) do inc(r);
	l := r-1;

	while (l > -1) or (r < n) do
		if (l = -1) or (r < n) and (a[r] - x < x - a[l]) then begin
			inc(ans, a[r] - x);
			x := a[r];
			inc(r);
		end else begin
			inc(ans, x - a[l]);
			x := a[l];
			dec(l);
		end;

	writeln(ans);
	a.free;
end.
