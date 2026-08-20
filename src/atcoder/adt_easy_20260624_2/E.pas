program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #prefix #sufix #sum
uses
	math;
const
	NN = 200 * 1000;
var
	n, i, j, k, x, l, r: int32;
	w, ans: int64;
	c: array [1 .. NN] of int8;
	a: array [1 .. 2 * NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do c[i] := 2;

	for j := 1 to k do begin
		read(i);
		dec(c[i]);
	end;
	readln;

	x := 0;
	for i := 1 to n do
		while c[i] > 0 do begin
			inc(x);
			a[x] := i;
			dec(c[i]);
		end;

	r := x+1;
	w := 0;
	while r-2 > 0 do begin
		dec(r, 2);
		inc(w, a[r+1] - a[r]);
	end;

	ans := w;
	l := 0;
	while l+2 <= x do begin
		dec(w, a[r+1] - a[r]);
		inc(r, 2);
		inc(l, 2);
		inc(w, a[l] - a[l-1]);
		ans := min(ans, w);
	end;

	writeln(ans);
end.
