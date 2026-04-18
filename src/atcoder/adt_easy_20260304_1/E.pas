program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 10 * 1000 * 1000;
var
	n, x, y, z, k, i: int32;
	good: array [1 .. nn] of int32;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for z := 1 to n do good[z] := 0;

	x := 1;
	while sqr(x) < n do begin
		y := x+1;
		z := sqr(x) + sqr(y);
		while z <= n do begin
			inc(good[z]);
			inc(y);
			z := sqr(x) + sqr(y);
		end;
		inc(x);
	end;

	k := 0;
	for z := 1 to n do
		if good[z] = 1 then begin
			inc(k);
			a[k] := z;
		end;

	writeln(k);
	for i := 1 to k-1 do write(a[i], ' ');
	if k > 0 then writeln(a[k]);
end.
