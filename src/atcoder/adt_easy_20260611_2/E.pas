program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, t, i, l, r, ans: int32;
	a: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, t);

	for i := 1 to n do read(a[i]);
	readln;

	l := 0;
	r := n;
	while (l < t) and (a[l+1] = 1) do inc(l);
	while (r > t) and (a[r] = 1) do dec(r);

	ans := r-l;
	i := r;
	while i > l do begin
		if a[i] = 1 then inc(ans);
		dec(i);
	end;

	writeln(ans);
end.
