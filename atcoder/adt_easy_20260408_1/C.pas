program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 30;
var
	n, m, l, r, e: int8;
	ans: int16;
	all: int32;
	s: string;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	ans := 0;
	all := (1 shl m) - 1;
	for r := 1 to n do begin
		readln(s);
		a[r] := 0;
		for e := 0 to m-1 do
			if s[e+1] = 'o' then inc(a[r], 1 shl e);
		for l := 1 to r-1 do
			if a[l] or a[r] = all then inc(ans);
	end;

	writeln(ans);
end.
