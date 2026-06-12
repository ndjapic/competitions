program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unfinished
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, x, ans: int32;
	win: boolean;
	a, c: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		for x := 1 to n do c[x] := 0;

		for i := 1 to n do begin
			read(x);
			inc(c[x]);
		end;
		readln;

		x := n;
		while (x > 0) and (c[x] = 0) do dec(x);
		win := not odd(c[x]);

		writeln(ans);

	end;
end.
