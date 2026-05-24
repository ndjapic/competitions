program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 100 * 1000;
var
	notc, tci, n, i, x: int32;
	s, ans: int64;
	a, b: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;
		for i := 1 to n do read(b[i]); readln;

		s := 0;
		ans := 0;
		for i := 1 to n do begin
			if a[i] > b[i] then begin
				x := a[i];
				a[i] := b[i];
				b[i] := x;
			end;
			inc(s, b[i]);
			ans := max(ans, a[i]);
		end;

		inc(ans, s);
		for i := 1 to n do
			ans := max(ans, s + a[i]);

		writeln(ans);

	end;
end.
