program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, m, i, j, ans: int32;
	c: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(c[i]); readln;

	ans := 0;
	for j := 1 to m do begin
		readln(i);
		if c[i] > 0 then begin
			dec(c[i]);
			inc(ans);
		end;
	end;

	writeln(ans);
end.
