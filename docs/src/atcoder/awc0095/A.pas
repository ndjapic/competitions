program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100 * 1000;
var
	n, m, i, k, c: int32;
	d: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	for i := 1 to n do d[i] := 0;

	while m > 0 do begin
		dec(m);
		read(i, c, k);
		inc(d[i], c);
		c := c div k;
		while k > 0 do begin
			dec(k);
			read(i);
			dec(d[i], c);
		end;
		readln;
	end;

	for i := 1 to n do writeln(d[i]);
end.
