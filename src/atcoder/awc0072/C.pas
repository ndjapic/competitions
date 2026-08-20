program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 200 * 1000 + 1;
var
	n, q, i, j, l, r: int32;
	s, d: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do read(s[i]);
	readln;

	for i := 1 to n+1 do d[i] := 0;

	for j := 1 to q do begin
		readln(l, r);
		inc(d[l]);
		dec(d[r+1]);
	end;

	for i := 1 to n do begin
		write(max(s[i] - d[i], 0));
		if i < n then write(' ');
		inc(d[i+1], d[i]);
	end;
	writeln;
end.
