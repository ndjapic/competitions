program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #graph #vertex #degree
const
	NN = 200 * 1000;
var
	n, m, i, a, b: int32;
	c: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ncr(n: int64; r: int8): int64;
begin
	if r = 1 then
		result := n
	else
		result := ncr(n-1, r-1) * n div r;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for a := 1 to n do c[a] := n-1;

	for i := 1 to m do begin
		readln(a, b);
		dec(c[a]);
		dec(c[b]);
	end;

	for a := 1 to n do begin
		write(ncr(c[a], 3));
		if a < n then write(' ');
	end;
	writeln;
end.
