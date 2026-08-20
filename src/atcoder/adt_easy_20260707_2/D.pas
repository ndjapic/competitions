program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 201;
var
	n, i, j, k, q: int32;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k, q);

	for j := 1 to k do read(a[j]);
	a[k+1] := n+1;
	readln;

	for i := 1 to q do begin
		read(j);
		if a[j+1] - a[j] > 1 then inc(a[j]);
	end;
	readln;

	for j := 1 to k-1 do write(a[j], ' ');
	writeln(a[k]);
end.
