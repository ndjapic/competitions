program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i, j: int8;
	a, f: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function fun(x: int32): int32;
var
	y: int32;
begin
	if f[x] = 0 then begin
		y := x;
		while y > 0 do begin
			inc(f[x], y mod 10);
			y := y div 10;
		end;
	end;
	result := f[x];
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	a[0] := 1;

	for i := 0 to n do f[i] := 0;

	for i := 1 to n do begin
		a[i] := 0;
		for j := 0 to i-1 do inc(a[i], fun(a[j]));
	end;

	writeln(a[n]);
end.
