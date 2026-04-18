program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int32;
	d: array [0 .. 2] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function is326like(n: int32): boolean;
var
	i: int8;
begin
	for i := 0 to 2 do begin
		d[i] := n mod 10;
		n := n div 10;
	end;

	result := d[2] * d[1] = d[0];
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	while not is326like(n) do inc(n);
	writeln(n);
end.
