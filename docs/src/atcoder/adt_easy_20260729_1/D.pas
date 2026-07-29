program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int32;
	d: array [1 .. 3] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function is326(n: int32): boolean;
var
	i: int8;
begin
	for i := 3 downto 1 do begin
		d[i] := n mod 10;
		n := n div 10;
	end;
	result := d[1] * d[2] = d[3];
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	while not is326(n) do inc(n);

	writeln(n);
end.
