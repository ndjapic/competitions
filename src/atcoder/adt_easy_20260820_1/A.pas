program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int32;
	e: int8;
	d: array [0 .. 3] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for e := 0 to 3 do begin
		d[e] := n mod 10;
		n := n div 10;
	end;

	for e := 3 downto 0 do write(d[e]);
	writeln;
end.
