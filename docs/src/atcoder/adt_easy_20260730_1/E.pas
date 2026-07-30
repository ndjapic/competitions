program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	k: int64;
	e: int8;
	d: array [0 .. 63] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(k);

	e := 0;
	while k > 0 do begin
		d[e] := k mod 2 * 2;
		k := k div 2;
		inc(e);
	end;

	while e > 0 do begin
		dec(e);
		write(d[e]);
	end;
	writeln;
end.
