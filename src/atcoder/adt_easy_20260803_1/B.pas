program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int32;
	e: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for e := 1 to 2 do begin
		n := (10 * n + n div 100) mod 1000;
		write(n);
		if e = 1 then write(' ');
	end;
	writeln;
end.
