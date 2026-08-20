program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, x, k, p: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x);

	for k := 1 to n do begin
		read(p);
		if p = x then writeln(k);
	end;
	readln;
end.
