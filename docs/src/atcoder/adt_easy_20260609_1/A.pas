program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i, a, ans: int8;
	c: array [1 .. 4] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for a := 1 to 4 do c[a] := 0;

	for i := 1 to 4 do begin
		read(a);
		inc(c[a]);
	end;
	readln;

	ans := 0;
	for a := 1 to 4 do inc(ans, c[a] div 2);
	writeln(ans);
end.
