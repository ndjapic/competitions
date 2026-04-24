program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i, a, a3, a2: int8;
	c: array [1 .. 13] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for a := 1 to 13 do c[a] := 0;

	for i := 1 to 7 do begin
		read(a);
		inc(c[a]);
	end;
	readln;

	a3 := 0;
	for a := 1 to 13 do
		if c[a] >= 3 then a3 := a;

	a2 := 0;
	for a := 1 to 13 do
		if (a <> a3) and (c[a] >= 2) then a2 := a;

	if (a2 > 0) and (a3 > 0) then
		writeln('Yes')
	else
		writeln('No');
end.
