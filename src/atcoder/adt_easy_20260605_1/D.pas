program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	AA = 1000;
var
	n, i: int8;
	a: int32;
	seen: array [1 .. AA] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for a := 1 to AA do seen[a] := false;

	for i := 1 to n do begin
		read(a);
		seen[a] := true;
	end;
	readln;

	a := 1;
	while not seen[a] do inc(a);
	while seen[a] do inc(a);

	writeln(a);
end.
