program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	CC = 100;
var
	n, i, y: int32;
	s, x: int8;
	seen: array [1 .. CC] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for s := 1 to CC do seen[s] := false;

	readln(n);

	for i := 1 to n do begin
		read(s);
		seen[s] := true;
	end;
	readln;

	x := 0;
	y := 0;
	for s := 1 to CC do
		if seen[s] then begin
			inc(x);
			inc(y, s);
		end;

	writeln(x, ' ', y);
end.
