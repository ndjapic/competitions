program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, m, i, a: int8;
	b: char;
	seen: array [1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for a := 1 to n do seen[a] := false;

	for i := 1 to m do begin
		readln(a, b, b);
		if not seen[a] and (b = 'M') then begin
			seen[a] := true;
			writeln('Yes');
		end else
			writeln('No');
	end;
end.
