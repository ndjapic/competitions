program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i, x: int8;
	seen: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for x := 1 to NN do seen[x] := false;

	readln(n);

	for i := 1 to n do begin
		read(x);
		seen[x] := true;
	end;
	readln;

	readln(x);
	if seen[x] then
		writeln('Yes')
	else
		writeln('No');
end.
