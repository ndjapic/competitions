program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, m, i, a: int8;
	b: char;
	taro: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for a := 1 to n do taro[a] := false;

	for i := 1 to m do begin
		readln(a, b, b);
		if taro[a] or (b = 'F') then
			writeln('No')
		else begin
			writeln('Yes');
			taro[a] := true;
		end;
	end;
end.
