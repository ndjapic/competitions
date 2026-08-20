program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 50;
var
	n, m, i, j, a, b: int8;
	ans: int32;
	mood: array [1 .. nn, 1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, m);

	for a := 2 to n do
		for b := 1 to a-1 do
			mood[a, b] := false;

	for i := 1 to m do begin
		read(a);
		for j := 2 to n do begin
			read(b);
			mood[max(a, b), min(a, b)] := true;
			a := b;
		end;
		readln;
	end;

	ans := 0;
	for a := 2 to n do
		for b := 1 to a-1 do
			if not mood[a, b] then inc(ans);

	writeln(ans);
end.
