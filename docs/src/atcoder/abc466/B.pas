program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	MM = 100;
var
	n, m, i, c, s: int8;
	ans: array [1 .. MM] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	Readln(n, m);

	for c := 1 to m do ans[c] := -1;

	for i := 1 to n do begin
		Readln(c, s);
		ans[c] := max(ans[c], s);
	end;

	for c := 1 to m-1 do write(ans[c], ' ');
	writeln(ans[m]);
end.
