program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, strutils;
const
	NN = 1000;
var
	n, m, i, j, ans: int32;
	s: array [1 .. NN] of string;
	tj: string;
	t: tdictionary<string, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do readln(s[i]);

	t := tdictionary<string, boolean>.create;
	for j := 1 to m do begin
		readln(tj);
		t.AddOrSetValue(tj, true);
	end;

	ans := 0;
	for i := 1 to n do
		if t.ContainsKey(rightstr(s[i], 3)) then inc(ans);

	writeln(ans);
	t.free;
end.
