program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
var
	k, m, i, j, a, b, ans: int32;
	win: tdictionary<int32, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(k, m);

	win := tdictionary<int32, boolean>.create;
	for i := 1 to k do begin
		read(a);
		win.AddOrSetValue(a, true);
	end;
	readln;

	ans := 0;
	for j := 1 to m do begin
		read(b);
		if win.ContainsKey(b) then inc(ans);
	end;
	readln;

	writeln(ans);
	win.free;
end.
