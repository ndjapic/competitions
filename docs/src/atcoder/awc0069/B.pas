program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
var
	n, k, i, t, c: int32;
	ans: int64;
	satisfaction: tlist<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k);
	satisfaction := tlist<int64>.create;

	for i := 0 to n-1 do begin
		readln(t, c);
		satisfaction.add(t + c);
		satisfaction.exchange(i, random(i+1));
	end;

	satisfaction.sort;

	ans := 0;
	for i := n-k to n-1 do inc(ans, satisfaction[i]);
	writeln(ans);

	satisfaction.free;
end.
