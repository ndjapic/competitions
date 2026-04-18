# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults, math;
var
	n, k, i, m, j, p: int32;
	ans, c: int64;
	profit: tlist<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k);

	profit := tlist<int64>.create;
	for i := 0 to n-1 do begin
		read(c, m);
		for j := 0 to m-1 do begin
			read(p);
			dec(c, p);
		end;
		readln;
		profit.add(-c);
		profit.exchange(i, random(i+1));
	end;
	profit.sort;

	ans := 0;
	for i := n-k to n-1 do
		inc(ans, max(0, profit[i]));

	writeln(ans);
	profit.free;
end.

```
