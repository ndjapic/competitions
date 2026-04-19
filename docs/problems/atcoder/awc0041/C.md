# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults, math;
var
	n, i, j, k, ai: int32;
	ans: int64;
	a: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k);

	a := tlist<int32>.create;
	for i := 0 to n-1 do begin
		read(ai);
		a.add(ai);
		a.exchange(i, random(i+1));
	end;
	readln;
	a.sort;

	ans := 0;
	i := 0;
	for j := n-1 downto 0 do begin
		while (i < j) and (a[i] + a[j] < k) do inc(i);
		inc(ans, max(j-i, 0));
	end;

	writeln(ans);
	a.free;
end.

```
