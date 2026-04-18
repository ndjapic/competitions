# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
const
	nn = 200 * 1000;
var
	n, i, ai: int32;
	s, ans: int64;
	a: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	a := tlist<int32>.create;
	for i := 0 to n-1 do begin
		read(ai);
		a.add(ai);
		a.exchange(i, random(i+1));
	end;
	readln;
	a.sort;

	ans := 0;
	s := 0;
	for i := 0 to n-1 do begin
		inc(ans, int64(i) * a[i] - s);
		inc(s, a[i]);
	end;

	writeln(ans);
	a.free;
end.

```
