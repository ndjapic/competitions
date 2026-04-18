# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults;
var
	n, i, k, ai, s, l, r: int32;
	ans: int64;
	b: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k);

	s := 0;
	b := tlist<int32>.create;
	b.add(0);

	for i := 0 to n-1 do begin
		read(ai);
		s := (s + ai) mod k;
		b.add(s);
		b.exchange(i, random(i+1));
	end;
	readln;
	b.sort;

	ans := 0;
	l := 0;
	for r := 0 to n do
		if b[l] < b[r] then
			l := r
		else
			inc(ans, r-l);

	writeln(ans);
	b.Free;
end.

```
