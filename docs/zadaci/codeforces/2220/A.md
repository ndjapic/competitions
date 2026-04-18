# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
var
	notc, tci, n, i, x: int32;
	a: tlist<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;
	a := tlist<int8>.create;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		a.clear;
		for i := 0 to n-1 do begin
			read(x);
			a.add(x);
			a.exchange(i, random(i+1));
		end;
		readln;
		a.sort;

		i := 1;
		while (i < n) and (a[i-1] < a[i]) do inc(i);

		if i < n then
			writeln(-1)
		else begin
			for i := 1 to n-1 do write(a[n-i], ' ');
			writeln(a[0]);
		end;

	end;
	a.free;
end.

```
