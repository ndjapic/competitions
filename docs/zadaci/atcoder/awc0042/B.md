# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults;
var
	n, i, di: int32;
	s, t: int64;
	d: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, s, t);
	d := tlist<int32>.create;

	for i := 0 to n-1 do begin
		read(di);
		d.add(di);
		d.exchange(i, random(i+1));
	end;
	readln;
	d.sort;

	for i := 0 to n-1 do
		if d[i] <= s then inc(s, d[i]);

	if s >= t then
		writeln('Yes')
	else
		writeln('No');
end.

```
