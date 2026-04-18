# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
var
	n, l, k, i, elm, s: int32;
	a: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	a := TList<int32>.Create;

	readln(n, l, k);

	for i := 0 to n-1 do begin
		read(elm);
		a.Add(elm);
	end;
	readln;
	a.Sort;

	i := 0;
	s := 0;
	while (i < n) and (i <= k) and (s + a[i] <= l) do begin
		inc(s, a[i]);
		inc(i);
	end;

	writeln(i);

	a.Free;
end.

```
