# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
var
	n, m, i, j, l, r, x: int32;
	a, b: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ReadAndSort(n: int32): TList<int32>;
var
	i, ai: int32;
begin
	Result := TList<int32>.Create;
	Result.Capacity := n;
	for i := 0 to n-1 do begin
		Read(ai);
		Result.Add(ai);
		Result.Exchange(i, Random(i+1));
	end;
	ReadLn;
	Result.Sort;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	ReadLn(n, m);
	a := ReadAndSort(n);
	b := ReadAndSort(m);

	l := 0;
	r := 1 shl 30;
	while r-l > 1 do begin
		x := (l+r) div 2;

		i := 0;
		while (i < n) and (a[i] <= x) do inc(i);

		j := 0;
		while (j < m) and (b[m-1-j] >= x) do inc(j);

		if i >= j then
			r := x
		else
			l := x;
	end;

	WriteLn(r);
	a.Free;
	b.Free;
end.

```
