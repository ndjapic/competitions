# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Classes;
const
	nn = 100;
var
	n, i, i0: int32;
	ios: string;
	sl: TStringList;
	prev, c: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	sl := TStringList.Create;

	i0 := 1;
	for i := 1 to n do begin
		readln(ios);
		sl.Add(ios);
	end;
	sl.Sort;

	prev[0] := -1;
	for i := 1 to n-1 do
		if sl[i-1] = sl[i] then
			prev[i] := prev[i-1]
		else
			prev[i] := i-1;

	i0 := 0;
	for i := 0 to n-1 do begin
		c[i] := i - prev[i];
		if c[i] > c[i0] then i0 := i;
	end;

	writeln(sl[i0]);

	sl.Free;
end.

```
