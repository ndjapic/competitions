# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
const
	nn = 200 * 1000;
var
	n, k, i, a, b: int32;
	ans: int64;
	d: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	ans := 0;
	d := TList<int32>.Create;

	for i := 1 to n do begin
		readln(a, b);
		inc(ans, a);
		d.Add(a-b);
	end;
	d.Sort;

	for i := n-k to n-1 do dec(ans, d[i]);

	writeln(ans);
	d.Free;
end.

```
