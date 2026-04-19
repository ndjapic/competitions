# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
var
	n, k, i, elm: int32;
	ans: int64;
	d: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	d := TList<int32>.Create;

	readln(n, k);

	for i := 0 to n-1 do begin
		read(elm);
		d.Add(elm);
	end;
	readln;
	d.Sort;

	ans := 0;
	for i := k to n-1 do inc(ans, d[i]);
	writeln(ans);

	d.Free;
end.

```
