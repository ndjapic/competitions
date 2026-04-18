# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
const
	nn = 200 * 1000;
var
	n, m, i, j, elm: int32;
	ans: int64;
	d, s: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	d := TList<int32>.Create;
	s := TList<int32>.Create;

	readln(n, m);

	for i := 0 to n-1 do begin
		read(elm);
		d.Add(elm);
	end;
	readln;
	d.Sort;

	for j := 0 to m-1 do begin
		read(elm);
		s.Add(elm);
	end;
	readln;
	s.Sort;

	j := 0;
	ans := 0;
	for i := 0 to n-1 do begin
		while (j < m-1) and (abs(d[i] - s[j+1]) <= abs(d[i] - s[j])) do inc(j);
		inc(ans, abs(d[i] - s[j]));
	end;

	writeln(ans);

	d.Free;
	s.Free;
end.

```
