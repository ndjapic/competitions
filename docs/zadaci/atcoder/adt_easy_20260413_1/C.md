# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	classes, strutils;
const
	nn = 1000;
var
	n, m, i, j, ans: int32;
	s: string;
	sl, tl: TStringList;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	sl := TStringList.Create;
	for i := 0 to n-1 do begin
		readln(s);
		sl.Add(RightStr(s, 3));
	end;
	sl.Sort;

	tl := TStringList.Create;
	for j := 0 to m-1 do begin
		readln(s);
		tl.Add(s);
	end;
	tl.Sort;

	ans := 0;
	j := 0;

	for i := 0 to n-1 do begin
		while (j < m) and (tl[j] < sl[i]) do inc(j);
		if (j < m) and (tl[j] = sl[i]) then inc(ans);
	end;

	writeln(ans);

	sl.Free;
	tl.Free;
end.

```
