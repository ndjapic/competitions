# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	classes, sysutils;
const
	nn = 500;
var
	n, k, i, j, x, y, c, cs, ans: int32;
	m: array [1 .. nn] of int8;
	w: array [1 .. nn] of TStringList;
	ios: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do begin
		readln(m[i]);
		readln(ios);
		w[i] := TStringList.Create;
		w[i].Delimiter := ' ';
		w[i].DelimitedText := ios;
		w[i].Sort;
	end;

	ans := 0;
	for i := 1 to n do begin
		for j := i+1 to n do begin
			c := 0;
			x := m[i] - 1;
			y := m[j] - 1;

			while (x >= 0) and (y >= 0) do begin
				cs := CompareStr(w[i][x], w[j][y]);
				if cs = 0 then inc(c);
				if cs >= 0 then dec(x);
				if cs <= 0 then dec(y);
			end;

			if c >= k then inc(ans);
		end;
		w[i].Free;
	end;
	writeln(ans);
end.

```
