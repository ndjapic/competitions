# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Classes, SysUtils;
const
	nn = 100;
var
	n, i, j, i0: int32;
	ios: string;
	sl: TStringList;
	s: array [1 .. nn] of string;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	sl := TStringList.Create;
	sl.Delimiter := ' ';

	i0 := 1;
	for i := 1 to n do begin
		readln(ios);
		sl.DelimitedText := ios;
		s[i] := sl[0];
		a[i] := StrToInt(sl[1]);
		if a[i] < a[i0] then i0 := i;
	end;

	for j := i0 to i0 + n - 1 do
		writeln(s[(j-1) mod n + 1]);

	sl.Free;
end.

```
