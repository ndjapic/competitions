# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	{Generics.Defaults, Generics.Collections,} sysutils, classes, math;
const
	nn = 200 * 1000;
var
	notc, tci: int32;
	n, i, x, y: int32;
	a: array [1 .. nn] of int32;
	s: array [0 .. nn] of int64;
	ans: int64;
	iosl: TStringList;
	ios: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	iosl := TStringList.Create;
	iosl.Delimiter := ' ';

	readln(notc);
	for tci := 1 to notc do begin

		readln(ios);
		iosl.DelimitedText := ios;
		n := StrToInt(iosl[0]);
		x := StrToInt(iosl[1]);
		y := StrToInt(iosl[2]);

		readln(ios);
		iosl.DelimitedText := ios;
		i := 0;
		s[0] := 0;
		for ios in iosl do begin
			inc(i);
			a[i] := StrToInt(ios);
			s[i] := s[i-1] + a[i] div x * y;
		end;

		ans := 0;
		for i := 1 to n do
			ans := max(ans, s[i-1] + a[i] + s[n] - s[i]);

		iosl.Clear;
		iosl.Add(IntToStr(ans));
		writeln(iosl.DelimitedText);
		flush(StdErr); flush(output); // DO NOT REMOVE

	end;

	FreeAndNil(iosl);
end.

```
