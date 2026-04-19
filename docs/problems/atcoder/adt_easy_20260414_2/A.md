# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	SysUtils, Classes;
var
	a, b: int8;
	s: string;
	sl: TStringList;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	sl := TStringList.Create;
	sl.Delimiter := 'x';
	readln(s);
	sl.DelimitedText := s;

	a := StrToInt(sl[0]);
	b := StrToInt(sl[1]);

	writeln(a * b);
	FreeAndNil(sl);
end.

```
