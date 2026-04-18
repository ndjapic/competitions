# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	classes;
var
	sl: TStringList;
	ios: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(ios);

	sl := TStringList.Create;
	sl.Delimiter := '|';
	sl.DelimitedText := ios;

	write(sl[0]);
	writeln(sl[2]);

	sl.Free;
end.

```
