# Задатак: A.pas

```pascal
program _A;
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

	sl := TStringList.Create;
	sl.Delimiter := ' ';

	readln(ios);
	sl.DelimitedText := ios;

	write(sl[0]);
	writeln(' san');

	sl.Free;
end.

```
