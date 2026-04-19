# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Classes;
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

	if sl[0] < sl[1] then
		writeln('Yes')
	else
		writeln('No');

	sl.Free;
end.

```
