# Задатак: B.pas

```pascal
program _B;
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

	if sl[1] = 'Ocelot' then
		writeln('Yes')
	else if sl[0] = 'Lynx' then
		writeln('Yes')
	else if sl[0] = 'Ocelot' then
		writeln('No')
	else if sl[1] = 'Lynx' then
		writeln('No')
	else
		writeln('Yes');

	sl.Free;
end.

```
