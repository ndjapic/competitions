# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	classes, sysutils;
var
	n, i, k: int32;
	s: string;
	sl: TStringList;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	sl := TStringList.Create;
	sl.Delimiter := ' ';

	for i := 1 to n do begin
		readln(s);
		sl.DelimitedText := s;
		k := StrToInt(sl[1]);
		if not odd(k) then
			writeln(sl[0])
		else if sl[0] = 'Yes' then
			writeln('No')
		else
			writeln('Yes');
	end;

	sl.Free;
end.

```
