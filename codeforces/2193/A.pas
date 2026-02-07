program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes, math;
var
	notc, tci: int32;
	n, s, x: int32;
	sl: TStringList;
	ios, str: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	sl := TStringList.Create;
	sl.Delimiter := ' ';

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, s, x); // Note: Local variable "n" is assigned but never used
		readln(ios);
		sl.DelimitedText := ios;

		for str in sl do dec(s, StrToInt(str));

		sl.Clear;
		if (s >= 0) and (s mod x = 0) then
			sl.Add('YES')
		else
			sl.Add('NO');

		writeln(sl.DelimitedText);
		flush(StdErr); flush(output); // DO NOT REMOVE

	end;

	FreeAndNil(sl);
end.
