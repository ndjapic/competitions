# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	{Generics.Defaults, Generics.Collections,} sysutils, classes{, math};
var
	notc, tci: int32;
	n, w: int32;
	sl: TStringList;
	ios: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	sl := TStringList.Create;
	sl.Delimiter := ' ';

	readln(notc);
	for tci := 1 to notc do begin

		readln(ios);
		sl.DelimitedText := ios;

		n := StrToInt(sl[0]);
		w := StrToInt(sl[1]);

		dec(n, n div w);

		sl.Clear;
		sl.Add(IntToStr(n));

		writeln(sl.DelimitedText);
		flush(StdErr); flush(output); // DO NOT REMOVE

	end;

	FreeAndNil(sl);
end.

```
