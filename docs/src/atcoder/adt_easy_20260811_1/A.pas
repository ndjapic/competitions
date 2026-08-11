program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	SysUtils;
var
	a, b: int8;
	Line: string;
	Tokens: TStringArray; // Није потребно декларисати дужину, Split је сам одређује
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	Readln(Line);
	Tokens := Line.Split(['x'], TStringSplitOptions.ExcludeEmpty);
	a := StrToInt(Tokens[0]);
	b := StrToInt(Tokens[1]);

	writeln(a * b);
end.
