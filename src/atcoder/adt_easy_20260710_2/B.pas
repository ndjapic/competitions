program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	SysUtils;
var
	x, y: int8;
	Line: string;
	Tokens: TStringArray;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	Readln(Line);
	Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);

	case Tokens[0][1] of
		'O': x := 1;
		'S': x := 2;
		'L': x := 3;
	end;

	case Tokens[1][1] of
		'O': y := 1;
		'S': y := 2;
		'L': y := 3;
	end;

	if x >= y then
		writeln('Yes')
	else
		writeln('No');
end.
