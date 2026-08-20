program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	SysUtils;
const
	N = 8;
var
	i, j: int8;
	Line: string;
	Tokens: TStringArray;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	Readln(Line);
	Tokens := Line.Split(['-'], TStringSplitOptions.ExcludeEmpty);
	i := StrToInt(Tokens[0]);
	j := StrToInt(Tokens[1]);

	inc(j);
	if j > N then begin
		inc(i);
		j := 1;
	end;

	writeln(i, '-', j);
end.
