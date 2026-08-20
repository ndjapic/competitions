program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults, SysUtils;
var
	n, i: int8;
	t: int32;
	s: TList<string>;
	Line: string;
	Tokens: TStringArray;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n);

	s := TList<string>.Create;
	t := 0;

	for i := 1 to n do begin
		Readln(Line);
		Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);

		s.Add(Tokens[0]);
		s.Exchange(i-1, Random(i));

		inc(t, StrToInt(Tokens[1]));
	end;
	s.Sort;

	writeln(s[t mod n]);
	s.Free;
end.


