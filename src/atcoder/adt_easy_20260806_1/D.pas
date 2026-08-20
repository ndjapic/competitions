program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	SysUtils;
const
	NN = 10;
var
	n, i: int8;
	s, c, Line: string;
	ch: char;
	Tokens: TStringArray;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	Readln(n);
	setlength(c, n);

	Readln(Line);
	Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);

	for i := 1 to n do begin
		s := Tokens[i-1];
		ch := s[1];

		if ch < 'd' then
			c[i] := '2'
		else if ch < 'g' then
			c[i] := '3'
		else if ch < 'j' then
			c[i] := '4'
		else if ch < 'm' then
			c[i] := '5'
		else if ch < 'p' then
			c[i] := '6'
		else if ch < 't' then
			c[i] := '7'
		else if ch < 'w' then
			c[i] := '8'
		else
			c[i] := '9';
	end;

	writeln(c);
end.

		Readln(Line);
		// Дели стринг, избацује вишак размака и смешта резултат у Tokens
		Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);
		
		// Сада кроз Tokens пролазите кроз обичан for-in круг (као у Пајтону!)
		// нпр. за конверзију: Број := StrToInt(Tokens[0]);
	end;
end.
